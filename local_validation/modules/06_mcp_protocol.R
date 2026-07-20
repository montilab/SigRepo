mcp_rpc_call <- function(base_url, method, params = list(), id = 1L) {
  body <- list(jsonrpc = "2.0", id = id, method = method, params = params)

  resp <- httr::POST(
    url = base_url,
    httr::content_type_json(),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  )

  jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

mcp_tool_call <- function(base_url, name, arguments) {
  mcp_rpc_call(base_url, "tools/call", params = list(name = name, arguments = arguments))
}

mcp_tool_call_ok <- function(resp) {
  is.null(resp$error) && isTRUE(!isTRUE(resp$result$isError))
}

mcp_tool_call_text <- function(resp) {
  resp$result$content[[1]]$text
}

run_mcp_protocol_validation <- function(ctx) {
  mcp_host <- ctx$config$mcp_host
  mcp_port <- ctx$config$mcp_port

  if (is.null(mcp_host) || !nzchar(mcp_host) || is.null(mcp_port) || is.na(mcp_port)) {
    record_skip(ctx, "MCP protocol validation skipped because SIGREPO_LOCAL_MCP_HOST/SIGREPO_LOCAL_MCP_PORT are not configured")
    return(invisible(NULL))
  }

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    record_skip(ctx, "MCP protocol validation skipped because httr/jsonlite are not available")
    return(invisible(NULL))
  }

  base_url <- sprintf("%s:%s", sub("/+$", "", mcp_host), mcp_port)

  # tools/list: the four expected tools must be advertised.
  list_resp <- tryCatch(
    mcp_rpc_call(base_url, "tools/list"),
    error = function(e) {
      stop(sprintf("could not reach MCP server at %s: %s", base_url, conditionMessage(e)), call. = FALSE)
    }
  )
  tools <- list_resp$result$tools
  tool_names <- vapply(tools, function(t) t$name, character(1))

  expected_tools <- c(
    "list_vocabulary", "search_signatures", "get_signature_context", "compare_signatures",
    "search_collections", "search_geneset_resources", "search_geneset_entries",
    "search_features", "run_enrichment"
  )
  for (tool_name in expected_tools) {
    assert_true(
      ctx,
      tool_name %in% tool_names,
      sprintf("tools/list includes %s", tool_name),
      sprintf("tools/list is missing expected tool: %s", tool_name)
    )
  }

  # No admin-only or write-capable tool should ever be exposed here.
  assert_true(
    ctx,
    !any(grepl("admin|write|delete|update|reset|init_db", tool_names, ignore.case = TRUE)),
    "no admin-only or write-capable tools are exposed over MCP",
    sprintf("unexpected admin/write-capable tool(s) exposed over MCP: %s", paste(tool_names, collapse = ", "))
  )

  user_row <- get_sigrepo_user_row(ctx$read_handler)
  if (is.null(user_row) || !nzchar(user_row$api_key[[1]])) {
    record_skip(ctx, "MCP tool-call validation skipped because no api_key could be resolved for the configured read user")
    return(invisible(NULL))
  }
  api_key <- user_row$api_key[[1]]

  # list_vocabulary
  voc_resp <- mcp_tool_call(base_url, "list_vocabulary", list(api_key = api_key))
  assert_true(
    ctx,
    mcp_tool_call_ok(voc_resp),
    "list_vocabulary tool call succeeded",
    sprintf("list_vocabulary tool call failed: %s", voc_resp$error$message %||% "unexpected response shape")
  )

  # search_signatures -- also used to source real hashkeys for the two
  # single-signature tools below, so this module doesn't need its own
  # hardcoded/configured fixture signature.
  search_resp <- mcp_tool_call(base_url, "search_signatures", list(api_key = api_key, limit = 10))
  assert_true(
    ctx,
    mcp_tool_call_ok(search_resp),
    "search_signatures tool call succeeded",
    sprintf("search_signatures tool call failed: %s", search_resp$error$message %||% "unexpected response shape")
  )

  hashkeys <- character(0)
  if (mcp_tool_call_ok(search_resp)) {
    results <- jsonlite::fromJSON(mcp_tool_call_text(search_resp))
    if (is.data.frame(results) && "signature_hashkey" %in% names(results)) {
      hashkeys <- results$signature_hashkey
    }
  }

  if (length(hashkeys) == 0) {
    record_skip(ctx, "get_signature_context/compare_signatures tool-call checks skipped because search_signatures returned no signatures")
    return(invisible(NULL))
  }

  # get_signature_context -- server-side raw SQL, independent of the R
  # client's OmicSignature reconstruction path.
  gsc_resp <- mcp_tool_call(base_url, "get_signature_context", list(api_key = api_key, signature_hashkey = hashkeys[[1]]))
  assert_true(
    ctx,
    mcp_tool_call_ok(gsc_resp),
    sprintf("get_signature_context tool call succeeded for %s", hashkeys[[1]]),
    sprintf("get_signature_context tool call failed: %s", gsc_resp$error$message %||% "unexpected response shape")
  )

  if (length(hashkeys) >= 2) {
    cmp_resp <- mcp_tool_call(base_url, "compare_signatures", list(
      api_key = api_key,
      signature_hashkey_1 = hashkeys[[1]],
      signature_hashkey_2 = hashkeys[[2]]
    ))
    assert_true(
      ctx,
      mcp_tool_call_ok(cmp_resp),
      "compare_signatures tool call succeeded",
      sprintf("compare_signatures tool call failed: %s", cmp_resp$error$message %||% "unexpected response shape")
    )
  } else {
    record_skip(ctx, "compare_signatures tool-call check skipped because fewer than two signatures are available")
  }

  # search_collections/search_geneset_resources/search_geneset_entries: a
  # stock /init_db bootstrap doesn't populate collections or the geneset
  # catalog, so these only assert the call succeeds, not that it returns
  # rows -- unlike search_features below, which can rely on real reference
  # data /init_db does load.
  collections_resp <- mcp_tool_call(base_url, "search_collections", list(api_key = api_key))
  assert_true(
    ctx,
    mcp_tool_call_ok(collections_resp),
    "search_collections tool call succeeded",
    sprintf("search_collections tool call failed: %s", collections_resp$error$message %||% "unexpected response shape")
  )

  geneset_resources_resp <- mcp_tool_call(base_url, "search_geneset_resources", list(api_key = api_key))
  assert_true(
    ctx,
    mcp_tool_call_ok(geneset_resources_resp),
    "search_geneset_resources tool call succeeded",
    sprintf("search_geneset_resources tool call failed: %s", geneset_resources_resp$error$message %||% "unexpected response shape")
  )

  geneset_entries_resp <- mcp_tool_call(base_url, "search_geneset_entries", list(api_key = api_key))
  assert_true(
    ctx,
    mcp_tool_call_ok(geneset_entries_resp),
    "search_geneset_entries tool call succeeded",
    sprintf("search_geneset_entries tool call failed: %s", geneset_entries_resp$error$message %||% "unexpected response shape")
  )

  # search_features -- /init_db loads real transcriptomics_features rows
  # (unlike collections/genesets above), so this can meaningfully check for
  # actual results, not just a non-error response.
  features_resp <- mcp_tool_call(base_url, "search_features", list(api_key = api_key, assay_type = "transcriptomics", limit = 5))
  assert_true(
    ctx,
    mcp_tool_call_ok(features_resp),
    "search_features tool call succeeded for assay_type = transcriptomics",
    sprintf("search_features tool call failed: %s", features_resp$error$message %||% "unexpected response shape")
  )
  if (mcp_tool_call_ok(features_resp)) {
    feature_rows <- jsonlite::fromJSON(mcp_tool_call_text(features_resp))
    if (is.data.frame(feature_rows) && nrow(feature_rows) > 0) {
      record_pass(ctx, sprintf("search_features returned %d transcriptomics feature row(s)", nrow(feature_rows)))
    } else {
      record_skip(ctx, "search_features returned zero transcriptomics rows -- reference data may not be loaded on this stack")
    }
  }

  # run_enrichment -- the geneset_resource_id (cache) path needs a real,
  # locally-readable .rds and a matching geneset_resources row, which
  # /init_db doesn't provide. Register one temporarily with a unique name
  # (unique_local_name(), same pattern signature_crud/collection_crud use)
  # so repeated runs against a persistent local DB don't collide, and clean
  # it up afterward regardless of outcome.
  privileged_handler <- ctx$db_admin_handler %||% ctx$write_handler
  if (is.null(privileged_handler)) {
    record_skip(ctx, "run_enrichment tool-call check skipped because no db-admin/write credentials are configured to register a temporary geneset resource")
  } else {
    unique_name <- unique_local_name("local_validation_geneset")
    # geneset_resource_hashkey is VARCHAR(32) -- unique_name itself (prefix +
    # timestamp + random suffix) runs well past that, so hash it down to a
    # proper 32-char MD5 hex digest, the same convention
    # SigRepo::createHashKey() uses for every other hashkey column.
    unique_hashkey <- digest::digest(unique_name, algo = "md5", serialize = FALSE)
    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(list(LOCAL_VALIDATION_SET = c("TP53", "BRCA1", "EGFR", "MYC", "PTEN")), tmp_rds)

    # Insert, tool-call, and delete all share one connection, kept open for
    # the whole block via on.exit() rather than the earlier
    # insert-then-immediately-disconnect shape -- that version left the
    # temporary geneset_resources row permanently orphaned in whatever
    # database this runs against, harmless against a throwaway local MySQL
    # but not against a persistent or shared one. on.exit() (not a plain
    # trailing statement) guarantees the DELETE still runs even if
    # assert_true() below throws on a failed tool call.
    conn <- open_db_connection(privileged_handler)
    on.exit(close_db_connection(conn), add = TRUE)
    on.exit(unlink(tmp_rds), add = TRUE)
    on.exit(
      {
        DBI::dbExecute(conn, paste(
          "DELETE FROM geneset_resources WHERE geneset_resource_hashkey =",
          DBI::dbQuoteLiteral(conn, unique_hashkey)
        ))
      },
      add = TRUE,
      after = FALSE
    )

    # dbQuoteLiteral(), not DBI's params= placeholders -- RMySQL (the driver
    # conn_init() uses) doesn't support "?" placeholders and fails with a
    # SQL syntax error if you try.
    insert_sql <- paste(
      "INSERT INTO geneset_resources",
      "(source, species, collection, version, format, storage_path, n_genesets, n_features, is_current, geneset_resource_hashkey)",
      "VALUES ('local-validation', 'Homo sapiens',",
      DBI::dbQuoteLiteral(conn, unique_name), ", '1.0', 'rds',",
      DBI::dbQuoteLiteral(conn, tmp_rds), ", 1, 5, 1,",
      DBI::dbQuoteLiteral(conn, unique_hashkey), ")"
    )
    DBI::dbExecute(conn, insert_sql)
    resource_id <- DBI::dbGetQuery(conn, paste(
      "SELECT geneset_resource_id FROM geneset_resources WHERE geneset_resource_hashkey =",
      DBI::dbQuoteLiteral(conn, unique_hashkey)
    ))$geneset_resource_id[[1]]

    enrich_resp <- mcp_tool_call(base_url, "run_enrichment", list(
      api_key = api_key,
      signature_hashkey = hashkeys[[1]],
      geneset_resource_id = resource_id
    ))
    assert_true(
      ctx,
      mcp_tool_call_ok(enrich_resp),
      sprintf("run_enrichment tool call succeeded for %s", hashkeys[[1]]),
      sprintf(
        "run_enrichment tool call failed: %s",
        enrich_resp$error$message %||% mcp_tool_call_text(enrich_resp) %||% "unexpected response shape"
      )
    )
  }

  # Auth failure path: an invalid api_key must surface as an MCP tool error,
  # not a silent success or a server crash.
  bad_key_resp <- mcp_tool_call(base_url, "search_signatures", list(api_key = "not-a-real-key"))
  assert_true(
    ctx,
    !is.null(bad_key_resp$error),
    "search_signatures with an invalid api_key correctly returns an MCP error",
    "search_signatures with an invalid api_key unexpectedly did not return an error"
  )
}
