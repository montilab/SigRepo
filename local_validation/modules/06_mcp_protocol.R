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

  expected_tools <- c("list_vocabulary", "search_signatures", "get_signature_context", "compare_signatures")
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
