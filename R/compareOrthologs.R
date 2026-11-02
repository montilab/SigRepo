############################################################
# Ortholog translation behind compareSignatures(map_orthologs = TRUE)
############################################################

#' @title translateFeatureTable
#' @description Rewrite a feature table (a signature's `signature` data frame
#' or its `difexp`) onto another identifier space through a query to target
#' mapping. A feature with several targets becomes one row per target, with
#' every other column copied; features without a target are dropped. Rows
#' that then share a target inside one group (or in the whole table when
#' `group_col` is `NULL` or absent) collapse to a single row: the one with
#' the largest absolute `prefer_col` for `"max_abs"`, the smallest for
#' `"min"`, or simply the first when `prefer_col` is not in the table.
#' Queries match case-insensitively and ignore surrounding whitespace.
#' @param tbl Data frame with a `feature_col` column.
#' @param mapping Data frame with `query` and `target` columns, possibly
#'   several targets per query.
#' @param feature_col Name of the feature column in `tbl`.
#' @param group_col Name of the group column, or `NULL`.
#' @param prefer_col Column that decides which duplicate survives.
#' @param prefer `"max_abs"` (largest absolute value wins) or `"min"`
#'   (smallest value wins).
#' @return A list: `table` (the rewritten data frame, same columns as `tbl`),
#'   `n_input` (rows in), `n_unmapped` (input rows with no target) and
#'   `n_output` (rows out).
#' @keywords internal
#' @noRd
translateFeatureTable <- function(tbl, mapping, feature_col, group_col = NULL, prefer_col, prefer = c("max_abs", "min")) {
  prefer <- base::match.arg(prefer)
  norm <- function(x) base::tolower(base::trimws(base::as.character(x)))

  map <- base::data.frame(
    key = norm(mapping$query),
    target = base::as.character(mapping$target),
    stringsAsFactors = FALSE
  )
  map <- map[!base::is.na(map$key) & !base::is.na(map$target) & base::nzchar(map$target), , drop = FALSE]
  map <- map[!base::duplicated(base::paste(map$key, map$target, sep = "\r")), , drop = FALSE]

  rows <- base::data.frame(row = base::seq_len(base::nrow(tbl)), key = norm(tbl[[feature_col]]), stringsAsFactors = FALSE)
  n_unmapped <- base::sum(!(rows$key %in% map$key))

  joined <- base::merge(rows, map, by = "key")
  out <- tbl[joined$row, , drop = FALSE]
  out[[feature_col]] <- joined$target

  # Which rows compete: same target, same group.
  dup_key <- joined$target
  if (!base::is.null(group_col) && group_col %in% base::colnames(out)) {
    dup_key <- base::paste(dup_key, base::as.character(out[[group_col]]), sep = "\r")
  }

  # Order so the preferred row of each duplicate set comes first, then keep it.
  if (prefer_col %in% base::colnames(out)) {
    value <- base::suppressWarnings(base::as.numeric(out[[prefer_col]]))
    rank_value <- if (prefer == "max_abs") -base::abs(value) else value
    ord <- base::order(rank_value, joined$row, na.last = TRUE)
  } else {
    ord <- base::order(joined$row)
  }
  out <- out[ord, , drop = FALSE]
  dup_key <- dup_key[ord]
  source_row <- joined$row[ord]
  keep <- !base::duplicated(dup_key)
  out <- out[keep, , drop = FALSE]
  source_row <- source_row[keep]

  # Back to the input's order so the result reads like the original table.
  out <- out[base::order(source_row, out[[feature_col]]), , drop = FALSE]
  base::rownames(out) <- NULL

  base::list(
    table = out,
    n_input = base::nrow(tbl),
    n_unmapped = n_unmapped,
    n_output = base::nrow(out)
  )
}

#' @title signatureOrganism
#' @description The organism recorded in an OmicSignature's metadata, or `NA`
#' when there is none.
#' @keywords internal
#' @noRd
signatureOrganism <- function(sig) {
  organism <- sig$metadata$organism
  if (base::is.null(organism) || base::length(organism) == 0 || base::is.na(organism[1]) ||
      !base::nzchar(base::trimws(base::as.character(organism[1])))) {
    return(NA_character_)
  }
  base::as.character(organism[1])
}

#' @title mapCompareSignatureOrganisms
#' @description The cross-species step of `compareSignatures(map_orthologs = TRUE)`.
#' Picks the target organism (the first signature's, unless `ortholog_target`
#' says otherwise), decides the target id space (Ensembl when the
#' target-organism signatures mostly carry Ensembl gene ids, gene symbols
#' otherwise), translates every signature from another organism through
#' [mapOrthologs()], and, when the id space is symbols, translates the
#' target-organism signatures that do not carry symbols yet through their own
#' reference table. Translated signatures are deep copies with the target
#' organism in their metadata and a `_to_<organism>` (or `_symbols`) suffix
#' on their name, so the caller's objects are untouched and the result
#' matrices stay addressable.
#' @param sig_lists `list(sig_list1, sig_list2)`; the second may be `NULL`.
#' @param conn_handler Connection handler for the reference lookups.
#' @param ortholog_target Organism to map onto, or `NULL` for the first
#'   signature's organism.
#' @param feature_col,score_col,p_value_col,group_col Column names, as in
#'   `compareSignatures()`.
#' @param verbose Whether to report what was mapped.
#' @return `sig_lists` with translated signatures in place of the originals.
#' @keywords internal
#' @noRd
mapCompareSignatureOrganisms <- function(sig_lists, conn_handler, ortholog_target, feature_col, score_col,
                                         p_value_col, group_col, verbose = FALSE) {
  n_first <- base::length(sig_lists[[1]])
  all_sigs <- c(sig_lists[[1]], sig_lists[[2]])
  # unname: vapply over a named list names its result, and a named length-1
  # value used as a data.frame column later becomes row names, with a warning.
  organisms <- base::unname(base::vapply(all_sigs, signatureOrganism, base::character(1)))
  if (base::any(base::is.na(organisms))) {
    base::stop(
      "\nSignature(s) without an organism in their metadata cannot be mapped: ",
      base::paste(base::names(all_sigs)[base::is.na(organisms)], collapse = ", "), ".\n"
    )
  }

  target <- if (!base::is.null(ortholog_target) && base::nzchar(base::trimws(base::as.character(ortholog_target)[1]))) {
    base::as.character(ortholog_target)[1]
  } else {
    organisms[1]
  }
  target_key <- base::tolower(base::trimws(target))
  is_target <- base::tolower(base::trimws(organisms)) == target_key

  # Id space of the target organism, read off the signatures already in it.
  target_features <- base::unlist(base::lapply(all_sigs[is_target], function(s) base::as.character(s$signature[[feature_col]])), use.names = FALSE)
  target_types <- classifyFeatureIdType(target_features)
  id_space <- if (base::length(target_types) == 0 || base::mean(target_types %in% "ensembl") > 0.5) "ensembl" else "symbol"
  SigRepo::verbose(base::sprintf("Mapping onto %s %s ids.\n", target, if (id_space == "ensembl") "Ensembl gene" else "gene symbol"))

  signature_features <- function(sig) {
    feats <- base::as.character(sig$signature[[feature_col]])
    if (!base::is.null(sig$difexp) && feature_col %in% base::colnames(sig$difexp)) {
      feats <- c(feats, base::as.character(sig$difexp[[feature_col]]))
    }
    base::unique(feats[!base::is.na(feats) & base::nzchar(feats)])
  }

  # Signatures that cannot be translated are left out with a warning, the way
  # compareSignatures() already treats requests it cannot retrieve: a sweep
  # over many signatures must not die on the one that has no orthologs.
  problems <- base::character()
  dropped <- base::rep(FALSE, base::length(all_sigs))
  for (i in base::seq_along(all_sigs)) {
    sig <- all_sigs[[i]]
    name <- base::names(all_sigs)[i]
    feats <- signature_features(sig)

    if (is_target[i]) {
      if (id_space != "symbol") next
      types <- classifyFeatureIdType(feats)
      if (base::mean(types %in% "symbol") > 0.5) next
      symbols <- lookupSignatureSymbols(conn_handler, feats, types, target)
      mapping <- base::data.frame(query = symbols$query, target = symbols$query_symbol, stringsAsFactors = FALSE)
      suffix <- "_symbols"
    } else {
      found <- mapOrthologs(conn_handler, ids = feats, from = organisms[i], to = target, id_type = "auto", verbose = FALSE)
      mapping <- base::data.frame(query = found$query, target = found[[id_space]], stringsAsFactors = FALSE)
      suffix <- base::paste0("_to_", base::gsub(" ", "_", base::trimws(target)))
    }
    mapping <- mapping[!base::is.na(mapping$target) & base::nzchar(mapping$target), , drop = FALSE]

    translated <- translateSignature(
      sig = sig, mapping = mapping, feature_col = feature_col, group_col = group_col,
      score_col = score_col, p_value_col = p_value_col, organism = target, suffix = suffix
    )
    if (base::is.null(translated$signature)) {
      problems <- c(problems, base::sprintf("'%s': %s", name, translated$reason))
      dropped[i] <- TRUE
      next
    }
    SigRepo::verbose(base::sprintf(
      "%s: %d of %d signature features mapped to %s (%s); %d dropped.\n",
      name, translated$n_output, translated$n_input, target, id_space, translated$n_unmapped
    ))
    all_sigs[[i]] <- translated$signature
    base::names(all_sigs)[i] <- base::paste0(name, suffix)
  }

  first <- base::seq_len(n_first)
  keep1 <- all_sigs[first][!dropped[first]]
  keep2 <- if (base::is.null(sig_lists[[2]])) NULL else all_sigs[-first][!dropped[-first]]
  if (base::length(problems) > 0) {
    too_few <- base::length(keep1) == 0 ||
      (!base::is.null(sig_lists[[2]]) && base::length(keep2) == 0) ||
      (base::is.null(sig_lists[[2]]) && base::length(keep1) < 2)
    if (too_few) {
      base::stop(
        "\nAfter ortholog mapping too few signatures remain to compare:\n",
        base::paste(problems, collapse = "\n"), "\n"
      )
    }
    base::warning(
      "Some signatures were left out of the comparison because they could not be mapped to ", target, ":\n",
      base::paste(problems, collapse = "\n"),
      call. = FALSE
    )
  }

  base::list(keep1, keep2)
}

#' @title translateSignature
#' @description Rebuild one OmicSignature on another id space: the signature
#' table through [translateFeatureTable()] keeping the strongest score per
#' duplicate, the difexp (when present) keeping the smallest p-value, or the
#' strongest score when it has no p-value column. A difexp that ends up empty
#' is dropped. The copy gets `organism` and a suffixed `signature_name`.
#' @return A list: `signature` (the new OmicSignature, or `NULL` when it could
#'   not be built, with `reason` saying why), `n_input`, `n_unmapped`,
#'   `n_output` for the signature table.
#' @keywords internal
#' @noRd
translateSignature <- function(sig, mapping, feature_col, group_col, score_col, p_value_col, organism, suffix) {
  sig_result <- translateFeatureTable(
    sig$signature, mapping, feature_col, group_col, prefer_col = score_col, prefer = "max_abs"
  )
  give_up <- function(reason) {
    base::list(
      signature = NULL, reason = reason, n_input = sig_result$n_input,
      n_unmapped = sig_result$n_unmapped, n_output = sig_result$n_output
    )
  }
  # OmicSignature refuses an empty signature table, and a bi-directional
  # signature whose every feature of one direction is unmapped (it needs both
  # group levels). Say which happened instead of letting the constructor fail.
  if (sig_result$n_output == 0) {
    return(give_up(base::sprintf("none of its %d features map to %s", sig_result$n_input, organism)))
  }
  if (!base::is.null(group_col) && group_col %in% base::colnames(sig$signature)) {
    groups_before <- base::unique(base::as.character(sig$signature[[group_col]]))
    groups_after <- base::unique(base::as.character(sig_result$table[[group_col]]))
    lost <- base::setdiff(groups_before, groups_after)
    if (base::length(lost) > 0) {
      return(give_up(base::sprintf(
        "every feature of group %s is unmapped to %s, so the signature cannot keep its %d groups",
        base::paste(base::sprintf("'%s'", lost), collapse = ", "), organism, base::length(groups_before)
      )))
    }
  }

  difexp <- NULL
  if (!base::is.null(sig$difexp) && feature_col %in% base::colnames(sig$difexp)) {
    has_p <- p_value_col %in% base::colnames(sig$difexp)
    dif_result <- translateFeatureTable(
      sig$difexp, mapping, feature_col, group_col = NULL,
      prefer_col = if (has_p) p_value_col else score_col,
      prefer = if (has_p) "min" else "max_abs"
    )
    if (dif_result$n_output > 0) {
      difexp <- dif_result$table
    }
  }

  # The source platform's probe ids no longer identify anything once the
  # features have been translated, and keeping them breaks OmicSignature's
  # rule that the signature's probe ids are a subset of the difexp's: the two
  # tables collapse duplicates by different criteria (strongest score against
  # smallest p-value), so they can keep different source rows for one target.
  drop_probe <- function(tbl) {
    if (base::is.null(tbl)) return(NULL)
    tbl[, base::setdiff(base::colnames(tbl), "probe_id"), drop = FALSE]
  }
  signature_tbl <- drop_probe(sig_result$table)
  difexp <- drop_probe(difexp)

  metadata <- sig$metadata
  metadata$organism <- organism
  metadata$signature_name <- base::paste0(resolveSignatureLabel(sig, label = NULL, fallback = "signature"), suffix)

  # OmicSignature$new() announces every object it creates even with
  # print_message = FALSE; the verbose line in the caller already says what
  # was built, so keep the console clean here.
  translated <- NULL
  built <- base::tryCatch({
    base::invisible(utils::capture.output(base::suppressMessages(
      translated <- OmicSignature::OmicSignature$new(
        metadata = metadata, signature = signature_tbl, difexp = difexp, print_message = FALSE
      )
    )))
    TRUE
  }, error = function(e) e)
  if (base::inherits(built, "error")) {
    return(give_up(base::sprintf(
      "the translated copy is not a valid OmicSignature (%s)", base::trimws(base::conditionMessage(built))
    )))
  }

  base::list(
    signature = translated,
    reason = NULL,
    n_input = sig_result$n_input,
    n_unmapped = sig_result$n_unmapped,
    n_output = sig_result$n_output
  )
}

#' @title lookupSignatureSymbols
#' @description The within-species step for a signature already in the target
#' organism whose ids are not symbols: open a connection and resolve its ids
#' through [lookupFeatureSymbols()].
#' @return Data frame with `query` and `query_symbol`.
#' @keywords internal
#' @noRd
lookupSignatureSymbols <- function(conn_handler, ids, id_types, organism) {
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
  SigRepo::checkPermissions(conn = conn, action_type = "SELECT", required_role = "viewer")
  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = c("organism_id", "organism"),
    filter_coln_var = "organism",
    filter_coln_val = base::list("organism" = organism),
    check_db_table = TRUE
  )
  if (base::nrow(organism_tbl) == 0) {
    base::try(base::suppressWarnings(DBI::dbDisconnect(conn)), silent = TRUE)
    base::stop("\nThere are no organisms returned for: ", organism, ".\n")
  }
  out <- lookupFeatureSymbols(conn, ids, id_types, organism_tbl$organism_id[1])
  base::try(base::suppressWarnings(DBI::dbDisconnect(conn)), silent = TRUE)
  out
}
