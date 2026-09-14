HYPER_DEFAULT_BACKGROUND <- 23467

# Column names a signature or difexp table may carry gene symbols under. Kept
# identical to SigRepo_Server's DIFEXP_SYMBOL_COLUMNS (api/lib/annotate.R).
HYPER_SYMBOL_COLUMNS <- c("gene_symbol", "symbol", "geneSymbol", "gene", "hgnc_symbol", "mgi_symbol")

cleanHypeRSymbols <- function(x) {
  x <- base::trimws(base::as.character(x))
  x[base::is.na(x) | !base::nzchar(x)] <- NA_character_
  x
}

#' First symbol column that is present and populated, or NULL
#' @noRd
hypeRSymbolColumn <- function(tbl) {
  if (!methods::is(tbl, "data.frame") || base::nrow(tbl) == 0) {
    return(NULL)
  }
  for (col in HYPER_SYMBOL_COLUMNS) {
    if (col %in% base::colnames(tbl) && base::any(!base::is.na(cleanHypeRSymbols(tbl[[col]])))) {
      return(col)
    }
  }
  NULL
}

#' feature_name -> gene_symbol from the reference table for one organism
#'
#' Reference tables are unique on (feature_name, organism_id), so the organism
#' filter is required. lookup_table_sql() matches on trim(lower()), so callers
#' must match the returned names case-insensitively.
#'
#' @return Named character vector (names = feature_name as stored).
#' @noRd
lookupReferenceSymbols <- function(conn_handler, assay_type, organism, feature_names) {
  ref_table <- base::switch(
    base::tolower(base::as.character(assay_type)[1]),
    transcriptomics = "transcriptomics_features",
    proteomics = "proteomics_features",
    NULL
  )
  feature_names <- base::unique(feature_names[!base::is.na(feature_names) & base::nzchar(feature_names)])

  if (base::is.null(conn_handler) || base::is.null(ref_table) || base::length(feature_names) == 0 ||
      base::length(organism) == 0 || base::is.na(organism[1]) || !base::nzchar(organism[1])) {
    return(base::character())
  }

  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  base::on.exit(base::try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = c("organism_id", "organism"),
    filter_coln_var = "organism",
    filter_coln_val = base::list("organism" = organism[1]),
    check_db_table = TRUE
  )
  if (base::nrow(organism_tbl) == 0) {
    return(base::character())
  }

  ref_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = ref_table,
    return_var = c("feature_name", "gene_symbol"),
    filter_coln_var = c("feature_name", "organism_id"),
    filter_coln_val = base::list("feature_name" = feature_names, "organism_id" = organism_tbl$organism_id[1]),
    filter_var_by = "AND",
    check_db_table = TRUE
  )

  symbols <- cleanHypeRSymbols(ref_tbl$gene_symbol)
  keep <- !base::is.na(symbols)
  stats::setNames(symbols[keep], base::as.character(ref_tbl$feature_name[keep]))
}

#' Gene symbols for each row of a signature's signature or difexp table
#'
#' First source that yields any symbol wins:
#'   1. a symbol column in the table itself;
#'   2. (signature table only) the difexp's symbol column joined on probe_id --
#'      probe_id is an opaque key, used here only to join the two tables;
#'   3. the reference table by feature_name + organism (needs conn_handler).
#'
#' @return list(symbols = chr aligned with rows, NA where unmapped; source = chr)
#' @noRd
resolveSignatureSymbols <- function(omic_signature, table = c("signature", "difexp"), conn_handler = NULL) {
  table <- base::match.arg(table)
  tbl <- omic_signature[[table]]
  n_rows <- base::nrow(tbl)

  own_col <- hypeRSymbolColumn(tbl)
  if (!base::is.null(own_col)) {
    return(base::list(symbols = cleanHypeRSymbols(tbl[[own_col]]), source = base::sprintf("%s$%s", table, own_col)))
  }

  if (base::identical(table, "signature")) {
    difexp_tbl <- omic_signature$difexp
    difexp_col <- hypeRSymbolColumn(difexp_tbl)
    if (!base::is.null(difexp_col) && "probe_id" %in% base::colnames(tbl) && "probe_id" %in% base::colnames(difexp_tbl)) {
      by_probe <- stats::setNames(cleanHypeRSymbols(difexp_tbl[[difexp_col]]), base::as.character(difexp_tbl$probe_id))
      symbols <- base::unname(by_probe[base::as.character(tbl$probe_id)])
      if (base::any(!base::is.na(symbols))) {
        return(base::list(symbols = symbols, source = base::sprintf("difexp$%s via probe_id", difexp_col)))
      }
    }
  }

  if ("feature_name" %in% base::colnames(tbl)) {
    feature_names <- base::trimws(base::as.character(tbl$feature_name))
    # OmicSignature$new() sorts signature/difexp rows by desc(abs(score)) when a
    # score column is present, so tbl's row order is not the caller's input
    # order. Sort before the lookup call so it (and its provenance) is
    # deterministic; row alignment below still keys off names, not order.
    reference <- lookupReferenceSymbols(
      conn_handler = conn_handler,
      assay_type = omic_signature$metadata$assay_type,
      organism = omic_signature$metadata$organism,
      feature_names = base::sort(feature_names)
    )
    if (base::length(reference) > 0) {
      base::names(reference) <- base::tolower(base::names(reference))
      symbols <- base::unname(reference[base::tolower(feature_names)])
      if (base::any(!base::is.na(symbols))) {
        return(base::list(symbols = symbols, source = "reference feature_name"))
      }
    }
  }

  base::list(symbols = base::rep(NA_character_, n_rows), source = NA_character_)
}

disambiguateHypeRLabels <- function(labels) {
  used <- base::character()
  for (i in base::seq_along(labels)) {
    candidate <- labels[i]
    suffix <- 2L
    while (candidate %in% used) {
      candidate <- base::sprintf("%s (%d)", labels[i], suffix)
      suffix <- suffix + 1L
    }
    labels[i] <- candidate
    used <- c(used, candidate)
  }
  labels
}

#' Resolve signatures plus their ids and display labels
#' @noRd
collectHypeRSignatures <- function(conn_handler, signature_id, signature_name, omic_signature, verbose) {
  signatures <- resolveHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    verbose = verbose
  )

  supplied_names <- if (!base::is.null(omic_signature) && !methods::is(omic_signature, "OmicSignature")) {
    base::names(omic_signature)
  } else {
    NULL
  }

  fetched_by_id <- base::is.null(omic_signature) && base::length(signature_id) > 0 &&
    base::any(!signature_id %in% c("", NA))
  ids <- if (fetched_by_id) base::names(signatures) else base::rep(NA_character_, base::length(signatures))

  labels <- base::vapply(base::seq_along(signatures), function(i) {
    supplied <- supplied_names[i]
    if (!base::is.null(supplied) && !base::is.na(supplied) && base::nzchar(supplied)) {
      return(base::as.character(supplied))
    }
    resolveSignatureLabel(signatures[[i]], label = NULL, fallback = base::sprintf("sig_%s", i))
  }, base::character(1))

  base::list(signatures = base::unname(signatures), ids = ids, labels = disambiguateHypeRLabels(labels))
}

emptyHypeRInfo <- function() {
  base::data.frame(
    query = base::character(), signature_id = base::character(), signature_name = base::character(),
    group_label = base::character(), n_features = base::integer(), n_dropped = base::integer(),
    symbol_source = base::character(), stringsAsFactors = FALSE
  )
}

hypeRInfoRow <- function(query, signature_id, label, group_label, n_features, n_dropped, symbol_source) {
  base::data.frame(
    query = query, signature_id = signature_id, signature_name = label, group_label = group_label,
    n_features = base::as.integer(n_features), n_dropped = base::as.integer(n_dropped),
    symbol_source = symbol_source, stringsAsFactors = FALSE
  )
}

hypeRSkip <- function(reason, message) {
  base::list(queries = base::list(), info = NULL, skip = base::list(reason = reason, message = message))
}

#' Hypergeometric query vectors for one signature
#' @noRd
buildHypergeometricQueries <- function(omic_signature, label, signature_id, split, conn_handler) {
  tbl <- omic_signature$signature
  if (!methods::is(tbl, "data.frame") || base::nrow(tbl) == 0) {
    return(hypeRSkip("empty_signature", "The signature table is empty."))
  }

  resolved <- resolveSignatureSymbols(omic_signature, "signature", conn_handler)
  if (base::all(base::is.na(resolved$symbols))) {
    return(hypeRSkip("no_gene_symbols", "No gene symbols in the signature, its difexp, or the reference tables."))
  }

  groups <- if (base::isTRUE(split) && "group_label" %in% base::colnames(tbl)) {
    base::trimws(base::as.character(tbl$group_label))
  } else {
    base::rep("", base::nrow(tbl))
  }
  groups[base::is.na(groups)] <- ""

  queries <- base::list()
  info <- base::list()
  # OmicSignature$new() sorts signature rows by desc(abs(score)) when a score
  # column is present, so tbl's row order is not the caller's input order.
  # Sort query vectors so output is deterministic regardless of that ordering
  # (the hypergeometric test treats a query as an unordered set anyway).
  for (group in base::sort(base::unique(groups))) {
    group_symbols <- resolved$symbols[groups == group]
    query <- base::sort(base::unique(group_symbols[!base::is.na(group_symbols)]))
    if (base::length(query) == 0) {
      next
    }
    query_name <- if (base::nzchar(group)) base::sprintf("%s | %s", label, group) else label
    queries[[query_name]] <- query
    info[[base::length(info) + 1]] <- hypeRInfoRow(
      query = query_name, signature_id = signature_id, label = label,
      group_label = if (base::nzchar(group)) group else NA_character_,
      n_features = base::length(query), n_dropped = base::sum(base::is.na(group_symbols)),
      symbol_source = resolved$source
    )
  }

  base::list(queries = queries, info = base::do.call(base::rbind, info), skip = NULL)
}

#' Weighted (ranked) kstest query vector for one signature
#' @noRd
buildKstestQuery <- function(omic_signature, label, signature_id, score_col, conn_handler) {
  tbl <- omic_signature$difexp
  if (!methods::is(tbl, "data.frame") || base::nrow(tbl) == 0) {
    return(hypeRSkip("no_difexp", "kstest needs a difexp table and this signature has none."))
  }
  if (!score_col %in% base::colnames(tbl)) {
    return(hypeRSkip(
      "missing_score_col",
      base::sprintf("difexp has no '%s' column (has: %s).", score_col, base::paste(base::colnames(tbl), collapse = ", "))
    ))
  }

  resolved <- resolveSignatureSymbols(omic_signature, "difexp", conn_handler)
  scores <- base::suppressWarnings(base::as.numeric(tbl[[score_col]]))
  keep <- !base::is.na(resolved$symbols) & !base::is.na(scores)
  if (!base::any(keep)) {
    return(hypeRSkip("no_gene_symbols", "No difexp row has both a gene symbol and a numeric score."))
  }

  best <- base::vapply(
    base::split(scores[keep], resolved$symbols[keep]),
    function(x) x[base::which.max(base::abs(x))],
    base::numeric(1)
  )
  query <- base::sort(best, decreasing = TRUE)

  base::list(
    queries = stats::setNames(base::list(query), label),
    info = hypeRInfoRow(
      query = label, signature_id = signature_id, label = label, group_label = NA_character_,
      n_features = base::length(query), n_dropped = base::sum(!keep), symbol_source = resolved$source
    ),
    skip = NULL
  )
}

#' Build query vectors for already-collected signatures
#' @noRd
buildHypeRQueries <- function(inputs, test, split, score_col, conn_handler) {
  queries <- base::list()
  info <- base::list()
  skipped <- base::list()

  for (i in base::seq_along(inputs$signatures)) {
    built <- if (base::identical(test, "hypergeometric")) {
      buildHypergeometricQueries(inputs$signatures[[i]], inputs$labels[i], inputs$ids[i], split, conn_handler)
    } else {
      buildKstestQuery(inputs$signatures[[i]], inputs$labels[i], inputs$ids[i], score_col, conn_handler)
    }

    if (!base::is.null(built$skip)) {
      skipped[[base::length(skipped) + 1]] <- base::data.frame(
        signature = inputs$labels[i], reason = built$skip$reason, message = built$skip$message,
        stringsAsFactors = FALSE
      )
      next
    }

    queries <- c(queries, built$queries)
    info[[base::length(info) + 1]] <- built$info
  }

  base::list(
    signatures = queries,
    info = if (base::length(info) > 0) base::do.call(base::rbind, info) else emptyHypeRInfo(),
    skipped = if (base::length(skipped) > 0) {
      base::do.call(base::rbind, skipped)
    } else {
      base::data.frame(signature = base::character(), reason = base::character(), message = base::character(), stringsAsFactors = FALSE)
    }
  )
}

#' Build hypeR query vectors from SigRepo signatures
#'
#' @description Turns SigRepo signatures into the query vectors
#' \code{hypeR::hypeR()} takes, and reports how each was built. Use it to
#' inspect exactly what \code{runHypeR()} will test.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Needed to fetch signatures by id/name and for reference-table symbol lookup.
#' @param signature_id One or more SigRepo signature IDs.
#' @param signature_name One or more SigRepo signature names.
#' @param omic_signature An \code{OmicSignature} object or a list of them. List
#' names, when given, become the query labels.
#' @param test \code{"hypergeometric"} (a character vector of symbols from the
#' signature table) or \code{"kstest"} (a named numeric vector ranked by
#' \code{score_col} from the full difexp table).
#' @param split Logical; for \code{"hypergeometric"}, build one vector per
#' \code{group_label} (e.g. one per arm of a bi-directional signature). Ignored by
#' \code{"kstest"}, whose ranked list already carries direction in its sign.
#' Defaults to \code{TRUE}.
#' @param score_col difexp column used to rank genes for \code{"kstest"}.
#' Defaults to \code{"score"}.
#' @param verbose Logical; print messages while fetching signatures.
#'
#' @details Gene symbols come from the first source that works: a symbol column
#' (\code{gene_symbol}, \code{symbol}, \code{geneSymbol}, \code{gene},
#' \code{hgnc_symbol}, \code{mgi_symbol}) in the table itself; for
#' \code{"hypergeometric"}, the difexp's symbol column joined on \code{probe_id};
#' then the reference feature table by \code{feature_name} and organism (needs
#' \code{conn_handler}). \code{probe_id} is never read as a symbol.
#'
#' @return A list with
#' \describe{
#'   \item{\code{signatures}}{Named list of query vectors, ready for \code{hypeR::hypeR()}.}
#'   \item{\code{info}}{Data frame, one row per query: \code{query}, \code{signature_id},
#'   \code{signature_name}, \code{group_label}, \code{n_features}, \code{n_dropped},
#'   \code{symbol_source}.}
#'   \item{\code{skipped}}{Data frame of signatures that produced no query:
#'   \code{signature}, \code{reason}, \code{message}.}
#' }
#'
#' @examples
#' utils::data("LLFS_Aging_Gene_2023", package = "SigRepo")
#' prepared <- SigRepo::prepareHypeRSignatures(omic_signature = LLFS_Aging_Gene_2023, verbose = FALSE)
#' prepared$info
#'
#' @export
prepareHypeRSignatures <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    test = c("hypergeometric", "kstest"),
    split = TRUE,
    score_col = "score",
    verbose = TRUE
) {
  test <- base::match.arg(test)

  inputs <- collectHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    verbose = verbose
  )

  buildHypeRQueries(inputs, test = test, split = split, score_col = score_col, conn_handler = conn_handler)
}
