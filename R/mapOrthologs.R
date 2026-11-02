#' @title mapOrthologs
#' @description Translate feature ids from one organism to the orthologous
#' genes of another through the repository's reference tables. Within the
#' source organism, a UniProt accession resolves to its gene symbol through
#' `proteomics_features` and an Ensembl gene id through
#' `transcriptomics_features`; the symbol (or an Entrez id directly) then
#' joins the `gene_orthologs` table, which holds the babelgene mouse to human
#' pairs loaded by [updateOrthologFeatureSet()], and the other organism's
#' symbol, Ensembl gene id and Entrez id come back. Human to mouse runs the
#' same join the other way round. In this release one of `from` and `to`
#' must be Homo sapiens and the other Mus musculus.
#'
#' Every ortholog babelgene reports is returned, so a gene with several
#' orthologs yields several rows; `support_n` (the number of orthology
#' sources agreeing on the pair) and `ambiguous` let the caller decide how to
#' treat them. Ids that could not be mapped are kept as rows with `NA` in the
#' target columns, so coverage is visible at a glance.
#' @param conn_handler An R object obtained from SigRepo::newConnHandler() (required)
#' @param ids Character vector of feature ids, all from the `from` organism.
#' Trimmed, de-duplicated, blanks dropped. Any mix of UniProt accessions,
#' Ensembl gene ids, Entrez ids and gene symbols when `id_type = "auto"`.
#' @param from Organism the ids belong to, as stored in the `organisms` table
#' (required), e.g. `"Mus musculus"`.
#' @param to Organism to map to. Defaults to `"Homo sapiens"`.
#' @param id_type How to read `ids`: `"auto"` (default) classifies each id by
#' its pattern; `"uniprot"`, `"ensembl"`, `"entrez"` or `"symbol"` force one
#' reading for every id.
#' @param verbose A logical value of whether to print diagnostic messages.
#' Defaults to 'TRUE'.
#' @return A data frame with one row per (input id, ortholog), in input order,
#' with columns `query` (the id as sent), `query_type` (`"uniprot"`,
#' `"ensembl"`, `"entrez"` or `"symbol"`), `query_symbol` (the symbol the id
#' resolved to in the `from` organism, `NA` when the reference table does not
#' know it), `organism` (the `to` organism), `symbol`, `ensembl`, `entrez`
#' (the ortholog in the `to` organism), `support_n` and `ambiguous` (`TRUE`
#' on every row of an id with more than one ortholog). Unmapped ids have `NA`
#' from `symbol` onwards and `ambiguous = FALSE`.
#'
#' @examples
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "your_username", password = "your_password",
#'   api_host = "http://localhost:8020"
#' )
#'
#' # Mouse UniProt accessions and symbols to human genes
#' SigRepo::mapOrthologs(
#'   conn_handler = conn_handler,
#'   ids = c("P02340", "Cdkn2a", "ENSMUSG00000026072"),
#'   from = "Mus musculus"
#' )
#'
#' # A human Ensembl id back to mouse
#' SigRepo::mapOrthologs(conn_handler, "ENSG00000141510", from = "Homo sapiens", to = "Mus musculus")
#' }
#'
#' @export
mapOrthologs <- function(
    conn_handler = NULL,
    ids,
    from,
    to = "Homo sapiens",
    id_type = c("auto", "uniprot", "ensembl", "entrez", "symbol"),
    verbose = TRUE
){

  id_type <- base::match.arg(id_type)

  # Whether to print the diagnostic messages. print_messages() sets a
  # package-wide option; put it back on exit so a nested call with
  # verbose = FALSE (compareSignatures() makes one) cannot silence its caller.
  old_verbose <- base::getOption("verbose")
  on.exit(base::options(verbose = old_verbose), add = TRUE)
  SigRepo::print_messages(verbose = verbose)

  # Argument checks that need no database ####
  if(base::length(from) != 1 || base::is.na(from) || !base::nzchar(base::trimws(from))){
    base::stop("'from' must be a single organism name, e.g. 'Mus musculus'.\n")
  }
  if(base::length(to) != 1 || base::is.na(to) || !base::nzchar(base::trimws(to))){
    base::stop("'to' must be a single organism name, e.g. 'Homo sapiens'.\n")
  }
  from_key <- base::tolower(base::trimws(from))
  to_key <- base::tolower(base::trimws(to))
  if(from_key == to_key){
    base::stop("'from' and 'to' must be different organisms.\n")
  }
  if(!"homo sapiens" %in% c(from_key, to_key)){
    base::stop("One of 'from' or 'to' must be 'Homo sapiens' in this release.\n")
  }
  from_is_human <- from_key == "homo sapiens"

  ids <- cleanCompareRequest(ids)
  if(base::length(ids) == 0){
    return(emptyOrthologFrame())
  }

  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "SELECT",
    required_role = "viewer"
  )

  fail <- function(...){
    base::try(base::suppressWarnings(DBI::dbDisconnect(conn)), silent = TRUE)
    base::stop(..., call. = FALSE)
  }

  # Both organisms must exist ####
  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = c("organism_id", "organism"),
    filter_coln_var = "organism",
    filter_coln_val = base::list("organism" = base::unique(c(from, to))),
    check_db_table = TRUE
  )
  pick_organism <- function(key){
    organism_tbl[base::tolower(base::trimws(base::as.character(organism_tbl$organism))) == key, , drop = FALSE]
  }
  from_row <- pick_organism(from_key)
  to_row <- pick_organism(to_key)
  if(base::nrow(from_row) == 0 || base::nrow(to_row) == 0){
    missing <- c(from, to)[c(base::nrow(from_row) == 0, base::nrow(to_row) == 0)]
    fail(base::sprintf("There are no organisms returned for: %s.\n", base::paste(missing, collapse = ", ")))
  }
  non_human_row <- if(from_is_human) to_row else from_row
  non_human_id <- non_human_row$organism_id[1]

  if(!orthologTableExists(conn)){
    fail(missingOrthologTableMessage())
  }
  if(countCurrentOrthologs(conn, non_human_id) == 0){
    fail(base::sprintf(
      "No ortholog reference is loaded for %s. Run updateOrthologFeatureSet() (API route /update_orthologs) first.\n",
      non_human_row$organism[1]
    ))
  }

  # Step 1: what kind of id is each one ####
  types <- if(id_type == "auto") classifyFeatureIdType(ids) else base::rep(id_type, base::length(ids))

  # Step 2: within the source organism, id -> gene symbol. The lookup may
  # settle an accession-shaped id as a symbol after all (see
  # lookupFeatureSymbols()); report the type it settled on.
  symbols_tbl <- lookupFeatureSymbols(conn, ids, types, from_row$organism_id[1])
  hit <- base::match(ids, symbols_tbl$query)
  query_symbol <- base::as.character(symbols_tbl$query_symbol[hit])
  if ("query_type" %in% base::colnames(symbols_tbl)) {
    types <- base::as.character(symbols_tbl$query_type[hit])
  }
  entrez_ids <- ids[!base::is.na(types) & types == "entrez"]

  # Step 3: across organisms, symbol or Entrez id -> ortholog ####
  pairs <- lookupOrthologPairs(
    conn = conn,
    symbols = base::unique(query_symbol[!base::is.na(query_symbol)]),
    entrez_ids = entrez_ids,
    non_human_organism_id = non_human_id,
    from_is_human = from_is_human
  )
  if(!"query_entrez" %in% base::colnames(pairs)){
    pairs$query_entrez <- base::rep(NA_character_, base::nrow(pairs))
  }
  pair_symbol_key <- base::tolower(base::trimws(base::as.character(pairs$query_symbol)))
  pair_entrez_key <- base::trimws(base::as.character(pairs$query_entrez))

  # Step 4: one block of rows per input id, in input order. Vectorised: an id
  # that is not an Entrez id joins the pairs on its symbol, an Entrez id joins
  # them on the source-side Entrez column, and whatever matched nothing gets
  # one NA row. (A per-id loop took 45 s for the 77,000 mouse symbols.)
  target_organism <- base::as.character(to_row$organism[1])
  ids_tbl <- base::data.frame(
    idx = base::seq_along(ids), query = ids, query_type = types, query_symbol = query_symbol,
    stringsAsFactors = FALSE
  )
  is_entrez <- !base::is.na(types) & types == "entrez"
  keep_cols <- c("idx", "query", "query_type", "query_symbol", "symbol", "ensembl", "entrez", "support_n")

  pairs$symbol_key <- pair_symbol_key
  by_symbol <- ids_tbl[!is_entrez & !base::is.na(ids_tbl$query_symbol), , drop = FALSE]
  by_symbol$symbol_key <- base::tolower(base::trimws(by_symbol$query_symbol))
  symbol_hits <- base::merge(
    by_symbol, pairs[!base::is.na(pairs$symbol_key), , drop = FALSE],
    by = "symbol_key", suffixes = c("", ".pair")
  )[, keep_cols, drop = FALSE]

  by_entrez <- ids_tbl[is_entrez, , drop = FALSE]
  entrez_pairs <- pairs[!base::is.na(pair_entrez_key), , drop = FALSE]
  entrez_pairs$query <- pair_entrez_key[!base::is.na(pair_entrez_key)]
  entrez_hits <- base::merge(by_entrez, entrez_pairs, by = "query", suffixes = c("", ".pair"))
  # An Entrez id has no symbol of its own; take the matched pair's.
  if (base::nrow(entrez_hits) > 0) {
    entrez_hits$query_symbol <- base::ifelse(
      base::is.na(entrez_hits$query_symbol), entrez_hits$query_symbol.pair, entrez_hits$query_symbol
    )
  }
  entrez_hits <- entrez_hits[, keep_cols, drop = FALSE]

  hits <- base::rbind(symbol_hits, entrez_hits)
  hits$support_n <- base::suppressWarnings(base::as.numeric(hits$support_n))
  misses <- ids_tbl[!(ids_tbl$idx %in% hits$idx), , drop = FALSE]
  miss_rows <- base::data.frame(
    idx = misses$idx, query = misses$query, query_type = misses$query_type, query_symbol = misses$query_symbol,
    symbol = base::rep(NA_character_, base::nrow(misses)), ensembl = base::rep(NA_character_, base::nrow(misses)),
    entrez = base::rep(NA_character_, base::nrow(misses)), support_n = base::rep(NA_real_, base::nrow(misses)),
    stringsAsFactors = FALSE
  )
  out <- base::rbind(hits, miss_rows)
  # Input order, then the best-supported ortholog first, then by symbol.
  out <- out[base::order(out$idx, -out$support_n, out$symbol, na.last = TRUE), , drop = FALSE]
  rows_per_id <- base::table(out$idx)
  out$ambiguous <- base::as.integer(rows_per_id[base::as.character(out$idx)]) > 1 & !base::is.na(out$symbol)
  out$organism <- base::rep(target_organism, base::nrow(out))
  out <- out[, c("query", "query_type", "query_symbol", "organism", "symbol", "ensembl", "entrez", "support_n", "ambiguous"), drop = FALSE]
  base::rownames(out) <- NULL

  # Disconnect from database (quietly: a connection that cannot be
  # disconnected must not turn a finished mapping into an error) ####
  base::try(base::suppressWarnings(DBI::dbDisconnect(conn)), silent = TRUE)

  SigRepo::verbose(base::sprintf(
    "%d of %d id(s) mapped from %s to %s.\n",
    base::length(base::unique(out$query[!base::is.na(out$symbol)])), base::length(ids),
    base::as.character(from_row$organism[1]), target_organism
  ))
  out
}

#' @title emptyOrthologFrame
#' @description A zero-row frame with the columns [mapOrthologs()] returns.
#' @keywords internal
#' @noRd
emptyOrthologFrame <- function(){
  base::data.frame(
    query = base::character(), query_type = base::character(), query_symbol = base::character(),
    organism = base::character(), symbol = base::character(), ensembl = base::character(),
    entrez = base::character(), support_n = base::numeric(), ambiguous = base::logical(),
    stringsAsFactors = FALSE
  )
}

#' @title idText
#' @description Identifiers as text: blanks become `NA`, and a number is
#' written out in full (`as.character(100000000)` would give "1e+08").
#' @keywords internal
#' @noRd
idText <- function(x){
  out <- if(base::is.numeric(x)){
    base::ifelse(base::is.na(x), NA_character_, base::format(x, scientific = FALSE, trim = TRUE))
  }else{
    base::trimws(base::as.character(x))
  }
  out[!base::is.na(out) & !base::nzchar(out)] <- NA_character_
  out
}

#' @title orthologTableExists
#' @description Whether the database has a `gene_orthologs` table at all. A
#' stack built from a SigRepo_Server release before the 2026-11-02 migration
#' has none, and the raw SQL error would not say what to do about it.
#' @keywords internal
#' @noRd
orthologTableExists <- function(conn){
  base::tryCatch(
    base::isTRUE(DBI::dbExistsTable(conn, "gene_orthologs")),
    error = function(e) FALSE
  )
}

#' @title missingOrthologTableMessage
#' @description The error text for a database without the `gene_orthologs` table.
#' @keywords internal
#' @noRd
missingOrthologTableMessage <- function(){
  base::paste0(
    "This database has no gene_orthologs table. Apply SigRepo_Server's ",
    "mysql/migrations/2026-11-02-gene-orthologs.sql (a fresh install gets it from /init_db_schema), ",
    "then load the pairs with updateOrthologFeatureSet() (API route /update_orthologs).\n"
  )
}

#' @title countCurrentOrthologs
#' @description Number of current `gene_orthologs` rows for a non-human organism.
#' @keywords internal
#' @noRd
countCurrentOrthologs <- function(conn, organism_id){
  found <- base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) AS n FROM gene_orthologs WHERE is_current = 1 AND organism_id = %s;",
    base::as.character(organism_id)
  )))
  base::as.integer(found$n[1])
}

#' @title lookupFeatureSymbols
#' @description The within-species step: for each id, the gene symbol its
#' own organism's reference table records for it. UniProt accessions go
#' through `proteomics_features`, Ensembl gene ids through
#' `transcriptomics_features` (both keyed on `(feature_name, organism_id)`,
#' matched case-insensitively), symbols stand for themselves, and Entrez ids
#' get `NA` here because they join `gene_orthologs` directly. Superseded
#' reference rows take part too: a retired accession still names the gene it
#' always did, and signatures deposited before the retirement still carry it.
#' @param conn An open database connection.
#' @param ids Cleaned ids.
#' @param id_types Their types from [classifyFeatureIdType()].
#' @param organism_id The organism the ids belong to.
#' @return Data frame with `query`, `query_symbol` (`NA` when unknown) and
#'   `query_type`, the type each id was finally read as.
#' @details Dozens of real human symbols fit the UniProt accession pattern
#' (H2AC11, B3GAT1, P2RY12, ...). An accession-shaped id the proteomics
#' table does not know is therefore read as the symbol it is, and its
#' `query_type` becomes `"symbol"`.
#' @keywords internal
#' @noRd
lookupFeatureSymbols <- function(conn, ids, id_types, organism_id){
  out <- base::data.frame(
    query = ids,
    query_symbol = base::rep(NA_character_, base::length(ids)),
    query_type = base::as.character(id_types),
    stringsAsFactors = FALSE
  )
  is_symbol <- !base::is.na(id_types) & id_types == "symbol"
  out$query_symbol[is_symbol] <- ids[is_symbol]

  sources <- c(uniprot = "proteomics_features", ensembl = "transcriptomics_features")
  for(type in base::names(sources)){
    sel <- !base::is.na(id_types) & id_types == type
    if(!base::any(sel)) next
    found <- fetchReferenceSymbols(conn, sources[[type]], ids[sel], organism_id)
    hit <- base::match(base::tolower(ids), base::tolower(found$feature_name))
    resolved <- sel & !base::is.na(hit)
    symbol <- base::trimws(base::as.character(found$gene_symbol[hit[resolved]]))
    symbol[base::is.na(symbol) | !base::nzchar(symbol)] <- NA_character_
    out$query_symbol[resolved] <- symbol
    if(type == "uniprot"){
      fallback <- sel & !resolved
      out$query_symbol[fallback] <- ids[fallback]
      out$query_type[fallback] <- "symbol"
    }
  }
  out
}

#' @title fetchReferenceSymbols
#' @description `feature_name` and `gene_symbol` rows of one reference table
#' for the given feature names and organism, fetched in chunks of 1000.
#' @keywords internal
#' @noRd
fetchReferenceSymbols <- function(conn, ref_table, feature_names, organism_id){
  frames <- base::lapply(chunkIds(feature_names), function(chunk){
    base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
      "SELECT feature_name, gene_symbol FROM %s WHERE organism_id = %s AND feature_name IN (%s);",
      ref_table, base::as.character(organism_id), quoteIdList(conn, chunk)
    )))
  })
  if(base::length(frames) == 0){
    return(base::data.frame(feature_name = base::character(), gene_symbol = base::character(), stringsAsFactors = FALSE))
  }
  base::do.call(base::rbind, frames)
}

#' @title lookupOrthologPairs
#' @description The cross-species step against `gene_orthologs`: current
#' pairs of the non-human organism whose source-side symbol or Entrez id is
#' among those given, with the other side's symbol, Ensembl and Entrez ids
#' and the support count. `from_is_human` decides which side is the source.
#' @param conn An open database connection.
#' @param symbols Source-side symbols to match.
#' @param entrez_ids Source-side Entrez ids to match (digits only).
#' @param non_human_organism_id `organism_id` of the non-human side.
#' @param from_is_human `TRUE` when the source side is human.
#' @return Data frame with `query_symbol`, `query_entrez`, `symbol`,
#'   `ensembl`, `entrez` (all character) and `support_n` (numeric).
#' @keywords internal
#' @noRd
lookupOrthologPairs <- function(conn, symbols, entrez_ids, non_human_organism_id, from_is_human){
  if(from_is_human){
    select <- "human_symbol AS query_symbol, human_entrez_id AS query_entrez, gene_symbol AS symbol, ensembl_gene_id AS ensembl, entrez_id AS entrez, support_n"
    symbol_col <- "human_symbol"
    entrez_col <- "human_entrez_id"
  }else{
    select <- "gene_symbol AS query_symbol, entrez_id AS query_entrez, human_symbol AS symbol, human_ensembl_gene_id AS ensembl, human_entrez_id AS entrez, support_n"
    symbol_col <- "gene_symbol"
    entrez_col <- "entrez_id"
  }
  prefix <- base::sprintf(
    "SELECT %s FROM gene_orthologs WHERE is_current = 1 AND organism_id = %s AND ",
    select, base::as.character(non_human_organism_id)
  )
  run <- function(where){
    base::suppressWarnings(DBI::dbGetQuery(conn, base::paste0(prefix, where, ";")))
  }
  frames <- base::lapply(chunkIds(symbols), function(chunk){
    run(base::sprintf("%s IN (%s)", symbol_col, quoteIdList(conn, chunk)))
  })
  entrez_ids <- entrez_ids[base::grepl("^[0-9]+$", entrez_ids)]
  frames <- c(frames, base::lapply(chunkIds(entrez_ids), function(chunk){
    run(base::sprintf("%s IN (%s)", entrez_col, base::paste(chunk, collapse = ", ")))
  }))

  if(base::length(frames) == 0){
    return(base::data.frame(
      query_symbol = base::character(), query_entrez = base::character(), symbol = base::character(),
      ensembl = base::character(), entrez = base::character(), support_n = base::numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- base::do.call(base::rbind, frames)
  out <- out[!base::duplicated(out), , drop = FALSE]
  out$query_symbol <- idText(out$query_symbol)
  out$query_entrez <- idText(out$query_entrez)
  out$symbol <- idText(out$symbol)
  out$ensembl <- idText(out$ensembl)
  out$entrez <- idText(out$entrez)
  out$support_n <- base::suppressWarnings(base::as.numeric(out$support_n))
  base::rownames(out) <- NULL
  out
}

#' @title chunkIds
#' @description Split a vector into chunks of `size` for IN clauses; an empty
#' vector gives an empty list.
#' @keywords internal
#' @noRd
chunkIds <- function(x, size = 1000){
  x <- base::unique(x[!base::is.na(x)])
  if(base::length(x) == 0) return(base::list())
  base::split(x, base::ceiling(base::seq_along(x) / size))
}

#' @title quoteIdList
#' @description A comma-separated list of SQL string literals.
#' @keywords internal
#' @noRd
quoteIdList <- function(conn, x){
  base::paste(base::vapply(x, function(v) base::as.character(DBI::dbQuoteLiteral(conn, v)), base::character(1)), collapse = ", ")
}
