#' @title updateOrthologFeatureSet
#' @description Load or refresh the mouse to human ortholog pairs in the
#' `gene_orthologs` reference table from babelgene, which ships its ortholog
#' tables inside the package (no network access). Every gene symbol stored in
#' the organism's transcriptomics and proteomics reference tables, and every
#' human symbol in the human ones, is looked up; the pairs found are compared
#' with what is stored: new pairs are inserted (through
#' [addOrthologFeatureSet()]), pairs still present are stamped with the
#' current babelgene version, and pairs babelgene no longer reports are
#' archived (`is_current = 0`). The table is empty until this has run once;
#' the API exposes the same operation as `POST /update_orthologs`.
#'
#' Human is always the other side of a pair because babelgene only maps to
#' and from human, so `organism` names the non-human side. In this release
#' only Mus musculus is accepted.
#' @param conn_handler An R object obtained from SigRepo::newConnHandler() (required)
#' @param organism The non-human organism to load pairs for, as stored in the
#' `organisms` table (required). Mus musculus in this release.
#' @param verbose A logical value of whether to print diagnostic messages.
#' Defaults to 'TRUE'.
#' @return Invisibly, a list with the counts `refreshed`, `added` and
#'   `archived` and the babelgene `version` written. Stops with "No further
#'   updates needed" when the stored pairs already match babelgene's.
#'
#' @examples
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "admin_user", password = "admin_password",
#'   api_host = "http://localhost:8020"
#' )
#' SigRepo::updateOrthologFeatureSet(conn_handler = conn_handler, organism = "Mus musculus")
#' }
#'
#' @export
updateOrthologFeatureSet <- function(
    conn_handler = NULL,
    organism,
    verbose = TRUE
){

  # Whether to print the diagnostic messages (restored on exit, so the
  # package-wide option does not leak into the caller's session)
  old_verbose <- base::getOption("verbose")
  on.exit(base::options(verbose = old_verbose), add = TRUE)
  SigRepo::print_messages(verbose = verbose)

  # Check organism (required) #####
  if(base::length(organism) != 1 || base::all(organism %in% c(NA, ""))){
    base::stop("'organism' must have a length of 1 and cannot be empty.\n")
  }

  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "UPDATE",
    required_role = "admin"
  )

  # Disconnect before stopping, but never let a failing disconnect hide the
  # message the caller needs to read.
  fail <- function(...){
    base::try(base::suppressWarnings(DBI::dbDisconnect(conn)), silent = TRUE)
    base::stop(..., call. = FALSE)
  }

  # Look up organism in the database
  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = "*",
    filter_coln_var = "organism",
    filter_coln_val = base::list("organism" = base::unique(organism)),
    check_db_table = TRUE
  )
  if(base::nrow(organism_tbl) == 0){
    fail("There are no organisms returned from the search parameters.\n")
  }
  organism_name <- base::as.character(organism_tbl$organism[1])
  organism_id <- organism_tbl$organism_id[1]
  organism_key <- base::tolower(base::trimws(organism_name))

  if(organism_key == "homo sapiens"){
    fail("Homo sapiens is the target side of every ortholog pair; run the update for the other organism (e.g. 'Mus musculus').\n")
  }
  if(organism_key != "mus musculus"){
    fail("Ortholog updates are limited to Mus musculus in this release.\n")
  }

  species <- babelgeneSpeciesName(organism_name, organism_tbl$prot_organism_taxid[1])
  if(base::is.na(species)){
    fail(base::sprintf("babelgene does not know the organism '%s'. Updates aborted.\n", organism_name))
  }

  # Human side: the organisms row the human symbols come from
  human_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "organisms",
    return_var = c("organism_id", "organism"),
    filter_coln_var = "organism",
    filter_coln_val = base::list("organism" = "Homo sapiens"),
    check_db_table = TRUE
  )
  if(base::nrow(human_tbl) == 0){
    fail("The organisms table has no 'Homo sapiens' row, so there is no human side to map to.\n")
  }

  # Symbols to look up: everything the reference tables know for each side
  SigRepo::verbose("Collecting gene symbols from the reference tables...\n")
  symbols <- referenceGeneSymbols(conn, organism_id)
  human_symbols <- referenceGeneSymbols(conn, human_tbl$organism_id[1])

  SigRepo::verbose(base::sprintf(
    "Looking up %d %s and %d human symbols in babelgene %s...\n",
    base::length(symbols), organism_name, base::length(human_symbols),
    base::as.character(utils::packageVersion("babelgene"))
  ))
  fetched <- babelgeneOrthologTable(symbols, human_symbols, organism_name, species)
  if(base::nrow(fetched) == 0){
    fail(base::sprintf("babelgene returned no ortholog pairs for %s. Updates aborted.\n", organism_name))
  }
  new_version <- fetched$version[1]

  # What is stored, and what to do with it. Archived rows take part so a pair
  # that comes back is re-activated rather than inserted twice.
  stored <- base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT gene_symbol, human_symbol, version, is_current FROM gene_orthologs WHERE organism_id = %s;",
    base::as.character(organism_id)
  )))
  current <- stored[base::as.integer(stored$is_current) == 1, , drop = FALSE]
  plan <- partitionOrthologUpdates(current, fetched)

  up_to_date <- base::nrow(current) > 0 &&
    base::all(base::as.character(current$version) == new_version) &&
    base::nrow(plan$add) == 0 && base::nrow(plan$archive) == 0
  if(up_to_date){
    fail("All ortholog pairs are up to date. No further updates needed.\n")
  }

  pair_hashes <- function(tbl){
    if(base::nrow(tbl) == 0) return(base::character())
    SigRepo::createHashKey(
      table = base::data.frame(
        gene_symbol = tbl$gene_symbol, human_symbol = tbl$human_symbol,
        organism_id = base::rep(organism_id, base::nrow(tbl)), stringsAsFactors = FALSE
      ),
      hash_var = "ortholog_hashkey",
      hash_columns = c("gene_symbol", "human_symbol", "organism_id"),
      hash_method = "md5"
    )$ortholog_hashkey
  }
  in_chunks <- function(x, size = 1000){
    if(base::length(x) == 0) return(base::list())
    base::split(x, base::ceiling(base::seq_along(x) / size))
  }
  sql_list <- function(x) base::paste0("'", base::gsub("'", "''", x, fixed = TRUE), "'", collapse = ", ")
  run_statement <- function(statement){
    base::tryCatch({
      base::suppressWarnings(DBI::dbExecute(conn = conn, statement = statement))
    }, error = function(e){
      fail(base::conditionMessage(e), "\n")
    })
  }

  SigRepo::verbose("Updating ortholog pairs...\n")

  # Pairs still reported: stamp the new version
  for(hashes in in_chunks(pair_hashes(plan$refresh))){
    run_statement(base::sprintf(
      "UPDATE gene_orthologs SET version = '%s', is_current = 1 WHERE ortholog_hashkey IN (%s);",
      new_version, sql_list(hashes)
    ))
  }

  # Pairs babelgene no longer reports: archive them
  for(hashes in in_chunks(pair_hashes(plan$archive))){
    run_statement(base::sprintf(
      "UPDATE gene_orthologs SET is_current = 0 WHERE ortholog_hashkey IN (%s);",
      sql_list(hashes)
    ))
  }

  # Pairs not current: re-activate the ones archived earlier, insert the rest
  if(base::nrow(plan$add) > 0){
    for(hashes in in_chunks(pair_hashes(plan$add))){
      run_statement(base::sprintf(
        "UPDATE gene_orthologs SET version = '%s', is_current = 1 WHERE ortholog_hashkey IN (%s);",
        new_version, sql_list(hashes)
      ))
    }
    add_key <- base::tolower(base::paste(plan$add$gene_symbol, plan$add$human_symbol, sep = "\r"))
    fetched_key <- base::tolower(base::paste(fetched$gene_symbol, fetched$human_symbol, sep = "\r"))
    SigRepo::addOrthologFeatureSet(
      conn_handler = conn_handler,
      ortholog_set = fetched[fetched_key %in% add_key, , drop = FALSE],
      verbose = FALSE
    )
  }

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  SigRepo::verbose(base::sprintf(
    "Orthologs for %s: %d refreshed, %d added, %d archived (babelgene %s).\n",
    organism_name, base::nrow(plan$refresh), base::nrow(plan$add), base::nrow(plan$archive), new_version
  ))
  base::invisible(base::list(
    refreshed = base::nrow(plan$refresh), added = base::nrow(plan$add),
    archived = base::nrow(plan$archive), version = new_version
  ))
}

#' @title referenceGeneSymbols
#' @description Every distinct, non-blank, current gene symbol an organism has
#' in the transcriptomics and proteomics reference tables: the set of symbols
#' worth asking babelgene about.
#' @param conn An open database connection.
#' @param organism_id The organism's id.
#' @return Character vector of symbols.
#' @keywords internal
#' @noRd
referenceGeneSymbols <- function(conn, organism_id){
  statement <- base::sprintf(
    "SELECT DISTINCT gene_symbol FROM transcriptomics_features WHERE organism_id = %s AND is_current = 1 AND gene_symbol IS NOT NULL AND gene_symbol <> ''
     UNION
     SELECT DISTINCT gene_symbol FROM proteomics_features WHERE organism_id = %s AND is_current = 1 AND gene_symbol IS NOT NULL AND gene_symbol <> '';",
    base::as.character(organism_id), base::as.character(organism_id)
  )
  found <- base::suppressWarnings(DBI::dbGetQuery(conn, statement))
  base::unique(base::trimws(base::as.character(found$gene_symbol)))
}
