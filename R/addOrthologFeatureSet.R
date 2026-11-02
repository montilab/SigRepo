#' @title addOrthologFeatureSet
#' @description Insert ortholog pairs into the `gene_orthologs` reference
#' table. Each row pairs a gene of a non-human organism with its human
#' ortholog; human is always the other side because babelgene, the source
#' behind [updateOrthologFeatureSet()], only maps to and from human. Pairs
#' already stored (same organism, gene symbol and human symbol) are skipped.
#' This is the insert half of the loader; most callers want
#' [updateOrthologFeatureSet()], which fetches the pairs from babelgene and
#' calls this function.
#' @param conn_handler An R object obtained from SigRepo::newConnHandler() (required)
#' @param ortholog_set A data frame with one row per pair. Required columns:
#' `organism` (the non-human organism, as stored in the `organisms` table),
#' `gene_symbol` (its gene symbol), `human_symbol` and `version` (the
#' babelgene version that produced the pair). Optional columns, blank when
#' absent: `ensembl_gene_id`, `entrez_id`, `human_ensembl_gene_id`,
#' `human_entrez_id`, `support`, `support_n`; `is_current` defaults to 1.
#' @param verbose A logical value of whether to print diagnostic messages.
#' Defaults to 'TRUE'.
#' @return Invisibly, the number of rows offered for insertion after
#'   duplicates were removed.
#'
#' @examples
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "admin_user", password = "admin_password",
#'   api_host = "http://localhost:8020"
#' )
#' SigRepo::addOrthologFeatureSet(
#'   conn_handler = conn_handler,
#'   ortholog_set = data.frame(
#'     organism = "Mus musculus", gene_symbol = "Trp53", human_symbol = "TP53",
#'     ensembl_gene_id = "ENSMUSG00000059552", human_ensembl_gene_id = "ENSG00000141510",
#'     version = "22.9"
#'   )
#' )
#' }
#'
#' @export
addOrthologFeatureSet <- function(
    conn_handler = NULL,
    ortholog_set,
    verbose = TRUE
){

  # Whether to print the diagnostic messages (restored on exit: the option is
  # package-wide and updateOrthologFeatureSet() calls this with verbose = FALSE)
  old_verbose <- base::getOption("verbose")
  on.exit(base::options(verbose = old_verbose), add = TRUE)
  SigRepo::print_messages(verbose = verbose)

  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)

  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "INSERT",
    required_role = "admin"
  )

  required_column_fields <- c("organism", "gene_symbol", "human_symbol", "version")
  optional_column_fields <- c(
    "ensembl_gene_id", "entrez_id", "human_ensembl_gene_id", "human_entrez_id", "support", "support_n"
  )
  db_table_name <- "gene_orthologs"
  table <- base::as.data.frame(ortholog_set, stringsAsFactors = FALSE)

  # Check required column fields
  if(base::any(!required_column_fields %in% base::colnames(table))){
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf(
      "'Ortholog' table is missing the following required column names: %s.\n",
      base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")
    ))
  }

  # Make sure required column fields do not have any empty values ####
  if(base::any(base::is.na(table[, required_column_fields]) | table[, required_column_fields] == "")){
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf(
      "\nAll required column names in 'Ortholog' table: %s cannot contain any empty values.\n",
      base::paste0(required_column_fields, collapse = ", ")
    ))
  }

  # Optional columns: present but blank when the caller left them out ####
  for(column in optional_column_fields){
    if(!column %in% base::colnames(table)){
      table[[column]] <- ""
    }
  }
  if(!"is_current" %in% base::colnames(table)){
    table[["is_current"]] <- 1
  }

  # Get organism id ####
  coln_var <- "organism"
  coln_var_id <- "organism_id"

  lookup_id_tbl <- SigRepo::getVariableID(
    conn = conn,
    db_table_name = "organisms",
    table = table,
    coln_var = coln_var,
    coln_var_id = coln_var_id,
    check_db_table = TRUE
  )

  table <- table |>
    dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |>
    dplyr::left_join(
      lookup_id_tbl |>
        dplyr::mutate(id = base::trimws(base::tolower(!!!rlang::syms(coln_var)))) |>
        dplyr::select(c("id", "organism_id")),
      by = "id"
    )

  # If any ID is missing, produce an error message
  if(base::any(table$organism_id %in% c("", NA))){
    base::suppressWarnings(DBI::dbDisconnect(conn))
    unknown_values <- table$organism[base::which(table$organism_id %in% c("", NA))]
    SigRepo::showOrganismErrorMessage(
      db_table_name = 'organisms',
      unknown_values = unknown_values
    )
    return(base::data.frame(table = "organisms", "unknown_values" = unknown_values))
  }

  # Create a hash key to look up values in database ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "ortholog_hashkey",
    hash_columns = c("gene_symbol", "human_symbol", "organism_id"),
    hash_method = "md5"
  )

  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    exclude_coln_names = "ortholog_id",
    check_db_table = FALSE
  )

  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "ortholog_hashkey",
    check_db_table = FALSE
  )

  # Insert table into database ####
  if(base::nrow(table) > 0){
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      table = table,
      batch_size = 5000,
      check_db_table = FALSE
    )
  }

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  # Return message
  SigRepo::verbose(base::sprintf("Finished uploading %d ortholog pair(s).\n", base::nrow(table)))
  base::invisible(base::nrow(table))
}
