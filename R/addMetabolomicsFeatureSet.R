#' @title addMetabolomicsFeatureSet
#' @description Add metabolomics feature set to a selected metabolite dictionary.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param feature_set A data frame containing metabolomics features.
#' @param feature_database Metabolomics dictionary to target. One of
#' refmet, hmdb, smiles, or inchikey.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @export
addMetabolomicsFeatureSet <- function(
    conn_handler = NULL,
    feature_set,
    feature_database,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)

  SigRepo::checkPermissions(
    conn = conn,
    action_type = "INSERT",
    required_role = "admin"
  )

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)
  db_table_name <- config$db_table_name
  table <- feature_set

  required_column_fields <- c("feature_name", "is_current", "version")
  if (config$maintenance_model == "curated") {
    required_column_fields <- c(required_column_fields, "chemical_name")
  } else if (!"chemical_name" %in% base::colnames(table)) {
    table <- table |> dplyr::mutate(chemical_name = NA_character_)
  }

  if (base::any(!required_column_fields %in% base::colnames(table))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\n'Metabolomics features' table is missing the following required column names: %s.\n",
        base::paste0(required_column_fields[base::which(!required_column_fields %in% base::colnames(table))], collapse = ", ")
      )
    )
  }

  if (base::any(base::is.na(table[, required_column_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nAll required column names in 'metabolomics features' table: %s cannot contain any empty values.\n",
        base::paste0(required_column_fields, collapse = ", ")
      )
    )
  }

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = "feature_name",
    hash_method = "md5"
  )

  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    exclude_coln_names = "feature_id",
    check_db_table = FALSE
  )

  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "feature_hashkey",
    check_db_table = FALSE
  )

  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    check_db_table = FALSE
  )

  base::suppressWarnings(DBI::dbDisconnect(conn))

  SigRepo::verbose("Finished uploading.\n")
}
