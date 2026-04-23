#' @title addMetabolomicsFeatureSet
#' @description Add metabolomics feature set to the canonical reference and xref tables.
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
  normalized_tbl <- normalizeMetabolomicsFeatureSet(
    feature_set = feature_set,
    feature_database = config$feature_database
  )

  required_column_fields <- c("feature_name", "is_current", "version")
  if (config$maintenance_model == "curated") {
    required_column_fields <- c(required_column_fields, "chemical_name")
  }

  if (base::any(base::is.na(normalized_tbl[, required_column_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(
      base::sprintf(
        "\nAll required column names in 'metabolomics features' table: %s cannot contain any empty values.\n",
        base::paste0(required_column_fields, collapse = ", ")
      )
    )
  }

  reference_tbl <- buildMetabolomicsReferenceRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  )

  reference_tbl <- addMetaboliteHashKey(reference_tbl)

  reference_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = config$reference_table,
    table = reference_tbl,
    exclude_coln_names = "metabolite_id",
    check_db_table = FALSE
  )

  reference_tbl <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = config$reference_table,
    table = reference_tbl,
    coln_var = "metabolite_hashkey",
    check_db_table = FALSE
  )

  if (base::nrow(reference_tbl) > 0) {
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = config$reference_table,
      table = reference_tbl,
      check_db_table = FALSE
    )
  }

  lookup_reference_tbl <- buildMetabolomicsReferenceRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  )

  lookup_reference_tbl <- addMetaboliteHashKey(lookup_reference_tbl)

  metabolite_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = config$reference_table,
    return_var = c("metabolite_id", "metabolite_hashkey"),
    filter_coln_var = "metabolite_hashkey",
    filter_coln_val = base::list("metabolite_hashkey" = base::unique(lookup_reference_tbl$metabolite_hashkey)),
    check_db_table = FALSE
  )

  xref_tbl <- buildMetabolomicsXrefRows(
    feature_set = normalized_tbl,
    feature_database = config$feature_database
  ) |>
    dplyr::left_join(
      lookup_reference_tbl |>
        dplyr::select(c("chemical_name", "refmet_name", "inchikey", "smiles", "metabolite_hashkey")),
      by = c("chemical_name", "refmet_name", "inchikey", "smiles")
    ) |>
    dplyr::left_join(
      metabolite_tbl,
      by = "metabolite_hashkey"
    ) |>
    dplyr::transmute(
      metabolite_id = .data$metabolite_id,
      source_db = .data$source_db,
      source_value = .data$source_value,
      is_primary = ifelse(.data$source_db %in% config$xref_source_db, 1, 0)
    ) |>
    dplyr::distinct()

  if (base::any(base::is.na(xref_tbl$metabolite_id))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("Unable to resolve metabolite IDs for all metabolomics xref rows.\n")
  }

  xref_tbl <- SigRepo::createHashKey(
    table = xref_tbl,
    hash_var = "xref_hashkey",
    hash_columns = c("metabolite_id", "source_db", "source_value"),
    hash_method = "md5"
  )

  xref_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = config$xref_table,
    table = xref_tbl,
    exclude_coln_names = "xref_id",
    check_db_table = FALSE
  )

  xref_tbl <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = config$xref_table,
    table = xref_tbl,
    coln_var = "xref_hashkey",
    check_db_table = FALSE
  )

  if (base::nrow(xref_tbl) > 0) {
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = config$xref_table,
      table = xref_tbl,
      check_db_table = FALSE
    )
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  SigRepo::verbose("Finished uploading.\n")
}
