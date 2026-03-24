#' @title addMetabolomicsSignatureSet
#' @description Add metabolomics signature feature set to database.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param signature_id Database ID of the signature (required)
#' @param signature_set A Data Frame; must contain the following column names:
#' feature_name, probe_id, score, group_label (required)
#' @param feature_database Metabolomics dictionary to target. One of
#' refmet, hmdb, smiles, or inchikey.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @export
addMetabolomicsSignatureSet <- function(
    conn_handler = NULL,
    signature_id,
    signature_set,
    feature_database,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)

  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "INSERT",
    required_role = "editor"
  )

  user_role <- conn_info$user_role[1]
  user_name <- conn_info$user[1]

  if (!base::length(signature_id) == 1 || signature_id %in% c(NA, "")) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("\n'signature_id' must have a length of 1 and cannot be empty.\n")
  }

  required_fields <- c("feature_name", "probe_id", "score", "group_label")
  if (!methods::is(signature_set, "data.frame") || base::nrow(signature_set) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop("\n'signature_set' must be a data frame and cannot be empty.\n")
  }

  if (base::any(!required_fields %in% base::colnames(signature_set))) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\n'signature_set' must have the following column names: %s.\n", base::paste0(required_fields, collapse = ", ")))
  }

  if (base::any(base::is.na(signature_set[, required_fields]) == TRUE)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\nAll required column names in 'signature_set': %s cannot contain any empty values.\n", base::paste0(required_fields, collapse = ", ")))
  }

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)
  db_table_name <- "signature_feature_set"
  ref_table <- config$db_table_name

  if (user_role != "admin") {
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = c("signature_id", "user_name"),
      filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = TRUE
    )
  } else {
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = "signature_id",
      filter_coln_val = list("signature_id" = signature_id),
      check_db_table = TRUE
    )
  }

  if (base::nrow(signature_tbl) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    base::stop(base::sprintf("\nThere is no signature_id = '%s' belonged to user = '%s' in the 'signatures' table of the SigRepo database.\n", signature_id, user_name))
  }

  table <- signature_set |>
    dplyr::mutate(
      signature_id = signature_id,
      assay_type = "metabolomics"
    )

  if (!"chemical_name" %in% base::colnames(table)) {
    table <- table |> dplyr::mutate(chemical_name = NA_character_)
  }

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = "feature_name",
    hash_method = "md5"
  )

  lookup_hashkey <- base::unique(table$feature_hashkey)

  lookup_feature_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = ref_table,
    return_var = c("feature_id", "feature_name", "feature_hashkey"),
    filter_coln_var = "feature_hashkey",
    filter_coln_val = list("feature_hashkey" = lookup_hashkey),
    check_db_table = TRUE
  )

  if (config$maintenance_model == "growing") {
    missing_feature_tbl <- table |>
      dplyr::filter(!.data$feature_hashkey %in% lookup_feature_id_tbl$feature_hashkey) |>
      dplyr::distinct(.data$feature_name, .data$chemical_name, .keep_all = TRUE) |>
      dplyr::mutate(
        is_current = 1,
        version = as.integer(base::format(base::Sys.Date(), "%m%d%Y"))
      ) |>
      dplyr::select(c("feature_name", "chemical_name", "is_current", "feature_hashkey", "version"))

    if (base::nrow(missing_feature_tbl) > 0) {
      missing_feature_tbl <- SigRepo::checkTableInput(
        conn = conn,
        db_table_name = ref_table,
        table = missing_feature_tbl,
        exclude_coln_names = "feature_id",
        check_db_table = FALSE
      )

      missing_feature_tbl <- SigRepo::removeDuplicates(
        conn = conn,
        db_table_name = ref_table,
        table = missing_feature_tbl,
        coln_var = "feature_hashkey",
        check_db_table = FALSE
      )

      SigRepo::insert_table_sql(
        conn = conn,
        db_table_name = ref_table,
        table = missing_feature_tbl,
        check_db_table = FALSE
      )

      lookup_feature_id_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = ref_table,
        return_var = c("feature_id", "feature_name", "feature_hashkey"),
        filter_coln_var = "feature_hashkey",
        filter_coln_val = list("feature_hashkey" = lookup_hashkey),
        check_db_table = FALSE
      )
    }
  }

  if (base::nrow(lookup_feature_id_tbl) != base::length(lookup_hashkey)) {
    base::suppressWarnings(DBI::dbDisconnect(conn))

    unknown_values <- table$feature_name[base::which(!table$feature_hashkey %in% lookup_feature_id_tbl$feature_hashkey)]

    SigRepo::showMetabolomicsErrorMessage(
      db_table_name = ref_table,
      unknown_values = unknown_values
    )

    return(base::data.frame(table = ref_table, unknown_values = unknown_values))
  }

  table <- table |>
    dplyr::left_join(
      lookup_feature_id_tbl |> dplyr::select(c("feature_hashkey", "feature_id")),
      by = "feature_hashkey"
    )

  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "sig_feature_hashkey",
    hash_columns = c("signature_id", "feature_id", "assay_type", "probe_id"),
    hash_method = "md5"
  )

  table <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    check_db_table = TRUE
  )

  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "sig_feature_hashkey",
    check_db_table = FALSE
  )

  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    check_db_table = FALSE
  )

  base::suppressWarnings(DBI::dbDisconnect(conn))

  return(base::invisible())
}
