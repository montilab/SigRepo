#' @title searchMetabolomicsFeatureSet
#' @description Search for metabolomics features in a selected metabolite dictionary.
#' @param conn_handler Optional R object obtained from SigRepo::newConnhandler().
#' If NULL, the stored internal handle is used.
#' @param feature_database Metabolomics dictionary to search. One of
#' refmet, hmdb, smiles, or inchikey.
#' @param feature_name A feature, or list of features, to look up.
#' @param chemical_name A chemical name, or list of chemical names, to look up.
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#'
#' @export
searchMetabolomicsFeatureSet <- function(
    conn_handler = NULL,
    feature_database,
    feature_name = NULL,
    chemical_name = NULL,
    verbose = TRUE
){

  SigRepo::print_messages(verbose = verbose)

  conn <- SigRepo::conn_init(conn_handler)

  SigRepo::checkPermissions(
    conn = conn,
    action_type = "SELECT",
    required_role = "viewer"
  )

  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)

  tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = config$db_table_name,
    return_var = "*",
    check_db_table = TRUE
  )

  filter_var_list <- base::list(
    "feature_name" = base::unique(feature_name),
    "chemical_name" = base::unique(chemical_name)
  )

  for (r in base::seq_along(filter_var_list)) {
    filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if (filter_status == TRUE) {
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
      tbl <- tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }

  base::suppressWarnings(DBI::dbDisconnect(conn))

  tbl
}
