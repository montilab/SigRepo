#' @title metabolomics_feature_tables
#' @description Internal lookup table for metabolomics feature dictionaries.
#'
#' @keywords internal
metabolomics_feature_tables <- base::data.frame(
  feature_database = c("refmet", "hmdb", "smiles", "inchikey"),
  db_table_name = c("refmet_features", "hmdb_features", "smiles_features", "inchikey_features"),
  maintenance_model = c("curated", "curated", "growing", "growing"),
  stringsAsFactors = FALSE
)

#' @title resolveMetabolomicsFeatureConfig
#' @description Resolve metabolomics dictionary metadata into a DB table config.
#' @param feature_database Optional metabolomics dictionary name.
#' @param metadata Optional OmicSignature metadata list.
#'
#' @keywords internal
resolveMetabolomicsFeatureConfig <- function(
    feature_database = NULL,
    metadata = NULL
){
  get_from_others <- function(metadata, field_name) {
    if (base::is.null(metadata) || !"others" %in% base::names(metadata)) {
      return(NULL)
    }

    others <- metadata$others
    if (!methods::is(others, "list") || !field_name %in% base::names(others)) {
      return(NULL)
    }

    value <- others[[field_name]]
    if (base::length(value) == 0 || base::all(value %in% c("", NA))) {
      return(NULL)
    }

    value[1]
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- get_from_others(metadata, "metabolomics_nomenclature")
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    base::stop(
      "Metabolomics signatures require a metabolite dictionary. ",
      "Provide 'metabolomics_nomenclature' when adding or updating a metabolomics signature, using one of: ",
      base::paste0(metabolomics_feature_tables$feature_database, collapse = "/"),
      "."
    )
  }

  feature_database <- base::trimws(base::tolower(feature_database[1]))

  config <- metabolomics_feature_tables |>
    dplyr::filter(.data$feature_database %in% .env$feature_database)

  if (base::nrow(config) != 1) {
    base::stop(
      "Invalid metabolomics dictionary. 'feature_database' must be one of: ",
      base::paste0(metabolomics_feature_tables$feature_database, collapse = "/"),
      "."
    )
  }

  base::as.list(config[1, , drop = FALSE])
}

#' @title addMetabolomicsNomenclature
#' @description Persist metabolomics nomenclature in metadata `others`.
#' @param metadata OmicSignature metadata list.
#' @param metabolomics_nomenclature Metabolomics dictionary name.
#'
#' @keywords internal
addMetabolomicsNomenclature <- function(
    metadata,
    metabolomics_nomenclature
) {

  config <- resolveMetabolomicsFeatureConfig(feature_database = metabolomics_nomenclature)

  if (!"others" %in% base::names(metadata) ||
      base::is.null(metadata$others) ||
      (base::is.atomic(metadata$others) && base::length(metadata$others) == 1 && metadata$others %in% c("", NA))) {
    metadata$others <- base::list()
  }

  if (!methods::is(metadata$others, "list")) {
    base::stop("'others' in OmicSignature metadata must be a list when used with metabolomics.")
  }

  metadata$others$metabolomics_nomenclature <- config$feature_database

  metadata
}
