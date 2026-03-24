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

  get_from_metadata <- function(metadata, field_name) {
    if (base::is.null(metadata) || !field_name %in% base::names(metadata)) {
      return(NULL)
    }

    value <- metadata[[field_name]]
    if (base::length(value) == 0 || base::all(value %in% c("", NA))) {
      return(NULL)
    }

    value[1]
  }

  infer_from_platform <- function(platform_name) {
    if (base::length(platform_name) == 0 || base::all(platform_name %in% c("", NA))) {
      return(NULL)
    }

    platform_name <- base::trimws(base::tolower(platform_name[1]))

    for (db_name in metabolomics_feature_tables$feature_database) {
      if (base::grepl(db_name, platform_name, fixed = TRUE)) {
        return(db_name)
      }
    }

    NULL
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- get_from_metadata(metadata, "metabolomics_database")
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- get_from_metadata(metadata, "feature_database")
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- get_from_metadata(metadata, "metabolite_dictionary")
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    others <- get_from_metadata(metadata, "others")

    if (methods::is(others, "list")) {
      if ("metabolomics_database" %in% base::names(others)) {
        feature_database <- others$metabolomics_database[1]
      } else if ("feature_database" %in% base::names(others)) {
        feature_database <- others$feature_database[1]
      } else if ("metabolite_dictionary" %in% base::names(others)) {
        feature_database <- others$metabolite_dictionary[1]
      }
    }
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    feature_database <- infer_from_platform(get_from_metadata(metadata, "platform"))
  }

  if (base::length(feature_database) == 0 || base::all(feature_database %in% c("", NA))) {
    base::stop(
      "Metabolomics signatures require a metabolite dictionary. ",
      "Set one of metadata$metabolomics_database, metadata$feature_database, ",
      "or metadata$metabolite_dictionary to one of: ",
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

#' @title normalizeMetabolomicsMetadata
#' @description Normalize metabolomics metadata and persist dictionary in `others`.
#' @param metadata OmicSignature metadata list.
#'
#' @keywords internal
normalizeMetabolomicsMetadata <- function(metadata) {

  config <- resolveMetabolomicsFeatureConfig(metadata = metadata)

  metadata$metabolomics_database <- config$feature_database

  if (!"others" %in% base::names(metadata) ||
      base::is.null(metadata$others) ||
      (base::is.atomic(metadata$others) && base::length(metadata$others) == 1 && metadata$others %in% c("", NA))) {
    metadata$others <- base::list()
  }

  if (!methods::is(metadata$others, "list")) {
    base::stop("'others' in OmicSignature metadata must be a list when used with metabolomics.")
  }

  metadata$others$metabolomics_database <- config$feature_database

  metadata
}
