#' @title metabolomics_feature_tables
#' @description Internal lookup table for metabolomics feature dictionaries.
#'
#' @keywords internal
metabolomics_feature_tables <- base::data.frame(
  feature_database = c("refmet", "hmdb", "smiles", "inchikey"),
  reference_table = c("metabolite_reference", "metabolite_reference", "metabolite_reference", "metabolite_reference"),
  xref_table = c("metabolite_xref", "metabolite_xref", "metabolite_xref", "metabolite_xref"),
  lookup_table = c("metabolite_reference", "metabolite_xref", "metabolite_xref", "metabolite_xref"),
  lookup_column = c("refmet_name", "source_value", "source_value", "source_value"),
  xref_source_db = c("refmet", "hmdb", "smiles", "inchikey"),
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

isEmptyMetabolomicsValue <- function(x) {
  base::length(x) == 0 || base::all(base::is.na(x) | base::trimws(base::as.character(x)) %in% c("", "NA", "NULL"))
}

getMetabolomicsColumn <- function(table, candidates, default = NULL) {
  matched <- candidates[candidates %in% base::colnames(table)]
  if (base::length(matched) == 0) {
    return(default)
  }
  table[[matched[1]]]
}

normalizeMetabolomicsFeatureSet <- function(
    feature_set,
    feature_database
) {
  table <- base::as.data.frame(feature_set, stringsAsFactors = FALSE)

  original_colnames <- base::colnames(table)
  normalized_colnames <- base::tolower(original_colnames)
  base::colnames(table) <- normalized_colnames

  feature_name <- getMetabolomicsColumn(table, "feature_name")
  if (is.null(feature_name)) {
    base::stop("'feature_set' must contain a 'feature_name' column.")
  }

  chemical_name <- getMetabolomicsColumn(table, c("chemical_name", "chemicalname"), NA_character_)
  refmet_name <- getMetabolomicsColumn(table, c("refmet_name", "refmet"), NA_character_)
  hmdb <- getMetabolomicsColumn(table, c("hmdb", "hmdb_id", "hmdb_ids"), NA_character_)
  smiles <- getMetabolomicsColumn(table, "smiles", NA_character_)
  inchikey <- getMetabolomicsColumn(table, c("inchikey", "inchi_key"), NA_character_)

  if (feature_database == "refmet" && isEmptyMetabolomicsValue(refmet_name)) {
    refmet_name <- feature_name
  }
  if (feature_database == "hmdb" && isEmptyMetabolomicsValue(hmdb)) {
    hmdb <- feature_name
  }
  if (feature_database == "smiles" && isEmptyMetabolomicsValue(smiles)) {
    smiles <- feature_name
  }
  if (feature_database == "inchikey" && isEmptyMetabolomicsValue(inchikey)) {
    inchikey <- feature_name
  }

  is_current <- getMetabolomicsColumn(table, "is_current")
  version <- getMetabolomicsColumn(table, "version")

  if (is.null(is_current) || is.null(version)) {
    missing_cols <- c("is_current", "version")[c(is.null(is_current), is.null(version))]
    base::stop(
      base::sprintf(
        "\n'Metabolomics features' table is missing the following required column names: %s.\n",
        base::paste0(missing_cols, collapse = ", ")
      )
    )
  }

  normalized <- base::data.frame(
    feature_name = base::as.character(feature_name),
    chemical_name = base::as.character(chemical_name),
    refmet_name = base::as.character(refmet_name),
    hmdb = base::as.character(hmdb),
    smiles = base::as.character(smiles),
    inchikey = base::as.character(inchikey),
    is_current = is_current,
    version = version,
    stringsAsFactors = FALSE
  )

  base::colnames(feature_set) <- original_colnames

  normalized
}

splitMetabolomicsIdentifiers <- function(values) {
  if (base::length(values) == 0) {
    return(character())
  }

  values <- base::as.character(values)
  values <- values[!base::is.na(values)]
  values <- base::trimws(values)
  values <- values[!values %in% c("", "NA", "NULL")]

  if (base::length(values) == 0) {
    return(character())
  }

  values <- values |>
    base::strsplit(split = ",", fixed = TRUE) |>
    base::unlist(use.names = FALSE) |>
    base::trimws()

  values <- values[!values %in% c("", "NA", "NULL")]

  base::unique(values)
}

normalizeSmilesForLookup <- function(values) {
  values <- base::as.character(values)
  values <- base::trimws(values)
  values <- base::gsub("\\\\", "", values, perl = TRUE)
  values <- base::gsub("/", "", values, fixed = TRUE)
  values
}

buildMetabolomicsReferenceRows <- function(feature_set, feature_database) {
  table <- normalizeMetabolomicsFeatureSet(
    feature_set = feature_set,
    feature_database = feature_database
  )

  table |>
    dplyr::transmute(
      chemical_name = dplyr::na_if(.data$chemical_name, ""),
      refmet_name = dplyr::na_if(.data$refmet_name, ""),
      inchikey = dplyr::na_if(.data$inchikey, ""),
      smiles = dplyr::na_if(.data$smiles, ""),
      is_current = .data$is_current,
      version = .data$version
    ) |>
    dplyr::distinct()
}

addMetaboliteHashKey <- function(reference_tbl) {
  hash_columns <- c("chemical_name", "refmet_name", "inchikey", "smiles")

  hash_ready_tbl <- reference_tbl |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hash_columns),
        ~ dplyr::if_else(base::is.na(.x), "__na__", base::as.character(.x))
      )
    )

  hash_tbl <- SigRepo::createHashKey(
    table = hash_ready_tbl,
    hash_var = "metabolite_hashkey",
    hash_columns = hash_columns,
    hash_method = "md5"
  ) |>
    dplyr::select(dplyr::all_of(c(hash_columns, "is_current", "version", "metabolite_hashkey")))

  reference_tbl |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hash_columns),
        ~ dplyr::if_else(base::is.na(.x), "__na__", base::as.character(.x))
      )
    ) |>
    dplyr::left_join(
      hash_tbl,
      by = c("chemical_name", "refmet_name", "inchikey", "smiles", "is_current", "version")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hash_columns),
        ~ dplyr::na_if(.x, "__na__")
      )
    )
}

buildMetabolomicsXrefRows <- function(feature_set, feature_database) {
  table <- normalizeMetabolomicsFeatureSet(
    feature_set = feature_set,
    feature_database = feature_database
  )

  source_columns <- c("hmdb", "smiles", "inchikey")
  xref_rows <- purrr::map_dfr(
    source_columns,
    function(source_name) {
      rows <- purrr::map_dfr(
        base::seq_len(base::nrow(table)),
        function(r) {
          values <- splitMetabolomicsIdentifiers(table[[source_name]][r])
          if (base::length(values) == 0) {
            return(base::data.frame())
          }

          base::data.frame(
            chemical_name = table$chemical_name[r],
            refmet_name = table$refmet_name[r],
            inchikey = table$inchikey[r],
            smiles = table$smiles[r],
            source_db = source_name,
            source_value = values,
            stringsAsFactors = FALSE
          )
        }
      )

      rows
    }
  )

  if (!isEmptyMetabolomicsValue(table$refmet_name)) {
    refmet_rows <- table |>
      dplyr::transmute(
        chemical_name = .data$chemical_name,
        refmet_name = .data$refmet_name,
        inchikey = .data$inchikey,
        smiles = .data$smiles,
        source_db = "refmet",
        source_value = .data$refmet_name
      )
    xref_rows <- dplyr::bind_rows(xref_rows, refmet_rows)
  }

  xref_rows |>
    dplyr::mutate(
      chemical_name = dplyr::na_if(.data$chemical_name, ""),
      refmet_name = dplyr::na_if(.data$refmet_name, ""),
      inchikey = dplyr::na_if(.data$inchikey, ""),
      smiles = dplyr::na_if(.data$smiles, "")
    ) |>
    dplyr::distinct()
}

resolveMetabolomicsSignatureMatches <- function(
    conn,
    feature_database,
    feature_values
) {
  config <- resolveMetabolomicsFeatureConfig(feature_database = feature_database)
  feature_values <- base::unique(base::trimws(base::as.character(feature_values)))
  feature_values <- feature_values[!feature_values %in% c("", "NA", "NULL") & !base::is.na(feature_values)]

  if (base::length(feature_values) == 0) {
    return(base::data.frame(
      input_feature_name = character(),
      metabolite_id = integer(),
      stringsAsFactors = FALSE
    ))
  }

  if (config$lookup_table == "metabolite_reference") {
    lookup_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = config$reference_table,
      return_var = c("metabolite_id", "refmet_name"),
      filter_coln_var = config$lookup_column,
      filter_coln_val = stats::setNames(base::list(feature_values), config$lookup_column),
      check_db_table = TRUE
    )

    if (base::nrow(lookup_tbl) == 0) {
      return(base::data.frame(
        input_feature_name = character(),
        metabolite_id = integer(),
        stringsAsFactors = FALSE
      ))
    }

    lookup_tbl |>
      dplyr::transmute(
        input_feature_name = .data$refmet_name,
        metabolite_id = .data$metabolite_id
      )
  } else {
    lookup_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = config$xref_table,
      return_var = c("metabolite_id", "source_db", "source_value"),
      filter_coln_var = c("source_db", "source_value"),
      filter_coln_val = base::list("source_db" = config$xref_source_db, "source_value" = feature_values),
      filter_var_by = "AND",
      check_db_table = TRUE
    )

    if (config$xref_source_db == "smiles") {
      matched_values <- base::unique(lookup_tbl$source_value)
      missing_values <- base::setdiff(feature_values, matched_values)

      if (base::length(missing_values) > 0) {
        all_smiles_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = config$xref_table,
          return_var = c("metabolite_id", "source_db", "source_value"),
          filter_coln_var = "source_db",
          filter_coln_val = base::list("source_db" = config$xref_source_db),
          check_db_table = TRUE
        )

        fallback_tbl <- all_smiles_tbl |>
          dplyr::mutate(normalized_source_value = normalizeSmilesForLookup(.data$source_value)) |>
          dplyr::inner_join(
            base::data.frame(
              input_feature_name = missing_values,
              normalized_source_value = normalizeSmilesForLookup(missing_values),
              stringsAsFactors = FALSE
            ),
            by = "normalized_source_value"
          ) |>
          dplyr::transmute(
            metabolite_id = .data$metabolite_id,
            source_db = .data$source_db,
            source_value = .data$input_feature_name
          )

        lookup_tbl <- dplyr::bind_rows(lookup_tbl, fallback_tbl) |>
          dplyr::distinct()
      }
    }

    lookup_tbl |>
      dplyr::transmute(
        input_feature_name = .data$source_value,
        metabolite_id = .data$metabolite_id
      )
  }
}

buildMetabolomicsAmbiguityReport <- function(lookup_tbl) {
  lookup_tbl |>
    dplyr::distinct(.data$input_feature_name, .data$metabolite_id) |>
    dplyr::arrange(.data$input_feature_name, .data$metabolite_id) |>
    dplyr::group_by(.data$input_feature_name) |>
    dplyr::summarise(
      issue_type = "ambiguous_mapping",
      candidate_metabolite_ids = base::paste0(.data$metabolite_id, collapse = ","),
      n_matches = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(feature_name = "input_feature_name")
}

insertMetabolomicsAmbiguityRows <- function(conn, ambiguity_tbl) {
  if (!methods::is(ambiguity_tbl, "data.frame") || base::nrow(ambiguity_tbl) == 0) {
    return(base::invisible(NULL))
  }

  all_tables <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
  if (!"signature_feature_set_ambiguity" %in% all_tables$Tables_in_sigrepo) {
    return(base::invisible(NULL))
  }

  ambiguity_tbl <- ambiguity_tbl |>
    dplyr::distinct(.data$sig_feature_hashkey, .data$candidate_metabolite_id)

  existing_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signature_feature_set_ambiguity",
    return_var = c("sig_feature_hashkey", "candidate_metabolite_id"),
    filter_coln_var = "sig_feature_hashkey",
    filter_coln_val = base::list("sig_feature_hashkey" = base::unique(ambiguity_tbl$sig_feature_hashkey)),
    check_db_table = FALSE
  )

  if (base::nrow(existing_tbl) > 0) {
    ambiguity_tbl <- ambiguity_tbl |>
      dplyr::anti_join(
        existing_tbl,
        by = c("sig_feature_hashkey", "candidate_metabolite_id")
      )
  }

  if (base::nrow(ambiguity_tbl) == 0) {
    return(base::invisible(NULL))
  }

  ambiguity_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = "signature_feature_set_ambiguity",
    table = ambiguity_tbl,
    exclude_coln_names = "ambiguity_id",
    check_db_table = FALSE
  )

  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = "signature_feature_set_ambiguity",
    table = ambiguity_tbl,
    check_db_table = FALSE
  )

  base::invisible(NULL)
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
