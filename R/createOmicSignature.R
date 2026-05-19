#' @title createOmicSignature
#' @description Get the signature set uploaded by a specific user in the database.
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param db_signature_tbl A Data Frame; must contain the following column names: 
#' 'signature_id'
#' 
#' @import OmicSignature
#' 
#' @keywords internal
#' 
#' @export
createOmicSignature <- function(
    conn_handler = NULL,
    db_signature_tbl
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Check if table is a data frame object and not empty
  if(!methods::is(db_signature_tbl, "data.frame") || base::nrow(db_signature_tbl) != 1){
    # Disconnect from database ####
    DBI::dbDisconnect(conn)    
    # Show message
    base::stop(base::sprintf("\n'db_signature_tbl' must be a data frame and cannot be empty.\n"))
  }
  
  # Create metadata
  metadata <- db_signature_tbl |>
    dplyr::mutate(PMID = base::as.character(.data$PMID)) |>
    dplyr::mutate_if(base::is.character, function(x){ base::ifelse(base::is.na(x), "", x) }) |>
    base::as.list()
  
  # Clean-up others ###
  if(base::length(metadata$others) > 0 & base::all(!metadata$others %in% c("", NA))){
    metadata$others <- base::strsplit(metadata$others, split = ";", perl = TRUE) |> base::unlist() |> base::trimws() |> 
      purrr::lmap(function(x){ 
        list_name <- base::gsub(pattern = "(.*?):(.*?)<(.*?)>", "\\1", x, perl = TRUE) |> base::trimws()
        list_value <- base::gsub(pattern = "(.*?):(.*?)<(.*?)>", "\\3", x, perl = TRUE) |> base::strsplit(split = ",", fixed = TRUE) |> base::unlist() |> base::trimws()
        list_obj <- base::list(object = list_value)
        base::names(list_obj) <- list_name
        return(list_obj)
      }) 
  }

  # Get assay_type
  assay_type <- db_signature_tbl$assay_type[1]

  # Get reference table
  if(assay_type == "transcriptomics"){
    ref_table <- "transcriptomics_features"
  }else if(assay_type == "proteomics"){
    ref_table <- "proteomics_features"
  }else if(assay_type == "metabolomics"){
    metabolomics_config <- resolveMetabolomicsFeatureConfig(metadata = metadata)
    ref_table <- metabolomics_config$reference_table
  }else if(assay_type == "methylomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
  }else if(assay_type == "genetic_variants"){
    ref_table <- "genetic_variants_features"
  }
  
  # If signature has difexp, get a copy by its signature hash key ####
  if(db_signature_tbl$has_difexp[1] == TRUE){
    # Get API URL
    api_url <- SigRepo::build_api_url(
      conn_handler = conn_handler,
      endpoint = "get_difexp",
      query = base::list(
        api_key = conn_info$api_key[1],
        signature_hashkey = db_signature_tbl$signature_hashkey[1]
      )
    )
    # Get difexp in database
    res <- httr::GET(url = api_url)
    # Check status code
    if(res$status_code != 200){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show message
      SigRepo::stop_for_api_error(
        res = res,
        api_url = api_url,
        action = "retrieve the difexp table from the SigRepo API"
      )
    }else{
      difexp <- jsonlite::fromJSON(jsonlite::fromJSON(base::rawToChar(res$content)))
    }
  }else{
    difexp <- NULL
  }
  
  # Create signature set
  signature <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_feature_set", 
    return_var = "*", 
    filter_coln_var = "signature_id", 
    filter_coln_val = base::list("signature_id" = db_signature_tbl$signature_id[1]),
    check_db_table = TRUE
  )
  
  # Look up feature_id ####
  lookup_feature_id <- base::unique(signature$feature_id[!base::is.na(signature$feature_id)])
  
  if (assay_type == "metabolomics" && base::length(lookup_feature_id) > 0) {
    feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = ref_table,
      return_var = c("metabolite_id", "refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey"),
      filter_coln_var = "metabolite_id",
      filter_coln_val = list("metabolite_id" = lookup_feature_id),
      check_db_table = TRUE
    ) |>
      dplyr::rename(feature_id = .data$metabolite_id)

    if ("feature_name" %in% base::colnames(signature) && base::any(!signature$feature_name %in% c(NA, "", "NULL"))) {
      feature_id_tbl <- feature_id_tbl |>
        dplyr::mutate(resolved_feature_name = NA_character_)
    } else if (metabolomics_config$feature_database == "refmet") {
      feature_id_tbl <- feature_id_tbl |>
        dplyr::mutate(resolved_feature_name = .data$refmet_name)
    } else if (metabolomics_config$feature_database == "refmet_id") {
      xref_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = metabolomics_config$xref_table,
        return_var = c("metabolite_id", "source_db", "source_value", "is_primary"),
        filter_coln_var = c("source_db", "metabolite_id"),
        filter_coln_val = list("source_db" = metabolomics_config$xref_source_db, "metabolite_id" = lookup_feature_id),
        filter_var_by = "AND",
        check_db_table = TRUE
      ) |>
        dplyr::arrange(dplyr::desc(.data$is_primary), .data$source_value) |>
        dplyr::group_by(.data$metabolite_id) |>
        dplyr::summarise(
          resolved_feature_name = base::paste0(base::unique(.data$source_value), collapse = ","),
          .groups = "drop"
        ) |>
        dplyr::rename(feature_id = .data$metabolite_id)

      feature_id_tbl <- feature_id_tbl |>
        dplyr::left_join(
          xref_tbl,
          by = "feature_id"
        )
    } else {
      xref_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = metabolomics_config$xref_table,
        return_var = c("metabolite_id", "source_db", "source_value", "is_primary"),
        filter_coln_var = c("source_db", "metabolite_id"),
        filter_coln_val = list("source_db" = metabolomics_config$xref_source_db, "metabolite_id" = lookup_feature_id),
        filter_var_by = "AND",
        check_db_table = TRUE
      ) |>
        dplyr::arrange(dplyr::desc(.data$is_primary), .data$source_value) |>
        dplyr::group_by(.data$metabolite_id) |>
        dplyr::summarise(
          resolved_feature_name = base::paste0(base::unique(.data$source_value), collapse = ","),
          .groups = "drop"
        ) |>
        dplyr::rename(feature_id = .data$metabolite_id)

      feature_id_tbl <- feature_id_tbl |>
        dplyr::left_join(
          xref_tbl,
          by = "feature_id"
        )
    }
  } else if (assay_type != "metabolomics") {
    feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = c("feature_id", "feature_name"), 
      filter_coln_var = "feature_id", 
      filter_coln_val = list("feature_id" = lookup_feature_id),
      check_db_table = TRUE
    )
  } else {
    feature_id_tbl <- base::data.frame(
      feature_id = numeric(),
      resolved_feature_name = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Add variables to table
  signature <- signature |> 
    dplyr::left_join(
      feature_id_tbl,
      by = "feature_id"
    )

  if (assay_type == "metabolomics") {
    ambiguity_lookup_tbl <- base::data.frame(
      sig_feature_hashkey = character(),
      ambiguity_feature_name = character(),
      stringsAsFactors = FALSE
    )

    all_tables <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
    if ("signature_feature_set_ambiguity" %in% all_tables[[1]] &&
        "sig_feature_hashkey" %in% base::colnames(signature)) {
      ambiguity_hashkeys <- base::unique(signature$sig_feature_hashkey[
        base::is.na(signature$feature_id) & !base::is.na(signature$sig_feature_hashkey)
      ])

      if (base::length(ambiguity_hashkeys) > 0) {
        ambiguity_lookup_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "signature_feature_set_ambiguity",
          return_var = c("sig_feature_hashkey", "candidate_metabolite_id"),
          filter_coln_var = "sig_feature_hashkey",
          filter_coln_val = base::list("sig_feature_hashkey" = ambiguity_hashkeys),
          check_db_table = FALSE
        ) |>
          dplyr::group_by(.data$sig_feature_hashkey) |>
          dplyr::summarise(
            ambiguity_feature_name = base::paste0(
              "ambiguous_metabolite_ids:",
              base::paste0(base::unique(.data$candidate_metabolite_id), collapse = ",")
            ),
            .groups = "drop"
          )
      }
    }

    signature <- signature |>
      dplyr::left_join(
        ambiguity_lookup_tbl,
        by = "sig_feature_hashkey"
      )

    if ("feature_name" %in% base::colnames(signature)) {
      signature <- signature |>
        dplyr::mutate(
          feature_name = dplyr::if_else(
            base::is.na(.data$feature_name) | .data$feature_name %in% c("", "NULL"),
            dplyr::coalesce(.data$resolved_feature_name, .data$ambiguity_feature_name),
            .data$feature_name
          )
        )
    } else {
      signature <- signature |>
        dplyr::mutate(feature_name = dplyr::coalesce(.data$resolved_feature_name, .data$ambiguity_feature_name))
    }
  }
  
  # Rename table with appropriate column names 
  if (assay_type == "metabolomics") {
    if ("feature_name" %in% base::colnames(signature)) {
      coln_names <- base::setdiff(
        base::colnames(signature),
        c("feature_id", "refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey", "resolved_feature_name", "ambiguity_feature_name")
      )
    } else {
      coln_names <- base::colnames(signature) |>
        base::replace(base::match("feature_id", base::colnames(signature)), "feature_name")
      coln_names <- coln_names[!coln_names %in% c("refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey", "resolved_feature_name", "ambiguity_feature_name")]
    }
  } else {
    coln_names <- base::colnames(signature) |> 
      base::replace(base::match("feature_id", base::colnames(signature)), "feature_name")
  }
  
  # Extract the table with appropriate column names ####
  signature <- signature |> 
    dplyr::select(dplyr::all_of(coln_names)) |>
    dplyr::mutate(group_label = if(metadata$direction_type[1] %in% c("bi-directional", "categorical")){ base::factor(.data$group_label, levels=base::unique(.data$group_label), labels=base::unique(.data$group_label)) }else{ .data$group_label })
  
  # Extract difexp with appropriate column names ####
  if(db_signature_tbl$has_difexp[1] == TRUE){
    difexp <- difexp |> 
      dplyr::mutate(group_label = if(metadata$direction_type[1] %in% c("bi-directional", "categorical")){ base::factor(.data$group_label, levels=base::unique(.data$group_label), labels=base::unique(.data$group_label)) }else{ .data$group_label })
  }
  
  # Create the OmicSignature object
  OmS <- base::tryCatch({
    OmicSignature::OmicSignature$new(
      metadata = metadata,
      signature = signature,
      difexp = difexp
    )
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("OmicSignature Error: ", e, "\n")
  })
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return Signature
  return(OmS)
  
}
