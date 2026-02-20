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
    conn_handler,
    db_signature_tbl
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init()
  
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
  
  # Get assay_type
  assay_type <- db_signature_tbl$assay_type[1]  
  
  # Get reference table
  if(assay_type == "transcriptomics"){
    ref_table <- "transcriptomics_features"
  }else if(assay_type == "proteomics"){
    ref_table <- "proteomics_features"
  }else if(assay_type == "metabolomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    #ref_table <- "metabolomics_features"
  }else if(assay_type == "methylomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    #ref_table <- "methylomics_features"
  }else if(assay_type == "genetic_variants"){
  
    ref_table <- "genetic_variants_features"
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
  
  # If signature has difexp, get a copy by its signature hash key ####
  if(db_signature_tbl$has_difexp[1] == TRUE){
    # Get API URL
    api_url <- base::sprintf("http://%s:%s/get_difexp?api_key=%s&signature_hashkey=%s", conn_handler$api_host[1], conn_handler$api_port[1], conn_info$api_key[1], db_signature_tbl$signature_hashkey[1])
    # Get difexp in database
    res <- httr::GET(url = api_url)
    # Check status code
    if(res$status_code != 200){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show message
      base::stop("\nSomething went wrong with API. Cannot get difexp table from the SigRepo database. Please contact admin for support.\n")
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
  lookup_feature_id <- base::unique(signature$feature_id)
  
  feature_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = ref_table, 
    return_var = c("feature_id", "feature_name"), 
    filter_coln_var = "feature_id", 
    filter_coln_val = list("feature_id" = lookup_feature_id),
    check_db_table = TRUE
  ) 
  
  # Add variables to table
  signature <- signature |> 
    dplyr::left_join(
      feature_id_tbl,
      by = "feature_id"
    )
  
  # Rename table with appropriate column names 
  coln_names <- base::colnames(signature) |> 
    base::replace(base::match("feature_id", base::colnames(signature)), "feature_name")
  
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







