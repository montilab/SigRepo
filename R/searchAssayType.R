#' @title searchAssayType
#' @description Search for omics assay types and its availability status.
#' @param assay_type A list of assay types to be looked up. 
#' Default is NULL which will return all of the assay types in the database.
#' 
#' @return A data frame with assay types and their availability status.
#' 
#' There are two availability status:
#' \code{"Available"} means the assay type currently exists in the database,
#' and users can upload signatures or collections that are specifically
#' associated with this assay type.
#' \code{"Unavailable"} means the assay type isn't existed in the database yet,
#' and users cannot upload signatures or collections that are specifically
#' associated with this assay type.
#' 
#' @export
searchAssayType <- function(
    assay_type = NULL
) {
  
  # Create the table of assay types and their availability
  assay_tbl <- base::data.frame(
    assay_type = c(
      "transcriptomics",
      "proteomics",
      "metabolomics",
      "methylomics",
      "SNPs"
    ),
    status = c(
      "Available",
      "Available",
      "Unavailable",
      "Unavailable",
      "Unavailable"
    ),
    stringsAsFactors = FALSE
  )
  
  # Look up assay_type
  if(base::length(assay_type) == 0 || base::all(assay_type %in% c("", NA))){
    
    return(assay_tbl) 
    
  }else{
    
    # Normalize the input
    assay_type <- base::trimws(base::tolower(assay_type))
    
    tbl <- assay_tbl |> 
      dplyr::filter(.data$assay_type %in% base::trimws(base::tolower(!!assay_type)))
      
    return(tbl) 
    
  }
  
}

