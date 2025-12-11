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
#' @examples
#' 
#' \dontrun{
#' 
#' # Search a list of assay types in the database 
#' SigRepo::searchAssayType(
#'   assay_type = "transcriptomics"
#' )
#' 
#' }
#' 
#' 
#' @export
searchAssayType <- function(
    assay_type = NULL
){
  
  # Get assay tbl
  assay_tbl <- SigRepo::assay_tbl
  
  # Look up assay_type
  if(base::length(assay_type) == 0 || base::all(assay_type %in% c("", NA))){
    
    return(assay_tbl) 
    
  }else{
    
    # Check if assay is available
    tbl <- assay_tbl |> dplyr::filter(base::trimws(base::tolower(.data$assay_type)) %in% base::trimws(base::tolower(!!assay_type)))
    
    # Return message
    if(base::nrow(tbl) == 0)
      base::stop(base::sprintf("Invalid assay type. 'assay_type' must be one of the following options: %s", base::paste0(assay_tbl$assay_type, collapse = "/")))
      
    # Return table
    return(tbl)
    
  }
  
}

