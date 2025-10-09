#' @title searchAssayType
#' @description Search for current omics assay types and filter them by availability status.
#'
#' @param type Character. One of \code{"All"}, \code{"Available"}, or \code{"Unavailable"}.
#'             Controls which assay types are returned. Default is \code{"All"}.
#'
#' @return A data frame with assay types and their availability status.
#'
#' @export
searchAssayType <- function(type = "All") {
  # Create the table of assay types and their availability
  tbl <- data.frame(
    Assay = c(
      "transcriptomics",
      "proteomics",
      "metabolomics",
      "methylomics",
      "genetic_variations",
      "DNA_Binding_Sites"
    ),
    Status = c(
      "Available",
      "Available",
      "Unavailable",
      "Unavailable",
      "Unavailable",
      "Unavailable"
    ),
    stringsAsFactors = FALSE
  )
  
  # Normalize the input
  type <- tolower(type)
  
  # Validate input
  if (!type %in% c("all", "available", "unavailable")) {
    stop("Invalid type. Please choose one of 'All', 'Available', or 'Unavailable'.")
  }
  
  # Filter based on status
  if (type != "all") {
    tbl <- tbl[tolower(tbl$Status) == type, ]
  }
  
  return(tbl)
}
