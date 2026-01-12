#' Exported assay table
#'
#' An assay table for the package
#' 
#' @keywords internal
#' 
#' @export
assay_tbl <- base::data.frame(
  assay_type = c(
    "transcriptomics",
    "proteomics",
    "metabolomics",
    "methylomics",
    "genetic_variants"
  ),
  status = c(
    "Available",
    "Available",
    "Unavailable",
    "Unavailable",
    "Available"
  ),
  stringsAsFactors = FALSE
)
