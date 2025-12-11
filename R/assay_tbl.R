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
    "snps"
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