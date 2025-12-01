#' @title protTransform
#' @description Function to dynamically transform the ftp uniprot data into the SigRepo dictionary structure
#' @param organism_code organism name you want to retrieve data from
#' @param tax_id Taxonomic id of organism 
#' @param version this is set the date of downloading the ftp file
#' @param is_current boolean for if the feature_names are current or not.
#' 
#' @keywords internal
#' 
#' @import readr
#'
#' @export
protTransform <- function(
    organism_code = "HUMAN", 
    organism_name = "Homo sapiens",
    tax_id = "9606",
    version = base::format(base::Sys.Date(), "%m%d%Y"),
    is_current = 1
) {
  
  # Construct filename and URL dynamically
  file_name <- base::paste0(organism_code, "_", tax_id, "_idmapping_selected.tab.gz")
  url <- base::paste0("https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/", file_name)
  
  # Download if not already present
  if (!base::file.exists(file_name)) {
    base::message("Downloading: ", url)
    utils::download.file(url, destfile = file_name, method = "curl")
  } else {
    base::message("File already exists: ", file_name)
  }
  
  # Read the tab-separated data
  raw_data <- readr::read_tsv(file_name, col_names = FALSE, show_col_types = FALSE)
  
  # Check minimum number of columns before renaming
  if (base::ncol(raw_data) < 2) {
    base::stop("The downloaded file does not have the expected structure.")
  }
  
  # Clean and transform
  cleaned_data <- raw_data |>
    dplyr::rename(
      "feature_name" = "X1",
      "gene_symbol" = "X2"
    ) |>
    dplyr::select(c("feature_name", "gene_symbol")) |>
    dplyr::mutate(
      organism = organism_name,
      version = base::format(
        base::as.Date(version, format = "%m%d%Y"),
        "%Y-%m-%d"
      ),
      is_current = is_current
    )
  
  return(cleaned_data)
  
}
