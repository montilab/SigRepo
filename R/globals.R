utils::globalVariables(".data")

# Create the table of assay types and their availability
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

# Create a list of direction types for package look up
direction_type <- c("uni-directional", "bi-directional", "categorical")

# Create test connection handler for testing
test_conn_handler <- base::list(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo",
  api_host = "sigrepo.org",
  api_port = 8020
)

# Combine all variables as one big list
global_var <- base::list(assay_tbl = assay_tbl, direction_type = direction_type, test_conn_handler = test_conn_handler)

# Save as package global var
base::save(global_var, file = "R/sysdata.rda")
