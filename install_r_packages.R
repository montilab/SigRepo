
# Create a list of start-up packages 
startup_packages <- c("BiocManager", "dplyr", "rvest", "xml2", "yaml")

# Select only the packages that aren't currently installed in the system
install_startup_packages <- startup_packages[base::which(!startup_packages %in% utils::installed.packages())]

# And finally we install the required packages
for(pkg in install_startup_packages) utils::install.packages(pkg, dependencies=TRUE, repos='http://cran.rstudio.com/')

# Load packages
library(BiocManager)
library(dplyr)
library(rvest)
library(xml2)
library(yaml)

# Get all available Bioconductor packages
url <- 'https://www.bioconductor.org/packages/release/bioc/'
biocPackages <- url |> 
  xml2::read_html() |> 
  rvest::html_table() |>
  base::lapply(`[[`, "Package") |>
  base::unlist()

# Read in package dependencies in DESCRIPTION
DESCRIPTION <- yaml::read_yaml("DESCRIPTION")

# Extract all imports packages
required_pkgs <- base::trimws(base::strsplit(DESCRIPTION$Imports, ",", fixed = TRUE)[[1]])

# Extract all required bioconductor packages
bioconductor_pkgs <- required_pkgs[base::which(required_pkgs %in% biocPackages)]

# Select only the packages that aren't currently installed in the system
install_bioconductor_pkgs <- bioconductor_pkgs[base::which(!bioconductor_pkgs %in% utils::installed.packages())]

# And finally we install the required Bioconductor packages
for(pkg in install_bioconductor_pkgs) BiocManager::install(pkg)

# Extract all required CRAN packages
cran_pkgs <- required_pkgs[base::which(!required_pkgs %in% bioconductor_pkgs)]

# Select only the packages that aren't currently installed in the system
install_cran_pkgs <- cran_pkgs[base::which(!cran_pkgs %in% utils::installed.packages())]

# And finally we install the required packages including their dependencies
for(pkg in install_cran_pkgs) utils::install.packages(pkg, dependencies = TRUE, repos = 'http://cran.rstudio.com/')

