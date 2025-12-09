#' @title updateProteomicsFeatureSet
#' @description Function to dynamically retrieve the ftp uniprot data from NCBI 
#' and update the proteomics feature set in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism An organism to extract the dataset and update its features (required)
#' @param verbose A logical value of whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' @keywords internal
#' 
#' @import biomaRt
#'
#' @export
updateProteomicsFeatureSet <- function(
    conn_handler,
    organism, 
    verbose = TRUE
) {
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "UPDATE",
    required_role = "admin"
  )  
  
  # Check organism (required) #####
  if(base::length(organism) != 1 || base::all(organism %in% c(NA, "")))
    base::stop("'organism' must have a length of 1 and cannot be empty.")

  # Look up organism in the database
  organism_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "organisms", 
    return_var = "*", 
    filter_coln_var = "organism", 
    filter_coln_val = base::list("organism" = base::unique(organism)),
    check_db_table = TRUE
  ) 
  
  # Check if organisms exists
  if(base::nrow(organism_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    
    # Show message
    base::stop(base::sprintf("There are no organisms returned from the search parameters.\n"))
    
  }else{
  
    # Save to temp storage ####
    tmp_path <- base::tempdir()
    
    # Construct filename and URL dynamically
    file_name <- base::paste0(organism_tbl$prot_organism_code[1], "_", organism_tbl$prot_organism_taxid[1], "_idmapping_selected.tab.gz")
    url <- base::paste0("https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/", file_name)
    
    # Show message
    SigRepo::verbose(base::sprintf("Downloading: %s", url))
      
    # Download if not already present
    base::tryCatch({
      utils::download.file(url = url, destfile = base::file.path(tmp_path, file_name), method = "curl")
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop("Download failed.\n")
    }) 
    
    # Read the tab-separated data
    raw_data <- readr::read_tsv(base::file.path(tmp_path, file_name), col_names = FALSE, show_col_types = FALSE)
    
    # Check minimum number of columns before renaming
    if (base::ncol(raw_data) < 2) {
      base::stop("The downloaded file does not have the expected structure.")
    }
    
    # Get proteomics features in the database
    proteomics_tbl <- SigRepo::searchProteomicsFeatureSet(
      conn_handler = conn_handler,
      organism = organism
    ) |> 
      dplyr::mutate(version = base::as.Date(version, format = "%Y-%m-%d")) |>
      dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    
    # Check if proteomics features exist in the database for the searched organism #####
    if(base::nrow(proteomics_tbl) == 0)
      base::stop(base::sprintf("There are no proteomics features existed in the database for organism = '%s'.", organism))
  
    # Clean and transform
    feature_tbl <- raw_data |>
      dplyr::transmute(
        feature_name = X1,
        gene_symbol = base::gsub("(.*)_(.*)", "\\1", X2),
        new_version = base::format(base::Sys.Date(), "%Y-%m-%d")
      ) |> 
      dplyr::distinct(feature_name, .keep_all = TRUE) |> 
      dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    
    # Get the overlapping features and gene symbols
    overlapping_features <- proteomics_tbl |> dplyr::inner_join(feature_tbl)
    
    # Only update when the length of the overlapping features is different
    if(base::nrow(overlapping_features) > 0 && base::nrow(overlapping_features) != base::nrow(proteomics_tbl)){
      
      # Create a feature list 
      feature_name_list <- base::paste0("'", overlapping_features$feature_name, "'", collapse = ", ")
        
      # Create SQL statement 
      statement <- base::sprintf(
        "
        UPDATE proteomics_features
        SET version = %s, is_current = 1
        WHERE feature_name IN (%s) AND organism_id = %s;
        ", version, feature_name_list, organism_tbl$organism_id[1]
      )
      
      # Run SQL
      base::tryCatch({
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
      }, error = function(e){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }) 
      
      # Update features with gene symbols have changed in new version
      update_features <- feature_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = feature_name)) |> 
        dplyr::inner_join(proteomics_tbl |> dplyr::transmute(feature_name = feature_name, orig_gene_symbol = gene_symbol)) |> 
        dplyr::mutate(is_current = 1, version = new_version)
      
      # Update each record individually
      purrr::walk(
        base::seq_len(base::nrow(update_features)),
        function(s){
          #s=1;
          # Create SQL statement 
          statement <- base::sprintf(
            "
            UPDATE proteomics_features
            SET gene_symbol = '%s', is_current = %s, version = %s
            WHERE feature_name = '%s' AND organism_id = %s;
            ", update_features$gene_symbol[s], update_features$is_current[s], update_features$version[s], update_features$feature_name[s], organism_tbl$organism_id[1]
          )
          # RUN SQL
          base::tryCatch({
            base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
          }, error = function(e){
            # Disconnect from database ####
            base::suppressWarnings(DBI::dbDisconnect(conn))  
            # Return error message
            base::stop(e, "\n")
          }) 
        }
      )
      
      # Get new features not existed in database yet
      new_features <- feature_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = feature_name)) |> 
        dplyr::anti_join(update_features |> dplyr::transmute(feature_name = feature_name)) |> 
        dplyr::mutate(organism = organism, is_current = 1, version = new_version) 
      
      # If new features are not empty, add the newly features to database
      if(base::nrow(new_features) > 0){
        SigRepo::addProteomicsFeatureSet(conn_handler = conn_handler, feature_set = new_features)
      }
      
    }
    
    # Return message
    SigRepo::verbose("Finished updating.\n")
    
  }
}
