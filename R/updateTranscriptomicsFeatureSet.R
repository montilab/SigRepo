#' @title updateTranscriptomicsFeatureSet
#' @description Function to dynamically transform the ftp uniprot data into the SigRepo dictionary structure
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism An organism to extract the dataset and update its features (required)
#' @param force A logical value of whether to force an update. Defaults to 'FALSE'.
#' @param verbose A logical value of whether to print diagnostic messages. Defaults to 'TRUE'.
#' 
#' @keywords internal
#' 
#' @import biomaRt
#'
#' @export
updateTranscriptomicsFeatureSet <- function(
    conn_handler,
    organism, 
    force = FALSE,
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

  # Define the force value
  force <- base::ifelse(force == TRUE, TRUE, FALSE)
  
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
    
    # Get the latest version available in the biomaRt
    current_ensembl_version <- biomaRt::listEnsembl() %>% 
      dplyr::mutate(version = base::gsub("(.*?)([1-9]{1,})", "\\2", .data$version)) %>% 
      dplyr::filter(biomart %in% "genes")
    
    # Check if the version is the same as current version #####
    if(force[1] == FALSE && current_ensembl_version$version[1] <= organism_tbl$biomart_version[1]){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      base::stop(base::sprintf("Current biomaRt version = '%s' is the same as the database version. Update aborted.", ))
    }   
    
    # Get biomaRt databaset
    ensembl <- base::tryCatch({
      biomaRt::useEnsembl(biomart = organism_tbl$biomart_db[1], dataset = organism_tbl$biomart_dataset[1], version = current_ensembl_version$version[1])
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      base::stop("Error in BiomaRt package:\n", base::as.character(e), "\n")
    })
    
    # Get transcriptomics features in the database
    transcriptomics_tbl <- SigRepo::searchTranscriptomicsFeatureSet(
      conn_handler = conn_handler,
      organism = organism
    ) |> dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    
    # Check if transcriptomics features exist in the database for the searched organism #####
    if(base::nrow(transcriptomics_tbl) == 0)
      base::stop(base::sprintf("There are no transcriptomics features existed in the database for organism = '%s'.", organism))
    
    # Grab ensembl ids and hgnc symbols 
    feature_tbl <- base::tryCatch({
      biomaRt::getBM(
        attributes = c("ensembl_gene_id", 'hgnc_symbol'), 
        mart = ensembl
      ) |> 
        dplyr::transmute(
          feature_name = ensembl_gene_id,
          gene_symbol = hgnc_symbol,
          new_version = current_ensembl_version$version[1]
        ) |> 
        dplyr::distinct(feature_name, .keep_all = TRUE) |> 
        dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }) 
    
    # Get the overlapping features and gene symbols
    overlapping_features <- transcriptomics_tbl |> dplyr::inner_join(feature_tbl)
    
    # Only update when the length of the overlapping features is different
    if(base::nrow(overlapping_features) > 0 && base::nrow(overlapping_features) != base::nrow(transcriptomics_tbl)){
      
      # Update each record individually
      purrr::walk(
        base::seq_len(base::nrow(overlapping_features)),
        function(s){
          #s=1;
          # Create SQL statement 
          statement <- base::sprintf(
            "
            UPDATE transcriptomics_features
            SET version = %s, is_current = 1
            WHERE feature_name = '%s' AND organism_id = %s;
            ", current_ensembl_version$version[1], overlapping_features$feature_name[s], organism_tbl$organism_id[1]
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
        }
      )
      
      # Update features with gene symbols have changed in new version
      update_features <- feature_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = feature_name)) |> 
        dplyr::inner_join(transcriptomics_tbl |> dplyr::transmute(feature_name = feature_name, orig_gene_symbol = gene_symbol)) |> 
        dplyr::mutate(is_current = 1, version = new_version)
      
      # Update each record individually
      purrr::walk(
        base::seq_len(base::nrow(update_features)),
        function(s){
          #s=1;
          # Create SQL statement 
          statement <- base::sprintf(
            "
            UPDATE transcriptomics_features
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
        SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = new_features)
      }
      
      # Archive the previous features as it is not existed in the new version
      archive_features <- transcriptomics_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = feature_name)) |> 
        dplyr::anti_join(update_features |> dplyr::transmute(feature_name = feature_name))
      
      # Check if archive features are empty
      if(base::nrow(archive_features) > 0){
        
        # Create a feature list 
        feature_name_list <- base::paste0("'", archive_features$feature_name, "'", collapse = ", ")
        
        # Create SQL statement 
        statement <- base::sprintf(
          "
          UPDATE transcriptomics_features
          SET is_current = 0
          WHERE feature_name IN (%s) AND organism_id = %s;
          ", feature_name_list, organism_tbl$organism_id[1]
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
    }
    
    # Create SQL statement to update version in organisms table
    statement <- base::sprintf(
      "
      UPDATE organisms
      SET biomart_version = '%s'
      WHERE organism_id = %s;
      ", current_ensembl_version$version[1], organism_tbl$organism_id[1]
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
    
    # Return message
    SigRepo::verbose("Finished updating.\n")
    
  }
}

