#' @title updateTranscriptomicsFeatureSet
#' @description Function to dynamically transform the ftp uniprot data into the SigRepo dictionary structure
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param organism An organism to extract the dataset and update its features (required)
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
  if(base::length(organism) != 1 || base::all(organism %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Return error message
    base::stop("'organism' must have a length of 1 and cannot be empty.\n")
  }
  
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
    
    # Show message
    SigRepo::verbose(base::sprintf("Getting the latest version available in the biomaRt...\n"))
    
    current_ensembl_version <- biomaRt::listEnsembl() %>% 
      dplyr::mutate(version = base::gsub("(.*?)([1-9]{1,})", "\\2", .data$version)) %>% 
      dplyr::filter(.data$biomart %in% "genes")
    
    # Show message
    SigRepo::verbose(base::sprintf("Checking biomaRt version against the database version...\n"))
    
    if(current_ensembl_version$version[1] < organism_tbl$biomart_version[1]){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      base::stop(base::sprintf("Current biomaRt version = '%s' is older than the database version = '%s'. Updates canceled.\n", current_ensembl_version$version[1], organism_tbl$biomart_version[1]))
    }else if(current_ensembl_version$version[1] == organism_tbl$biomart_version[1]){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      base::stop(base::sprintf("Current biomaRt version = '%s' is the same as the database version = '%s'. No updates needed.\n", current_ensembl_version$version[1], organism_tbl$biomart_version[1]))
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
    transcriptomics_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "transcriptomics_features", 
      return_var = "*", 
      filter_coln_var = "organism_id", 
      filter_coln_val = base::list("organism_id" = organism_tbl$organism_id[1]),
      check_db_table = TRUE
    )  |> 
      dplyr::transmute(
        feature_name = base::trimws(base::tolower(.data$feature_name)),
        gene_symbol = base::trimws(base::tolower(.data$gene_symbol)),
        orig_feature_name = feature_name,
        orig_gene_symbol = gene_symbol,
        organism_id = .data$organism_id,
        is_current = .data$is_current,
        version = .data$version
      ) |>
      dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })
    
    # Check if transcriptomics features exist in the database for the searched organism #####
    if(base::nrow(transcriptomics_tbl) == 0){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn)) 
      # Return error message
      base::stop(base::sprintf("There are no transcriptomics features existed in the database for organism = '%s'.\n", organism))
    }
    
    # Grab ensembl ids and hgnc symbols 
    feature_tbl <- base::tryCatch({
      biomaRt::getBM(
        attributes = c("ensembl_gene_id", 'hgnc_symbol'), 
        mart = ensembl
      ) |> 
        dplyr::transmute(
          feature_name = base::trimws(base::tolower(.data$ensembl_gene_id)),
          gene_symbol = base::trimws(base::tolower(.data$hgnc_symbol)),
          new_feature_name = base::trimws(.data$ensembl_gene_id),
          new_gene_symbol = base::trimws(.data$hgnc_symbol),
          new_organism_id =  organism_tbl$organism_id[1],
          new_is_current = 1,
          new_version = current_ensembl_version$version[1],
        ) |> 
        dplyr::distinct(.data$feature_name, .keep_all = TRUE) |> 
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
      
      # Show message
      SigRepo::verbose(base::sprintf("Updating features to latest version...\n"))
      
      # Update table with 1000 records each time
      n_feature_ingest <- 1000
      n_feature <- base::nrow(overlapping_features)
      n_remainder <- n_feature %% n_feature_ingest
      n_loop <- base::round(n_feature/n_feature_ingest) + base::ifelse(n_remainder > 0, 1, 0)
      
      purrr::walk(
        base::seq_len(n_loop),
        function(s){
          #s=1;
          if(n_loop == 1 && n_remainder == 0){
            feature_list <- base::paste0("'", overlapping_features$orig_feature_name[1:n_feature_ingest], "'", collapse = ", ")
          }else if(n_loop == 1 && n_remainder > 0){
            feature_list <- base::paste0("'", overlapping_features$orig_feature_name[1:remainder], "'", collapse = ", ")
          }else if(n_loop > 1 && n_remainder == 0){
            feature_list <- base::paste0("'", overlapping_features$orig_feature_name[((s-1)*n_feature_ingest):(s*n_feature_ingest)], "'", collapse = ", ")
          }else if(n_loop > 1 && n_remainder > 0 && s == n_loop){
            feature_list <- base::paste0("'", overlapping_features$orig_feature_name[((s-1)*n_feature_ingest):(((s-1)*n_feature_ingest)+n_remainder)], "'", collapse = ", ")
          }
          
          # Create SQL statement 
          statement <- base::sprintf(
            "
            UPDATE transcriptomics_features
            SET version = %s, is_current = 1
            WHERE feature_name IN (%s) AND organism_id = %s;
            ", current_ensembl_version$version[1], feature_list, organism_tbl$organism_id[1]
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
      
      # Update features with new gene symbols
      update_features <- feature_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = .data$feature_name)) |> 
        dplyr::inner_join(transcriptomics_tbl |> dplyr::transmute(feature_name = .data$feature_name, orig_feature_name = .data$orig_feature_name))

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
            ", update_features$new_gene_symbol[s], update_features$new_is_current[s], update_features$new_version[s], update_features$orig_feature_name[s], organism_tbl$organism_id[1]
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
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = .data$feature_name)) |> 
        dplyr::anti_join(update_features |> dplyr::transmute(feature_name = .data$feature_name)) |> 
        dplyr::transmute(feature_name = .data$new_feature_name, gene_symbol = .data$new_gene_symbol, organism = organism, is_current = .data$new_is_current, version = .data$new_version) 
      
      # If new features are not empty, add the newly features to the database
      if(base::nrow(new_features) > 0){
        SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = new_features)
      }
      
      # Archive the previous features as they are not existed in the new version anymore
      archive_features <- transcriptomics_tbl |> 
        dplyr::anti_join(overlapping_features |> dplyr::transmute(feature_name = .data$feature_name)) |> 
        dplyr::anti_join(update_features |> dplyr::transmute(feature_name = .data$feature_name))
      
      # Check if archive features are empty
      if(base::nrow(archive_features) > 0){
        
        # Update table with 1000 records each time
        n_feature_ingest <- 1000
        n_feature <- base::nrow(archive_features)
        n_remainder <- n_feature %% n_feature_ingest
        n_loop <- base::round(n_feature/n_feature_ingest) + base::ifelse(n_remainder > 0, 1, 0)
        
        # Update each record individually
        purrr::walk(
          base::seq_len(base::nrow(update_features)),
          function(s){
            #s=1;
            if(n_loop == 1 && n_remainder == 0){
              feature_list <- base::paste0("'", overlapping_features$orig_feature_name[1:n_feature_ingest], "'", collapse = ", ")
            }else if(n_loop == 1 && n_remainder > 0){
              feature_list <- base::paste0("'", overlapping_features$orig_feature_name[1:remainder], "'", collapse = ", ")
            }else if(n_loop > 1 && n_remainder == 0){
              feature_list <- base::paste0("'", overlapping_features$orig_feature_name[((s-1)*n_feature_ingest):(s*n_feature_ingest)], "'", collapse = ", ")
            }else if(n_loop > 1 && n_remainder > 0 && s == n_loop){
              feature_list <- base::paste0("'", overlapping_features$orig_feature_name[((s-1)*n_feature_ingest):(((s-1)*n_feature_ingest)+n_remainder)], "'", collapse = ", ")
            }
            
            # Create SQL statement 
            statement <- base::sprintf(
              "
              UPDATE transcriptomics_features
              SET is_current = 0
              WHERE feature_name IN (%s) AND organism_id = %s;
              ", feature_list, organism_tbl$organism_id[1]
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
      }
      
      # Show message
      SigRepo::verbose(base::sprintf("Updating biomart to latest version...\n"))
      
      # Create SQL statement to update version in organisms table
      statement <- base::sprintf(
        "
        UPDATE organisms
        SET biomart_version = '%s', biomart_updated_date = '%s'
        WHERE organism_id = %s;
        ", current_ensembl_version$version[1], base::as.Date(base::Sys.Date(), format = "%Y-%m-%d"), organism_tbl$organism_id[1]
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
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      
      # Return message
      SigRepo::verbose("Finished updating.\n")
      
    }else{
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      
      # Return message
      SigRepo::verbose("All features are up to date. No updates needed.\n")
      
    }
  }
}  
  