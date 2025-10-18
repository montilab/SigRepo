#' @title addCollection
#' @description Add signature collections to database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @param omic_collection A collection of OmicSignature objects from 
#' OmicSignature package (required) 
#' @param visibility A logical value indicates whether or not to allow others 
#' to view and access one's uploaded collection. Default is \code{FALSE}.
#' @param return_collection_id a logical value indicates whether or not to return
#' the ID of the uploaded collection. Default is \code{FALSE}.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}. #test
#' 
#' @export
addCollection <- function(
    conn_handler,
    omic_collection,
    visibility = FALSE,
    return_collection_id = FALSE,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get table name in database ####
  db_table_name <- "collection"
  
  # Get visibility ####
  visibility <- base::ifelse(visibility == TRUE, 1, 0)
  
  # Create collection metadata table ####
  metadata_tbl <- SigRepo::createCollectionMetadata(
    conn_handler = conn_handler, 
    omic_collection = omic_collection
  ) 
  
  # Add additional variables in collection metadata table ####
  metadata_tbl <- metadata_tbl |> 
    dplyr::mutate(
      user_name = user_name,
      visibility = visibility
    )
  
  # Create a hash key to look up whether collection is already existed in the database ####
  metadata_tbl <- SigRepo::createHashKey(
    table = metadata_tbl,
    hash_var = "collection_hashkey",
    hash_columns = c("collection_name", "user_name"),
    hash_method = "md5"
  )
  
  # Check if collection exists using its hash key ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "collection_hashkey",
    filter_coln_val = base::list("collection_hashkey" = metadata_tbl$collection_hashkey),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(base::nrow(collection_tbl) > 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Show message
    SigRepo::verbose(
      base::sprintf("\tYou already uploaded a collection with the name = '%s' to the SigRepo database.\n", metadata_tbl$collection_name),
      base::sprintf("\tID of the uploaded collection: %s\n", collection_tbl$collection_id)
    )
    
    # Return collection id
    if(return_collection_id == TRUE){
      return(collection_tbl$collection_id[1])
    }
    
  }else{
    
    # 1. Uploading each signature in the collection into the database
    SigRepo::verbose("Uploading each signature in the collection to the database...\n")
    
    # Extract omic_sig_list from omic_collection ####
    omic_sig_list <- omic_collection$OmicSigList
    
    # Add signature into the database ####
    signature_id_list <- c()
    
    for(c in base::seq_along(omic_sig_list)){
      #c=1;
      signature_id <- base::tryCatch({
        SigRepo::addSignature(
          omic_signature = omic_sig_list[[c]],
          conn_handler = conn_handler,
          visibility = visibility,
          return_signature_id = TRUE,
          verbose = FALSE
        )
      }, error = function(e){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }) 
      # Check if warning table is returned
      if(base::length(signature_id) == 0){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::warning(base::sprintf("Cannot upload signature_name = '%s' to the database.\n", omic_sig_list[[c]]$metadata$signature_name[1]))
        # Exit loop
        break
      }
      signature_id_list <- c(signature_id_list, signature_id)
    }
    
    # Check signature_id_list
    if(base::length(signature_id_list) != base::length(omic_sig_list)){ return(base::invisible()) }
    
    # Reset options
    SigRepo::print_messages(verbose = verbose)
    
    # 2. Uploading collection metadata into database
    SigRepo::verbose("Uploading collection metadata to the database...\n")
    
    # Check table against database table ####
    metadata_tbl <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = c("collection_id", "date_created"),
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # Look up collection id for the next step ####
    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "collection_hashkey",
      filter_coln_val = base::list("collection_hashkey" = metadata_tbl$collection_hashkey),
      check_db_table = FALSE
    ) 
    
    # 3. Adding user to collection access table after collection
    # was imported successfully in step (1)
    SigRepo::verbose("Adding user to collection access table in the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addUserToCollection(
        conn_handler = conn_handler,
        collection_id = collection_tbl$collection_id,
        user_name = user_name,
        access_type = "owner",
        verbose = FALSE
      )
    }, error = function(e){
      # Delete signature
      SigRepo::deleteCollection(conn_handler = conn_handler, collection_id = collection_tbl$collection_id, verbose = FALSE)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }) 
    
    # Reset options
    SigRepo::print_messages(verbose = verbose)
    
    # 4. Adding signature to collection access table after collection
    # was imported successfully in step (1)
    SigRepo::verbose("Adding signature to the collection access table of the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addSignatureToCollection(
        conn_handler = conn_handler,
        collection_id = collection_tbl$collection_id,
        signature_id = signature_id_list,
        verbose = FALSE
      )
    }, error = function(e){
      # Delete signature
      SigRepo::deleteCollection(conn_handler = conn_handler, collection_id = collection_tbl$collection_id[1], verbose = FALSE)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }) 
    
    # Reset options
    SigRepo::print_messages(verbose = verbose)
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return message
    SigRepo::verbose("Finished uploading.\n")
    SigRepo::verbose(base::sprintf("ID of the uploaded collection: %s\n", collection_tbl$collection_id[1]))
    
    # Return collection id
    if(return_collection_id == TRUE){
      return(collection_tbl$collection_id[1])
    }

  }
}

