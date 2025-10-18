#' @title deleteCollection
#' @description Delete a collection from the collection table of the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @param collection_id ID of the collection being removed from the database (required)
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
deleteCollection <- function(
    conn_handler, 
    collection_id,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "DELETE",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Get unique collection id
  collection_id <- base::unique(collection_id) 
  
  # Get table name in database ####
  db_table_name <- "collection"
  
  # Check collection_id
  if(!length(collection_id) == 1 || collection_id %in% c(NA, "")){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("\n'collection_id' must have a length of 1 and cannot be empty.\n")
  }
  
  # Check if signature exists ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "collection_id",
    filter_coln_val = list("collection_id" = collection_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("\nThere is no collection_id = '%s' existed in the 'collection' table of the SigRepo database.\n", collection_id))
    
  }else{
    
    # If user is not admin, check if it has access to signature
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      collection_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = db_table_name,
        return_var = "*",
        filter_coln_var = c("collection_id", "user_name"), 
        filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(collection_tbl) == 0){
        
        collection_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "collection_access",
          return_var = "*",
          filter_coln_var = c("collection_id", "user_name", "access_type"),
          filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name, access_type = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user has access, get the signature metadata table 
        if(nrow(collection_access_tbl) > 0){
          
          collection_tbl <- SigRepo::lookup_table_sql(
            conn = conn,
            db_table_name = db_table_name,
            return_var = "*",
            filter_coln_var = "collection_id",
            filter_coln_val = list("collection_id" = collection_access_tbl$collection_id),
            check_db_table = FALSE
          )
          
        }else{
          
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(base::sprintf("\nUser = '%s' does not have permission to delete collection_id = '%s' from the SigRepo database.\n", user_name, collection_id))
          
        }
      }
    }
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove collection_id = '%s' from 'collection' table of the database.", collection_id))
    
    # Delete signature from collection metadata table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = FALSE
    )

    # Return message
    SigRepo::verbose(base::sprintf("Remove collection_id = '%s' from 'collection_access' table of the database.", collection_id))

    # Delete user from collection_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "collection_access",
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = TRUE
    )
    
    # Return message
    SigRepo::verbose(base::sprintf("Remove signatures belongs to collection_id = '%s' from 'signature_collection_access' table of the database.", collection_id))
    
    # Delete signature from signature_collection_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_collection_access",
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    
    # Return message
    SigRepo::verbose(base::sprintf("collection_id = '%s' has been removed.", collection_id))

  } 
}








