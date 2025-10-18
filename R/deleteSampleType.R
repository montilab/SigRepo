#' @title deleteSampleType
#' @description Remove sample types from database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @param sample_type A list of sample types to be removed (required)
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
deleteSampleType <- function(
    conn_handler,
    sample_type,
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
    required_role = "admin"
  )
  
  # Delete specific sample types from the database ####
  SigRepo::delete_table_sql(
    conn = conn,
    db_table_name = "sample_types",
    delete_coln_var = "sample_type",
    delete_coln_val = base::unique(sample_type),
    check_db_table = FALSE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))  
  
  # Return message
  SigRepo::verbose("Finished deleting.\n")
  
}



