#' @title searchOrganism
#' @description Search for an organism in the database, can handle a singular organism or list 
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param organism an organism, or  list of organisms to search by. 
#' Default is NULL which will return all of the organisms in the database.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'
#' @export
searchOrganism <- function(
    conn_handler,
    organism = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)

  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Look up signatures
  if(base::length(organism) == 0 || base::all(organism %in% c("", NA))){
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "organism", 
      check_db_table = TRUE
    )  
    
  }else{
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "organism", 
      filter_coln_var = "organism", 
      filter_coln_val = base::list("organism" = base::unique(organism)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(organism_tbl)

}







