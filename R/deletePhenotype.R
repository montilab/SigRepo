#' @title deletePhenotype
#' @description Remove phenotypes from database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param phenotype A list of phenotypes to be removed (required) 
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' @examples
#' \dontrun{
#' # example code
#' SigRepo::deletePhenotype(conn_handler = conn_handler,
#'                         phenotype = "test_phenotype",
#'                         verbose = TRUE)
#' }
#' 
#' @export
deletePhenotype <- function(
    conn_handler,
    phenotype,
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
  
  # Delete specific phenotypes from the database ####
  SigRepo::delete_table_sql(
    conn = conn,
    db_table_name = "phenotypes",
    delete_coln_var = "phenotype",
    delete_coln_val = base::unique(phenotype),
    check_db_table = FALSE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  # Return message
  SigRepo::verbose("Finished deleting.\n")
  
}



