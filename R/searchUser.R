#' @title searchUser
#' @description Get phenotypes in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param user_name A list of user names to search by. Defaults to 'NULL', which
#' will return all of the users in the database
#' @param verbose Logical; whether or not to print the
#' diagnostic messages. Defaults to 'TRUE'.
#' @examples
#' \dontrun{
#' SigRepo::searchUser(conn_handler = conn_handler,
#'                     user_namne = "John_Doe")
#' }
#' 
#' 
#' @export
searchUser <- function(
    conn_handler,
    user_name = NULL,
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
    required_role = "editor"
  )
  
  # Look up signatures
  if(base::length(user_name) == 0 || base::all(user_name %in% c("", NA))){
    
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = c("user_name", "user_first", "user_last", "user_affiliation", "user_email", "user_role", "active"), 
      check_db_table = TRUE
    )  
    
  }else{
    
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = c("user_name", "user_first", "user_last", "user_affiliation", "user_email", "user_role", "active"), 
      filter_coln_var = "user_name", 
      filter_coln_val = base::list("user_name" = base::unique(user_name)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(user_tbl)

}







