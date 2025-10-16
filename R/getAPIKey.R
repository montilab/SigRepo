#' @title getAPIKey
#' @description Get API Key of a specific user in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @export
getAPIKey <- function(
    conn_handler
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Look up api key
  api_key_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "api_key"), 
    filter_coln_var = "user_name", 
    filter_coln_val = base::list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return table
  return(api_key_tbl)
  
}



