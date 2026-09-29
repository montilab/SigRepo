#' @title searchPlatform
#' @description Search for platform in the database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param platform A platform, or a list of platform names to be looked up. Defaults to
#' 'NULL', which will return all of the platforms in the database.
#' @param verbose Logical; whether or not to print the diagnostic messages. 
#' Defaults to 'TRUE'.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Create a connection handler
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", 
#'   host = "sigrepo.org", 
#'   port = 3306, 
#'   user = "your_username", 
#'   password = "your_password"
#' )
#' 
#' # Search for a list of platforms in the database
#' SigRepo::searchPlatform(
#'   conn_handler = conn_handler,
#'   platform = "test_platform",
#'   verbose = TRUE
#' )
#' 
#' }
#'       
#' @export
searchPlatform <- function(
    conn_handler = NULL,
    platform = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler)
  on.exit(conn_close(conn), add = TRUE)
 
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Look up signatures
  if(base::length(platform) == 0 || base::all(platform %in% c("", NA))){

    platform_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "platforms",
      return_var = "platform",
      check_db_table = TRUE
    )

  }else{

    platform_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "platforms",
      return_var = "platform",
      filter_coln_var = "platform",
      filter_coln_val = base::list("platform" = base::unique(platform)),
      check_db_table = TRUE
    )

  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(platform_tbl)

}







