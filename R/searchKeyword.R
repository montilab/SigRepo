#' @title searchKeyword
#' @description Search for platform in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param keyword A list of keywords to be looked up. 
#' Default is NULL which will return all of the keywords in the database.
#' @param verbose A logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
searchKeyword <- function(
    conn_handler,
    keyword = NULL,
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
  
  # Look up keyword
  if(base::length(keyword) == 0 || base::all(keyword %in% c("", NA))){
    
    keyword_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "keywords", 
      return_var = "keyword", 
      check_db_table = TRUE
    )  
    
  }else{
    
    keyword_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "keywords", 
      return_var = "keyword", 
      filter_coln_var = "keyword", 
      filter_coln_val = base::list("keyword" = base::unique(keyword)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(keyword_tbl)

}







