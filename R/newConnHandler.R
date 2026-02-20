

############################################################
# newConnHandler
############################################################

#' @title newConnHandler
#' @description Create a handler to connect to a remote database.
#' @param dbname Name of MySQL database (required)
#' @param host Name of the server where MySQL database is hosted on (required)
#' @param port Port on the server to connect to MySQL database (required)
#' @param user Name of user to establish the connection (required)
#' @param password Password associated with the user (required)
#' @param api_host Name of the server where the API is hosted on (required)
#' @param api_port Port on the server to access the API (required)
#' @return A list object containing connection parameters.
#' @export

newConnHandler <- function(
    dbname = "sigrepo",
    host = "sigrepo.org",
    port = 3306,
    user = "guest",
    password = "guest",
    api_host = "sigrepo.org",
    api_port = 8020
){
  
 
  
  # Input checks
  stopifnot(
    "'dbname' must have length 1 and not be empty." =
      (length(dbname) == 1 && !dbname %in% c(NA, "")),
    "'host' must have length 1 and not be empty." =
      (length(host) == 1 && !host %in% c(NA, "")),
    "'port' must have length 1 and be numeric." =
      (length(port) == 1 && is.numeric(port)),
    "'user' must have length 1 and not be empty." =
      (length(user) == 1 && !user %in% c(NA, "")),
    "'password' must have length 1 and not be empty." =
      (length(password) == 1 && !password %in% c(NA, "")),
    "'api_host' must have length 1 and not be empty." =
      (length(api_host) == 1 && !api_host %in% c(NA, "")),
    "'api_port' must have length 1 and be numeric." =
      (length(api_port) == 1 && is.numeric(api_port))
  )
  
  handle <- list(
    dbname = dbname,
    host = host,
    port = as.numeric(port),
    user = user,
    password = password,
    api_host = api_host,
    api_port = as.numeric(api_port)
  )
  
  # store internally
  
  .sigrepo_env$handle <- handle
  
  return(invisible(handle))
}


############################################################
# Handle Management
############################################################

#' @keywords internal
#' @export
init_handle <- function(handle){
  
  if (is.null(handle))
    stop("Cannot initialize handle with NULL.")
  
  .sigrepo_env$handle <- handle
  
  invisible(.sigrepo_env$handle)
}


#' @keywords internal
#' @export
get_handle <- function(){
  
  if (is.null(.sigrepo_env$handle))
    stop("No active handle for the database connection found.")
  
  return(.sigrepo_env$handle)
}


#' @keywords internal
#' @export
clear_handle <- function(){
  
  if (is.null(.sigrepo_env$handle))
    stop("No active handle to clear.")
  
  .sigrepo_env$handle <- NULL
  
  invisible(TRUE)
}


############################################################
# conn_init
############################################################

#' @title conn_init
#' @description Initiate a remote database connection
#' @param conn_handler Optional handler from newConnHandler().
#' If NULL, will use stored internal handle.
#' @return A MySQL connection object.
#' @export
#' @import DBI RMySQL

conn_init <- function(conn_handler = NULL){
  
  if (!is.null(conn_handler)) {
    # Optional: update stored handle
    init_handle(conn_handler)
  } else {
    conn_handler <- get_handle()
  }
  
  if (is.null(conn_handler)) {
    stop("No active handle for the database connection found.")
  }
  
  dbname <- conn_handler$dbname
  host <- conn_handler$host
  port <- conn_handler$port
  user <- conn_handler$user
  password <- conn_handler$password
  
  conn <- tryCatch({
    DBI::dbConnect(
      drv = RMySQL::MySQL(),
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    )
  }, error = function(e){
    stop(
      "Failed to connect to the database.\n",
      "Please check your host, username, password, and network connection.\n",
      "Technical details: ", as.character(e)
    )
  })
  
  ##########################################################
  # Root Auto-Provision Logic
  ##########################################################
  
  if (user == "root") {
    
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "user_name",
      filter_coln_var = "user_name",
      filter_coln_val = list("user_name" = user),
      check_db_table = TRUE
    )
    
    if (nrow(user_tbl) == 0) {
      
      table <- data.frame(
        user_name = user,
        user_password = password,
        user_email = "root@bu.edu",
        user_first = "root",
        user_last = "root",
        user_affiliation = "Boston University",
        user_role = "admin",
        active = 1,
        stringsAsFactors = FALSE
      )
      
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "user_password_hashkey",
        hash_columns = "user_password",
        hash_method = "md5"
      )
      
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "api_key",
        hash_columns = c("user_name", "user_email", "user_role"),
        hash_method = "md5"
      )
      
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "user_hashkey",
        hash_columns = "user_name",
        hash_method = "md5"
      )
      
      table <- SigRepo::removeDuplicates(
        conn = conn,
        db_table_name = "users",
        table = table,
        coln_var = "user_hashkey",
        check_db_table = FALSE
      )
      
      SigRepo::insert_table_sql(
        conn = conn,
        db_table_name = "users",
        table = table,
        check_db_table = FALSE
      )
    }
  }
  
  return(conn)
}
