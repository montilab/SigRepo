#' @title newConnHandler
#' @description Create a handler to connect to a remote database.
#' @param dbname Name of MySQL database to point to (required)
#' @param host Name of the server where MySQL database is hosted on (required)
#' @param port Port on the server to connect to MySQL database (required)
#' @param api_port Port on the server to access database API (required)
#' @param user Name of user to establish the connection (required)
#' @param password Password associated with the user (required)
#' @return A list of user credentials to establish connection to the remote database.
#' 
#' @export
newConnHandler <- function(
    dbname = 'sigrepo', 
    host = "sigrepo.org", 
    port = 3306, 
    api_port = 8020,
    user = "guest", 
    password = "guest"
){
  
  # Check dbname ####
  base::stopifnot("'dbname' must have a length of 1 and cannot be empty." = 
                    (base::length(dbname) == 1 && !dbname %in% c(NA, "")))
  
  # Check host ####
  base::stopifnot("'host' must have a length of 1 and cannot be empty." = 
                    (base::length(host) == 1 && !host %in% c(NA, "")))
  
  # Check port ####
  base::stopifnot("'port' must have a length of 1 and cannot be empty and must be a numeric value." = 
                    (base::length(port) == 1 && !port %in% c(NA, "")))
  
  # Check api_port ####
  base::stopifnot("'api_port' must have a length of 1 and cannot be empty and must be a numeric value." = 
                    (base::length(api_port) == 1 && !api_port %in% c(NA, ""))) 
  
  # Check user ####
  base::stopifnot("'user' must have a length of 1 and cannot be empty." = 
                    (base::length(user) == 1 && !user %in% c(NA, "")))
  
  # Check password ####
  base::stopifnot("'password' must have a length of 1 and cannot be empty." = 
                    (base::length(password) == 1 && !password %in% c(NA, "")))
  
  # Return connection handler ###
  return(
    base::list(
      dbname = dbname,
      host = host,
      port = port,
      api_port = api_port,
      user = user,
      password = password
    )
  )
  
}

#' @title conn_init
#' @description Initiate a remote database connection
#' @param conn_handler A handler uses to establish connection to a remote database 
#' obtained from SigRepo::newConnhandler() (required)
#' 
#' @keywords internal
#' 
#' @return a MySQL connection class object
#' 
#' @export
#' @import DBI RMySQL 
conn_init <- function(conn_handler){
  
  # Extract user credentials
  dbname <- conn_handler$dbname
  host <- conn_handler$host
  port <- conn_handler$port
  user <- conn_handler$user
  password <- conn_handler$password
  
  # Check connection
  conn <- base::tryCatch({
    DBI::dbConnect(
      drv = RMySQL::MySQL(),
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    )
  }, error = function(e){
    base::stop(e, "\n")
  })
  
  # If user is root, validate if root exists in the users table of the database
  if(user == "root"){
    
    # Look up user in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = "user_name", 
      filter_coln_var = "user_name", 
      filter_coln_val = list("user_name" = user), 
      check_db_table = TRUE
    )
    
    # If user has a root access but does not exist in users table of the database
    # Then add root to users table with the default settings below
    if(base::nrow(user_tbl) == 0){
      
      # Default settings for root ####
      table <- base::data.frame(
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
      
      # Check for duplicated emails ####
      SigRepo::checkDuplicatedEmails(
        conn = conn,
        db_table_name = "users",
        table = table,
        coln_var = "user_email",
        check_db_table = FALSE
      )
      
      # Create a hash key for user password ####
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "user_password_hashkey",
        hash_columns = "user_password",
        hash_method = "md5"
      )
      
      # Create api keys ####
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "api_key",
        hash_columns = c("user_name", "user_email", "user_role"),
        hash_method = "md5"
      )
      
      # Create a hash key to check for duplicates ####
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "user_hashkey",
        hash_columns = "user_name",
        hash_method = "md5"
      )
      
      # Remove duplicates ####
      table <- SigRepo::removeDuplicates(
        conn = conn,
        db_table_name = "users",
        table = table,
        coln_var = "user_hashkey",
        check_db_table = FALSE
      )
      
      # Insert table into database ####
      SigRepo::insert_table_sql(
        conn = conn, 
        db_table_name = "users", 
        table = table,
        check_db_table = FALSE
      )  
      
    }
  }
  
  # Return connection
  return(conn)
  
}



