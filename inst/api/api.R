
# For API 
library(plumber)
library(httr)
library(jsonlite)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST_DB_NET"), 
  port = as.integer(Sys.getenv("PORT")),
  user = Sys.getenv("DB_USER"), 
  password = Sys.getenv("PASSWORD")
)

# 
# This is a Plumber API. You can run the API by clicking the 'run API' button above
# 
# Found out more about building APIs with Plumber here:
#
#       https://www.rplumber.io/
#

# API title and description
#* @apiTitle Plumber API
#* @apiDescription This is a server for accessing data on the SigRepo Database
#* @apiContact list(name = "Reina Chau", email = "rchau88@bu.edu") 

# Set-up header access

#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Log some information about the incoming request
#* @filter logger
function(req){
  base::cat(base::as.character(Sys.time()), "-",
            req$REQUEST_METHOD, req$PATH_INFO, "-",
            req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# Create a list of serializers to return the object ####
serializers <- list(
  "html" = plumber::serializer_html(),
  "json" = plumber::serializer_json(),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

# Create difexp directory
data_path <- base::file.path("/difexp")
base::dir.create(path = data_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")

#* Store difexp in the database
#* @parser multi
#* @parser rds
#* @param api_key
#* @param signature_hashkey
#* @param difexp:file
#' @post /store_difexp
store_difexp <- function(res, api_key, signature_hashkey, difexp){
  
  # parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid api key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Store difexp
  if(base::is.data.frame(difexp[[1]])){
    base::saveRDS(difexp[[1]], file = base::file.path(data_path, base::paste0(signature_hashkey, ".RDS")))
  }else{
    warn_tbl <- base::data.frame(MESSAGES = base::sprintf("difexp is not a valid file."))
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }  
  
}


#* Get difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @get /get_difexp
get_difexp <- function(res, api_key, signature_hashkey){
  
  # Parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid API Key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Get difexp path
  difexp_path <- base::file.path(data_path, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_path)){
    difexp <- base::readRDS(difexp_path)
    return(jsonlite::toJSON(difexp, pretty=TRUE))
  }else{
    warn_tbl <- base::data.frame(MESSAGES = base::sprintf("There is no difexp file found for signature_hashkey = '%s'", signature_hashkey))
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }  
  
}

#* Delete difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @delete /delete_difexp
delete_difexp <- function(res, api_key, signature_hashkey){
  
  # Parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid API Key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Get difexp path
  difexp_path <- base::file.path(data_path, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_path)){
    base::unlink(difexp_path)
  }
  
  # Return messages
  tbl <- base::data.frame(MESSAGES = base::sprintf("difexp file has been removed for signature_hashkey = '%s'", signature_hashkey))
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}

#* Activate registered users in the database
#* @param user_name
#* @param api_key
#' @get /activate_user
activate_user <- function(res, user_name, api_key){
  
  variables <- c("user_name", "api_key")
  
  # Check parameters
  if(base::missing(user_name) || base::missing(api_key)){
    
    missing_variables <- c(base::missing(user_name),  base::missing(api_key))
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[which(missing_variables==TRUE)], collapse = ", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  user_name <- base::trimws(user_name[1])
  api_key <- base::trimws(api_key[1]) 
  
  # Check user_name ####
  if(user_name %in% c(NA, "")){
    
    error_message <- "user_name cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    # Check user table
    check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
    # If user exists, throw an error
    if(nrow(check_user_tbl) == 0){
      error_message <- base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name)
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(!api_key %in% base::Sys.getenv("API_KEY")){
    
    error_message <- "Invalid API Key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Activate user
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, active = TRUE)
  
  # Send email to users to notify their account are activated
  api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/send_notifications/activate_user?user_name=%s&api_key=%s", user_name, api_key)
  
  # Send email to users through montilab server API
  res <- httr::GET(url = api_url)
  
  # Check status code
  if(res$status_code != 200){
    MESSAGES <- base::sprintf("Something went wrong with the API. Cannot activate user. Please contact admin for support.")
  }else{
    MESSAGES <- base::sprintf("User = '%s' has been activated. A notified email has been sent to user.", user_name)
  }
  
  # Return message ####
  res$serializer <- serializers[["json"]]
  res$status <- 200
  tbl <- base::data.frame(MESSAGES = MESSAGES)
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}
