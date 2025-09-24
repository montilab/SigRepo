#' @title addSignatureWithID
#' @description Add signature to database with an assigned ID
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param omic_signature An R6 class object from OmicSignature package (required)
#' @param assign_signature_id Assign an unique ID to the uploaded signature (required)
#' @param assign_user_name Assign an unique user name to the uploaded signature (required) 
#' @param visibility A logical value indicates whether or not to allow others  
#' to view and access one's uploaded signature. Default is \code{FALSE}.
#' @param check_difexp A logical value indicates whether or not to check difexp
#' table in the database. Default is \code{TRUE}.
#' @param verbose A logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{FALSE}.
#'
#' @keywords internal
#' 
#' @export
addSignatureWithID <- function(
    conn_handler,
    omic_signature,
    assign_signature_id,
    assign_user_name,
    visibility = FALSE,
    check_difexp = TRUE,
    verbose = FALSE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )

  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Get visibility ####
  visibility <- base::ifelse(visibility == TRUE, 1, 0)
  
  # Check assign_signature_id
  if(!base::length(assign_signature_id) == 1 || base::all(assign_signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'assign_signature_id' must have a length of 1 and cannot be empty.\n")
  }else{
    signature_id <- assign_signature_id
  }
  
  # Check assign_user_name
  if(!base::length(assign_user_name) == 1 || base::all(assign_user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'assign_user_name' must have a length of 1 and cannot be empty.\n")
  }else{
    user_name <- assign_user_name
  }
  
  # Check and create signature metadata table ####
  metadata_tbl <- SigRepo::createSignatureMetadata(
    conn_handler = conn_handler,
    omic_signature = omic_signature
  )
  
  # Add additional variables in signature metadata table ####
  # Keep its original id and name of the user who owned the signature
  metadata_tbl <- metadata_tbl |> 
    dplyr::mutate(
      signature_id = signature_id[1],
      user_name = user_name[1],
      visibility = visibility[1]
    )
  
  # Create a new hash key for the signature ####
  metadata_tbl <- SigRepo::createHashKey(
    table = metadata_tbl,
    hash_var = "signature_hashkey",
    hash_columns = c("signature_name", "user_name"),
    hash_method = "md5"
  )
  
  # Check table against database table ####
  metadata_tbl <- SigRepo::checkTableInput(
    conn = conn,
    db_table_name = db_table_name,
    table = metadata_tbl, 
    exclude_coln_names = "date_created",
    check_db_table = FALSE
  )
  
  # Put signature back to its original form
  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    table = metadata_tbl,
    check_db_table = FALSE
  ) 
  
  # Put difexp back to its original form
  if(metadata_tbl$has_difexp[1] == TRUE && check_difexp == TRUE){
    # Extract difexp from omic_signature ####
    difexp <- omic_signature$difexp
    # Save difexp to local storage ####
    data_path <- base::tempdir()
    base::saveRDS(difexp, file = base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
    # Get API URL
    api_url <- base::sprintf("http://%s:%s/store_difexp?api_key=%s&signature_hashkey=%s", conn_handler$host[1], conn_handler$api_port[1], conn_info$api_key[1], metadata_tbl$signature_hashkey[1])
    # Store difexp in database
    res <- 
      httr::POST(
        url = api_url,
        body = base::list(
          difexp = httr::upload_file(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")), "application/rds")
        )
      )
    # Check status code
    if(res$status_code != 200){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))
      # Show message
      base::stop("Something went wrong with API. Cannot re-upload difexp table to the database. Please contact admin for support.\n")
    }else{
      # Remove files from file system 
      base::unlink(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
    }
  }
  
  # Put signature set back to its original form
  SigRepo::addTranscriptomicsSignatureSet(
    conn_handler = conn_handler,
    signature_id = metadata_tbl$signature_id[1],
    organism_id = metadata_tbl$organism_id[1],
    signature_set = omic_signature$signature,
    verbose = verbose
  )
  
  # Add user to signature access table after signature
  SigRepo::addUserToSignature(
    conn_handler = conn_handler,
    signature_id = metadata_tbl$signature_id[1],
    user_name = user_name,
    access_type = "owner",
    verbose = verbose
  )

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))   
  
  # Return 
  return(base::invisible())
  
}  



