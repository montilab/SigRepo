#' @title addSignature
#' @description Add signature to database
#' @param conn_handler An R object obtained from SigRepo::newConnhandler() (required) 
#' @param omic_signature An OmicSignature R6 object from the
#' OmicSignature package (required).
#' @param visibility Logical; whether the uploaded collection should be visible 
#' and accessible to others, Defaults to 'FALSE'
#' @param add_users A Data Frame; must contain the following column names:
#' 'user_name', 'access'. Access types are owner, viewer, or editor.This argument is only relevant when 
#' visibility is set to 'FALSE'. 
#' @param return_signature_id Logical; if 'TRUE', the function will 
#' return the ID generated for the newly uploaded signature. Defaults to 'FALSE'.
#' @param return_missing_features Logical; if set to 
#' 'TRUE' the function will return a list of missing features present in the OmicSignature. Defaults to 'FALSE'
#' @param verbose Logical; whether to print diagnostic messages. Defaults to 'TRUE'
#' 
#' @examples
#' \dontrun{
#' # SigRepo::addSignature(
#' # requried
#' conn_handler = conn_handler,
#' omic_signature = omc_signature_1,
#' # optional
#' visibility = FALSE, # default is false
#' add_users = c("John", "Jane"),
#' return_signature_id = TRUE,
#' verbose = TRUE)
#' }
#' 
#' 
#' @export


addSignatureRefactor <- function(
    conn_handler,
    omic_signature,
    visibility = TRUE,
    add_users = NULL,
    return_signature_id = FALSE,
    return_missing_features = FALSE,
    verbose = TRUE
){
  
  SigRepo::print_messages(verbose)
  
  conn <- SigRepo::conn_init(conn_handler)
  
  #--------------------------------------------------------
  # Helpers
  #--------------------------------------------------------
  rollback <- function(e, signature_id = NULL) {
    if (!is.null(signature_id)) {
      SigRepo::deleteSignature(conn_handler, signature_id, verbose = FALSE)
    }
    suppressWarnings(DBI::dbDisconnect(conn))
    stop(e)
  }
  
  with_rollback <- function(expr, signature_id = NULL) {
    tryCatch(expr, error = function(e) rollback(e, signature_id))
  }
  
  validate_add_users <- function(add_users) {
    if (is.null(add_users)) return(data.frame())
    if (!is.data.frame(add_users) ||
        !all(c("user_name", "access") %in% colnames(add_users))) {
      stop("<add_users> must be a data frame with columns 'user_name' and 'access'")
    }
    
    valid_users <- SigRepo::searchUser(conn_handler, user_name = add_users$user_name)
    missing <- setdiff(add_users$user_name, valid_users$user_name)
    if (length(missing))
      stop(sprintf("These users do not exist: %s", paste(missing, collapse=", ")))
    
    add_users
  }
  
  #--------------------------------------------------------
  # Permission check
  #--------------------------------------------------------
  info <- SigRepo::checkPermissions(conn, "INSERT", "editor")
  user_name <- info$user[1]
  visibility <- ifelse(visibility, 1, 0)
  
  if (visibility == 1) {
    if (!is.null(add_users))
      warning("visibility = 1 → signature is public; add_users ignored.")
    add_users <- data.frame()
  } else {
    add_users <- validate_add_users(add_users)
  }
  
  #--------------------------------------------------------
  # Validate signature & create metadata
  #--------------------------------------------------------
  omic_signature <- SigRepo::checkOmicSignature(omic_signature = omic_signature)
  
  metadata_tbl <- SigRepo::createSignatureMetadata(
    conn_handler = conn_handler,
    omic_signature = omic_signature, 
    verbose = FALSE
  ) |>
    dplyr::mutate(
      user_name = user_name,
      visibility = visibility
    ) |>
    SigRepo::createHashKey(
      hash_var     = "signature_hashkey",
      hash_columns = c("signature_name", "user_name"),
      hash_method  = "md5"
    )
  
  #--------------------------------------------------------
  # Check for existing signature
  #--------------------------------------------------------
  existing <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signatures", 
    return_var = "*", 
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
    check_db_table = TRUE
  ) 
  
  if (nrow(existing)) {
   
    warning(sprintf(
      "You already uploaded a signature '%s' (ID %s).",
      existing$signature_name[1], existing$signature_id[1]
    ))
  }
  
  #--------------------------------------------------------
  # Insert metadata
  #--------------------------------------------------------
  SigRepo::verbose("Uploading signature metadata...\n")
  
  SigRepo::insert_table_sql(conn, "signatures", metadata_tbl, check_db_table = FALSE)
  
  # retrieve new ID
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list(signature_hashkey = metadata_tbl$signature_hashkey[1]),
    check_db_table = FALSE
  )
  signature_id <- signature_tbl$signature_id[1]
  
  #--------------------------------------------------------
  # Save difexp (if applicable)
  #--------------------------------------------------------
  if (metadata_tbl$has_difexp) {
    
    with_rollback({
      SigRepo::verbose("Saving difexp...\n")
      
      path <- tempfile(fileext = ".RDS")
      saveRDS(omic_signature$difexp, path)
      
      api_url <- sprintf(
        "http://%s:%s/store_difexp?api_key=%s&signature_hashkey=%s",
        conn_handler$api_host[1],
        conn_handler$api_port[1],
        info$api_key[1],
        metadata_tbl$signature_hashkey[1]
      )
      
      res <- httr::POST(api_url, body = list(
        difexp = httr::upload_file(path, "application/rds")
      ))
      
      if (res$status_code != 200)
        stop("Error uploading difexp via API.")
      
      unlink(path)
    }, signature_id)
  }
  
  #--------------------------------------------------------
  # Add users (owner + extra)
  #--------------------------------------------------------
  with_rollback({
    SigRepo::verbose("Adding owner user...\n")
    SigRepo::addUserToSignature(conn_handler, signature_id, user_name, "owner", FALSE)
    
    if (nrow(add_users)) {
      SigRepo::verbose("Adding additional users...\n")
      SigRepo::addUserToSignature(
        conn_handler, signature_id,
        add_users$user_name,
        add_users$access,
        FALSE
      )
    }
  }, signature_id)
  
  #--------------------------------------------------------
  # Assay-specific signature-set upload
  #--------------------------------------------------------
  assay <- signature_tbl$assay_type[1]
  
  assay_handlers <- list(
    transcriptomics = SigRepo::addTranscriptomicsSignatureSet,
    proteomics      = SigRepo::addProteomicsSignatureSet,
    SNPs            = SigRepo::addSNPsSignatureSet
  )
  
  if (!assay %in% names(assay_handlers)) {
    SigRepo::showAssayTypeErrorMessage(assay)
  } else {
    
    handler <- assay_handlers[[assay]]
    
    warn_tbl <- with_rollback({
      handler(
        conn_handler,
        signature_id,
        signature_tbl$organism_id[1],
        omic_signature$signature,
        verbose = FALSE
      )
    }, signature_id)
    
    if (is.data.frame(warn_tbl) && nrow(warn_tbl)) {
      SigRepo::deleteSignature(conn_handler, signature_id, verbose = FALSE)
      suppressWarnings(DBI::dbDisconnect(conn))
      if (return_missing_features) return(warn_tbl)
      return(invisible())
    }
  }
  
  #--------------------------------------------------------
  # Finish
  #--------------------------------------------------------
  suppressWarnings(DBI::dbDisconnect(conn))
  SigRepo::verbose("Finished uploading.\n")
  SigRepo::verbose(sprintf("Signature ID: %s\n", signature_id))
  
  if (return_signature_id) signature_id else invisible()
}
