#' @title searchSampleType
#' @description Search sample types in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param sample_type A sample type, or a  list of sample types to search by. Default is NULL which
#' will return all of the sample types in the database.
#' @param brenda_accession A list of Brenda accession to search by. Default is NULL.
#' @param verbose A logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' @examples
#' \dontrun{
#' SigRepo::searchSampleType(conn_handler = conn_handler,
#'                           sample_type = "test_sample_type",
#'                           )
#' }
#' 
#'
#' @export
searchSampleType <- function(
    conn_handler,
    sample_type = NULL,
    brenda_accession = NULL,
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
  
  # Look up signatures
  if(base::length(sample_type) == 0 || base::all(sample_type %in% c("", NA))){
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type", "brenda_accession"),
      check_db_table = TRUE
    )  
    
  }else{
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type", "brenda_accession"),
      filter_coln_var = "sample_type", 
      filter_coln_val = base::list("sample_type" = base::unique(sample_type)),
      check_db_table = TRUE
    ) 
    
  }
  
  # Get a list of filtered variables
  filter_var_list <- base::list(
    "brenda_accession" = base::unique(brenda_accession)
  )
  
  # Filter table with given search variables
  for(r in base::seq_along(filter_var_list)){
    #r=1;
    filter_status <- base::ifelse(base::length(filter_var_list[[r]]) == 0 || base::all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if(filter_status == TRUE){
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][base::which(!filter_var_list[[r]] %in% c(NA, ""))]
      sample_type_tbl <- sample_type_tbl |> dplyr::filter(base::trimws(base::tolower(!!!rlang::syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(sample_type_tbl)

}







