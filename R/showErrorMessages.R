
#' @title showOrganismErrorMessage
#' @description Error message for trying to add unknown organisms to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @keywords internal
#' 
#' @export
showOrganismErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(
    base::sprintf("\nThe following organisms do not exist in the '%s' table of the database: %s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchOrganisms()' to see a list of available organisms.\n"),
    base::sprintf("\nTo add these organisms to our database, please contact our admin for support.\n")
  ) 
  
}

#' @title showPlatformErrorMessage
#' @description Error message for trying to add unknown platforms to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @keywords internal
#' 
#' @export
showPlatformErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(
    base::sprintf("\nThe following platforms do not exist in the '%s' table of the database: %s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchPlatforms()' to see a list of available platforms.\n"),
    base::sprintf("\nIf you think you can use one of the already available platforms, please update your signature accordingly!\n"),
    base::sprintf("\nOtherwise, please consider adding your newly defined platforms to the database using SigRepo::addPlatform() before importing your signature.\n")
  )
  
}

#' @title showSampleTypeErrorMessage
#' @description Error message for trying to add unknown sample types to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @keywords internal
#' 
#' @export
showSampleTypeErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(
    base::sprintf("\nThe following sample types do not existed in the '%s' table of the database: %s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchSampleTypes()' to see a list of available sample types.\n"),
    base::sprintf("\nTo add these sample types to our database, please contact our admin for support.\n")
  )
  
}

#' @title showTranscriptomicsErrorMessage
#' @description Error message for trying to add unknown Transcriptomics Features to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @keywords internal
#' 
#' @export 
showTranscriptomicsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchTranscriptomicsFeatureSet()' to see a list of available features.\n"),
    base::sprintf("\nTo add these features to our database, please contact our admin for support.\n")
  )
  
}

#' @title showProteomicsErrorMessage
#' @description Error message for trying to add unknown Proteomics Features to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @keywords internal
#' 
#' @export 
showProteomicsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchProteomicsFeatureSet()' to see a list of available features.\n"),
    base::sprintf("\nTo add these features to our database, please contact our admin for support.\n")
  )
  
}

#' @title showAssayTypeErrorMessage
#' @description Error message for trying to add unknown assay types to the database
#' @param unknown_values The unknown assay type
#' @keywords internal
#' @export
showAssayTypeErrorMessage <- function(
    unknown_values
){
  
  base::stop(
    base::sprintf("\nThe following assay does not currently exist in the database yet: '%s'\n", unknown_values)
  )
  
}

