
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
    base::sprintf("\nYou can use 'searchOrganism()' to see a list of available organisms.\n"),
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
    base::sprintf("\nYou can use 'searchPlatform()' to see a list of available platforms.\n"),
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
    base::sprintf("\nYou can use 'searchSampleType()' to see a list of available sample types.\n"),
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

#' @title showMetabolomicsErrorMessage
#' @description Error message for trying to add unknown Metabolomics Features to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' @param feature_database Optional metabolomics nomenclature name
#' @param attempted_growth Logical; whether growth was attempted during import
#'
#' @keywords internal
#'
#' @export
showMetabolomicsErrorMessage <- function(
    db_table_name,
    unknown_values,
    feature_database = NULL,
    attempted_growth = FALSE
){

  message_text <- c(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")),
    base::sprintf("\nYou can use 'searchMetabolomicsFeatureSet()' to see a list of available features.\n")
  )

  if (feature_database %in% c("hmdb", "refmet")) {
    message_text <- c(
      message_text,
      base::sprintf("\nCurated dictionaries such as HMDB and RefMet must be loaded before importing signatures.\n")
    )
  } else if (feature_database %in% c("smiles", "inchikey")) {
    if (attempted_growth) {
      message_text <- c(
        message_text,
        base::sprintf("\nSigRepo attempted to grow the '%s' dictionary during signature import, but these values were still unresolved.\n", feature_database),
        base::sprintf("\nThis usually means the inserted values could not be matched back uniquely after import.\n")
      )
    } else {
      message_text <- c(
        message_text,
        base::sprintf("\nSMILES and InChIKey dictionaries can grow during signature import when feature names are new.\n")
      )
    }
  }

  do.call(base::warning, as.list(message_text))

}

#' @title showMetabolomicsAmbiguityMessage
#' @description Warning message for ambiguous metabolomics identifier mappings
#' @param ambiguity_tbl Table returned by metabolomics ambiguity resolution
#'
#' @keywords internal
#'
#' @export
showMetabolomicsAmbiguityMessage <- function(
    ambiguity_tbl
){

  if (!methods::is(ambiguity_tbl, "data.frame") || base::nrow(ambiguity_tbl) == 0) {
    return(base::invisible(NULL))
  }

  ambiguity_text <- base::apply(
    ambiguity_tbl,
    1,
    function(row) {
      base::sprintf(
        "'%s' mapped to candidate metabolite_id(s): %s",
        row[["feature_name"]],
        row[["candidate_metabolite_ids"]]
      )
    }
  )

  base::warning(
    "\nAmbiguous metabolomics identifier mappings were detected:\n",
    base::paste0(ambiguity_text, collapse = "\n"),
    "\nResolve these mappings before importing the signature.\n"
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
showGeneticVariantsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s\n", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchGeneticVariantsFeatureSet()' to see a list of available features.\n"),
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
