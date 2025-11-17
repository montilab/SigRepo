# Unit testing the core workflow of the SigRepo package
# newConnHandler
# addSignature
# searchSignature
# getSignature
# updateSignature
# deleteSignature


# test connection handler
test_conn <<- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo"
)



# testing for Transcriptomic Signature ### 
test_that("addSignature correctly adds a transcriptomic signature into the database", {
  
  # Create signature object 
  test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))

  # Ensure a proper error message is displayed when using a wrong 
  # connection handler
  wrong_connection_handler <- -1
  expect_error( 
    
    error_id <- SigRepo::addSignature(
    conn_handler = wrong_connection_handler,
    omic_signature = test_transcriptomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  ),"Cannot connect to the database. Please check your connection handler.")
  
  
  # calling the function to add the test signature into the database ####
  # using <<- to add the id globally so the tests can use it ####
  transcriptomics_signature_id <<- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_transcriptomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  # Check that the function returned a single signature id ###
  expect_true(base::length(transcriptomics_signature_id) == 1)
  expect_type(transcriptomics_signature_id, "double") 
  
})



test_that("searchSignature accurately searches for the provided signature in the database", {
  
  # Searching for the signature by id
  search_result <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  # Check that the search result contains the expected signature id
  expect_equal(search_result$signature_id, transcriptomics_signature_id)
  
})



test_that("getSignature properly retrieves the added transcriptomic signature from the database", {
  
  # Retrieving the signature
  retrieved_signature <- SigRepo::getSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  # Check if retrieved_signature is an OmicSignature class object ####
  expect_true(methods::is(retrieved_signature[[1]], "OmicSignature"))
  
})


test_that("updateSignature updates the old data with the new data in the database",{
  
  # using the revised transcriptomics signature
  test_transcriptomics_sig_revised <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics_revised.rds"))
  
  # updating the signature in the database
  expect_no_error({
    updated_signature_id <- SigRepo::updateSignature(
      conn_handler = test_conn,
      signature_id = transcriptomics_signature_id,
      omic_signature = test_transcriptomics_sig_revised,
      verbose = FALSE
    )
  })
  
})

test_that("deleteSignature deleted the provided signature from the database along with its difexp file", {
  
  # Deleting the signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = transcriptomics_signature_id,
      verbose = FALSE
    )
  })
  
  # Confirm it is deleted from the database
  deleted_result <- SigRepo::getSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  expect_null(deleted_result)
  
})




