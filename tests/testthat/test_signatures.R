# testing script for signature functions in the SigRepo package
# addSignature
# searchSignature

test_that("addSignature correctly adds a signature into the database", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  expect_true(base::length(omic_signature_id) == 1)
  expect_type(omic_signature_id, "double") 
  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
  
})

test_that("searchSignature correctly searches for the desired signature", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  
  signature_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_id = omic_signature_id,
    verbose = FALSE
  )
  #print(str(signature_search))
  
  expect_true(methods::is(signature_search, "data.frame"))
  expect_true(nrow(signature_search) > 0)
  expect_equal(signature_search$signature_name[1], "test_signature")
  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
  
  
})

test_that("searchSignature returns all signatures when no filters provided", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  all_signatures <- SigRepo::searchSignature(
    conn_handler = test_conn,
    verbose = FALSE
  )
  
  #print("nrow(all_signatures)")
  
  expect_true(methods::is(all_signatures, "data.frame"))
  expect_true(nrow(all_signatures) >= 0)
})

test_that("searchSignature filters by organism", {
  test_conn <- create_test_conn()
  
  organism_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  
  #print(head(organism_search))
  
  expect_true(methods::is(organism_search, "data.frame"))
  expect_true(nrow(organism_search) > 0)
  
})

test_that("searchSignature filters by phenotype", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  phenotype_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    phenotype = "Aging",
    verbose = FALSE
  )

  expect_true(methods::is(phenotype_search, "data.frame"))
  expect_true(nrow(phenotype_search) > 0)
})

test_that("searchSignature filters by platform", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  platform_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    platform = "transcriptomics by array",
    verbose = FALSE
  )

  expect_true(methods::is(platform_search, "data.frame"))
  expect_true(nrow(platform_search) > 0)
})

test_that("searchSignature returns empty result for non-existent signature", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  signature_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = "non_existent_signature_xyz123",
    verbose = FALSE
  )
  
  expect_true(methods::is(signature_search, "data.frame"))
  expect_true(nrow(signature_search) == 0)
})

test_that("addSignature handles duplicate signatures", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  # Try to add same signature again
  # If verbose is FALSE, we will not get any warnings or messages that the 
  # signature is already in the DB
  expect_no_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  }) 
  # However, if verbose is set to TRUE, we will get a message that will look like
  # You already uploaded a signature with the name = 'test_signature' to the SigRepo Database.
  # ID of the uploaded signature: 882
  expect_message({
    SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
    )
  }) 

  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
})

test_that("addSignature validates input data frame", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  # Test with NULL
  expect_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      signature_tbl = NULL,
      verbose = FALSE
    )
  })
  
  # Test with empty data frame
  expect_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      signature_tbl = data.frame(),
      verbose = FALSE
    )
  })
  
})

test_that("searchSignature handles NULL connection handler", {
  expect_error(
    SigRepo::searchSignature(conn_handler = NULL)
  )
})

test_that("addSignature handles NULL connection handler", {
  signature_table <- data.frame(
    signature_name = "test",
    pmid = "12345678"
  )
  
  expect_error(
    SigRepo::addSignature(conn_handler = NULL, signature_tbl = signature_table)
  )
})

test_that("searchSignature with multiple filters works correctly", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  multi_filter_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    platform = "transcriptomics by array",
    phenotype = "Aging",
    verbose = FALSE
  )
  
  # added an expect equal for the expected amount of columns. It should always be 27
  testthat::expect_true(methods::is(multi_filter_search, "data.frame"))
  testthat::expect_equal(ncol(multi_filter_search), 27)
})

test_that("searchSignature returns consistent results", {
  test_conn <- SigRepo:::global_var[["test_conn_handler"]]
  
  # Run search twice with same parameters
  result1 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  
  result2 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  
  # Should return same results
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(ncol(result1), ncol(result2))
  
  
})