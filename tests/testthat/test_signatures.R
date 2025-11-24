# testing script for signature functions in the SigRepo package
# addSignature
# searchSignature

test_that("addSignature correctly adds a signature into the database", {
  test_conn <- create_test_conn()
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
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
  test_conn <- create_test_conn()
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
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
  test_conn <- create_test_conn()
  
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
})

# test_that("searchSignature filters by phenotype", {
#   test_conn <- create_test_conn()
#   
#   phenotype_search <- SigRepo::searchSignature(
#     conn_handler = test_conn,
#     phenotype = "test_phenotype",
#     verbose = FALSE
#   )
#   
#   expect_true(methods::is(phenotype_search, "data.frame"))
# })

# test_that("searchSignature filters by platform", {
#   test_conn <- create_test_conn()
#   
#   platform_search <- SigRepo::searchSignature(
#     conn_handler = test_conn,
#     platform = "test_platform",
#     verbose = FALSE
#   )
#   
#   expect_true(methods::is(platform_search, "data.frame"))
# })

test_that("searchSignature returns empty result for non-existent signature", {
  test_conn <- create_test_conn()
  
  signature_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = "non_existent_signature_xyz123",
    verbose = FALSE
  )
  
  expect_true(methods::is(signature_search, "data.frame"))
  expect_true(nrow(signature_search) == 0)
})

test_that("addSignature handles duplicate signatures", {
  test_conn <- create_test_conn()
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
    )
  })
  
  # Try to add same signature again - should handle gracefully
  expect_no_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
    )
  }) 
  # # If error is expected
  # expect_error({
  #   SigRepo::addSignature(
  #     conn_handler = test_conn,
  #     omic_signature = test_transcriptomics_sig,
  #     verbose = FALSE
  #   )
  # })
  
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
  test_conn <- create_test_conn()
  
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
  
  # # If error is expected
  # expect_no_error({
  #   SigRepo::addSignature(
  #     conn_handler = test_conn,
  #     signature_tbl = data.frame(),
  #     verbose = FALSE
  #   )
  # })
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

# test_that("searchSignature with multiple filters works correctly", {
#   test_conn <- create_test_conn()
#   
#   multi_filter_search <- SigRepo::searchSignature(
#     conn_handler = test_conn,
#     organism = "Homo sapiens",
#     platform = "test_platform",
#     phenotype = "test_phenotype",
#     verbose = FALSE
#   )
#   
#   expect_true(methods::is(multi_filter_search, "data.frame"))
# })

test_that("searchSignature returns consistent results", {
  test_conn <- create_test_conn()
  
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