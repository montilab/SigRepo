# tests or signature collection 


test_that("addCollection correctly adds a signature into the database", {
  
  test_conn <- create_test_conn()
  
  expect_no_error({
    test_collection_sig <- base::readRDS(
      testthat::test_path("test_data", "test_data_collection.rds")
    )
  })
  
  expect_no_error({
    omic_collection_id <- SigRepo::addCollection(
      conn_handler = test_conn,
      omic_collection = test_collection_sig,
      return_collection_id = TRUE,
      verbose = TRUE
    )
  })
  
  # id of the omic_collection should be returned
  expect_true(length(omic_collection_id) == 1)
  expect_type(omic_collection_id, "double")
  
  # remove the signature collection
  expect_no_error({
    SigRepo::deleteCollection(
      conn_handler = test_conn,
      collection_id = omic_collection_id,
      verbose = TRUE
    )
    
    # remove signatures
    expect_no_error({
      SigRepo::deleteSignature(
        conn_handler = test_conn,
        signature_name = "sig_for_collection_1",
        verbose = TRUE
      )
    })
    
    expect_no_error({
      SigRepo::deleteSignature(
        conn_handler = test_conn,
        signature_name = "sig_for_collection_2",
        verbose = TRUE
      )
    })
  })
})


test_that("searchSignature correctly searches for the desired signature",{
  
  test_conn <- create_test_conn()
  
  # create the test collection
  
  expect_no_error({
    test_collection_sig <-  base::readRDS(testthat::test_path("test_data", "test_data_collection.rds"))
  })
  
  expect_no_error({
    omic_collection_id <- SigRepo::addCollection(
      conn_handler = test_conn,
      omic_collection = test_collection_sig,
      return_collection_id = TRUE,
      verbose = FALSE
    )
  })
  
  collection_search <- SigRepo::searchCollection(
    conn_handler = test_conn,
    collection_id = omic_collection_id,
    verbose = FALSE
  )
  expect_true(methods::is(collection_search,"data.frame"))
  expect_true(nrow(collection_search) > 0)
  expect_equal(collection_search$signature_name[1], "sig_for_collection_1")
  
  # remove collection
  expect_no_error({
    SigRepo::deleteCollection(
      conn_handler = test_conn,
      collection_id = omic_collection_id,
      verbose = FALSE
    )
  })
  
  # remove signatures
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_name = "sig_for_collection_1",
      verbose = FALSE
    )
  })
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_name = "sig_for_collection_2",
      verbose = FALSE
    )
  })
})


# searchCollection correctly returns all collections when no filters are provided.

test_that("searchCollection returns all signatures when no filters provided",{
  test_conn <- create_test_conn()
  
  all_collections <- SigRepo::searchCollection(
    conn_handler = test_conn,
    verbose = FALSE
  )
  
  expect_true(methods::is(all_collections,"data.frame"))
  expect_true(nrow(all_collections) >= 0)
})


  
 

test_that("searchCollection returns empty result for non-existent signature", {
  test_conn <- create_test_conn()
  
  collection_search <- SigRepo::searchCollection(
    conn_handler = test_conn,
    collection_name = "non_existent_collection_xyz123",
    verbose = FALSE
  )
  
  expect_true(methods::is(collection_search, "data.frame"))
  expect_true(nrow(collection_search) == 0)
})

# test_that("addCollection handles duplicate signatures", {
#   test_conn <- create_test_conn()
#   
#   # Create test signature data
#   expect_no_error({test_collection_sig <- base::readRDS(testthat::test_path("test_data", "test_data_collection.rds"))})
#   
#   
#   expect_no_error({
#     omic_collection_id <- SigRepo::addCollection(
#       conn_handler = test_conn,
#       omic_collection = test_collection_sig,
#       return_collection_id = TRUE,
#       verbose = FALSE
#     )
#   })
#   
#   # Try to add same signature again
#   # If verbose is FALSE, we will not get any warnings or messages that the 
#   # signature is already in the DB
#   expect_error({
#     SigRepo::addCollection(
#       conn_handler = test_conn,
#       omic_collection = test_collection_sig,
#       return_collection_id = TRUE,
#       verbose = TRUE
#     )
#   }) 
#  
#   
#   
#   # remove signature
#   expect_no_error({
#     SigRepo::deleteCollection(
#       conn_handler = test_conn,
#       collection_id = omic_collection_id,
#       verbose = FALSE
#     )
#   })
#   
#   # remove signatures
#   expect_no_error({
#     SigRepo::deleteSignature(
#       conn_handler = test_conn,
#       signature_name = "sig_for_collection_1",
#       verbose = FALSE
#     )
#   })
#   
#   expect_no_error({
#     SigRepo::deleteSignature(
#       conn_handler = test_conn,
#       signature_name = "sig_for_collection_2",
#       verbose = FALSE
#     )
#   })
#   
#   
# })



test_that("searchCollection handles NULL connection handler", {
  expect_error(
    SigRepo::searchCollection(conn_handler = NULL)
  )
})

test_that("addCollection handles NULL connection handler", {
  signature_table <- data.frame(
    signature_name = "test",
    pmid = "12345678"
  )
  
  expect_error(
    SigRepo::addCollection(conn_handler = NULL, omic_collection = signature_table)
  )
})



test_that("searchCollection returns consistent results", {
  test_conn <- create_test_conn()
  
  # Run search twice with same parameters
  result1 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    user_name = "montilab",
    verbose = FALSE
  )
  
  result2 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    user_name = "montilab",
    verbose = FALSE
  )
  
  # Should return same results
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(ncol(result1), ncol(result2))
  
  
})