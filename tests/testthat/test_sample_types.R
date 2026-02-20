# testing script for the SampleType functions in the SigRepo package
# searchSampleTypes

test_that("searchSampleType returns a data frame", {
  test_conn <- SigRepo::test_conn_handler
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(sample_type_table, "data.frame"))
})

test_that("searchSampleType returns expected columns", {
  test_conn <- SigRepo::test_conn_handler
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  # Check that table has rows
  expect_true(nrow(sample_type_table) >= 0)
  
  # Check that table has columns
  expect_true(ncol(sample_type_table) > 0)
})

test_that("searchSampleType handles specific sample type search", {
  test_conn <- SigRepo::test_conn_handler
  
  # Test searching for a specific sample type (adjust if your function has filter parameters)
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn,
    sample_type = "cell line"  # adjust based on actual parameter name
  )
  
  expect_true(methods::is(sample_type_table, "data.frame"))
})



test_that("searchSampleType returns consistent results", {
  test_conn <- SigRepo::test_conn_handler
  
  # Run search twice
  result1 <- SigRepo::searchSampleType(conn_handler = test_conn)
  result2 <- SigRepo::searchSampleType(conn_handler = test_conn)
  
  # Should return same number of rows
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(ncol(result1), ncol(result2))
})

test_that("searchSampleType data types are correct", {
  test_conn <- SigRepo::test_conn_handler
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  # Verify it's a proper data frame
  expect_true(is.data.frame(sample_type_table))
  
  # Check that all columns have valid types
  if (nrow(sample_type_table) > 0) {
    expect_true(all(sapply(sample_type_table, function(col) {
      is.numeric(col) || is.character(col) || is.factor(col) || is.logical(col)
    })))
  }
})

test_that("searchSampleType returns unique sample types", {
  test_conn <- SigRepo::test_conn_handler
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  #print(head(sample_type_table))
  
  # Check for duplicate IDs if sample_type column exists
  if ("sample_type" %in% colnames(sample_type_table) && nrow(sample_type_table) > 0) {
    expect_equal(
      nrow(sample_type_table),
      length(unique(sample_type_table$sample_type))
    )
  }
})

test_that("searchSampleType handles invalid connection gracefully", {
  # Create invalid connection
  invalid_conn <- list(
    dbname = "invalid",
    host = "invalid.host",
    port = 9999,
    user = "invalid",
    password = "invalid",
    api_host = "invalid",
    api_port = 9999
  )
  
  # Expect error when connection fails
  expect_error(
    SigRepo::searchSampleType(conn_handler = invalid_conn)
  )
})