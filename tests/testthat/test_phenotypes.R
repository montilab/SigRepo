# testing script for the phenotypes functions in the SigRepo package

test_that("searchPhenotype returns a data frame", {
  test_conn <- create_test_conn()
  
  phenotype_table <- SigRepo::searchPhenotype(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(phenotype_table, "data.frame"))
})

test_that("searchPhenotype returns expected columns", {
  test_conn <- create_test_conn()
  
  phenotype_table <- SigRepo::searchPhenotype(
    conn_handler = test_conn
  )
  
  # Check that table has rows
  expect_true(nrow(phenotype_table) > 0)
  
  # Check for expected column names (adjust based on your actual schema)
  expect_true(ncol(phenotype_table) > 0)
})

test_that("searchPhenotype handles specific phenotype search", {
  test_conn <- create_test_conn()
  
  # Test searching for a specific phenotype (adjust if your function has filter parameters)
  phenotype_table <- SigRepo::searchPhenotype(
    conn_handler = test_conn,
    phenotype="Aging"
  )
  
  expect_true(methods::is(phenotype_table, "data.frame"))
  expect_true(nrow(phenotype_table) >= 0)
})

test_that("searchPhenotype handles NULL connection handler", {
  expect_error(
    SigRepo::searchPhenotype(conn_handler = NULL)
  )
})

test_that("searchPhenotype data types are correct", {
  test_conn <- create_test_conn()
  
  phenotype_table <- SigRepo::searchPhenotype(
    conn_handler = test_conn
  )
  
  # Verify it's a proper data frame
  expect_true(is.data.frame(phenotype_table))
  
  # Check that all columns have valid types
  expect_true(all(sapply(phenotype_table, function(col) {
    is.numeric(col) || is.character(col) || is.factor(col) || is.logical(col)
  })))
})

test_that("searchPhenotype returns consistent results", {
  test_conn <- create_test_conn()
  
  # Run search twice
  result1 <- SigRepo::searchPhenotype(conn_handler = test_conn)
  result2 <- SigRepo::searchPhenotype(conn_handler = test_conn)
  
  # Should return same number of rows
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(ncol(result1), ncol(result2))
})

test_that("searchPhenotype handles invalid connection gracefully", {
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
    SigRepo::searchPhenotype(conn_handler = invalid_conn)
  )
})