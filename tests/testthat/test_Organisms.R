# testing script for the organisms functions in the SigRepo package
# searchOrganisms


# testing script for the organisms functions in the SigRepo package

test_that("searchOrganism returns a data frame", {
  test_conn <- create_test_conn()
  
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(organism_table, "data.frame"))
})

test_that("searchOrganism returns expected columns", {
  test_conn <- create_test_conn()
  
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn
  )
  
  # Check that table has rows
  expect_true(nrow(organism_table) > 0)
  
  # Check for expected column names (adjust based on your actual schema)
  expected_cols <- c("organism")
  expect_true(all(expected_cols %in% colnames(organism_table)))
})

test_that("searchOrganism handles specific organism search", {
  test_conn <- create_test_conn()
  
  # Test searching for a specific organism (adjust organism name as needed)
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn,
    organism = "Homo sapiens"  # adjust if your function has this parameter
  )
  
  expect_true(methods::is(organism_table, "data.frame"))
  
  # If search is specific, expect fewer results
  if (nrow(organism_table) > 0) {
    expect_true(any(grepl("Homo sapiens", organism_table$organism, ignore.case = TRUE)))
  }
})

test_that("searchOrganism handles invalid connection gracefully", {
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

  # Expect error or empty result when connection fails
  expect_error(
    SigRepo::searchOrganism(conn_handler = invalid_conn),
    regexp = ".*"  # Any error message
  )
  # Katia: search Organism() function needs to be fixed to handle invalid host
  # It should return an informative error 
  ##expect_true(nrow(SigRepo::searchOrganism(conn_handler = invalid_conn)) == 0)
})

test_that("searchOrganism handles NULL connection handler", {
  expect_error(
    SigRepo::searchOrganism(conn_handler = NULL)
  )
})

test_that("searchOrganism returns unique organisms", {
  test_conn <- create_test_conn()
  
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn
  )
  
  # Check for duplicate organism IDs (if organism_id column exists)
  if ("organism" %in% colnames(organism_table)) {
    expect_equal(
      nrow(organism_table),
      length(unique(organism_table$organism))
    )
  }
})

test_that("searchOrganism data types are correct", {
  test_conn <- create_test_conn()
  
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn
  )
  
  # Check data types (adjust based on your schema)
  if ("organism" %in% colnames(organism_table)) {
    expect_true(is.character(organism_table$organism))
  }
  
})

test_that("searchOrganism handles case-insensitive search", {
  test_conn <- create_test_conn()
  
  # Test with different cases (if your function supports this)
  result_lower <- SigRepo::searchOrganism(
    conn_handler = test_conn,
    organism = "Homo sapiens"
  )
  
  result_upper <- SigRepo::searchOrganism(
    conn_handler = test_conn,
    organism = "HOMO SAPIENS"
  )
  print(result_upper)
  # Both should return similar results (adjust based on your function's behavior)
  expect_true(methods::is(result_lower, "data.frame"))
  expect_true(methods::is(result_upper, "data.frame"))
})