# testing scripts for the signature collection functions in the SigRepo package


# test connection handler
test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = "sigrepo.org",  
    port = 3306,
    user = "montilab",       # account for testing
    password = "sigrepo"    # password for testing
  )
  
  # Expect test_conn is a list object with 7 elements
  expect_true(base::is.list(test_conn))
  expect_true(base::length(test_conn) == 7)
  expect_true(base::all(c("dbname", "host", "port", "user", "password", "api_host", "api_port") %in% base::names(test_conn)))
  
})