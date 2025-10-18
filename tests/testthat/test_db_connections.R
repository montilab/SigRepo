# testing scripts for the signature collection functions in the SigRepo package


# test connection handler
test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = "sigrepo.org",  
    port = 3306,
    user = "guest",       # account for testing
    password = "guest"    # password for testing
  )
  
  # Expect test_conn is a list object with 6 elements
  expect_true(base::is.list(test_conn))
  expect_true(base::length(test_conn) == 6)
  expect_true(base::all(c("dbname", "host", "port", "user", "password", "api_port") %in% base::names(test_conn)))
  
})