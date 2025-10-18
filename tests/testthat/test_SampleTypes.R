# testing script for the SampleType functions in the SigRepo package
# searchSampleTypes



# test connection handler
test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = "sigrepo.org",  
    port = 3306,
    user = "montilab", 
    password = "sigrepo"
  )
  
  # Expect test_conn is a list object with 6 elements
  expect_true(base::is.list(test_conn))
  expect_true(base::length(test_conn) == 6)
  expect_true(base::all(c("dbname", "host", "port", "user", "password", "api_port") %in% base::names(test_conn)))
  
})

test_that("searchSampleTypes correctly searches for the desired sample type", {
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(sample_type_table, "data.frame"))
  
})

