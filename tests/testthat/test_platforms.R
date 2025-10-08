# script to test the platform functions for SigRepo
# addPlatform
# searchPlatforms




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



# Testing the add Platform works
test_that("addPlatform correctly adds the desired platform into the database",{
  
  # loading in test data 
  platforms_table <<- base::data.frame(platform_name = "test_platform")
  
  # add platform function
  expect_no_error({
    SigRepo::addPlatform(
      conn_handler = test_conn,
      platform_tbl = platforms_table,
      verbose = TRUE
    )
  })
  
})

test_that("searchPlatforms correctly searches for the desired platform",{
  
  platforms_search <- SigRepo::searchPlatform(
    conn_handler = test_conn,
    platform_name = "test_platform",
    verbose = TRUE
  )
  
  expect_equal(platforms_search$platform_name[1], "test_platform")
  
})


