# script to test the platform functions for SigRepo
# addPlatform
# searchPlatforms


# test connection handler
test_conn <<- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo"
)



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


