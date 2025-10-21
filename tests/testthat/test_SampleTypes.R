# testing script for the SampleType functions in the SigRepo package
# searchSampleTypes



# test connection handler
test_conn <<- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo"
)



test_that("searchSampleTypes correctly searches for the desired sample type", {
  
  sample_type_table <- SigRepo::searchSampleType(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(sample_type_table, "data.frame"))
  
})

