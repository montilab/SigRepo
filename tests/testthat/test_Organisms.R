# testing script for the organisms functions in the SigRepo package
# searchOrganisms




# test connection handler
test_conn <<- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo"
)



test_that("searchOrganisms correctly searches for the desired sample type", {
  
  organism_table <- SigRepo::searchOrganism(
    conn_handler = test_conn
  )
  
  expect_true(methods::is(organism_table, "data.frame"))
  
})

