# testing script for phenotype function in the database
# addPhenotype
# searchPhenotypes



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


test_that("addPhenotype correctly adds the phenotype into the database",{
  
  #reading in the phenotype data
  phenotype_table <<- base::data.frame(phenotype = "test_phenotype")
  
  expect_no_error({
    SigRepo::addPhenotype(
      conn_handler = test_conn,
      phenotype_tbl = phenotype_table,
      verbose = TRUE
    )
  })
  
})

test_that("searchPhenotypes correcty searches for the desired phenotype",{
  
  phenotype_search <- SigRepo::searchPhenotype(
    conn_handler = test_conn,
    phenotype = 'test_phenotype',
    verbose = TRUE
  )
  
  expect_equal(phenotype_search$phenotype[1], "test_phenotype")
  
})
