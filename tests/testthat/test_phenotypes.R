# testing script for phenotype function in the database
# addPhenotype
# searchPhenotypes



# test connection handler
test_conn <<- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "sigrepo.org",  
  port = 3306,
  user = "montilab", 
  password = "sigrepo"
)



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
