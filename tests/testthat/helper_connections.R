#Sourced before tests run
# Shared helper functions for test database connections

create_test_conn <- function() {
  SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = "sigrepo.org",  
    port = 3306,
    user = "montilab", 
    password = "sigrepo"
  )
}

# Use this if you need cleanup
local_test_conn <- function(env = parent.frame()) {
  conn <- create_test_conn()
  withr::defer(
    try(DBI::dbDisconnect(conn), silent = TRUE), 
    envir = env
  )
  conn
}