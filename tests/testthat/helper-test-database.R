# Database tests connect with SigRepo::test_conn_handler, whose user and
# password come only from SIGREPO_TEST_DB_USER and SIGREPO_TEST_DB_PASSWORD.
# Skip instead of failing to connect when they are not set, e.g. on a
# contributor's machine or in a workflow without the test-account secrets.
test_database_conn <- function(){
  test_conn <- SigRepo::test_conn_handler
  testthat::skip_if(
    !base::nzchar(test_conn$user) || !base::nzchar(test_conn$password),
    "SIGREPO_TEST_DB_USER and SIGREPO_TEST_DB_PASSWORD are not set"
  )
  test_conn
}
