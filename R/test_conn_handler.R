# Builds the value behind the exported `test_conn_handler` binding. Reads
# SIGREPO_TEST_* environment variables so CI can point the package's own
# testthat suite at an ephemeral local MySQL/API stack instead of the shared
# sigrepo.org instance, without requiring any changes to test files that
# reference `SigRepo::test_conn_handler` as a plain object. Falls back to the
# original hardcoded defaults when the env vars are unset, so local usage is
# unaffected.
.default_test_conn_handler <- function(){
  base::list(
    dbname = base::Sys.getenv("SIGREPO_TEST_DB_NAME", "sigrepo"),
    host = base::Sys.getenv("SIGREPO_TEST_DB_HOST", "sigrepo.org"),
    port = base::as.numeric(base::Sys.getenv("SIGREPO_TEST_DB_PORT", "3306")),
    user = base::Sys.getenv("SIGREPO_TEST_DB_USER", "montilab"),
    password = base::Sys.getenv("SIGREPO_TEST_DB_PASSWORD", "sigrepo"),
    api_host = base::Sys.getenv("SIGREPO_TEST_API_HOST", "http://142.93.67.157:8020"),
    api_port = base::as.numeric(base::Sys.getenv("SIGREPO_TEST_API_PORT", "8020"))
  )
}

#' Exported connection handler
#'
#' A test connection handler for the package. Points at the shared
#' sigrepo.org test instance by default; set the `SIGREPO_TEST_DB_*` /
#' `SIGREPO_TEST_API_*` environment variables before loading the package to
#' point it at a different (e.g. local, ephemeral) SigRepo stack instead.
#'
#' @keywords internal
#'
#' @export
test_conn_handler <- .default_test_conn_handler()
