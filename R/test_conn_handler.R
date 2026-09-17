# Builds the value behind the exported `test_conn_handler` binding. Reads
# SIGREPO_TEST_* environment variables so tests, vignettes and CI choose which
# SigRepo stack to use without changing files that reference
# `SigRepo::test_conn_handler` as a plain object. The database user and
# password have no defaults: credentials must never ship in package source,
# because a SigRepo login is also a MySQL login whose grants reach past the
# client's own permission checks.
.default_test_conn_handler <- function(){
  base::list(
    dbname = base::Sys.getenv("SIGREPO_TEST_DB_NAME", "sigrepo"),
    host = base::Sys.getenv("SIGREPO_TEST_DB_HOST", "sigrepo.org"),
    port = base::as.numeric(base::Sys.getenv("SIGREPO_TEST_DB_PORT", "3306")),
    user = base::Sys.getenv("SIGREPO_TEST_DB_USER"),
    password = base::Sys.getenv("SIGREPO_TEST_DB_PASSWORD"),
    api_host = base::Sys.getenv("SIGREPO_TEST_API_HOST", "http://142.93.67.157:8020"),
    api_port = base::as.numeric(base::Sys.getenv("SIGREPO_TEST_API_PORT", "8020"))
  )
}

#' Exported connection handler
#'
#' A connection handler for the package's tests and vignettes, built from
#' environment variables each time it is used:
#' \code{SIGREPO_TEST_DB_USER} and \code{SIGREPO_TEST_DB_PASSWORD} (required,
#' no defaults), and optionally \code{SIGREPO_TEST_DB_NAME},
#' \code{SIGREPO_TEST_DB_HOST}, \code{SIGREPO_TEST_DB_PORT},
#' \code{SIGREPO_TEST_API_HOST} and \code{SIGREPO_TEST_API_PORT}, which default
#' to sigrepo.org. When the user or password is unset, \code{user} and
#' \code{password} are empty, the database tests skip, and the vignettes show
#' their code without running it.
#'
#' Use a dedicated test account that owns only toy data, never a personal or
#' admin login.
#'
#' @keywords internal
#'
#' @export
test_conn_handler <- .default_test_conn_handler()
