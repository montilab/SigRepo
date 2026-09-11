# on load private environment

.sigrepo_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .sigrepo_env$handle <- NULL

  # Re-bind the exported `test_conn_handler` object as an active binding so
  # it re-reads SIGREPO_TEST_* environment variables on every access, rather
  # than freezing whatever values were present when the package happened to
  # be installed/loaded. See R/test_conn_handler.R. makeActiveBinding()
  # refuses to replace an existing regular binding, so the plain value
  # created by evaluating test_conn_handler.R has to be removed first.
  ns <- base::asNamespace(pkgname)
  base::rm(list = "test_conn_handler", envir = ns)
  base::makeActiveBinding("test_conn_handler", .default_test_conn_handler, ns)
}