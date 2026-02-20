# on load private environment

.sigrepo_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .sigrepo_env$handle <- NULL
}