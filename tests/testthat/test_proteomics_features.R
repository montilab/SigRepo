# testing scripts for proteomics feature set functions in the SigRepo package

test_that("addProteomicsFeatureSet forwards conn_handler to conn_init", {
  source_lines <- readLines(testthat::test_path("..", "..", "R", "addProteomicsFeatureSet.R"))
  source_text <- paste(source_lines, collapse = "\n")
  
  expect_match(
    source_text,
    "addProteomicsFeatureSet <- function\\(\\s*\\n\\s*conn_handler = NULL,"
  )
  
  expect_match(
    source_text,
    "conn <- SigRepo::conn_init\\(conn_handler\\)"
  )
})
