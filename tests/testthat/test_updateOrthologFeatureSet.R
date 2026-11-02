# updateOrthologFeatureSet() argument handling, with the database and
# babelgene replaced. The real load against a stack is in test_orthologs_db.R.

mock_update_db <- function() {
  testthat::local_mocked_bindings(
    conn_init = function(...) structure(list(), class = "mock_conn"),
    conn_close = function(...) invisible(NULL),
    checkPermissions = function(...) data.frame(user = "admin", user_role = "admin", stringsAsFactors = FALSE),
    lookup_table_sql = function(conn, db_table_name, ..., filter_coln_val = NULL) {
      organism <- filter_coln_val$organism
      data.frame(
        organism_id = if (tolower(organism) == "homo sapiens") 2L else 1L,
        organism = organism,
        prot_organism_taxid = if (tolower(organism) == "homo sapiens") "9606" else "10116",
        stringsAsFactors = FALSE
      )
    },
    .package = "SigRepo",
    .env = parent.frame()
  )
}

test_that("updateOrthologFeatureSet rejects human before touching babelgene", {
  mock_update_db()
  expect_error(
    updateOrthologFeatureSet(conn_handler = list(), organism = "Homo sapiens", verbose = FALSE),
    "target side"
  )
})

test_that("updateOrthologFeatureSet rejects organisms outside the beta scope", {
  mock_update_db()
  expect_error(
    updateOrthologFeatureSet(conn_handler = list(), organism = "Rattus norvegicus", verbose = FALSE),
    "limited to Mus musculus"
  )
})

test_that("updateOrthologFeatureSet requires exactly one organism", {
  expect_error(updateOrthologFeatureSet(conn_handler = list(), organism = c("Mus musculus", "Homo sapiens"), verbose = FALSE), "length of 1")
  expect_error(updateOrthologFeatureSet(conn_handler = list(), organism = "", verbose = FALSE), "length of 1")
})
