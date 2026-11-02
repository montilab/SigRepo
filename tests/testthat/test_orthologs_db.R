# The gene_orthologs reference table against a real MySQL: the add and
# update functions and mapOrthologs(). Read-mostly, and only against a test
# stack (CI's ephemeral one, or the local Docker stack via SIGREPO_TEST_*),
# never production. The rows these tests insert carry version '0.0.0-test'
# and are removed again.

skip_unless_test_database <- function(){
  test_conn <- test_database_conn()
  testthat::skip_if(
    test_conn$host %in% c("sigrepo.org", "142.93.67.157"),
    "refusing to run database tests against production at sigrepo.org / 142.93.67.157"
  )
  test_conn
}

ci_pairs <- function() {
  data.frame(
    organism = "Mus musculus",
    gene_symbol = c("Zzztest1", "Zzztest2", "Zzztest2"),
    human_symbol = c("ZZZTEST1", "ZZZTEST2", "ZZZTEST2B"),
    ensembl_gene_id = c("ENSMUSG00000099901", "ENSMUSG00000099902", "ENSMUSG00000099902"),
    entrez_id = c("999901", "999902", "999902"),
    human_ensembl_gene_id = c("ENSG00000299901", "ENSG00000299902", "ENSG00000299903"),
    human_entrez_id = c("899901", "899902", "899903"),
    support = "CI", support_n = c(12, 3, 3),
    version = "0.0.0-test",
    stringsAsFactors = FALSE
  )
}

remove_ci_pairs <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  DBI::dbExecute(conn, "DELETE FROM gene_orthologs WHERE version = '0.0.0-test'")
  base::suppressWarnings(DBI::dbDisconnect(conn))
}

count_ci_pairs <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM gene_orthologs WHERE version = '0.0.0-test'")$n
  base::suppressWarnings(DBI::dbDisconnect(conn))
  n
}

# ---- addOrthologFeatureSet() -------------------------------------------------

test_that("addOrthologFeatureSet inserts pairs once and skips duplicates", {
  test_conn <- skip_unless_test_database()
  withr::defer(remove_ci_pairs(test_conn))
  remove_ci_pairs(test_conn)

  SigRepo::addOrthologFeatureSet(conn_handler = test_conn, ortholog_set = ci_pairs(), verbose = FALSE)
  SigRepo::addOrthologFeatureSet(conn_handler = test_conn, ortholog_set = ci_pairs(), verbose = FALSE)

  expect_equal(count_ci_pairs(test_conn), 3)
})

test_that("addOrthologFeatureSet stores ids, support and the version on the row", {
  test_conn <- skip_unless_test_database()
  withr::defer(remove_ci_pairs(test_conn))
  remove_ci_pairs(test_conn)
  SigRepo::addOrthologFeatureSet(conn_handler = test_conn, ortholog_set = ci_pairs(), verbose = FALSE)

  conn <- SigRepo::conn_init(test_conn)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM gene_orthologs WHERE version = '0.0.0-test' AND human_symbol = 'ZZZTEST1'")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  expect_equal(nrow(row), 1)
  expect_equal(row$gene_symbol, "Zzztest1")
  expect_equal(row$ensembl_gene_id, "ENSMUSG00000099901")
  expect_equal(as.integer(row$entrez_id), 999901L)
  expect_equal(row$human_ensembl_gene_id, "ENSG00000299901")
  expect_equal(as.integer(row$human_entrez_id), 899901L)
  expect_equal(as.integer(row$support_n), 12L)
  expect_equal(as.integer(row$is_current), 1L)
})

test_that("addOrthologFeatureSet refuses a table missing a required column", {
  test_conn <- skip_unless_test_database()
  expect_error(
    SigRepo::addOrthologFeatureSet(test_conn, ci_pairs()[, c("organism", "gene_symbol")], verbose = FALSE),
    "missing the following required column"
  )
})

# ---- updateOrthologFeatureSet() ----------------------------------------------

count_current_orthologs <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM gene_orthologs WHERE is_current = 1 AND version <> '0.0.0-test'")$n
  base::suppressWarnings(DBI::dbDisconnect(conn))
  n
}

test_that("updateOrthologFeatureSet is a no-op once the stored pairs match babelgene", {
  test_conn <- skip_unless_test_database()
  # The first load takes about a minute and is run by hand on a test stack
  # (updateOrthologFeatureSet(organism = "Mus musculus")); this test checks
  # the steady state so a second run never rewrites the table.
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  expect_error(
    SigRepo::updateOrthologFeatureSet(conn_handler = test_conn, organism = "Mus musculus", verbose = FALSE),
    "No further updates needed"
  )
})

test_that("updateOrthologFeatureSet stored the canonical Trp53 to TP53 pair", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  conn <- SigRepo::conn_init(test_conn)
  row <- DBI::dbGetQuery(conn, "SELECT human_symbol, human_ensembl_gene_id, ensembl_gene_id, support_n, version FROM gene_orthologs WHERE gene_symbol = 'Trp53' AND is_current = 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  expect_equal(row$human_symbol, "TP53")
  expect_equal(row$human_ensembl_gene_id, "ENSG00000141510")
  expect_equal(row$ensembl_gene_id, "ENSMUSG00000059552")
  expect_equal(row$version, as.character(utils::packageVersion("babelgene")))
})
