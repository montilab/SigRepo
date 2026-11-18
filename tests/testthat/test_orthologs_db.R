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
  # The table comes from SigRepo_Server (#166). A test stack built from a
  # server release without it, such as the client's own CI database, has
  # nothing for these tests to check.
  conn <- SigRepo::conn_init(test_conn)
  has_table <- orthologTableExists(conn)
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(!has_table, "the test database has no gene_orthologs table (apply SigRepo_Server's 2026-11-02 migration)")
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

# suppressWarnings around every dbGetQuery below: RMySQL warns that it imports
# unsigned integers as numeric, which is noise here.
count_ci_pairs <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  n <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM gene_orthologs WHERE version = '0.0.0-test'"))$n
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
  row <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT * FROM gene_orthologs WHERE version = '0.0.0-test' AND human_symbol = 'ZZZTEST1'"))
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
  n <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM gene_orthologs WHERE is_current = 1 AND version <> '0.0.0-test'"))$n
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
  row <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT human_symbol, human_ensembl_gene_id, ensembl_gene_id, support_n, version FROM gene_orthologs WHERE gene_symbol = 'Trp53' AND is_current = 1"))
  base::suppressWarnings(DBI::dbDisconnect(conn))
  expect_equal(row$human_symbol, "TP53")
  expect_equal(row$human_ensembl_gene_id, "ENSG00000141510")
  expect_equal(row$ensembl_gene_id, "ENSMUSG00000059552")
  expect_equal(row$version, as.character(utils::packageVersion("babelgene")))
})

remove_zzz_pairs <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  DBI::dbExecute(conn, "DELETE FROM gene_orthologs WHERE gene_symbol LIKE 'Zzztest%'")
  base::suppressWarnings(DBI::dbDisconnect(conn))
}

zzz_rows <- function(test_conn){
  conn <- SigRepo::conn_init(test_conn)
  rows <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT gene_symbol, human_symbol, is_current, version FROM gene_orthologs WHERE gene_symbol LIKE 'Zzztest%' ORDER BY gene_symbol, human_symbol"))
  base::suppressWarnings(DBI::dbDisconnect(conn))
  rows
}

test_that("updateOrthologFeatureSet restamps, archives, inserts and re-activates pairs", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  withr::defer(remove_zzz_pairs(test_conn))
  remove_zzz_pairs(test_conn)

  # Three test pairs stored at an old version, beside the real ones.
  SigRepo::addOrthologFeatureSet(conn_handler = test_conn, ortholog_set = ci_pairs(), verbose = FALSE)

  # babelgene is replaced by a table that keeps every real pair unchanged,
  # keeps two of the three test pairs, and adds a new one.
  conn <- SigRepo::conn_init(test_conn)
  real <- base::suppressWarnings(DBI::dbGetQuery(conn, "
    SELECT o.organism, g.gene_symbol, g.ensembl_gene_id, g.entrez_id, g.human_symbol, g.human_ensembl_gene_id,
           g.human_entrez_id, g.support, g.support_n, g.version
    FROM gene_orthologs g JOIN organisms o USING (organism_id)
    WHERE o.organism = 'Mus musculus' AND g.is_current = 1 AND g.gene_symbol NOT LIKE 'Zzztest%'"))
  base::suppressWarnings(DBI::dbDisconnect(conn))
  real[] <- lapply(real, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  live_version <- real$version[1]
  test_pairs <- ci_pairs()
  test_pairs$version <- live_version
  test_pairs$support_n <- as.character(test_pairs$support_n)
  new_pair <- test_pairs[1, ]
  new_pair$gene_symbol <- "Zzztest3"; new_pair$human_symbol <- "ZZZTEST3"
  new_pair$ensembl_gene_id <- "ENSMUSG00000099903"; new_pair$human_ensembl_gene_id <- "ENSG00000299904"
  fetched_without_2b <- rbind(real[, ORTHOLOG_COLUMNS], test_pairs[test_pairs$human_symbol != "ZZZTEST2B", ORTHOLOG_COLUMNS], new_pair[, ORTHOLOG_COLUMNS])
  fetched_full <- rbind(fetched_without_2b, test_pairs[test_pairs$human_symbol == "ZZZTEST2B", ORTHOLOG_COLUMNS])

  testthat::local_mocked_bindings(babelgeneOrthologTable = function(...) fetched_without_2b, .package = "SigRepo")
  msgs <- testthat::capture_messages(
    SigRepo::updateOrthologFeatureSet(conn_handler = test_conn, organism = "Mus musculus", verbose = TRUE)
  )
  expect_true(any(grepl("refreshed, 1 added, 1 archived", msgs)), info = paste(msgs, collapse = " | "))

  rows <- zzz_rows(test_conn)
  expect_equal(rows$human_symbol, c("ZZZTEST1", "ZZZTEST2", "ZZZTEST2B", "ZZZTEST3"))
  expect_equal(as.integer(rows$is_current), c(1L, 1L, 0L, 1L))
  # Refreshed and inserted rows carry the new version; an archived row keeps
  # the version it was last seen at.
  expect_equal(rows$version, c(live_version, live_version, "0.0.0-test", live_version))

  # The archived pair comes back: re-activated and restamped, not inserted twice.
  testthat::local_mocked_bindings(babelgeneOrthologTable = function(...) fetched_full, .package = "SigRepo")
  SigRepo::updateOrthologFeatureSet(conn_handler = test_conn, organism = "Mus musculus", verbose = FALSE)
  rows <- zzz_rows(test_conn)
  expect_equal(nrow(rows), 4)
  expect_equal(as.integer(rows$is_current), c(1L, 1L, 1L, 1L))
  expect_equal(rows$version, rep(live_version, 4))

  # And now nothing is left to do.
  expect_error(
    SigRepo::updateOrthologFeatureSet(conn_handler = test_conn, organism = "Mus musculus", verbose = FALSE),
    "No further updates needed"
  )
})

# ---- mapOrthologs() ----------------------------------------------------------

test_that("mapOrthologs resolves every mouse id type for Adipoq to the human ADIPOQ ids", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  # Q60994 is the UniProt accession of mouse adiponectin, 11450 its Entrez id;
  # UniProt's gene name and the MGI symbol agree for this gene.
  out <- SigRepo::mapOrthologs(test_conn, c("Q60994", "ENSMUSG00000022878", "Adipoq", "11450"), from = "Mus musculus", verbose = FALSE)
  expect_equal(out$query_type, c("uniprot", "ensembl", "symbol", "entrez"))
  expect_equal(out$query_symbol, rep("Adipoq", 4))
  expect_equal(out$symbol, rep("ADIPOQ", 4))
  expect_equal(out$ensembl, rep("ENSG00000181092", 4))
  expect_equal(out$entrez, rep("9370", 4))
  expect_false(any(out$ambiguous))
})

test_that("mapOrthologs returns every human ortholog of an ambiguous mouse gene", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  # Mouse Orm1 (UniProt Q60590) has two human orthologs, ORM1 and ORM2.
  out <- SigRepo::mapOrthologs(test_conn, "Q60590", from = "Mus musculus", verbose = FALSE)
  expect_setequal(out$symbol, c("ORM1", "ORM2"))
  expect_true(all(out$ambiguous))
  expect_equal(out$query, rep("Q60590", 2))
})

test_that("a UniProt gene name that differs from the MGI symbol cannot cross to human", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  # UniProt names mouse p53 (P02340) "Tp53"; MGI, and therefore babelgene,
  # say "Trp53". The within-species step succeeds and the cross-species hop
  # fails, which the result shows as a resolved query_symbol and NA targets.
  conn <- SigRepo::conn_init(test_conn)
  stored <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT gene_symbol FROM proteomics_features WHERE feature_name = 'P02340'"))$gene_symbol
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(!identical(stored, "Tp53"), "this stack's proteomics reference does not name P02340 'Tp53'")
  out <- SigRepo::mapOrthologs(test_conn, "P02340", from = "Mus musculus", verbose = FALSE)
  expect_equal(out$query_symbol, "Tp53")
  expect_true(is.na(out$symbol))
  # The MGI symbol itself maps.
  expect_equal(SigRepo::mapOrthologs(test_conn, "Trp53", from = "Mus musculus", verbose = FALSE)$symbol, "TP53")
})

test_that("mapOrthologs maps a human Ensembl id back to the mouse gene", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  back <- SigRepo::mapOrthologs(test_conn, "ENSG00000141510", from = "Homo sapiens", to = "Mus musculus", verbose = FALSE)
  expect_equal(back$symbol, "Trp53")
  expect_equal(back$ensembl, "ENSMUSG00000059552")
  expect_equal(back$organism, "Mus musculus")
})

test_that("mapOrthologs keeps an unknown id as an unmapped row", {
  test_conn <- skip_unless_test_database()
  testthat::skip_if(count_current_orthologs(test_conn) == 0, "no ortholog reference loaded on the test stack")
  out <- SigRepo::mapOrthologs(test_conn, c("Trp53", "Notagene123"), from = "Mus musculus", verbose = FALSE)
  expect_equal(out$query, c("Trp53", "Notagene123"))
  expect_true(is.na(out$symbol[2]))
})
