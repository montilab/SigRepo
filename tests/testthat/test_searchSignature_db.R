# searchSignature() against a real MySQL (issues #230 and #231).
#
# The matching rule and the WHERE clause are unit tested without a database in
# test_search_matching.R and test_lookup_where_clause.R. What needs a database
# is that the two halves meet correctly: the SQL narrows to a superset and the
# R rule settles it, over vocabulary tables the query joins through.
#
# Read-only, and only against a test stack (CI's ephemeral one, or the local
# Docker stack via SIGREPO_TEST_*), never production.

skip_unless_test_database <- function(){
  test_conn <- test_database_conn()
  testthat::skip_if(
    test_conn$host %in% c("sigrepo.org", "142.93.67.157"),
    "refusing to run database tests against production at sigrepo.org / 142.93.67.157"
  )
  test_conn
}

# Fetch a value the assertions can be written against, so the tests describe
# behaviour rather than hard-coding this stack's contents.
stack_value <- function(conn, statement){
  found <- DBI::dbGetQuery(conn, statement)
  if(base::nrow(found) == 0) NULL else found[[1]][1]
}

# ---- partial matching (#230) ------------------------------------------------

test_that("a term nobody has as a whole name finds every name containing it", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  expected <- DBI::dbGetQuery(
    conn,
    "SELECT signature_name FROM signatures WHERE signature_name LIKE '%LLFS%'"
  )$signature_name
  base::suppressWarnings(DBI::dbDisconnect(conn))
  # Skip only when the stack genuinely lacks the data; the assertion below has
  # to fail, not skip, when the search itself cannot find it.
  testthat::skip_if(base::length(expected) < 2, "the test database has fewer than two LLFS signatures")

  found <- SigRepo::searchSignature(conn_handler = test_conn, signature_name = "LLFS", verbose = FALSE)

  expect_setequal(found$signature_name, expected)
})

test_that("a whole signature name finds that signature and no other", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  name <- stack_value(conn, "SELECT signature_name FROM signatures WHERE signature_name LIKE '%LLFS%' LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(name), "the test database has no LLFS signatures")

  found <- SigRepo::searchSignature(conn_handler = test_conn, signature_name = name, verbose = FALSE)

  expect_identical(base::unique(found$signature_name), name)
})

test_that("a sample type picked whole does not drag in the ones containing it", {
  # The reason the rule is not plain "contains": 975 of 6,565 sample types have
  # another as a substring, and the Shiny app picks these from a dropdown.
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, sample_type = "liver", verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no liver signatures")
  expect_identical(base::unique(found$sample_type), "liver")
})

test_that("a partial sample type does reach the ones containing it", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  expected <- DBI::dbGetQuery(
    conn,
    "SELECT DISTINCT t.sample_type FROM sample_types t
       JOIN signatures s ON s.sample_type_id = t.sample_type_id
      WHERE t.sample_type LIKE '%liv%'"
  )$sample_type
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::length(expected) < 2, "the test database has fewer than two liv* sample types in use")

  found <- SigRepo::searchSignature(conn_handler = test_conn, sample_type = "liv", verbose = FALSE)

  expect_setequal(base::unique(found$sample_type), expected)
})

test_that("a partial organism resolves through the organisms table", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  expected <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n FROM signatures s JOIN organisms o ON o.organism_id = s.organism_id
      WHERE o.organism = 'Mus musculus'"
  )$n[1]
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(expected == 0, "the test database has no mouse signatures")

  found <- SigRepo::searchSignature(conn_handler = test_conn, organism = "Mus", verbose = FALSE)

  expect_identical(base::nrow(found), base::as.integer(expected))
  expect_identical(base::unique(found$organism), "Mus musculus")
})

test_that("a term matching nothing returns no rows rather than everything", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = "no_signature_is_called_this_zzz",
    verbose = FALSE
  )

  expect_identical(base::nrow(found), 0L)
})

# ---- the fields #231 asked for ----------------------------------------------

test_that("direction_type filters, which the required OmicSignature fields need", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, direction_type = "uni-directional", verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no uni-directional signatures")
  expect_identical(base::unique(found$direction_type), "uni-directional")
})

test_that("assay_type filters", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, assay_type = "proteomics", verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no proteomics signatures")
  expect_identical(base::unique(found$assay_type), "proteomics")
})

test_that("an unknown direction_type says what the choices are", {
  test_conn <- skip_unless_test_database()

  expect_error(
    SigRepo::searchSignature(conn_handler = test_conn, direction_type = "bidirectional", verbose = FALSE),
    "uni-directional"
  )
})

test_that("year filters exactly, not as a substring", {
  # Otherwise year = 201 would return 2010 through 2019.
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, year = 2025, verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no 2025 signatures")
  expect_identical(base::unique(found$year), 2025L)
})

test_that("has_difexp filters", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, has_difexp = TRUE, verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no signatures with difexp")
  expect_identical(base::unique(found$has_difexp), 1L)
})

test_that("PMID filters", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  pmid <- stack_value(conn, "SELECT PMID FROM signatures WHERE PMID IS NOT NULL LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(pmid), "the test database has no signatures with a PMID")

  found <- SigRepo::searchSignature(conn_handler = test_conn, PMID = pmid, verbose = FALSE)

  expect_identical(base::unique(found$PMID), base::as.integer(pmid))
})

test_that("keywords match partially, which is the point of searching prose", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  keyword <- stack_value(conn, "SELECT keywords FROM signatures WHERE keywords IS NOT NULL AND keywords <> '' LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(keyword), "the test database has no keywords")
  term <- base::substr(keyword, 1, 4)

  found <- SigRepo::searchSignature(conn_handler = test_conn, keywords = term, verbose = FALSE)

  expect_true(base::nrow(found) > 0)
  expect_true(base::all(base::grepl(term, found$keywords, fixed = TRUE, ignore.case = FALSE)))
})

test_that("description is searchable, which #231 asked about", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  description <- stack_value(conn, "SELECT description FROM signatures WHERE description IS NOT NULL AND description <> '' LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(description), "the test database has no descriptions")

  found <- SigRepo::searchSignature(
    conn_handler = test_conn,
    description = base::substr(description, 1, 10),
    verbose = FALSE
  )

  expect_true(base::nrow(found) > 0)
})

test_that("covariates is searchable, which #231 asked about", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  covariates <- stack_value(conn, "SELECT covariates FROM signatures WHERE covariates IS NOT NULL AND covariates <> '' LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(covariates), "the test database has no covariates")

  found <- SigRepo::searchSignature(
    conn_handler = test_conn,
    covariates = base::substr(covariates, 1, 6),
    verbose = FALSE
  )

  expect_true(base::nrow(found) > 0)
})

# ---- platform, under either spelling (#231) ---------------------------------

test_that("platform is accepted as the OmicSignature spelling of platform_name", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  platform <- stack_value(conn, "SELECT platform_name FROM platforms LIMIT 1")
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::is.null(platform), "the test database has no platforms")

  by_alias <- SigRepo::searchSignature(conn_handler = test_conn, platform = platform, verbose = FALSE)
  by_column <- SigRepo::searchSignature(conn_handler = test_conn, platform_name = platform, verbose = FALSE)

  expect_identical(by_alias, by_column)
})

test_that("giving both spellings of platform is refused rather than guessed at", {
  test_conn <- skip_unless_test_database()

  expect_error(
    SigRepo::searchSignature(
      conn_handler = test_conn,
      platform = "a",
      platform_name = "b",
      verbose = FALSE
    ),
    "platform"
  )
})

# ---- the one internal caller that resolves names ----------------------------

test_that("compareSignatures still treats a partial name as not found", {
  # It resolves names through searchSignature(), so partial matching could have
  # made it silently compare whatever a fragment happened to hit. It re-filters
  # on exact equality, and this pins that: "LLFS" matches two signatures in
  # search but must still be reported as no such signature here.
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  n_llfs <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n FROM signatures WHERE signature_name LIKE '%LLFS%'"
  )$n[1]
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(n_llfs == 0, "the test database has no LLFS signatures")

  expect_error(
    SigRepo::compareSignatures(
      conn_handler = test_conn,
      signature_names = c("LLFS", "LLFS"),
      verbose = FALSE
    ),
    "LLFS"
  )
})

# ---- filters still combine --------------------------------------------------

test_that("two filters narrow together rather than either one alone", {
  test_conn <- skip_unless_test_database()

  both <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Mus musculus",
    direction_type = "uni-directional",
    verbose = FALSE
  )

  testthat::skip_if(base::nrow(both) == 0, "the test database has no uni-directional mouse signatures")
  expect_identical(base::unique(both$organism), "Mus musculus")
  expect_identical(base::unique(both$direction_type), "uni-directional")
})
