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
  # The reason the rule is not plain "contains": 975 of 6,565 sample types are
  # contained in another, and the Shiny app picks these from a dropdown.
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

test_that("a blank term beside a real one does not return everything", {
  test_conn <- skip_unless_test_database()

  for(blank in c("", "   ")){
    for(field in c("signature_name", "keywords", "description", "organism", "sample_type")){
      found <- base::do.call(
        SigRepo::searchSignature,
        c(
          base::list(conn_handler = test_conn, verbose = FALSE),
          stats::setNames(base::list(c("no_value_is_called_this_zzz", blank)), field)
        )
      )
      expect_identical(base::nrow(found), 0L, info = base::sprintf("%s with blank '%s'", field, blank))
    }
  }
})

test_that("adding a filter never adds a signature", {
  # "Exact wins" is decided over the whole column. A name that is some
  # signature's whole name must stay exact when another filter excludes that
  # signature, rather than widening to the names that contain it.
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  pair <- DBI::dbGetQuery(
    conn,
    "SELECT a.signature_name AS whole_name, b.type AS other_type
       FROM signatures a
       JOIN signatures b
         ON b.signature_name <> a.signature_name
        AND LOCATE(LOWER(a.signature_name), LOWER(b.signature_name)) > 0
      WHERE b.type NOT IN (SELECT c.type FROM signatures c WHERE c.signature_name = a.signature_name)
      LIMIT 1"
  )
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(
    base::nrow(pair) == 0,
    "the test database has no signature name contained in another of a different type"
  )

  alone <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = pair$whole_name,
    verbose = FALSE
  )
  narrowed <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = pair$whole_name,
    type = pair$other_type,
    verbose = FALSE
  )

  expect_true(base::nrow(alone) > 0)
  expect_identical(base::setdiff(narrowed$signature_id, alone$signature_id), base::numeric(0))
})

# ---- the fields #231 asked for ----------------------------------------------

test_that("type filters, which the required OmicSignature fields need", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, type = "uni-directional", verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no uni-directional signatures")
  expect_identical(base::unique(found$type), "uni-directional")
})

test_that("assay_type filters", {
  test_conn <- skip_unless_test_database()

  found <- SigRepo::searchSignature(conn_handler = test_conn, assay_type = "proteomics", verbose = FALSE)

  testthat::skip_if(base::nrow(found) == 0, "the test database has no proteomics signatures")
  expect_identical(base::unique(found$assay_type), "proteomics")
})

test_that("an unknown type says what the choices are", {
  test_conn <- skip_unless_test_database()

  expect_error(
    SigRepo::searchSignature(conn_handler = test_conn, type = "bidirectional", verbose = FALSE),
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

# ---- platform (#231) --------------------------------------------------------

# #231 asked why the argument was platform_name when OmicSignature calls the
# field platform. #242 has since renamed the column and the argument, so there
# is one spelling and nothing left to alias.

test_that("a whole platform finds the signatures on that platform and no other", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  in_use <- DBI::dbGetQuery(
    conn,
    "SELECT p.platform, COUNT(*) AS n FROM signatures s
       JOIN platforms p ON p.platform_id = s.platform_id
      GROUP BY p.platform ORDER BY n DESC LIMIT 1"
  )
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(base::nrow(in_use) == 0, "the test database has no signatures with a platform")

  found <- SigRepo::searchSignature(conn_handler = test_conn, platform = in_use$platform[1], verbose = FALSE)

  expect_identical(base::nrow(found), base::as.integer(in_use$n[1]))
  expect_identical(base::unique(found$platform), in_use$platform[1])
})

test_that("a partial platform resolves through the platforms table", {
  test_conn <- skip_unless_test_database()
  conn <- SigRepo::conn_init(conn_handler = test_conn)
  platform <- stack_value(
    conn,
    "SELECT p.platform FROM signatures s JOIN platforms p ON p.platform_id = s.platform_id
      WHERE CHAR_LENGTH(p.platform) > 4 LIMIT 1"
  )
  testthat::skip_if(base::is.null(platform), "the test database has no signatures with a platform")
  # Every character but the last: nobody's whole value unless another platform
  # happens to be named exactly that, which the expectation below allows for.
  term <- base::substr(platform, 1, base::nchar(platform) - 1)
  expected <- DBI::dbGetQuery(
    conn,
    base::sprintf(
      "SELECT DISTINCT p.platform FROM signatures s JOIN platforms p ON p.platform_id = s.platform_id
        WHERE LOCATE(LOWER(%s), LOWER(p.platform)) > 0",
      DBI::dbQuoteString(conn, term)
    )
  )$platform
  exact_exists <- DBI::dbGetQuery(
    conn,
    base::sprintf(
      "SELECT COUNT(*) AS n FROM platforms WHERE TRIM(LOWER(platform)) = TRIM(LOWER(%s))",
      DBI::dbQuoteString(conn, term)
    )
  )$n[1] > 0
  base::suppressWarnings(DBI::dbDisconnect(conn))
  testthat::skip_if(exact_exists, "the shortened platform is itself a platform on this stack")

  found <- SigRepo::searchSignature(conn_handler = test_conn, platform = term, verbose = FALSE)

  expect_setequal(base::unique(found$platform), expected)
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
    type = "uni-directional",
    verbose = FALSE
  )

  testthat::skip_if(base::nrow(both) == 0, "the test database has no uni-directional mouse signatures")
  expect_identical(base::unique(both$organism), "Mus musculus")
  expect_identical(base::unique(both$type), "uni-directional")
})
