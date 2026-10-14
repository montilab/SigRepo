# Offline tests for mergeDifexp(). Signatures come from the bundled example
# objects, and the database helpers it relies on -- searchSignature() and
# getSignature() -- are replaced with local mocks, so the tests cover the
# merge itself and the id/name resolution without a database.

example_signatures <- function(){
  env <- base::environment()
  utils::data(
    list = c("omic_signature_1", "omic_signature_2", "omic_signature_3"),
    package = "SigRepo",
    envir = env
  )
  base::list(v1 = env$omic_signature_1, v2 = env$omic_signature_2, v3 = env$omic_signature_3)
}

# A small hand-made signature whose difexp we control exactly. Metadata is
# borrowed from a bundled signature so it passes OmicSignature's checks. ####
toy_signature <- function(name, difexp){
  metadata <- example_signatures()$v1$metadata
  metadata$signature_name <- name
  difexp$group_label <- base::factor(base::ifelse(difexp$score > 0, "up", "down"))
  signature <- difexp[, c("probe_id", "feature_name", "score", "group_label")]
  OmicSignature::OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp)
}

toy_difexp <- function(probe_id, feature_name, score, p_value = 0.01, logfc = score / 2){
  base::data.frame(
    probe_id = base::as.character(probe_id), feature_name = feature_name, score = score,
    logfc = logfc, p_value = p_value, adj_p = p_value, stringsAsFactors = FALSE
  )
}

toy_pair <- function(){
  a <- toy_signature("sigA", toy_difexp(
    probe_id = 1:4, feature_name = c("G1", "G2", "G3", "G2"), score = c(1, -2, 3, 5)
  ))
  b <- toy_signature("sigB", toy_difexp(
    probe_id = 1:3, feature_name = c("G1", "G3", "G4"), score = c(-1, 0.5, 2)
  ))
  base::list(a = a, b = b)
}

mock_database <- function(sigs, rows){
  calls <- base::new.env()
  calls$get <- base::list()
  filter_rows <- function(signature_id, signature_name){
    hits <- rows
    if(base::length(signature_id) > 0){
      hits <- hits[base::as.character(hits$signature_id) %in% base::trimws(base::as.character(signature_id)), , drop = FALSE]
    }
    if(base::length(signature_name) > 0){
      hits <- hits[base::tolower(hits$signature_name) %in% base::tolower(base::trimws(signature_name)), , drop = FALSE]
    }
    hits
  }
  search_fn <- function(conn_handler = NULL, signature_id = NULL, signature_name = NULL, ..., verbose = TRUE){
    filter_rows(signature_id, signature_name)[, c("signature_id", "signature_name", "user_name"), drop = FALSE]
  }
  get_fn <- function(conn_handler = NULL, signature_name = NULL, signature_id = NULL, verbose = TRUE){
    calls$get[[base::length(calls$get) + 1L]] <- base::list(signature_id = signature_id, signature_name = signature_name)
    hits <- filter_rows(signature_id, signature_name)
    if(base::nrow(hits) == 0) return(NULL)
    stats::setNames(sigs[hits$key], hits$signature_name)
  }
  base::list(calls = calls, search = search_fn, get = get_fn)
}

mock_conn_handler <- base::list(dbname = "sigrepo", host = "mock", port = 3306, user = "tester", password = "x")

## Shape ####

test_that("the result is a feature-by-signature matrix with the union of features", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs)

  expect_true(base::is.matrix(m))
  expect_type(m, "double")
  expect_equal(base::colnames(m), c("a", "b"))
  expect_setequal(base::rownames(m), c("G1", "G2", "G3", "G4"))
  expect_equal(m["G1", ], c(a = 1, b = -1))
  expect_equal(m["G3", ], c(a = 3, b = 0.5))
  expect_true(base::is.na(m["G4", "a"]))
  expect_true(base::is.na(m["G2", "b"]))
})

test_that("unnamed objects are named by their signature_name", {
  sigs <- base::unname(toy_pair())

  m <- SigRepo::mergeDifexp(omic_signatures = sigs)

  expect_equal(base::colnames(m), c("sigA", "sigB"))
})

test_that("features = 'intersect' keeps only features present in every table", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs, features = "intersect")

  expect_setequal(base::rownames(m), c("G1", "G3"))
  expect_false(base::anyNA(m))
})

## Duplicate features ####

test_that("duplicate feature names collapse to the value with the largest absolute size by default", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs)

  # G2 appears twice in sigA with scores -2 and 5.
  expect_equal(m["G2", "a"], 5)
  expect_equal(base::sum(base::rownames(m) == "G2"), 1L)
})

test_that("collapse = 'mean' averages duplicate feature names", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs, collapse = "mean")

  expect_equal(m["G2", "a"], 1.5)
})

test_that("collapse = 'first' keeps the first row of a duplicated feature", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs, collapse = "first")

  expect_equal(m["G2", "a"], -2)
})

test_that("collapse = 'min' keeps the smallest value of a duplicated feature", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs, collapse = "min")

  expect_equal(m["G2", "a"], -2)
  expect_equal(m["G1", "a"], 1)
})

test_that("collapse = 'min' on a p-value column keeps the most significant probe", {
  a <- toy_signature("sigA", toy_difexp(
    probe_id = 1:3, feature_name = c("G1", "G2", "G2"), score = c(1, 2, 3),
    p_value = c(0.5, 0.2, 0.001)
  ))

  m <- SigRepo::mergeDifexp(omic_signatures = base::list(a = a), value_col = "p_value", collapse = "min")

  expect_equal(m["G2", "a"], 0.001)
  expect_equal(m["G1", "a"], 0.5)
  expect_equal(base::sum(base::rownames(m) == "G2"), 1L)
})

test_that("collapse = 'min' ignores NA values within a feature and returns NA when all are NA", {
  a <- toy_signature("sigA", toy_difexp(
    probe_id = 1:4, feature_name = c("G1", "G1", "G2", "G2"), score = c(1, 2, 3, 4),
    p_value = c(NA, 0.3, NA, NA)
  ))

  m <- SigRepo::mergeDifexp(omic_signatures = base::list(a = a), value_col = "p_value", collapse = "min")

  expect_equal(m["G1", "a"], 0.3)
  expect_true(base::is.na(m["G2", "a"]))
})

## Columns ####

test_that("value_col picks another difexp column", {
  sigs <- toy_pair()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs, value_col = "logfc")

  expect_equal(m["G1", ], c(a = 0.5, b = -0.5))
})

test_that("a signature whose difexp lacks value_col is dropped with a warning", {
  sigs <- toy_pair()
  difexp <- sigs$b$difexp
  difexp$logfc <- NULL
  sigs$b$difexp <- difexp

  expect_warning(
    m <- SigRepo::mergeDifexp(omic_signatures = sigs, value_col = "logfc"),
    "b"
  )
  expect_equal(base::colnames(m), "a")
})

## Missing difexp ####

test_that("a signature without a difexp table is dropped with a warning", {
  sigs <- toy_pair()
  no_difexp <- OmicSignature::OmicSignature$new(metadata = sigs$b$metadata, signature = sigs$b$signature)
  sigs$b <- no_difexp

  expect_warning(m <- SigRepo::mergeDifexp(omic_signatures = sigs), "b")
  expect_equal(base::colnames(m), "a")
})

test_that("it is an error when no signature has a usable difexp table", {
  sigs <- toy_pair()
  sigs <- base::lapply(sigs, function(s) OmicSignature::OmicSignature$new(metadata = s$metadata, signature = s$signature))

  expect_error(
    suppressWarnings(SigRepo::mergeDifexp(omic_signatures = sigs)),
    "difexp"
  )
})

test_that("it is an error when no signatures are given at all", {
  expect_error(SigRepo::mergeDifexp(), "signature_ids")
})

## Bundled example signatures ####

test_that("the bundled signatures merge into one matrix with every feature once", {
  sigs <- example_signatures()

  m <- SigRepo::mergeDifexp(omic_signatures = sigs)

  expect_equal(base::colnames(m), c("v1", "v2", "v3"))
  expect_setequal(base::rownames(m), base::unique(sigs$v1$difexp$feature_name))
  expect_false(base::any(base::duplicated(base::rownames(m))))
})

## Database resolution ####

test_that("signatures requested by id and name are fetched through getSignature", {
  sigs <- toy_pair()
  rows <- base::data.frame(
    signature_id = c(21L, 22L), signature_name = c("sigA", "sigB"), user_name = "tester",
    key = c("a", "b"), stringsAsFactors = FALSE
  )
  db <- mock_database(sigs, rows)
  testthat::local_mocked_bindings(searchSignature = db$search, getSignature = db$get, .package = "SigRepo")

  m <- SigRepo::mergeDifexp(conn_handler = mock_conn_handler, signature_ids = 21, signature_names = "sigB")

  expect_equal(base::colnames(m), c("sigA", "sigB"))
  expect_equal(base::length(db$calls$get), 2L)
  expect_equal(m["G1", ], c(sigA = 1, sigB = -1))
})

test_that("a connection is required to fetch by id", {
  expect_error(SigRepo::mergeDifexp(signature_ids = 1), "conn_handler")
})
