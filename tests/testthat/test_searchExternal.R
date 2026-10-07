# searchExternal() dispatcher. The signature resolver and the three adapters
# are mocked, so this exercises argument handling, direction splitting,
# fan-out and the return shape only.

fake_signature <- function(feature_name, score = NULL, organism = "Homo sapiens", name = "fake_sig") {
  sig <- base::data.frame(probe_id = base::seq_along(feature_name), feature_name = feature_name, stringsAsFactors = FALSE)
  if (!base::is.null(score)) sig$score <- score
  base::list(signature = sig, metadata = base::list(signature_name = name, organism = organism, assay_type = "transcriptomics"))
}
one_hit <- function(source, query) {
  bindExternalHits(source, query$direction, list(list(id = paste0(source, "-hit"), title = "t", score = 1)))
}

test_that("searchExternal with one source returns a data frame", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) one_hit("rummagene", query)
  )
  out <- searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "rummagene", verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(out$id, "rummagene-hit")
  expect_equal(out$direction, "combined")
})

test_that("searchExternal with several sources returns a named list and survives one failure", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) one_hit("rummagene", query),
    externalRummageo = function(query, limit, timeout) stop("rummageo is down"),
    externalCoresh = function(query, limit, calculate_pvalues, timeout) one_hit("coresh", query)
  )
  expect_warning(
    out <- searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), verbose = FALSE),
    "rummageo: rummageo is down"
  )
  expect_type(out, "list")
  expect_equal(names(out), c("rummagene", "rummageo", "coresh"))
  expect_equal(nrow(out$rummageo), 0)
  expect_equal(out$coresh$id, "coresh-hit")
})

test_that("searchExternal with a single failing source errors instead of warning", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalCoresh = function(query, limit, calculate_pvalues, timeout) stop("CORESH job x failed.")
  )
  expect_error(
    searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "coresh", verbose = FALSE),
    "CORESH job x failed"
  )
})

test_that("searchExternal direction = 'both' stacks up and down with a direction column", {
  seen <- character()
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) { seen <<- c(seen, query$genes); one_hit("rummagene", query) }
  )
  sig <- fake_signature(c("UP1", "UP2", "DN1", "DN2"), score = c(2, 1, -1, -2))
  out <- searchExternal(omic_signature = sig, source = "rummagene", direction = "both", verbose = FALSE)
  expect_equal(out$direction, c("up", "down"))
  expect_setequal(seen, c("UP1", "UP2", "DN1", "DN2"))
  expect_equal(names(attr(out, "query")), c("up", "down"))
})

test_that("searchExternal passes limit and calculate_pvalues through to CORESH", {
  seen <- list()
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalCoresh = function(query, limit, calculate_pvalues, timeout) {
      seen <<- list(limit = limit, pv = calculate_pvalues, timeout = timeout)
      one_hit("coresh", query)
    }
  )
  searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "coresh",
                 limit = 7, calculate_pvalues = TRUE, timeout = 33, verbose = FALSE)
  expect_equal(seen, list(limit = 7L, pv = TRUE, timeout = 33))
})

test_that("searchExternal validates its arguments", {
  expect_error(searchExternal(omic_signature = fake_signature(c("A", "B")), source = "enrichr"), "should be one of")
  expect_error(searchExternal(omic_signature = fake_signature(c("A", "B")), source = "coresh", limit = 0), "positive integer")
})

test_that("searchExternal reports how many hits came back per source when verbose", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character(),
    externalRummagene = function(query, limit, timeout) {
      out <- one_hit("rummagene", query); attr(out, "total_count") <- 27875; out
    },
    externalCoresh = function(query, limit, calculate_pvalues, timeout) one_hit("coresh", query)
  )
  expect_message(
    searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "rummagene", limit = 25, verbose = TRUE),
    "rummagene: 1 of 27,875 matching hits returned \\(limit = 25\\)"
  )
  expect_message(
    searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "coresh", limit = 25, verbose = TRUE),
    "coresh: 1 hit returned \\(limit = 25\\)"
  )
})
