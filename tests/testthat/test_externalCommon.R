# Shared plumbing behind searchExternal(): URLs, HTTP wrappers and the common
# data frame every external adapter returns. No network: externalGraphql is
# tested with a mocked externalPostJson.

test_that("externalUrl returns defaults and honours options", {
  expect_equal(externalUrl("rummagene"), "https://rummagene.com/graphql")
  expect_equal(externalUrl("rummageo"), "https://rummageo.com/graphql")
  expect_equal(externalUrl("coresh"), "https://alserglab.wustl.edu/coresh-back")
  withr::with_options(list(sigrepo.coresh_url = "http://localhost:8000"), {
    expect_equal(externalUrl("coresh"), "http://localhost:8000")
  })
  expect_error(externalUrl("nope"), "Unknown external source")
})

test_that("emptyExternalFrame has the common columns and no rows", {
  f <- emptyExternalFrame("coresh")
  expect_s3_class(f, "data.frame")
  expect_equal(nrow(f), 0)
  expect_equal(colnames(f), EXTERNAL_COLUMNS)
})

test_that("bindExternalHits orders common columns first, fills gaps, ranks", {
  rows <- list(
    list(id = "a", title = "A", score = 2, pvalue = 0.01, extra = "x"),
    list(id = "b", title = "B", score = 1, pvalue = NULL, extra = "y")
  )
  f <- bindExternalHits("rummagene", "up", rows)
  expect_equal(nrow(f), 2)
  expect_equal(colnames(f)[seq_along(EXTERNAL_COLUMNS)], EXTERNAL_COLUMNS)
  expect_equal(f$source, c("rummagene", "rummagene"))
  expect_equal(f$direction, c("up", "up"))
  expect_equal(f$rank, 1:2)
  expect_equal(f$extra, c("x", "y"))
  expect_true(is.na(f$pvalue[2]))
  expect_true(all(is.na(f$url)))
})

test_that("bindExternalHits with no rows returns the empty frame", {
  f <- bindExternalHits("coresh", "combined", list())
  expect_equal(nrow(f), 0)
  expect_equal(colnames(f), EXTERNAL_COLUMNS)
})

test_that("rbindExternalFrames unions columns", {
  a <- bindExternalHits("coresh", "up", list(list(id = "a", gpl = "GPL1")))
  b <- emptyExternalFrame("coresh", "down")
  f <- rbindExternalFrames(list(a, b))
  expect_equal(nrow(f), 1)
  expect_true("gpl" %in% colnames(f))
})

test_that("externalGraphql surfaces GraphQL errors", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(errors = list(list(message = "boom")))
  )
  expect_error(externalGraphql("http://x", "{ q }", list()), "boom")
})
