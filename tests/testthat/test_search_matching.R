# How searchSignature() decides what a search term matches (issues #230, #231).
#
# One rule, no argument for the caller to set: an exact match wins if one
# exists, otherwise the term is a substring. That keeps a value picked from a
# dropdown meaning itself -- 975 of the repository's 6,565 sample types contain
# another sample type as a substring, so "M cell" must not drag in "41-M cell"
# -- while a term nobody has as a whole value still searches.
#
# Pure: no database, no connection. The SQL side only narrows the rows this
# runs over, and LIKE '%term%' is a superset of both branches below.

test_that("a term that is a whole value matches only that value", {
  values <- c("M cell", "41-M cell", "58CrPM cell")

  expect_identical(
    SigRepo:::search_match_rows(values, "M cell"),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("a term that is nobody's whole value matches every value containing it", {
  values <- c("liver tumor", "mouse liver", "kidney")

  expect_identical(
    SigRepo:::search_match_rows(values, "liver"),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("matching ignores case and surrounding whitespace, as the old lookup did", {
  values <- c("  Mus Musculus ", "Homo sapiens")

  expect_identical(
    SigRepo:::search_match_rows(values, "mus musculus"),
    c(TRUE, FALSE)
  )
})

test_that("a partial term finds the value it is part of", {
  expect_identical(
    SigRepo:::search_match_rows(c("Mus musculus", "Homo sapiens"), "Mus"),
    c(TRUE, FALSE)
  )
})

test_that("each term decides for itself whether it was exact", {
  # "M cell" is exact and must stay narrow; "liver" is not and must widen.
  # Getting this wrong in either direction is the whole risk of the rule.
  values <- c("M cell", "41-M cell", "liver tumor", "kidney")

  expect_identical(
    SigRepo:::search_match_rows(values, c("M cell", "liver")),
    c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("regex metacharacters in a term are literal, not a pattern", {
  # 97 sample types and 34 phenotypes contain one. "BE(2)-M17 cell" read as a
  # regex would match "BE2-M17 cell" and not itself, so copying a value out of
  # the results table and searching for it would find nothing.
  values <- c("BE(2)-M17 cell", "BE2-M17 cell")

  expect_identical(
    SigRepo:::search_match_rows(values, "BE(2)-M17 cell"),
    c(TRUE, FALSE)
  )
})

test_that("a dot is a dot, not any character", {
  values <- c("alpha-TC1.6 cell", "alpha-TC1x6 cell")

  expect_identical(
    SigRepo:::search_match_rows(values, "TC1.6"),
    c(TRUE, FALSE)
  )
})

test_that("no term matches nothing rather than everything", {
  expect_identical(
    SigRepo:::search_match_rows(c("a", "b"), character()),
    c(FALSE, FALSE)
  )
})

test_that("an NA value matches no term", {
  expect_identical(
    SigRepo:::search_match_rows(c("liver", NA), "liver"),
    c(TRUE, FALSE)
  )
})

test_that("no values gives no matches rather than an error", {
  expect_identical(SigRepo:::search_match_rows(character(), "liver"), logical(0))
})
