# ---- hypeRDotData() ----

test_that("hypeRDotData keeps the top genesets by their best value across queries", {
  testthat::skip_if_not_installed("hypeR")
  q1 <- make_dot_hyp(c("S1", "S2", "S3"), c(0.01, 0.5, 0.2))
  q2 <- make_dot_hyp(c("S2", "S4"), c(0.001, 0.3))
  dots <- SigRepo::hypeRDotData(hypeR::multihyp$new(data = list(q1 = q1, q2 = q2)), top = 2)

  expect_equal(dots$query, c("q1", "q1", "q2"))
  expect_equal(dots$label, c("S2", "S1", "S2"))
  expect_equal(levels(dots$label_abrv), c("S1", "S2"))
  expect_equal(levels(dots$query_label), c("q1", "q2"))
  expect_equal(dots$significance, -log10(c(0.5, 0.01, 0.001)))
  expect_equal(dots$size, c(20, 10, 10))
})

test_that("hypeRDotData applies the cutoffs per query and floors zero values", {
  testthat::skip_if_not_installed("hypeR")
  q1 <- make_dot_hyp(c("S1", "S2"), c(0, 0.5))
  q2 <- make_dot_hyp(c("S1", "S3"), c(0.02, 0.04))
  dots <- SigRepo::hypeRDotData(hypeR::multihyp$new(data = list(q1 = q1, q2 = q2)), fdr = 0.1)

  expect_equal(dots$label, c("S1", "S1", "S3"))
  expect_equal(dots$significance[1], -log10(0.02 / 10))
  expect_equal(levels(dots$query_label), c("q1", "q2"))

  all_zero <- SigRepo::hypeRDotData(make_dot_hyp("S1", 0))
  expect_equal(all_zero$significance, 300)
})

test_that("hypeRDotData takes score from nes (fgsea) or score (kstest), and size from size_by", {
  testthat::skip_if_not_installed("hypeR")
  fgsea <- make_dot_hyp(c("A", "B"), c(0.01, 0.02), test = "fgsea", extra = list(nes = c(2.1, -1.5)))
  kstest <- make_dot_hyp(c("A", "B"), c(0.01, 0.02), test = "kstest", extra = list(score = c(0.6, -0.4)))
  hyper <- make_dot_hyp(c("A", "B"), c(0.01, 0.02))

  expect_equal(SigRepo::hypeRDotData(fgsea)$score, c(2.1, -1.5))
  expect_equal(SigRepo::hypeRDotData(kstest)$score, c(0.6, -0.4))
  expect_true(all(is.na(SigRepo::hypeRDotData(hyper)$score)))
  expect_equal(SigRepo::hypeRDotData(hyper, size_by = "overlap")$size, c(1, 2))
  expect_true(all(is.na(SigRepo::hypeRDotData(hyper, size_by = "none")$size)))
  expect_equal(SigRepo::hypeRDotData(hyper, val = "pval")$significance, -log10(c(0.005, 0.01)))
})

test_that("hypeRDotData returns a 0-row frame with the same columns when nothing passes", {
  testthat::skip_if_not_installed("hypeR")
  dots <- SigRepo::hypeRDotData(make_dot_hyp(c("A", "B"), c(0.5, 0.6)), fdr = 0.05)
  expect_equal(nrow(dots), 0L)
  expect_equal(colnames(dots), c("query", "query_label", "signature_name", "group_label", "direction", "test",
                                 "label", "label_abrv", "pval", "fdr", "significance", "score", "size"))
})

test_that("hypeRDotData truncates long geneset labels and keeps them unique", {
  testthat::skip_if_not_installed("hypeR")
  labels <- c("HALLMARK_VERY_LONG_SUFFIX_ONE", "HALLMARK_VERY_LONG_SUFFIX_TWO", "SHORT")
  dots <- SigRepo::hypeRDotData(make_dot_hyp(labels, c(0.01, 0.02, 0.03)), abrv = 10)
  expect_equal(as.character(dots$label_abrv), c("HALLMARK_V...", "HALLMARK_V... (2)", "SHORT"))
})

test_that("hypeRDotData labels queries by wrapped name, a function, or errors on bad labels", {
  testthat::skip_if_not_installed("hypeR")
  info <- list("SigRepo Signature Name" = "LLFS_Aging_Gene_2023", "Group Label" = "Group1", "SigRepo Direction" = "")
  single <- make_dot_hyp("A", 0.01, info = info)
  expect_equal(SigRepo::hypeRDotData(single)$query, "LLFS_Aging_Gene_2023 | Group1")
  expect_equal(as.character(SigRepo::hypeRDotData(single)$query_label), "LLFS_Aging_Gene_2023 |\nGroup1")

  pair <- hypeR::multihyp$new(data = list(a = make_dot_hyp("A", 0.01, info = info), b = make_dot_hyp("A", 0.02, info = info)))
  by_group <- SigRepo::hypeRDotData(pair, query_labels = function(info) info$group_label)
  expect_equal(levels(by_group$query_label), c("Group1", "Group1 (2)"))

  expect_error(SigRepo::hypeRDotData(pair, query_labels = "x"), "'query_labels' must be NULL or a function", fixed = TRUE)
  expect_error(SigRepo::hypeRDotData(pair, query_labels = function(info) "one"),
               "'query_labels' must return a character vector of 2 non-empty label(s)", fixed = TRUE)
  expect_error(SigRepo::hypeRDotData(list()), "'hyp_obj' must be a hypeR hyp or multihyp object", fixed = TRUE)
  expect_error(SigRepo::hypeRDotData(single, top = 0), "'top' must be a single number of at least 1", fixed = TRUE)
})
