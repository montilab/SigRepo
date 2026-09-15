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

# ---- hypeREnrichmentData() ----

test_that("hypeREnrichmentData ES matches every fgsea and kstest score in the results", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("fgsea")
  sig <- make_hyper_fgsea_sig()
  gs <- hyper_fgsea_genesets()
  results <- list(
    fgsea = list(res = SigRepo::runHypeR(omic_signature = sig, genesets = gs, test = "fgsea", verbose = FALSE), column = "es"),
    kstest = list(res = suppressWarnings(SigRepo::runHypeR(omic_signature = sig, genesets = gs, test = "kstest", direction = "both", verbose = FALSE)), column = "score"),
    ranked = list(res = suppressWarnings(SigRepo::runHypeR(omic_signature = sig, genesets = gs, test = "kstest", power = 0, verbose = FALSE)), column = "score")
  )

  for (case in names(results)) {
    res <- results[[case]]$res
    hyps <- if (inherits(res, "multihyp")) res$data else list(query = res)
    for (query in names(hyps)) {
      table <- hyps[[query]]$data
      for (k in seq_len(nrow(table))) {
        enrichment <- SigRepo::hypeREnrichmentData(res, table$label[k], query = if (inherits(res, "multihyp")) query else NULL)
        expect_equal(signif(enrichment$summary$es, 2), signif(table[[results[[case]]$column]][k], 2),
                     info = paste(case, query, table$label[k]))
      }
    }
  }
})

test_that("hypeREnrichmentData reproduces the tie rules: first extremum for kstest, 0 for fgsea", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("fgsea")
  run <- function(...) suppressWarnings(SigRepo::runHypeR(signature = hyper_tie_stats(), genesets = hyper_tie_genesets(), verbose = FALSE, ...))

  weighted <- run(test = "kstest")
  expect_equal(SigRepo::hypeREnrichmentData(weighted, "TIE")$summary$es, -0.5)
  expect_equal(weighted$data$score[weighted$data$label == "TIE"], -0.5)

  ranked <- run(test = "kstest", power = 0)
  tie_ranked <- SigRepo::hypeREnrichmentData(ranked, "TIE")
  expect_equal(signif(tie_ranked$summary$es, 2), ranked$data$score[ranked$data$label == "TIE"])

  gsea <- run(test = "fgsea")
  tie_gsea <- SigRepo::hypeREnrichmentData(gsea, "TIE", query = "signature | up")
  expect_equal(tie_gsea$summary$es, 0)
  expect_true(is.na(tie_gsea$summary$es_position))
  expect_true(is.na(tie_gsea$summary$pval))
  expect_false(any(tie_gsea$ticks$leading_edge))
})

test_that("hypeREnrichmentData curve, ticks and leading edge follow the ranking", {
  testthat::skip_if_not_installed("hypeR")
  res <- suppressWarnings(SigRepo::runHypeR(signature = hyper_tie_stats(), genesets = hyper_tie_genesets(),
                                            test = "kstest", verbose = FALSE))
  top <- SigRepo::hypeREnrichmentData(res, "TOP")
  expect_equal(nrow(top$curve), 11L)
  expect_equal(top$ticks$position, 1:3)
  expect_equal(top$ticks$gene, c("T01", "T02", "T03"))
  expect_equal(top$ticks$score, c(5, 4, 3))
  expect_true(all(top$ticks$leading_edge))
  expect_equal(top$summary$es_position, 3L)

  bottom <- SigRepo::hypeREnrichmentData(res, "BOT")
  expect_true(bottom$summary$es < 0)
  expect_true(all(bottom$ticks$position[bottom$ticks$leading_edge] >= bottom$summary$es_position))
  expect_equal(bottom$summary$leading_edge_genes, c("T09", "T10", "T11"))
})

test_that("hypeREnrichmentData errors on hypergeometric results, bad queries and unusable genesets", {
  testthat::skip_if_not_installed("hypeR")
  hyper <- runToyHypeR(signature = list(a = c("A", "B"), b = c("C", "D")), genesets = hyper_genesets(), verbose = FALSE)
  expect_error(SigRepo::hypeREnrichmentData(hyper, "SET_AB", query = "a"), "needs kstest or fgsea results", fixed = TRUE)

  gs <- c(hyper_tie_genesets(), list(ABSENT = c("ZZ1", "ZZ2")))
  ks <- suppressWarnings(SigRepo::runHypeR(signature = list(x = hyper_tie_stats(), y = hyper_tie_stats()), genesets = gs,
                                           test = "kstest", verbose = FALSE))
  expect_error(SigRepo::hypeREnrichmentData(ks, "TOP"), "'query' is required for a result with 2 queries: 'x', 'y'", fixed = TRUE)
  expect_error(SigRepo::hypeREnrichmentData(ks, "TOP", query = "z"), "'query' must be one of: 'x', 'y'", fixed = TRUE)
  expect_error(SigRepo::hypeREnrichmentData(ks, "NOPE", query = "x"), "Geneset 'NOPE' is not in this result's genesets", fixed = TRUE)
  expect_error(SigRepo::hypeREnrichmentData(ks, "ABSENT", query = "x"), "'ABSENT' has no genes in this ranking", fixed = TRUE)
})
