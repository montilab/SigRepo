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

test_that("hypeRDotData errors with a named message when the size_by column is missing", {
  testthat::skip_if_not_installed("hypeR")
  no_geneset <- make_dot_hyp(c("A", "B"), c(0.01, 0.02), extra = list(geneset = NULL))
  expect_error(SigRepo::hypeRDotData(no_geneset, size_by = "geneset"),
               "size_by = \"geneset\" needs a 'geneset' column in the results", fixed = TRUE)

  no_overlap <- make_dot_hyp(c("A", "B"), c(0.01, 0.02), extra = list(overlap = NULL))
  expect_error(SigRepo::hypeRDotData(no_overlap, size_by = "overlap"),
               "size_by = \"overlap\" needs a 'overlap' column in the results", fixed = TRUE)

  # "none" needs neither column, so it is unaffected.
  expect_equal(nrow(SigRepo::hypeRDotData(no_geneset, size_by = "none")), 2L)
})

test_that("hypeRDotData breaks shared top-N ties by best pval, then label", {
  testthat::skip_if_not_installed("hypeR")
  # Same fdr for both genesets ("S_A" sorts first alphabetically), but "S_Z"
  # has the smaller pval and so must be the one kept at top = 1.
  tied <- make_dot_hyp(c("S_A", "S_Z"), c(0.05, 0.05), extra = list(pval = c(0.02, 0.01)))
  dots <- SigRepo::hypeRDotData(tied, top = 1)
  expect_equal(dots$label, "S_Z")
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

test_that("hypeREnrichmentData weighted kstest running_score matches hand-derived values on the TIE fixture", {
  testthat::skip_if_not_installed("hypeR")
  res <- suppressWarnings(SigRepo::runHypeR(signature = hyper_tie_stats(), genesets = hyper_tie_genesets(),
                                            test = "kstest", verbose = FALSE))
  tie <- SigRepo::hypeREnrichmentData(res, "TIE")
  # By hand: TIE hits only T06 (score 0.5, position 6) of 11. p_hit is 0 until
  # position 6 then 1 (all its weight is there); p_mis is cumsum(!hit)/10.
  # running = p_hit - p_mis.
  expect_equal(tie$curve$running_score, c(-0.1, -0.2, -0.3, -0.4, -0.5, 0.5, 0.4, 0.3, 0.2, 0.1, 0))
})

test_that("hypeREnrichmentData errors for absolute = TRUE kstest results", {
  testthat::skip_if_not_installed("hypeR")
  res <- suppressWarnings(SigRepo::runHypeR(signature = hyper_tie_stats(), genesets = hyper_tie_genesets(),
                                            test = "kstest", absolute = TRUE, verbose = FALSE))
  expect_error(
    SigRepo::hypeREnrichmentData(res, "TIE"),
    "absolute = TRUE kstest results have no running-sum curve; hypeR scores them as max - min", fixed = TRUE
  )
})

test_that("hypeREnrichmentData ticks and leading edge use fgsea's own (sorted) ranking, not signature order", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("fgsea")
  set.seed(42)
  stats <- hyper_fgsea_stats()
  shuffled <- stats[sample(base::length(stats))]
  res <- SigRepo::runHypeR(signature = shuffled, genesets = hyper_fgsea_genesets(), test = "fgsea", verbose = FALSE)

  hyps <- if (inherits(res, "multihyp")) res$data else list(query = res)
  sorted_names <- base::names(base::sort(shuffled, decreasing = TRUE))
  for (query in base::names(hyps)) {
    table <- hyps[[query]]$data
    for (k in base::seq_len(base::nrow(table))) {
      geneset <- table$label[k]
      enrichment <- SigRepo::hypeREnrichmentData(res, geneset, query = if (inherits(res, "multihyp")) query else NULL)

      expected_le <- table$le[k]
      expected_le <- if (base::nzchar(expected_le)) base::strsplit(expected_le, ",", fixed = TRUE)[[1]] else base::character()
      expect_equal(sort(enrichment$summary$leading_edge_genes), sort(expected_le), info = paste(query, geneset))

      ticks <- enrichment$ticks
      expect_equal(ticks$gene, sorted_names[ticks$position], info = paste(query, geneset))
    }
  }
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

# ---- plotHypeRDots() ----

test_that("plotHypeRDots draws one column per query, including a one-query multihyp", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("ggplot2")
  one <- hypeR::multihyp$new(data = list(only = make_dot_hyp(c("A", "B", "C"), c(0.001, 0.01, 0.2))))
  plot <- SigRepo::plotHypeRDots(one)
  built <- ggplot2::ggplot_build(plot)

  expect_equal(nrow(built$data[[1]]), 3L)
  expect_equal(built$layout$panel_params[[1]]$y$get_labels(), c("C", "B", "A"))
  expect_equal(built$layout$panel_params[[1]]$x$get_labels(), "only")
  expect_equal(rlang::as_label(plot$layers[[1]]$mapping$colour), "significance")
  expect_equal(plot$scales$get_scales("colour")$name, "-log10(FDR)")
  expect_equal(plot$scales$get_scales("colour")$get_transformation()$name, "identity")
  expect_equal(plot$scales$get_scales("size")$name, "Geneset size")
})

test_that("plotHypeRDots colours by score for ranked tests and rejects it for hypergeometric", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("ggplot2")
  fgsea <- make_dot_hyp(c("A", "B"), c(0.01, 0.02), test = "fgsea", extra = list(nes = c(2, -1)))
  plot <- SigRepo::plotHypeRDots(fgsea, color_by = "score", size_by = "none")
  expect_equal(plot$scales$get_scales("colour")$name, "NES")
  expect_null(plot$scales$get_scales("size"))
  expect_equal(SigRepo::plotHypeRDots(fgsea, val = "pval")$scales$get_scales("colour")$name, "-log10(p)")

  kstest <- make_dot_hyp("A", 0.01, test = "kstest", extra = list(score = 0.5))
  expect_equal(SigRepo::plotHypeRDots(kstest, color_by = "score")$scales$get_scales("colour")$name, "Score")
  expect_error(SigRepo::plotHypeRDots(make_dot_hyp("A", 0.01), color_by = "score"),
               "color_by = \"score\" needs kstest or fgsea results", fixed = TRUE)
})

test_that("plotHypeRDots shows an empty-state plot and angles labels for many queries", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("ggplot2")
  empty <- SigRepo::plotHypeRDots(make_dot_hyp("A", 0.5), fdr = 0.05)
  expect_equal(ggplot2::ggplot_build(empty)$data[[1]]$label, "No genesets pass the cutoffs")

  many <- hypeR::multihyp$new(data = stats::setNames(lapply(1:5, function(i) make_dot_hyp("A", 0.01)), paste0("q", 1:5)))
  expect_equal(SigRepo::plotHypeRDots(many)$theme$axis.text.x$angle, 45)
  expect_null(SigRepo::plotHypeRDots(hypeR::multihyp$new(data = list(q1 = make_dot_hyp("A", 0.01))))$theme$axis.text.x$angle)
})

# ---- plotHypeREnrichment() ----

test_that("plotHypeREnrichment draws the running score, hits and ES for fgsea", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("fgsea")
  res <- SigRepo::runHypeR(omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(), test = "fgsea", verbose = FALSE)
  plot <- SigRepo::plotHypeREnrichment(res, "TOP", query = "fg | up")

  geoms <- unname(vapply(plot$layers, function(layer) class(layer$geom)[1], ""))
  expect_equal(geoms, c("GeomHline", "GeomLine", "GeomSegment", "GeomVline"))
  expect_equal(plot$labels$title, "TOP")
  expect_match(plot$labels$subtitle, "^fg \\| up\nES = 1, NES = [0-9.]+, p = .+, FDR = .+$")
  expect_equal(plot$labels$x, "Rank in ranking")
  expect_no_error(ggplot2::ggplot_build(plot))

  filtered <- SigRepo::runHypeR(omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(), test = "fgsea",
                                fdr = 1e-12, verbose = FALSE)
  expect_match(SigRepo::plotHypeREnrichment(filtered, "TOP", query = "fg | up")$labels$subtitle, "not in the result table", fixed = TRUE)

  tie <- suppressWarnings(SigRepo::runHypeR(signature = hyper_tie_stats(), genesets = hyper_tie_genesets(), test = "fgsea", verbose = FALSE))
  tie_geoms <- unname(vapply(SigRepo::plotHypeREnrichment(tie, "TIE", query = "signature | up")$layers, function(layer) class(layer$geom)[1], ""))
  expect_false("GeomVline" %in% tie_geoms)
})

test_that("plotHypeREnrichment labels a kstest down ranking and draws a Venn for hypergeometric", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("ggplot2")
  ks <- suppressWarnings(SigRepo::runHypeR(omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(),
                                           test = "kstest", direction = "both", verbose = FALSE))
  expect_equal(SigRepo::plotHypeREnrichment(ks, "BOTTOM", query = "fg | down")$labels$x, "Rank in ranking (negated scores)")

  hyper <- runToyHypeR(signature = list(a = c("A", "B", "X1"), b = c("C", "D")), genesets = hyper_genesets(), verbose = FALSE)
  venn <- SigRepo::plotHypeREnrichment(hyper, "SET_AB", query = "a", title = "custom")
  expect_equal(unname(vapply(venn$layers, function(layer) class(layer$geom)[1], ""))[1], "GeomCircle")
  expect_equal(venn$labels$subtitle, sprintf("a\noverlap = 3, p = %s, FDR = %s",
                                            format(signif(hyper$data$a$data$pval[hyper$data$a$data$label == "SET_AB"], 2)),
                                            format(signif(hyper$data$a$data$fdr[hyper$data$a$data$label == "SET_AB"], 2))))
})

# ---- plotHypeRMap() ----

test_that("plotHypeRMap returns hypeR's enrichment map when geneset pairs share genes", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("visNetwork")
  hyper <- runToyHypeR(signature = list(a = sprintf("G%02d", 1:12), b = sprintf("G%02d", 29:40)),
                       genesets = hyper_fgsea_genesets(), verbose = FALSE)

  expect_true(inherits(SigRepo::plotHypeRMap(hyper, query = "a"), "visNetwork"))
  both <- SigRepo::plotHypeRMap(hyper)
  expect_equal(names(both), c("a", "b"))
  expect_true(all(vapply(both, inherits, TRUE, "visNetwork")))
})

test_that("plotHypeRMap returns NULL with a warning when hypeR would fail", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("visNetwork")
  disjoint <- list(A = c("X1", "X2", "X3"), B = c("Y1", "Y2", "Y3"), C = c("Z1", "Z2", "Z3"))
  hyper <- runToyHypeR(signature = c("X1", "Y1", "Z1", "Q"), genesets = disjoint, verbose = FALSE)

  expect_warning(
    no_edges <- SigRepo::plotHypeRMap(hyper),
    "No geneset pair in 'signature' reaches similarity_cutoff = 0.2; lower it to draw a map.", fixed = TRUE
  )
  expect_null(no_edges)
  expect_warning(
    no_rows <- SigRepo::plotHypeRMap(hyper, fdr = 1e-12),
    "No genesets pass the cutoffs for 'signature'.", fixed = TRUE
  )
  expect_null(no_rows)
})

test_that("plotHypeRMap warns distinctly when top leaves only one geneset", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("visNetwork")
  hyper <- runToyHypeR(signature = sprintf("G%02d", 1:12), genesets = hyper_fgsea_genesets(), verbose = FALSE)

  expect_warning(
    one <- SigRepo::plotHypeRMap(hyper, top = 1),
    "Only one geneset passes the cutoffs for 'signature'; a map needs at least two.", fixed = TRUE
  )
  expect_null(one)
})

test_that("plotHypeRMap hierarchy maps need rgsets genesets", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("visNetwork")
  testthat::skip_if_not_installed("igraph")
  gs <- hyper_fgsea_genesets()
  hyper <- runToyHypeR(signature = sprintf("G%02d", 1:12), genesets = gs, verbose = FALSE)
  expect_error(SigRepo::plotHypeRMap(hyper, type = "hmap"), "type = \"hmap\" needs rgsets genesets", fixed = TRUE)

  nodes <- data.frame(label = names(gs), row.names = paste0("n", seq_along(gs)))
  edges <- data.frame(from = c("n1", "n3"), to = c("n2", "n4"))
  rg <- hypeR::rgsets$new(gs, nodes, edges, name = "toy", version = "v1", quiet = TRUE)
  hierarchical <- runToyHypeR(signature = sprintf("G%02d", 1:12), genesets = rg, verbose = FALSE)
  expect_true(inherits(SigRepo::plotHypeRMap(hierarchical, type = "hmap"), "visNetwork"))
})
