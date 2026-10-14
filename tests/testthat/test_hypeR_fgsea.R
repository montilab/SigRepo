# ---- Engine: runFgseaHyps() ----

test_that("runFgseaHyps matches fgsea::fgseaMultilevel() with the same seed, split by ES sign", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  stats <- hyper_fgsea_stats()
  gs <- hyper_fgsea_genesets()

  run <- SigRepo:::runFgseaHyps(stats, gs, background = 23467, direction = "both", power = 1, seed = 1,
                                fgsea_args = list(), quiet = TRUE)
  set.seed(1)
  direct <- as.data.frame(suppressWarnings(fgsea::fgseaMultilevel(gs, stats, sampleSize = 101, minSize = 1, maxSize = Inf, gseaParam = 1)))

  expect_equal(names(run$hyps), c("up", "down"))
  expect_equal(run$hyps$up$data$label, c("TOP", "TOP_MIX", "SPREAD"))
  expect_equal(run$hyps$down$data$label, c("BOTTOM", "BOTTOM_MIX"))
  ours <- rbind(run$hyps$up$data, run$hyps$down$data)
  ref <- direct[match(ours$label, direct$pathway), ]
  expect_identical(ours$pval, signif(ref$pval, 2))
  expect_identical(ours$fdr, signif(ref$padj, 2))
  expect_identical(ours$nes, ref$NES)
  expect_identical(ours$es, ref$ES)
  expect_identical(ours$le, vapply(ref$leadingEdge, paste, "", collapse = ","))
  expect_identical(ours$hits, vapply(ref$leadingEdge, paste, "", collapse = " , "))
  expect_equal(colnames(ours), c("label", "pval", "fdr", "lte", "es", "nes", "signature", "geneset", "overlap", "le", "hits"))
  expect_true(all(run$hyps$up$data$es > 0))
  expect_true(all(run$hyps$down$data$es < 0))
})

test_that("runFgseaHyps puts an ES = 0 pathway on neither side and does not list it as dropped", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  run <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "both", 1, 1, list(), TRUE)
  expect_false("MIDDLE" %in% c(run$hyps$up$data$label, run$hyps$down$data$label))
  expect_identical(run$dropped, character())
})

test_that("runFgseaHyps is reproducible with a seed and restores the caller's RNG state", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  run <- function(seed) SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "both", 1, seed, list(), TRUE)

  set.seed(99)
  before <- .Random.seed
  first <- run(1)
  expect_identical(.Random.seed, before)
  second <- run(1)
  expect_identical(first$hyps$up$data, second$hyps$up$data)
  expect_identical(first$hyps$down$data, second$hyps$down$data)

  set.seed(5)
  before <- .Random.seed
  run(NULL)
  expect_false(identical(.Random.seed, before))
})

test_that("withHypeRSeed leaves no .Random.seed behind when the caller had none", {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  saved <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (had) assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
  if (had) rm(".Random.seed", envir = globalenv())

  value <- SigRepo:::withHypeRSeed(1, stats::runif(1))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  set.seed(1)
  expect_equal(value, stats::runif(1))
})

test_that("runFgseaHyps keeps one side for direction up or down", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  up <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "up", 1, 1, list(), TRUE)
  down <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "down", 1, 1, list(), TRUE)
  expect_equal(names(up$hyps), "up")
  expect_equal(names(down$hyps), "down")
  expect_equal(down$hyps$down$data$label, c("BOTTOM", "BOTTOM_MIX"))
})

test_that("runFgseaHyps reduces genesets to a gene-vector background and records dropped pathways", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  bg <- sprintf("G%02d", 1:30)
  reduced <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), bg, "up", 1, 1, list(), TRUE)
  expect_equal(reduced$hyps$up$data$geneset[reduced$hyps$up$data$label == "SPREAD"], 4L)
  expect_equal(reduced$hyps$up$info$Background, "30")
  numbers <- lapply(c(100, 23467), function(n) SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), n, "up", 1, 1, list(), TRUE))
  expect_identical(numbers[[1]]$hyps$up$data, numbers[[2]]$hyps$up$data)

  sized <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "both", 1, 1, list(minSize = 6), TRUE)
  expect_equal(sized$dropped, "SPREAD")
  expect_equal(sized$hyps$up$info[["fgsea Args"]], "minSize=6")
  expect_equal(sized$hyps$up$info[["Min Size"]], "6")
})

test_that("runFgseaHyps returns 0-row hyps instead of erroring when fgsea drops every pathway", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  run <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "both", 1, 1, list(minSize = 50), TRUE)
  cols <- c("label", "pval", "fdr", "lte", "es", "nes", "signature", "geneset", "overlap", "le", "hits")
  expect_equal(names(run$hyps), c("up", "down"))
  expect_equal(nrow(run$hyps$up$data), 0L)
  expect_equal(nrow(run$hyps$down$data), 0L)
  expect_equal(colnames(run$hyps$up$data), cols)
  expect_equal(colnames(run$hyps$down$data), cols)
  expect_equal(run$dropped, names(hyper_fgsea_genesets()))
})

test_that("runFgseaHyps keeps a 0-row hyp for the empty side rather than dropping it", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  gs <- hyper_fgsea_genesets()[c("TOP", "TOP_MIX")]
  run <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), gs, 23467, "both", 1, 1, list(), TRUE)
  expect_equal(nrow(run$hyps$down$data), 0L)
  expect_equal(nrow(run$hyps$up$data), 2L)
})

test_that("runFgseaHyps hyps carry hypeR's info keys first, then fgsea's, and gsets in args", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  run <- SigRepo:::runFgseaHyps(hyper_fgsea_stats(), hyper_fgsea_genesets(), 23467, "up", 1, 1, list(), TRUE)
  info <- run$hyps$up$info
  expect_equal(names(info), c(
    "hypeR", "Signature Head", "Signature Size", "Signature Type", "Genesets", "Background", "P-Value", "FDR",
    "Test", "Power", "Absolute", "fgsea", "Sample Size", "Min Size", "Max Size", "Seed", "fgsea Args"
  ))
  expect_true(all(vapply(info, is.character, TRUE)))
  expect_equal(info$Test, "fgsea")
  expect_equal(info$Seed, "1")
  expect_true(inherits(run$hyps$up$args$genesets, "gsets"))
  expect_length(run$hyps$up$plots, 0L)
})

# ---- Rankings: prepareHypeRSignatures(test = "fgsea") ----

test_that("fgsea uses the kstest up ranking, one per signature, and always tests both directions", {
  sig <- make_hyper_fgsea_sig()
  ks <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)

  both <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "fgsea", verbose = FALSE)
  expect_equal(names(both$signatures), "fg")
  expect_equal(both$signatures[["fg"]], ks$signatures[["fg | up"]])
  expect_equal(both$info$direction, "both")
})

test_that("fgsea ranks a categorical signature once per category", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_categorical_sig(), test = "fgsea", verbose = FALSE)
  expect_equal(names(prepared$signatures), c("cat | red", "cat | white"))
  expect_equal(prepared$info$direction, c("both", "both"))
})

test_that("fgsea native input must be named numeric, tests both directions, and rejects ks_source", {
  expect_error(
    SigRepo::prepareHypeRSignatures(signature = c("A", "B"), test = "fgsea", verbose = FALSE),
    "test = \"fgsea\" needs scores: pass a named numeric vector", fixed = TRUE
  )
  native <- SigRepo::prepareHypeRSignatures(signature = hyper_fgsea_stats(), test = "fgsea", verbose = FALSE)
  expect_equal(native$info$direction, "both")
  expect_error(
    SigRepo::prepareHypeRSignatures(signature = hyper_fgsea_stats(), test = "fgsea", ks_source = "signature", verbose = FALSE),
    "'ks_source' applies to SigRepo signatures", fixed = TRUE
  )
})

# ---- runHypeR(test = "fgsea") ----

test_that("runHypeR fgsea defaults to both sides and matches runFgseaHyps with provenance appended", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_fgsea_sig()
  ranking <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "fgsea", verbose = FALSE)$signatures[[1]]

  res <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = sig, genesets = hyper_fgsea_genesets(), test = "fgsea",
                           fdr_scope = "query", verbose = FALSE)
  engine <- SigRepo:::runFgseaHyps(ranking, hyper_fgsea_genesets(), 23467, "both", 1, 1, list(), TRUE)

  expect_true(inherits(res, "multihyp"))
  expect_equal(names(res$data), c("fg | up", "fg | down"))
  expect_equal(res$data[["fg | up"]]$data, engine$hyps$up$data)
  expect_equal(res$data[["fg | down"]]$data, engine$hyps$down$data)

  info <- res$data[["fg | down"]]$info
  expect_equal(utils::tail(names(info), length(SigRepo:::HYPER_PROVENANCE_KEYS)), SigRepo:::HYPER_PROVENANCE_KEYS)
  expect_equal(
    unlist(info[c("Test", "SigRepo Direction", "SigRepo Ranked Table", "SigRepo Score Column", "SigRepo Split",
                  "SigRepo Background Source", "SigRepo FDR Scope")], use.names = FALSE),
    c("fgsea", "down", "difexp", "score", "", "number", "query")
  )
})

test_that("runHypeR fgsea always returns both sides, as a multihyp, and has no direction argument", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  expect_false("direction" %in% names(formals(SigRepo::runHypeR)))
  single <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(),
                              test = "fgsea", verbose = FALSE)
  expect_true(inherits(single, "multihyp"))
  expect_equal(names(single$data), c("fg | up", "fg | down"))
  expect_true(all(single$data[["fg | up"]]$data$es > 0))
  expect_true(all(single$data[["fg | down"]]$data$es < 0))
  expect_error(
    SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(),
                      test = "fgsea", direction = "up", verbose = FALSE),
    "unused argument (direction = \"up\")", fixed = TRUE
  )

  native <- SigRepo::runHypeR(organism = "Homo sapiens", signature = hyper_fgsea_stats(), genesets = hyper_fgsea_genesets(), test = "fgsea", verbose = FALSE)
  expect_true(inherits(native, "multihyp"))
  expect_equal(names(native$data), c("signature | up", "signature | down"))
  expect_equal(native$data[[1]]$info[["SigRepo Ranked Table"]], "")
})

test_that("runHypeR fgsea pools FDR across sides and rankings by default and filters on it", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  sigs <- list(a = make_hyper_fgsea_sig("a"), b = make_hyper_fgsea_sig("b"))
  gs <- hyper_fgsea_genesets()

  per_query <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = sigs, genesets = gs, test = "fgsea", fdr_scope = "query", verbose = FALSE)
  pooled <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = sigs, genesets = gs, test = "fgsea", verbose = FALSE)

  all_p <- unlist(lapply(per_query$data, function(h) h$data$pval), use.names = FALSE)
  expect_equal(unlist(lapply(pooled$data, function(h) h$data$fdr), use.names = FALSE), signif(p.adjust(all_p, method = "fdr"), 2))
  expect_equal(pooled$data[["a | up"]]$info[["SigRepo FDR Scope"]], "run")

  filtered <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = sigs, genesets = gs, test = "fgsea", fdr = 0.001, verbose = FALSE)
  for (nm in names(pooled$data)) {
    keep <- pooled$data[[nm]]$data$fdr <= 0.001
    expect_equal(filtered$data[[nm]]$data, pooled$data[[nm]]$data[keep, , drop = FALSE])
    expect_equal(filtered$data[[nm]]$info$FDR, "0.001")
  }

  query_filtered <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = sigs, genesets = gs, test = "fgsea", fdr_scope = "query",
                                      fdr = 0.001, verbose = FALSE)
  expect_true(all(query_filtered$data[["b | down"]]$data$fdr <= 0.001))
  expect_equal(query_filtered$data[["b | down"]]$args$fdr, 0.001)
})

test_that("runHypeR fgsea returns 0-row hyps instead of erroring when fgsea drops every pathway", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  res <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(), test = "fgsea",
                           fgsea_args = list(minSize = 50), verbose = FALSE)
  expect_true(inherits(res, "multihyp"))
  expect_true(all(vapply(res$data, function(h) nrow(h$data) == 0L, logical(1))))
})

test_that("runHypeR fgsea runs each category of a categorical signature", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  gs <- list(S1 = c("A", "C"), S2 = c("B", "D"), S3 = c("E", "F"))
  res <- suppressWarnings(SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_categorical_sig(), genesets = gs, test = "fgsea", verbose = FALSE))
  expect_equal(names(res$data), c("cat | red | up", "cat | red | down", "cat | white | up", "cat | white | down"))
  expect_equal(res$data[["cat | white | down"]]$info[["Group Label"]], "white")
})

test_that("runHypeR fgsea rejects absolute, bad seeds and reserved fgsea_args, and warns on plotting", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  run <- function(...) SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(),
                                         test = "fgsea", verbose = FALSE, ...)

  expect_error(run(absolute = TRUE), "unused argument (absolute = TRUE)", fixed = TRUE)
  for (bad in list("1", c(1, 2), NA_real_, Inf)) {
    expect_error(run(seed = bad), "'seed' must be NULL or a single number", fixed = TRUE)
  }
  expect_error(run(fgsea_args = list(15)), "'fgsea_args' must be a named list", fixed = TRUE)
  expect_error(run(fgsea_args = list(gseaParam = 2, stats = 1)), "'fgsea_args' cannot set 'gseaParam', 'stats'", fixed = TRUE)
  expect_error(run(fgsea_args = list(scoreType = "pos")), "'fgsea_args' cannot set 'scoreType'", fixed = TRUE)
  expect_warning(run(plotting = TRUE), "fgsea makes no per-geneset plots; plotting is ignored.", fixed = TRUE)
})

test_that("runHypeR fgsea re-emits fgsea's warnings once with an fgsea: prefix", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  tied <- hyper_fgsea_stats()
  tied[1:10] <- 2
  warnings <- testthat::capture_warnings(
    SigRepo::runHypeR(organism = "Homo sapiens", signature = list(x = tied, y = tied), genesets = hyper_fgsea_genesets(), test = "fgsea", verbose = FALSE)
  )
  ties <- grep("^fgsea: There are ties in the preranked stats", warnings, value = TRUE)
  expect_length(ties, 1L)
})

test_that("runHypeR fgsea results work with hypeR's tooling and hypeRToExcel()", {
  testthat::skip_if_not_installed("fgsea")
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("reactable")
  testthat::skip_if_not_installed("visNetwork")
  testthat::skip_if_not_installed("openxlsx")
  res <- SigRepo::runHypeR(organism = "Homo sapiens", omic_signature = make_hyper_fgsea_sig(), genesets = hyper_fgsea_genesets(), test = "fgsea", verbose = FALSE)

  expect_no_error(hypeR::hyp_dots(res, merge = TRUE))
  expect_true(inherits(hypeR::rctbl_build(res), "shiny.tag"))
  expect_no_error(hypeR::hyp_emap(res))
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)
  index <- SigRepo::hypeRToExcel(res, file_path = xlsx)
  expect_equal(openxlsx::getSheetNames(xlsx), c("index", "fg | up", "fg | down", "versioning"))
  expect_equal(index$direction, c("up", "down"))

  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  utils::capture.output(hypeR::hyp_to_rmd(res, file_path = html, title = "fgsea"))
  expect_true(file.exists(html))
})
