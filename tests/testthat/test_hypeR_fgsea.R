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

test_that("fgsea uses the kstest up ranking, one per signature, with the requested direction in info", {
  sig <- make_hyper_fgsea_sig()
  ks <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)

  both <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "fgsea", verbose = FALSE)
  expect_equal(names(both$signatures), "fg")
  expect_equal(both$signatures, ks$signatures)
  expect_equal(both$info$direction, "both")

  down <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "fgsea", direction = "down", verbose = FALSE)
  expect_equal(down$signatures, ks$signatures)
  expect_equal(down$info$direction, "down")
})

test_that("fgsea ranks a categorical signature once per category", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_categorical_sig(), test = "fgsea", verbose = FALSE)
  expect_equal(names(prepared$signatures), c("cat | red", "cat | white"))
  expect_equal(prepared$info$direction, c("both", "both"))
})

test_that("fgsea native input must be named numeric; direction is allowed, ks_source is not", {
  expect_error(
    SigRepo::prepareHypeRSignatures(signature = c("A", "B"), test = "fgsea", verbose = FALSE),
    "test = \"fgsea\" needs scores: pass a named numeric vector", fixed = TRUE
  )
  up <- SigRepo::prepareHypeRSignatures(signature = hyper_fgsea_stats(), test = "fgsea", direction = "up", verbose = FALSE)
  expect_equal(up$info$direction, "up")
  expect_error(
    SigRepo::prepareHypeRSignatures(signature = hyper_fgsea_stats(), test = "fgsea", ks_source = "signature", verbose = FALSE),
    "'direction' and 'ks_source' apply to SigRepo signatures", fixed = TRUE
  )
})
