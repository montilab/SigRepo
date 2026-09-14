test_that("hypergeometric results match a direct hypeR::hypeR() call", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, split = FALSE, verbose = FALSE)$signatures[[1]]

  direct <- hypeR::hypeR(vec, hyper_genesets(), test = "hypergeometric")
  res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), split = FALSE, verbose = FALSE)

  expect_true(inherits(res, "hyp"))
  expect_equal(res$data, direct$data)
})

test_that("kstest results match a direct hypeR::hypeR() call at default power and power = 0", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]

  for (pwr in c(1, 0)) {
    direct <- hypeR::hypeR(vec, hyper_genesets(), test = "kstest", power = pwr)
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", power = pwr, verbose = FALSE)
    expect_equal(res$data, direct$data)
  }
})

test_that("split queries return a multihyp with aligned SigRepo provenance", {
  testthat::skip_if_not_installed("hypeR")

  res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE)

  expect_true(inherits(res, "multihyp"))
  expect_equal(names(res$data), c("sig_a | Old", "sig_a | Young"))

  provenance_keys <- c("SigRepo Signature ID", "SigRepo Signature Name", "Group Label", "Symbol Source")
  for (hyp_obj in res$data) {
    expect_equal(utils::tail(names(hyp_obj$info), 4), provenance_keys)
  }
  expect_equal(res$data[["sig_a | Old"]]$info[["Group Label"]], "Old")
  expect_equal(res$data[["sig_a | Old"]]$info[["SigRepo Signature Name"]], "sig_a")
  expect_equal(res$data[["sig_a | Old"]]$info[["SigRepo Signature ID"]], "")
  expect_equal(res$data[["sig_a | Old"]]$info[["Symbol Source"]], "signature$symbol")
})

test_that("hypeR's own plotting and export functions work on the result", {
  testthat::skip_if_not_installed("hypeR")
  res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE)

  expect_no_error(hypeR::hyp_dots(res))

  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)
  expect_no_error(hypeR::hyp_to_excel(res, file_path = xlsx))
  expect_true(file.exists(xlsx))
})

test_that("background = \"difexp\" uses each signature's measured genes", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())

  res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), background = "difexp", verbose = FALSE)

  expect_equal(res$data[["sig_a | Old"]]$info$Background, "5")
  expect_equal(res$data[["sig_a | Young"]]$info$Background, "5")
})

test_that("background = \"difexp\" falls back to 23467 with a warning when there is no difexp", {
  testthat::skip_if_not_installed("hypeR")

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(),
                             background = "difexp", split = FALSE, verbose = FALSE),
    "no difexp gene symbols for 'sig_a'; used background = 23467", fixed = TRUE
  )
  expect_equal(res$info$Background, "23467")
})

test_that("unusable signatures are skipped with a warning; all unusable is an error", {
  testthat::skip_if_not_installed("hypeR")
  bad <- make_hyper_sig("bad", signature = hyper_sig_table(symbols = FALSE))

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = list(good = make_hyper_sig("good"), bad = bad),
                             genesets = hyper_genesets(), split = FALSE, verbose = FALSE),
    "Skipped 1 signature(s): 'bad' (no_gene_symbols", fixed = TRUE
  )
  expect_true(inherits(res, "hyp"))

  expect_error(
    suppressWarnings(SigRepo::runHypeR(omic_signature = bad, genesets = hyper_genesets(), verbose = FALSE)),
    "No signature produced a hypeR query vector", fixed = TRUE
  )
})

test_that("runHypeR requires genesets and rejects the removed arguments", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()

  expect_error(SigRepo::runHypeR(omic_signature = sig, verbose = FALSE), "'genesets' must be \"msigdb\"", fixed = TRUE)
  expect_error(SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), method = "gsea"), "unused argument")
  expect_error(SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "gsea"), "should be one of")
})
