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

# ---- Task 3c regressions (stress-test bugs B1, B2, B5, U1) ----

test_that("B5/E08: runHypeR rejects a background that is not a number, a gene vector or \"difexp\"", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()
  called <- FALSE
  testthat::local_mocked_bindings(
    collectHypeRSignatures = function(...) {
      called <<- TRUE
      stop("signature work started")
    },
    .package = "SigRepo"
  )

  for (bad in list("Difexp", "DIFEXP", "TP53", NA, NULL, -1, 0, Inf, c(100, 200), TRUE)) {
    expect_error(
      SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), background = bad, verbose = FALSE),
      "background must be a number, a gene vector, or \"difexp\"", fixed = TRUE
    )
  }
  expect_false(called)
})

test_that("B5/E09: runHypeR rejects a split that is not TRUE or FALSE", {
  testthat::skip_if_not_installed("hypeR")
  for (bad in list("yes", NA)) {
    expect_error(
      SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), split = bad, verbose = FALSE),
      "'split' must be TRUE or FALSE", fixed = TRUE
    )
  }
})

test_that("B5/E10: runHypeR rejects omic_signature together with signature_id", {
  testthat::skip_if_not_installed("hypeR")
  expect_error(
    SigRepo::runHypeR(conn_handler = list(), signature_id = "1", omic_signature = make_hyper_sig(),
                      genesets = hyper_genesets(), verbose = FALSE),
    "Supply either 'omic_signature' or 'signature_id'/'signature_name', not both", fixed = TRUE
  )
})

test_that("B2: runHypeR multihyp names are unique", {
  testthat::skip_if_not_installed("hypeR")
  gs <- list(SET1 = c("TP53", "MYC", "EGFR", "KRAS"), SET2 = c("GENE_A", "GENE_B", "BRCA1"))
  a <- make_hyper_sig("X", signature = data.frame(
    probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(2, -2),
    group_label = factor(c("Up", "Down")), symbol = c("TP53", "MYC")
  ))
  b <- make_hyper_sig("X | Up", signature = data.frame(
    probe_id = "p1", feature_name = "f1", score = 1, group_label = factor(NA), symbol = "EGFR"
  ), direction_type = "uni-directional")

  res <- SigRepo::runHypeR(omic_signature = list(a, b), genesets = gs, verbose = FALSE)

  expect_equal(names(res$data), c("X | Down", "X | Up", "X | Up (2)"))
  expect_equal(res$data[["X | Up (2)"]]$info[["SigRepo Signature Name"]], "X | Up")
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)
  expect_no_error(hypeR::hyp_to_excel(res, file_path = xlsx))
})

test_that("B1: the documented sheet-name workaround makes hyp_to_excel() work on long query names", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("openxlsx")
  gs <- list(SET1 = c("TP53", "MYC", "EGFR", "KRAS"), SET2 = c("GENE_A", "GENE_B", "BRCA1"))
  long <- make_hyper_sig("Aging_Hs_SomeTissue_Treatment_vs_Control_Author2025", signature = data.frame(
    probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(2, -2),
    group_label = factor(c("Up", "Down")), symbol = c("TP53", "MYC")
  ))
  colon <- make_hyper_sig("Aging: a/b [c]?*", signature = data.frame(
    probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(2, -2),
    group_label = factor(c("Up", "Down")), symbol = c("TP53", "MYC")
  ))
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)

  hyp <- SigRepo::runHypeR(omic_signature = list(long, colon), genesets = gs, verbose = FALSE)
  expect_error(hypeR::hyp_to_excel(hyp, file_path = xlsx), "Max length is 31 characters", fixed = TRUE)

  names(hyp$data) <- make.unique(substr(gsub("[][\\\\/?*:]", "_", names(hyp$data)), 1, 28))
  expect_no_error(hypeR::hyp_to_excel(hyp, file_path = xlsx))

  sheets <- openxlsx::getSheetNames(xlsx)
  expect_equal(sheets, c(names(hyp$data), "versioning"))
  expect_true(all(nchar(sheets) <= 31))
  expect_false(any(grepl("[][\\\\/?*:]", sheets)))
})

test_that("U1: weighted kstest drops genesets whose hits all score 0, with a warning", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_zero_score_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]
  expect_equal(vec, c(A = 2, D = 1, B = 0, C = -1))

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_zero_score_genesets(),
                             test = "kstest", power = 1, verbose = FALSE),
    "Dropped 1 geneset(s) whose hits all have score 0 (undefined for weighted kstest, power != 0): S2", fixed = TRUE
  )
  expect_false("S2" %in% res$data$label)

  direct <- hypeR::hypeR(vec, list(S1 = c("A", "D"), S3 = "C"), test = "kstest", power = 1)
  expect_equal(res$data$score[res$data$label == "S1"], direct$data$score[direct$data$label == "S1"])
  expect_equal(res$data, direct$data)
})

test_that("U1: power = 0 drops nothing and does not warn", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_zero_score_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]

  expect_no_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_zero_score_genesets(),
                             test = "kstest", power = 0, verbose = FALSE)
  )
  expect_true("S2" %in% res$data$label)
  expect_equal(res$data, hypeR::hypeR(vec, hyper_zero_score_genesets(), test = "kstest", power = 0)$data)
})

test_that("U1: dropZeroWeightGenesets keeps the input kind and returns it unchanged when nothing is dropped", {
  testthat::skip_if_not_installed("hypeR")
  query <- c(A = 2, D = 1, B = 0, C = -1)
  gs_list <- c(hyper_zero_score_genesets(), list(S4 = c("B", "Z"), S5 = "NOT_IN_QUERY"))

  from_list <- SigRepo:::dropZeroWeightGenesets(gs_list, query)
  expect_equal(from_list$genesets, gs_list[c("S1", "S3", "S5")])
  expect_equal(from_list$dropped, c("S2", "S4"))

  gsets_obj <- hypeR::gsets$new(gs_list, name = "test", version = "v1", quiet = TRUE)
  from_gsets <- SigRepo:::dropZeroWeightGenesets(gsets_obj, query)
  expect_true(inherits(from_gsets$genesets, "gsets"))
  expect_equal(from_gsets$genesets$genesets, gs_list[c("S1", "S3", "S5")])
  expect_equal(from_gsets$genesets$name, "test")
  expect_equal(from_gsets$genesets$version, "v1")

  nodes <- data.frame(label = names(gs_list), row.names = paste0("n", seq_along(gs_list)))
  edges <- data.frame(from = "n1", to = "n2")
  rgsets_obj <- hypeR::rgsets$new(gs_list, nodes, edges, name = "rtest", version = "v2", quiet = TRUE)
  from_rgsets <- SigRepo:::dropZeroWeightGenesets(rgsets_obj, query)
  expect_true(inherits(from_rgsets$genesets, "rgsets"))
  expect_equal(names(from_rgsets$genesets$genesets), c("S1", "S3", "S5"))
  expect_equal(from_rgsets$dropped, c("S2", "S4"))

  no_zero <- c(A = 2, B = 1)
  unchanged <- SigRepo:::dropZeroWeightGenesets(gsets_obj, no_zero)
  expect_identical(unchanged$genesets, gsets_obj)
  expect_identical(unchanged$dropped, character())
})

test_that("U1 fix round 1: the zero-weight check sees the gene-vector background reduction", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_zero_score_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]
  gs <- list(S1 = c("A", "D"), S2 = c("B", "A"), S3 = "C", S4 = c("B", "A", "X"))
  bg <- c("B", "C", "D")

  # After reduction to bg, S2 and S4 hit only B (score 0); S1 keeps D, S3 keeps C.
  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = gs, test = "kstest", background = bg, verbose = FALSE),
    "Dropped 2 geneset(s) whose hits all have score 0 (undefined for weighted kstest, power != 0): S2, S4",
    fixed = TRUE
  )

  direct <- hypeR::hypeR(vec, gs[c("S1", "S3")], test = "kstest", background = bg, power = 1)
  expect_equal(res$data, direct$data)

  # Helper: background is applied only to the check; the returned object is not reduced.
  dz <- SigRepo:::dropZeroWeightGenesets(gs, vec, background = bg)
  expect_equal(dz$dropped, c("S2", "S4"))
  expect_identical(dz$genesets, gs[c("S1", "S3")])
  expect_identical(SigRepo:::dropZeroWeightGenesets(gs, vec)$dropped, character())
  expect_identical(SigRepo:::dropZeroWeightGenesets(gs, vec, background = 23467)$dropped, character())
})
