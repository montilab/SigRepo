test_that("hypergeometric results match a direct hypeR::hypeR() call", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, split = FALSE, verbose = FALSE)$signatures[[1]]

  direct <- hypeR::hypeR(vec, hyper_genesets(), test = "hypergeometric")
  res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), split = FALSE, verbose = FALSE)

  expect_true(inherits(res, "hyp"))
  expect_equal(res$data, direct$data)
})

test_that("kstest results match a direct hypeR::hypeR() call: weighted at power 1, ranked at power 0", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]

  weighted <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", power = 1, verbose = FALSE)
  expect_equal(weighted$data, hypeR::hypeR(vec, hyper_genesets(), test = "kstest", power = 1)$data)
  expect_equal(weighted$info[["Signature Type"]], "weighted")

  # power = 0 is hypeR's ranked signature (gene names in rank order), not the
  # weighted branch with all weights 1, which scores differently.
  ranked <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", power = 0, verbose = FALSE)
  expect_equal(ranked$data, hypeR::hypeR(names(vec), hyper_genesets(), test = "kstest")$data)
  expect_equal(ranked$info[["Signature Type"]], "ranked")
})

test_that("split queries return a multihyp with aligned SigRepo provenance", {
  testthat::skip_if_not_installed("hypeR")

  res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE)

  expect_true(inherits(res, "multihyp"))
  expect_equal(names(res$data), c("sig_a | Old", "sig_a | Young"))

  provenance_keys <- SigRepo:::HYPER_PROVENANCE_KEYS
  for (hyp_obj in res$data) {
    expect_equal(utils::tail(names(hyp_obj$info), length(provenance_keys)), provenance_keys)
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
  # A list input stays a multihyp even when only one signature survives.
  expect_true(inherits(res, "multihyp"))
  expect_equal(names(res$data), "good")

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

test_that("B1: hypeRToExcel() writes long and unsafe query names as safe, unique sheets with an index", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("openxlsx")
  gs <- list(SET1 = c("TP53", "MYC", "EGFR", "KRAS"), SET2 = c("GENE_A", "GENE_B", "BRCA1"))
  arms <- function() data.frame(
    probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(2, -2),
    group_label = factor(c("Up", "Down")), symbol = c("TP53", "MYC")
  )
  long <- make_hyper_sig("Aging_Hs_SomeTissue_Treatment_vs_Control_Author2025", signature = arms())
  colon <- make_hyper_sig("Aging: a/b [c]?*", signature = arms())
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)

  hyp <- SigRepo::runHypeR(omic_signature = list(long, colon), genesets = gs, verbose = FALSE)
  original_names <- names(hyp$data)
  expect_error(hypeR::hyp_to_excel(hyp, file_path = xlsx), "Max length is 31 characters", fixed = TRUE)

  index <- SigRepo::hypeRToExcel(hyp, file_path = xlsx)

  sheets <- openxlsx::getSheetNames(xlsx)
  expect_equal(sheets, c("index", index$sheet, "versioning"))
  expect_true(all(nchar(sheets) <= 31))
  expect_false(any(grepl("[][\\\\/?*:]", sheets)))
  expect_equal(anyDuplicated(tolower(sheets)), 0L)
  # The two long names share their first 31 characters, so they differ only by suffix.
  expect_equal(index$sheet[1:2], c("Aging_Hs_SomeTissue_Treatment_v", "Aging_Hs_SomeTissue_Treatme (2)"))
  expect_equal(index$query, original_names)
  expect_equal(index$group_label, c("Down", "Up", "Down", "Up"))
  expect_equal(openxlsx::read.xlsx(xlsx, sheet = "index")$query, original_names)
  # The caller's object keeps its names.
  expect_equal(names(hyp$data), original_names)
})

test_that("hypeRSheetNames sanitises, truncates and de-duplicates without regard to case", {
  out <- SigRepo:::hypeRSheetNames(
    c("Index", "a/b", "A_B", "'quoted'", "", strrep("x", 40), strrep("X", 40)),
    reserved = c("index", "versioning")
  )
  expect_equal(out, c("Index (2)", "a_b", "A_B (2)", "quoted", "sheet", strrep("x", 31), paste0(strrep("X", 27), " (2)")))
  expect_true(all(nchar(out) <= 31))
})

test_that("hypeRToExcel() names a single hyp's sheet from its provenance and rejects other objects", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("openxlsx")
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)

  res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), split = FALSE, verbose = FALSE)
  expect_true(inherits(res, "hyp"))
  index <- SigRepo::hypeRToExcel(res, file_path = xlsx, index = FALSE)
  expect_equal(openxlsx::getSheetNames(xlsx), c("sig_a", "versioning"))
  expect_equal(index$signature_name, "sig_a")

  expect_error(SigRepo::hypeRToExcel(list(), file_path = xlsx), "must be a hypeR hyp or multihyp", fixed = TRUE)
})

test_that("U1: weighted kstest drops genesets whose hits all score 0, with a warning", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_zero_score_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]
  expect_equal(vec, c(A = 2, D = 1, B = 0, C = -1))

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_zero_score_genesets(),
                             test = "kstest", power = 1, verbose = FALSE),
    "Dropped 1 geneset(s) that hypeR's kstest cannot score (all hits score 0, or the geneset covers every query gene): S2", fixed = TRUE
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
  expect_equal(res$data, hypeR::hypeR(names(vec), hyper_zero_score_genesets(), test = "kstest")$data)
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
    "Dropped 2 geneset(s) that hypeR's kstest cannot score (all hits score 0, or the geneset covers every query gene): S2, S4",
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

# ---- Final-review fix wave (F1-F4) ----

test_that("F1: kstest pval and fdr do not depend on power (only score does)", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())

  by_label <- function(res) res$data[order(res$data$label), , drop = FALSE]
  weighted <- by_label(SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", power = 1, verbose = FALSE))
  unweighted <- by_label(SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", power = 0, verbose = FALSE))

  expect_identical(weighted$label, unweighted$label)
  expect_identical(weighted$pval, unweighted$pval)
  expect_identical(weighted$fdr, unweighted$fdr)
})

test_that("F2: background = \"difexp\" removes hypergeometric query genes that difexp did not measure", {
  testthat::skip_if_not_installed("hypeR")
  sig_tbl <- hyper_sig_table()
  sig_tbl$symbol <- c("A", "B", "C", "Z")
  sig <- make_hyper_sig(signature = sig_tbl, difexp = hyper_difexp_table())
  difexp_symbols <- unique(sig$difexp$gene_symbol)
  expect_false("Z" %in% difexp_symbols)

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), background = "difexp",
                             split = FALSE, verbose = FALSE),
    "background = \"difexp\": removed 1 query gene(s) not measured in difexp from: 'sig_a' (1)", fixed = TRUE
  )

  direct <- hypeR::hypeR(c("A", "B", "C"), hyper_genesets(), test = "hypergeometric", background = difexp_symbols)
  expect_equal(res$data, direct$data)
})

test_that("F3: a query with no genesets left is skipped and the rest of the batch still runs", {
  testthat::skip_if_not_installed("hypeR")
  zero_sig <- make_hyper_zero_score_sig()
  other_sig <- make_hyper_sig(difexp = hyper_difexp_table())
  other_vec <- SigRepo::prepareHypeRSignatures(omic_signature = other_sig, test = "kstest", verbose = FALSE)$signatures[[1]]

  # The reviewer's reproduction used list(S2 = "B"), but hypeR 2.0.0's kstest
  # errors on any one-geneset list ("dim(X) must have a positive length"), so
  # other_sig could never return a result with it. S4 also hits only B.
  gs <- list(S2 = "B", S4 = c("B", "X"))

  warnings <- NULL
  expect_no_error(
    warnings <- testthat::capture_warnings(
      res <- SigRepo::runHypeR(omic_signature = list(zero_sig, other_sig), genesets = gs,
                               test = "kstest", verbose = FALSE)
    )
  )

  expect_length(warnings, 2)
  expect_match(warnings[1], "Dropped 2 geneset(s) that hypeR's kstest cannot score (all hits score 0, or the geneset covers every query gene): S2, S4", fixed = TRUE)
  expect_match(warnings[2], "Skipped 1 query(ies) with nothing left to test after removing zero-weight genesets or unmeasured genes: 'zero'", fixed = TRUE)

  expect_true(inherits(res, "multihyp"))
  expect_equal(names(res$data), "sig_a")
  expect_equal(res$data[[1]]$data, hypeR::hypeR(other_vec, gs, test = "kstest")$data)
  expect_equal(res$data[[1]]$info[["SigRepo Signature Name"]], "sig_a")
})

test_that("F3: every query skipped is a clear error", {
  testthat::skip_if_not_installed("hypeR")
  expect_error(
    suppressWarnings(SigRepo::runHypeR(omic_signature = make_hyper_zero_score_sig(), genesets = list(S2 = "B"),
                                       test = "kstest", verbose = FALSE)),
    "No query had genesets left to test", fixed = TRUE
  )
})

test_that("F4: kstest drops a geneset that covers every query gene, at power 1 and power 0", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_cover_sig()
  vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]
  expect_equal(vec, c(A = 3, B = 2, C = -1))
  gs <- list(ALL = c("A", "B", "C", "X"), S2 = "A", S3 = "B")

  for (pwr in c(1, 0)) {
    expect_no_error(expect_warning(
      res <- SigRepo::runHypeR(omic_signature = sig, genesets = gs, test = "kstest", power = pwr, verbose = FALSE),
      "Dropped 1 geneset(s) that hypeR's kstest cannot score (all hits score 0, or the geneset covers every query gene): ALL",
      fixed = TRUE
    ))
    expect_false("ALL" %in% res$data$label)
    direct_sig <- if (pwr == 0) names(vec) else vec
    expect_equal(res$data, hypeR::hypeR(direct_sig, list(S2 = "A", S3 = "B"), test = "kstest", power = pwr)$data)
  }
})

# ---- Evaluation fixes (W1-W15) ----

test_that("W3: the return type follows the input, not how many queries survive", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())
  run <- function(...) SigRepo::runHypeR(genesets = hyper_genesets(), verbose = FALSE, ...)

  expect_true(inherits(run(omic_signature = sig), "multihyp"))
  expect_true(inherits(run(omic_signature = sig, split = FALSE), "hyp"))
  expect_true(inherits(run(omic_signature = sig, test = "kstest"), "hyp"))
  expect_true(inherits(run(omic_signature = sig, test = "kstest", direction = "down"), "hyp"))
  expect_true(inherits(run(omic_signature = sig, test = "kstest", direction = "both"), "multihyp"))
  expect_true(inherits(run(omic_signature = list(sig), split = FALSE), "multihyp"))
  expect_true(inherits(run(omic_signature = list(sig), test = "kstest"), "multihyp"))

  # A signature without group_label still gives a multihyp when split = TRUE.
  tbl <- hyper_sig_table()
  tbl$group_label <- NULL
  uni <- make_hyper_sig(signature = tbl, direction_type = "uni-directional")
  uni_res <- run(omic_signature = uni)
  expect_true(inherits(uni_res, "multihyp"))
  expect_equal(names(uni_res$data), "sig_a")
})

test_that("W3: one id is a single input; several ids give a multihyp", {
  testthat::skip_if_not_installed("hypeR")
  testthat::local_mocked_bindings(
    getSignature = function(conn_handler, signature_id = NULL, signature_name = NULL, verbose = TRUE) {
      list(make_hyper_sig(paste0("db_", signature_id)))
    },
    .package = "SigRepo"
  )
  one <- SigRepo::runHypeR(conn_handler = list(), signature_id = 7, genesets = hyper_genesets(), split = FALSE, verbose = FALSE)
  expect_true(inherits(one, "hyp"))
  expect_equal(one$info[["SigRepo Signature ID"]], "7")

  two <- SigRepo::runHypeR(conn_handler = list(), signature_id = c(7, 8), genesets = hyper_genesets(), split = FALSE, verbose = FALSE)
  expect_true(inherits(two, "multihyp"))
  expect_equal(names(two$data), c("db_7", "db_8"))
})

test_that("W1: direction = \"down\" and \"both\" rank the negated scores and match hypeR", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig(difexp = hyper_difexp_table())
  up_vec <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)$signatures[[1]]
  down_vec <- sort(-up_vec, decreasing = TRUE)

  down <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", direction = "down", verbose = FALSE)
  expect_equal(down$data, hypeR::hypeR(down_vec, hyper_genesets(), test = "kstest")$data)
  expect_equal(down$info[["SigRepo Direction"]], "down")

  both <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", direction = "both", verbose = FALSE)
  expect_equal(names(both$data), c("sig_a | up", "sig_a | down"))
  expect_equal(both$data[["sig_a | up"]]$data, hypeR::hypeR(up_vec, hyper_genesets(), test = "kstest")$data)
  expect_equal(both$data[["sig_a | down"]]$data, down$data)
  expect_equal(unname(vapply(both$data, function(h) h$info[["SigRepo Direction"]], "")), c("up", "down"))
})

test_that("W1/W5: direction and ks_source are rejected for a hypergeometric test", {
  testthat::skip_if_not_installed("hypeR")
  for (args in list(list(direction = "down"), list(ks_source = "signature"))) {
    expect_error(
      do.call(SigRepo::runHypeR, c(list(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE), args)),
      "'direction' and 'ks_source' only apply when test = \"kstest\"", fixed = TRUE
    )
  }
})

test_that("W5: ks_source = \"signature\" runs kstest on a signature without difexp", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()
  expect_error(
    suppressWarnings(SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", verbose = FALSE)),
    "No signature produced a hypeR query vector", fixed = TRUE
  )

  res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), test = "kstest", ks_source = "signature", verbose = FALSE)
  expect_equal(res$data, hypeR::hypeR(c(A = 3, B = 2, C = -2, D = -3), hyper_genesets(), test = "kstest")$data)
  expect_equal(res$info[["SigRepo Ranked Table"]], "signature")
  expect_equal(res$info[["Symbol Source"]], "signature$symbol")
})

test_that("W6: provenance records background source, split, score column and dropped genesets", {
  testthat::skip_if_not_installed("hypeR")

  hyper <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE)$data[[1]]$info
  expect_equal(
    unlist(hyper[SigRepo:::HYPER_PROVENANCE_KEYS[5:13]], use.names = FALSE),
    c("", "", "", "TRUE", "number", "0", "0", "0", "")
  )

  expect_warning(
    ks <- SigRepo::runHypeR(omic_signature = make_hyper_zero_score_sig(), genesets = hyper_zero_score_genesets(),
                            test = "kstest", background = "difexp", verbose = FALSE),
    "Dropped 1 geneset(s)", fixed = TRUE
  )
  expect_equal(
    unlist(ks$info[SigRepo:::HYPER_PROVENANCE_KEYS[5:13]], use.names = FALSE),
    c("up", "difexp", "score", "", "difexp", "0", "0", "1", "S2")
  )

  expect_warning(
    fallback <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(),
                                  background = "difexp", split = FALSE, verbose = FALSE),
    "no difexp gene symbols", fixed = TRUE
  )
  expect_equal(fallback$info[["SigRepo Background Source"]], "difexp-fallback")
  expect_equal(fallback$info[["SigRepo Split"]], "FALSE")
})

test_that("W6: unmapped features and removed query genes are recorded per query", {
  testthat::skip_if_not_installed("hypeR")
  sig_tbl <- hyper_sig_table()
  sig_tbl$symbol <- c("A", NA, "C", "Z")
  sig <- make_hyper_sig(signature = sig_tbl, difexp = hyper_difexp_table())

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), background = "difexp", verbose = FALSE),
    "removed 1 query gene(s) not measured in difexp from: 'sig_a | Young' (1)", fixed = TRUE
  )
  expect_equal(res$data[["sig_a | Old"]]$info[["SigRepo Features Unmapped"]], "1")
  expect_equal(res$data[["sig_a | Old"]]$info[["SigRepo Query Genes Removed"]], "0")
  expect_equal(res$data[["sig_a | Young"]]$info[["SigRepo Features Unmapped"]], "0")
  expect_equal(res$data[["sig_a | Young"]]$info[["SigRepo Query Genes Removed"]], "1")
})

test_that("W13: a gene-vector background also removes hypergeometric query genes outside it", {
  testthat::skip_if_not_installed("hypeR")
  bg <- c("A", "B", "C", "X1", "X2", "X3")

  expect_warning(
    res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), background = bg,
                             split = FALSE, verbose = FALSE),
    "background: removed 1 query gene(s) not in the background gene vector from: 'sig_a' (1)", fixed = TRUE
  )
  expect_equal(res$data, hypeR::hypeR(c("A", "B", "C"), hyper_genesets(), background = bg)$data)
  expect_equal(res$info[["SigRepo Background Source"]], "genes")
  expect_equal(res$info[["SigRepo Query Genes Removed"]], "1")
})

test_that("W8: empty placeholder plots are removed unless plotting = TRUE", {
  testthat::skip_if_not_installed("hypeR")
  sig <- make_hyper_sig()

  plain <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), split = FALSE, verbose = FALSE)
  expect_length(plain$plots, 0L)
  expect_equal(plain$data, hypeR::hypeR(c("A", "B", "C", "D"), hyper_genesets())$data)

  plotted <- SigRepo::runHypeR(omic_signature = sig, genesets = hyper_genesets(), split = FALSE, plotting = TRUE, verbose = FALSE)
  expect_gt(length(plotted$plots), 0L)
})

test_that("W14: difexp symbols are resolved once per signature for kstest with background = \"difexp\"", {
  testthat::skip_if_not_installed("hypeR")
  calls <- 0L
  testthat::local_mocked_bindings(
    lookupReferenceSymbols = function(conn_handler, assay_type, organism, feature_names) {
      calls <<- calls + 1L
      c(f1 = "A", f2 = "B", f3 = "C", f4 = "D", f5 = "E", f6 = "E")
    },
    .package = "SigRepo"
  )
  difexp <- hyper_difexp_table()
  difexp$gene_symbol <- NULL
  sig <- make_hyper_sig(difexp = difexp)

  res <- SigRepo::runHypeR(conn_handler = NULL, omic_signature = sig, genesets = hyper_genesets(),
                           test = "kstest", background = "difexp", verbose = FALSE)
  expect_equal(calls, 1L)
  expect_equal(res$info$Background, "5")
  expect_equal(res$info[["Symbol Source"]], "reference feature_name")
})

test_that("W2: query_names renames queries before hypeR runs", {
  testthat::skip_if_not_installed("hypeR")
  res <- SigRepo::runHypeR(
    omic_signature = list(make_hyper_sig("first"), make_hyper_sig("second")), genesets = hyper_genesets(),
    query_names = function(info) substr(info$group_label, 1, 1), verbose = FALSE
  )
  expect_equal(names(res$data), c("O", "Y", "O (2)", "Y (2)"))
  expect_equal(res$data[["O (2)"]]$info[["SigRepo Signature Name"]], "second")

  expect_error(
    SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), query_names = "{label}", verbose = FALSE),
    "'query_names' must be NULL or a function", fixed = TRUE
  )
})

test_that("W11: hypeR's downstream functions accept runHypeR() results", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("igraph")
  testthat::skip_if_not_installed("visNetwork")
  testthat::skip_if_not_installed("reactable")
  gs <- list(
    SET_AB = c("A", "B", "X1", "X2"), SET_ABX = c("A", "B", "X1", "X3"), SET_CD = c("C", "D", "X3"),
    SET_CDX = c("C", "D", "X4", "X5"), SET_E = c("E", "X4", "X5"), SET_AE = c("A", "E", "X6")
  )
  sig <- make_hyper_sig(difexp = hyper_difexp_table())
  res <- SigRepo::runHypeR(omic_signature = sig, genesets = gs, verbose = FALSE)

  expect_no_error(hypeR::hyp_emap(res, top = 6, similarity_cutoff = 0.1))
  expect_no_error(hypeR::hyp_dots(res, merge = TRUE))
  expect_no_error(hypeR::hyp_show(res$data[[1]]))
  expect_true(inherits(hypeR::rctbl_build(res), "shiny.tag"))

  table_dir <- tempfile()
  on.exit(unlink(table_dir, recursive = TRUE), add = TRUE)
  hypeR::hyp_to_table(res, file_path = table_dir)
  expect_setequal(list.files(table_dir), paste0(names(res$data), ".txt"))

  nodes <- data.frame(label = names(gs), row.names = paste0("n", seq_along(gs)))
  edges <- data.frame(from = c("n1", "n3"), to = c("n2", "n4"))
  rg <- hypeR::rgsets$new(gs, nodes, edges, name = "toy", version = "v1", quiet = TRUE)
  res_rg <- SigRepo::runHypeR(omic_signature = sig, genesets = rg, test = "kstest", verbose = FALSE)
  expect_no_error(hypeR::hyp_hmap(res_rg, top = 6))
  expect_no_error(hypeR::hyp_to_graph(res_rg))
})

test_that("W11: hyp_to_rmd() renders a runHypeR() multihyp", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  res <- SigRepo::runHypeR(omic_signature = make_hyper_sig(), genesets = hyper_genesets(), verbose = FALSE)
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)

  utils::capture.output(hypeR::hyp_to_rmd(res, file_path = html, title = "SigRepo hypeR"))
  expect_true(file.exists(html))
})
