test_that("hypeRSymbolColumn picks the first populated symbol column", {
  tbl <- data.frame(gene_symbol = c(NA, ""), symbol = c("A", "B"), stringsAsFactors = FALSE)
  expect_equal(SigRepo:::hypeRSymbolColumn(tbl), "symbol")
  expect_null(SigRepo:::hypeRSymbolColumn(data.frame(feature_name = "f1")))
  expect_null(SigRepo:::hypeRSymbolColumn(NULL))
})

test_that("lookupReferenceSymbols returns nothing without a connection", {
  expect_identical(
    SigRepo:::lookupReferenceSymbols(NULL, "transcriptomics", "Homo sapiens", c("f1", "f2")),
    character()
  )
})

test_that("hypergeometric splits by group_label using the signature's own symbols", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), verbose = FALSE)

  expect_equal(names(prepared$signatures), c("sig_a | Old", "sig_a | Young"))
  expect_equal(prepared$signatures[["sig_a | Old"]], c("A", "B"))
  expect_equal(prepared$signatures[["sig_a | Young"]], c("C", "D"))
  expect_equal(prepared$info$group_label, c("Old", "Young"))
  expect_equal(prepared$info$symbol_source, c("signature$symbol", "signature$symbol"))
  expect_equal(prepared$info$n_features, c(2L, 2L))
  expect_equal(nrow(prepared$skipped), 0L)
})

test_that("split = FALSE gives one hypergeometric vector per signature", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), split = FALSE, verbose = FALSE)

  expect_equal(names(prepared$signatures), "sig_a")
  expect_equal(prepared$signatures[["sig_a"]], c("A", "B", "C", "D"))
  expect_true(is.na(prepared$info$group_label))
})

test_that("a signature without group_label gives one vector named after the signature", {
  tbl <- hyper_sig_table()
  tbl$group_label <- NULL
  sig <- make_hyper_sig(signature = tbl, direction_type = "uni-directional")

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = sig, verbose = FALSE)

  expect_equal(names(prepared$signatures), "sig_a")
})

test_that("hypergeometric falls back to difexp symbols joined on probe_id", {
  sig <- make_hyper_sig(signature = hyper_sig_table(symbols = FALSE), difexp = hyper_difexp_table())

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = sig, verbose = FALSE)

  expect_equal(prepared$signatures[["sig_a | Old"]], c("A", "B"))
  expect_equal(prepared$info$symbol_source[1], "difexp$gene_symbol via probe_id")
})

test_that("hypergeometric falls back to the reference table by feature_name and organism", {
  seen <- NULL
  testthat::local_mocked_bindings(
    lookupReferenceSymbols = function(conn_handler, assay_type, organism, feature_names) {
      seen <<- list(assay_type = assay_type, organism = organism, feature_names = feature_names)
      c(F1 = "A", F2 = "B", F3 = "C")
    },
    .package = "SigRepo"
  )
  sig <- make_hyper_sig(signature = hyper_sig_table(symbols = FALSE))

  prepared <- SigRepo::prepareHypeRSignatures(conn_handler = list(), omic_signature = sig, verbose = FALSE)

  expect_equal(seen$assay_type, "transcriptomics")
  expect_equal(seen$organism, "Homo sapiens")
  expect_equal(seen$feature_names, c("f1", "f2", "f3", "f4"))
  expect_equal(prepared$signatures[["sig_a | Old"]], c("A", "B"))
  expect_equal(prepared$signatures[["sig_a | Young"]], "C")
  expect_equal(prepared$info$n_dropped, c(0L, 1L))
  expect_equal(prepared$info$symbol_source[1], "reference feature_name")
})

test_that("probe_id is never used as a symbol: no symbols and no connection means skipped", {
  sig <- make_hyper_sig(signature = hyper_sig_table(symbols = FALSE))

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = sig, verbose = FALSE)

  expect_length(prepared$signatures, 0L)
  expect_equal(prepared$skipped$signature, "sig_a")
  expect_equal(prepared$skipped$reason, "no_gene_symbols")
})

test_that("kstest ranks the full difexp, collapsing duplicate symbols by max |score|", {
  sig <- make_hyper_sig(difexp = hyper_difexp_table())

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", verbose = FALSE)

  expect_equal(names(prepared$signatures), "sig_a")
  expect_equal(prepared$signatures[["sig_a"]], c(A = 3, B = 2, E = -1, C = -2, D = -3))
  expect_equal(prepared$info$symbol_source, "difexp$gene_symbol")
  expect_true(is.na(prepared$info$group_label))
})

test_that("kstest skips signatures without difexp or without the score column", {
  no_difexp <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), test = "kstest", verbose = FALSE)
  expect_equal(no_difexp$skipped$reason, "no_difexp")

  no_score <- SigRepo::prepareHypeRSignatures(
    omic_signature = make_hyper_sig(difexp = hyper_difexp_table()),
    test = "kstest", score_col = "logFC", verbose = FALSE
  )
  expect_equal(no_score$skipped$reason, "missing_score_col")
  expect_match(no_score$skipped$message, "logFC", fixed = TRUE)
})

test_that("labels come from supplied list names, then signature_name, with (2) for duplicates", {
  named <- SigRepo::prepareHypeRSignatures(
    omic_signature = list(first = make_hyper_sig("x"), second = make_hyper_sig("y")),
    split = FALSE, verbose = FALSE
  )
  expect_equal(names(named$signatures), c("first", "second"))

  dupes <- SigRepo::prepareHypeRSignatures(
    omic_signature = list(make_hyper_sig("dup"), make_hyper_sig("dup"), make_hyper_sig("dup")),
    split = FALSE, verbose = FALSE
  )
  expect_equal(names(dupes$signatures), c("dup", "dup (2)", "dup (3)"))
})

test_that("signature ids are recorded when signatures are fetched by id", {
  testthat::local_mocked_bindings(
    getSignature = function(conn_handler, signature_id = NULL, signature_name = NULL, verbose = TRUE) {
      list(make_hyper_sig("db_sig"))
    },
    .package = "SigRepo"
  )

  inputs <- SigRepo:::collectHypeRSignatures(
    conn_handler = list(), signature_id = 101, signature_name = NULL,
    omic_signature = NULL, verbose = FALSE
  )

  expect_equal(inputs$ids, "101")
  expect_equal(inputs$labels, "db_sig")
})

test_that("prepareHypeRSignatures rejects the removed test aliases", {
  expect_error(
    SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), test = "gsea", verbose = FALSE),
    "should be one of"
  )
})
