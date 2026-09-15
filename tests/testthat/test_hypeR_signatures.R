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

# ---- Task 3c regressions (stress-test bugs B2-B5) ----

test_that("B2: query names are unique even when a label collides with another signature's group suffix", {
  a <- make_hyper_sig("X", signature = data.frame(
    probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(2, -2),
    group_label = factor(c("Up", "Down")), symbol = c("TP53", "MYC")
  ))
  b <- make_hyper_sig("X | Up", signature = data.frame(
    probe_id = "p1", feature_name = "f1", score = 1, group_label = factor(NA), symbol = "EGFR"
  ), direction_type = "uni-directional")

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = list(a, b), verbose = FALSE)

  expect_equal(names(prepared$signatures), c("X | Down", "X | Up", "X | Up (2)"))
  expect_identical(prepared$info$query, names(prepared$signatures))
  expect_equal(prepared$info$signature_name, c("X", "X", "X | Up"))
  expect_equal(prepared$signatures[["X | Up (2)"]], "EGFR")
})

test_that("B3: the probe_id join uses (probe_id, feature_name) so a repeated probe_id keeps its own symbol", {
  d <- data.frame(probe_id = c("p1", "p1"), feature_name = c("f1", "f2"), gene_symbol = c("GENE_A", "GENE_B"),
                  score = c(3, 2.9), p_value = .01, adj_p = .05, group_label = factor(c("Up", "Up")))
  s <- data.frame(probe_id = "p1", feature_name = "f2", score = 2.9, group_label = factor("Up"))

  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig("dup", s, d), verbose = FALSE)

  expect_equal(prepared$signatures[[1]], "GENE_B")
  expect_equal(prepared$info$symbol_source, "difexp$gene_symbol via probe_id")
})

test_that("B3: a non-unique probe_id without feature_name on both sides skips the probe_id join", {
  meta <- list(assay_type = "transcriptomics", organism = "Homo sapiens")
  difexp_no_fn <- data.frame(probe_id = c("p1", "p1", "p2"), gene_symbol = c("GENE_A", "GENE_B", "GENE_C"))

  # Neither table has feature_name: nothing else to try, so no symbols.
  neither <- SigRepo:::resolveSignatureSymbols(
    list(metadata = meta, signature = data.frame(probe_id = c("p1", "p2")), difexp = difexp_no_fn),
    "signature"
  )
  expect_true(all(is.na(neither$symbols)))
  expect_true(is.na(neither$source))

  # Only the signature has feature_name: fall through to the reference lookup.
  testthat::local_mocked_bindings(
    lookupReferenceSymbols = function(conn_handler, assay_type, organism, feature_names) c(f1 = "REF_1", f2 = "REF_2"),
    .package = "SigRepo"
  )
  sig_only <- SigRepo:::resolveSignatureSymbols(
    list(metadata = meta, signature = data.frame(probe_id = c("p1", "p2"), feature_name = c("f1", "f2")),
         difexp = difexp_no_fn),
    "signature", conn_handler = list()
  )
  expect_equal(sig_only$symbols, c("REF_1", "REF_2"))
  expect_equal(sig_only$source, "reference feature_name")

  # A unique probe_id without feature_name still joins on probe_id alone.
  unique_probe <- SigRepo:::resolveSignatureSymbols(
    list(metadata = meta, signature = data.frame(probe_id = c("p2", "p1")),
         difexp = data.frame(probe_id = c("p1", "p2"), gene_symbol = c("GENE_A", "GENE_C"))),
    "signature"
  )
  expect_equal(unique_probe$symbols, c("GENE_C", "GENE_A"))
  expect_equal(unique_probe$source, "difexp$gene_symbol via probe_id")
})

test_that("B4: kstest collapse keeps the positive value on an opposite-sign |score| tie, whatever the row order", {
  d <- data.frame(probe_id = paste0("p", 1:3), feature_name = paste0("f", 1:3), gene_symbol = c("TP53", "TP53", "MYC"),
                  score = c(2, -2, 1), p_value = .01, adj_p = .05, group_label = factor(c("Up", "Down", "Up")))
  s <- data.frame(probe_id = "p3", feature_name = "f3", score = 1, group_label = factor("Up"))
  f <- function(dd) {
    SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig("t", s, dd), test = "kstest",
                                    verbose = FALSE)$signatures[[1]][["TP53"]]
  }

  expect_equal(c(f(d), f(d[c(2, 1, 3), ])), c(2, 2))
})

test_that("B5/E09: prepareHypeRSignatures rejects a split that is not TRUE or FALSE", {
  for (bad in list("yes", NA, c(TRUE, FALSE), 1)) {
    expect_error(
      SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), split = bad, verbose = FALSE),
      "'split' must be TRUE or FALSE", fixed = TRUE
    )
  }
})

test_that("B5/E10: omic_signature together with signature_id or signature_name is an error", {
  expect_error(
    SigRepo:::collectHypeRSignatures(conn_handler = list(), signature_id = "1", signature_name = NULL,
                                     omic_signature = make_hyper_sig(), verbose = FALSE),
    "Supply either 'omic_signature' or 'signature_id'/'signature_name', not both", fixed = TRUE
  )
  expect_error(
    SigRepo::prepareHypeRSignatures(conn_handler = list(), signature_name = "x",
                                    omic_signature = make_hyper_sig(), verbose = FALSE),
    "Supply either 'omic_signature' or 'signature_id'/'signature_name', not both", fixed = TRUE
  )
  # Empty ids/names are not a conflict.
  expect_no_error(
    SigRepo::prepareHypeRSignatures(signature_id = c("", NA), signature_name = character(),
                                    omic_signature = make_hyper_sig(), verbose = FALSE)
  )
})

# ---- Evaluation fixes (W1, W2, W5) ----

test_that("W1: direction = \"down\" ranks the negated scores; \"both\" builds one query per direction", {
  sig <- make_hyper_sig(difexp = hyper_difexp_table())

  down <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", direction = "down", verbose = FALSE)
  expect_equal(names(down$signatures), "sig_a")
  expect_equal(down$signatures[["sig_a"]], c(D = 3, C = 2, E = 1, B = -2, A = -3))
  expect_equal(down$info$direction, "down")

  both <- SigRepo::prepareHypeRSignatures(omic_signature = sig, test = "kstest", direction = "both", verbose = FALSE)
  expect_equal(names(both$signatures), c("sig_a | up", "sig_a | down"))
  expect_equal(both$signatures[["sig_a | up"]], c(A = 3, B = 2, E = -1, C = -2, D = -3))
  expect_equal(both$signatures[["sig_a | down"]], down$signatures[["sig_a"]])
  expect_equal(both$info$direction, c("up", "down"))
  expect_equal(both$info$query, names(both$signatures))

  hyper <- SigRepo::prepareHypeRSignatures(omic_signature = sig, verbose = FALSE)
  expect_true(all(is.na(hyper$info$direction)))
})

test_that("W5: ks_source = \"signature\" ranks the signature table and reports its own skip reasons", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), test = "kstest",
                                              ks_source = "signature", verbose = FALSE)
  expect_equal(prepared$signatures[["sig_a"]], c(A = 3, B = 2, C = -2, D = -3))
  expect_equal(prepared$info$symbol_source, "signature$symbol")

  no_score <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), test = "kstest",
                                              ks_source = "signature", score_col = "logFC", verbose = FALSE)
  expect_equal(no_score$skipped$reason, "missing_score_col")
  expect_match(no_score$skipped$message, "signature has no 'logFC' column", fixed = TRUE)
})

test_that("W1/W5: kstest-only arguments are rejected for hypergeometric", {
  expect_error(
    SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), direction = "both", verbose = FALSE),
    "'direction' and 'ks_source' only apply when test = \"kstest\"", fixed = TRUE
  )
  expect_error(
    SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), test = "kstest", direction = "sideways", verbose = FALSE),
    "should be one of"
  )
})

test_that("W2: query_names receives the info table and its names are validated and de-duplicated", {
  seen <- NULL
  prepared <- SigRepo::prepareHypeRSignatures(
    omic_signature = make_hyper_sig(),
    query_names = function(info) {
      seen <<- info
      rep("same", nrow(info))
    },
    verbose = FALSE
  )
  expect_equal(seen$query, c("sig_a | Old", "sig_a | Young"))
  expect_equal(names(prepared$signatures), c("same", "same (2)"))
  expect_equal(prepared$info$query, c("same", "same (2)"))

  for (bad in list(function(info) "one", function(info) c("a", NA), function(info) c("a", ""), function(info) 1:2)) {
    expect_error(
      SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), query_names = bad, verbose = FALSE),
      "'query_names' must return a character vector of 2 non-empty name(s)", fixed = TRUE
    )
  }
})

test_that("collectHypeRSignatures marks a single OmicSignature as a single input and a list as not", {
  expect_true(SigRepo:::collectHypeRSignatures(NULL, NULL, NULL, make_hyper_sig(), FALSE)$single)
  expect_false(SigRepo:::collectHypeRSignatures(NULL, NULL, NULL, list(make_hyper_sig()), FALSE)$single)
})

# ---- W4: hypeR-native input and getHypeRDifexp() ----

test_that("W4: a character vector is used as given, minus NA/empty symbols and repeats", {
  prepared <- SigRepo::prepareHypeRSignatures(signature = c("B", NA, " ", "A", "B"), verbose = FALSE)
  expect_equal(prepared$signatures, list(signature = c("B", "A")))
  expect_equal(prepared$info$n_dropped, 2L)
  expect_equal(prepared$info$symbol_source, "supplied")
  expect_true(is.na(prepared$info$signature_id))
})

test_that("W4: a named list gives one query per element; a weighted vector keeps its order", {
  prepared <- SigRepo::prepareHypeRSignatures(
    signature = list(up = c("A", "B"), ranked = c(C = -1, A = 2, B = NA)),
    test = "kstest", verbose = FALSE
  )
  expect_equal(names(prepared$signatures), c("up", "ranked"))
  expect_equal(prepared$signatures$ranked, c(C = -1, A = 2))
  expect_equal(prepared$info$n_dropped, c(0L, 1L))
})

test_that("W4: invalid hypeR-native input is rejected with a clear message", {
  prep <- function(...) SigRepo::prepareHypeRSignatures(verbose = FALSE, ...)

  expect_error(prep(signature = list(c("A", "B"))), "must be non-empty with a unique, non-empty name", fixed = TRUE)
  expect_error(prep(signature = list(a = "A", a = "B")), "must be non-empty with a unique, non-empty name", fixed = TRUE)
  expect_error(prep(signature = c(A = 1, B = 2)), "test = \"hypergeometric\" needs a character vector", fixed = TRUE)
  expect_error(prep(signature = c(1, 2), test = "kstest"), "must be a character vector of gene symbols or a named numeric", fixed = TRUE)
  expect_error(prep(signature = c(A = 1, A = 2), test = "kstest"), "has duplicated gene names", fixed = TRUE)
  expect_error(prep(signature = "A", omic_signature = make_hyper_sig()), "Supply either 'signature' or SigRepo signatures", fixed = TRUE)
  expect_error(prep(signature = "A", signature_id = 5), "Supply either 'signature' or SigRepo signatures", fixed = TRUE)
  expect_error(prep(signature = "A", test = "kstest", direction = "down"), "with 'signature', order the vector yourself", fixed = TRUE)
})

test_that("W4: an all-NA element is skipped, and query_names applies to native input", {
  prepared <- SigRepo::prepareHypeRSignatures(
    signature = list(a = c("A", "B"), empty = NA_character_),
    query_names = function(info) toupper(info$query), verbose = FALSE
  )
  expect_equal(names(prepared$signatures), "A")
  expect_equal(prepared$skipped$signature, "empty")
  expect_equal(prepared$skipped$reason, "empty_signature")
})

test_that("W4: getHypeRDifexp adds resolved symbols and leaves out signatures without difexp", {
  testthat::local_mocked_bindings(
    lookupReferenceSymbols = function(conn_handler, assay_type, organism, feature_names) c(f1 = "A", f2 = "B"),
    .package = "SigRepo"
  )
  difexp <- hyper_difexp_table()
  difexp$gene_symbol <- NULL
  with_difexp <- make_hyper_sig("with", difexp = difexp)

  expect_warning(
    tables <- SigRepo::getHypeRDifexp(omic_signature = list(with_difexp, make_hyper_sig("without")), verbose = FALSE),
    "No difexp table for 'without'; left out.", fixed = TRUE
  )
  expect_equal(names(tables), "with")
  tbl <- tables$with
  expect_equal(nrow(tbl), nrow(difexp))
  expect_equal(tbl$resolved_symbol[match(c("f1", "f2", "f3"), tbl$feature_name)], c("A", "B", NA))
  expect_true(all(tbl$resolved_symbol_source == "reference feature_name"))
})

# ---- Direction types and ranking checks ----

test_that("categorical hypergeometric splits each category by score sign; zero scores join neither", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_categorical_sig(), verbose = FALSE)

  expect_equal(names(prepared$signatures), c("cat | red | up", "cat | red | down", "cat | white | up", "cat | white | down"))
  expect_equal(unname(prepared$signatures), list("A", "B", c("D", "F"), "E"))
  expect_equal(prepared$info$group_label, c("red", "red", "white", "white"))
  expect_equal(prepared$info$direction, c("up", "down", "up", "down"))
  expect_false("C" %in% unlist(prepared$signatures))
})

test_that("categorical hypergeometric without scores splits by category only; split = FALSE gives one query", {
  sig <- make_hyper_categorical_sig()
  tbl <- sig$signature
  tbl$score <- NA_real_
  no_score <- list(metadata = sig$metadata, signature = tbl, difexp = sig$difexp)

  by_group <- SigRepo:::buildHypergeometricQueries(no_score, "cat", NA_character_, TRUE, SigRepo:::newHypeRSymbolResolver(NULL))
  expect_equal(names(by_group$queries), c("cat | red", "cat | white"))
  expect_true(all(is.na(by_group$info$direction)))

  one <- SigRepo::prepareHypeRSignatures(omic_signature = sig, split = FALSE, verbose = FALSE)
  expect_equal(one$signatures, list(cat = c("A", "B", "C", "D", "E", "F")))
})

test_that("bi-directional hypergeometric still splits by group_label only, not by sign", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(), verbose = FALSE)
  expect_equal(names(prepared$signatures), c("sig_a | Old", "sig_a | Young"))
})

test_that("categorical kstest ranks each category's own difexp rows, in both directions", {
  prepared <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_categorical_sig(), test = "kstest",
                                              direction = "both", verbose = FALSE)

  expect_equal(names(prepared$signatures), c("cat | red | up", "cat | red | down", "cat | white | up", "cat | white | down"))
  expect_equal(prepared$signatures[["cat | red | up"]], c(A = 2, C = 0.5, E = 0.2, F = 0.1, D = -0.3, B = -1))
  expect_equal(prepared$signatures[["cat | white | down"]], c(C = 2, A = 1, F = 0.5, B = -0.4, E = -1, D = -3))
  expect_equal(prepared$info$group_label, c("red", "red", "white", "white"))

  up_only <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_categorical_sig(), test = "kstest", verbose = FALSE)
  expect_equal(names(up_only$signatures), c("cat | red", "cat | white"))
})

test_that("a categorical category with unsigned scores is skipped; the other categories still run", {
  prepared <- SigRepo::prepareHypeRSignatures(
    omic_signature = make_hyper_categorical_sig(c(2, 1, 0.5, 0.3, 0.2, 0.1)), test = "kstest", verbose = FALSE
  )
  expect_equal(names(prepared$signatures), "cat | white")
  expect_equal(prepared$skipped$signature, "cat | red")
  expect_equal(prepared$skipped$reason, "unsigned_score")
})

test_that("a categorical difexp without group_label is skipped for kstest", {
  sig <- make_hyper_categorical_sig()
  difexp <- sig$difexp
  difexp$group_label <- NA
  bare <- list(metadata = sig$metadata, signature = sig$signature, difexp = difexp)
  built <- SigRepo:::buildKstestQueries(bare, "cat", NA_character_, "score", "up", "difexp", SigRepo:::newHypeRSymbolResolver(NULL))
  expect_equal(built$skip$reason, "no_group_label")
})

test_that("kstest skips a ranking with constant or one-signed scores", {
  unsigned <- hyper_difexp_table()
  unsigned$score <- abs(unsigned$score)
  skipped <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(difexp = unsigned), test = "kstest", verbose = FALSE)
  expect_equal(skipped$skipped$reason, "unsigned_score")
  expect_match(skipped$skipped$message, "all >= 0", fixed = TRUE)

  tbl <- hyper_sig_table()
  tbl$score <- 1
  constant <- SigRepo::prepareHypeRSignatures(omic_signature = make_hyper_sig(signature = tbl, direction_type = "uni-directional"),
                                              test = "kstest", ks_source = "signature", verbose = FALSE)
  expect_equal(constant$skipped$reason, "constant_score")
})
