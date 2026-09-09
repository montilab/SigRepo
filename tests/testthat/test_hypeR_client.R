test_that("prepareHypeRSignatures builds hypergeometric query vectors", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  prepared <- SigRepo::prepareHypeRSignatures(
    omic_signature = LLFS_Aging_Gene_2023,
    method = "hypergeo",
    verbose = FALSE
  )

  expect_true(methods::is(prepared, "list"))
  expect_true("signatures" %in% base::names(prepared))
  expect_true("metadata" %in% base::names(prepared))
  expect_equal(base::length(prepared$signatures), 1)
  expect_true(base::length(prepared$signatures[[1]]) > 0)
  expect_true(methods::is(prepared$metadata, "data.frame"))
})

test_that("prepareHypeRSignatures builds ranked query vectors for GSEA-style enrichment", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  prepared <- SigRepo::prepareHypeRSignatures(
    omic_signature = LLFS_Aging_Gene_2023,
    method = "gsea",
    verbose = FALSE
  )

  expect_equal(base::length(prepared$signatures), 1)
  expect_true(is.numeric(prepared$signatures[[1]]))
  expect_true(!is.null(base::names(prepared$signatures[[1]])))
  expect_true(base::length(prepared$signatures[[1]]) > 0)
})

test_that("runHypeR runs hypergeometric enrichment on an OmicSignature object", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  feature_hits <- base::unique(utils::head(LLFS_Aging_Gene_2023$signature$feature_name, 10))
  genesets <- base::list(
    hit_set = feature_hits,
    miss_set = base::paste0("missing_", base::seq_len(10))
  )

  hyp_res <- SigRepo::runHypeR(
    omic_signature = LLFS_Aging_Gene_2023,
    genesets = genesets,
    method = "hypergeo",
    plotting = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_true(methods::is(hyp_res, "list"))
  expect_true("result" %in% base::names(hyp_res))
  expect_s3_class(hyp_res$result, "R6")
  expect_true("hyp" %in% class(hyp_res$result))
})

test_that("runHypeR passes additional hypeR arguments through dots", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  feature_hits <- base::unique(utils::head(LLFS_Aging_Gene_2023$signature$feature_name, 10))
  genesets <- base::list(
    hit_set = feature_hits,
    miss_set = base::paste0("missing_", base::seq_len(10))
  )

  hyp_res <- SigRepo::runHypeR(
    omic_signature = LLFS_Aging_Gene_2023,
    genesets = genesets,
    method = "hypergeo",
    plotting = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    background = 12345
  )

  expect_equal(hyp_res$result$args$background, 12345)
})

test_that("runHypeR runs GSEA-style enrichment on an OmicSignature object", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  feature_hits <- base::unique(utils::head(LLFS_Aging_Gene_2023$difexp$feature_name, 10))
  genesets <- base::list(
    hit_set = feature_hits,
    miss_set = base::paste0("missing_", base::seq_len(10))
  )

  hyp_res <- base::suppressWarnings(
    SigRepo::runHypeR(
      omic_signature = LLFS_Aging_Gene_2023,
      genesets = genesets,
      method = "gsea",
      plotting = FALSE,
      quiet = TRUE,
      verbose = FALSE
    )
  )

  expect_true("hyp" %in% class(hyp_res$result))
})

test_that("runHypeR handles multiple signatures and returns multihyp", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  feature_hits <- base::unique(utils::head(LLFS_Aging_Gene_2023$signature$feature_name, 10))
  genesets <- base::list(
    hit_set = feature_hits,
    miss_set = base::paste0("missing_", base::seq_len(10))
  )

  sig_list <- base::list(
    first_sig = LLFS_Aging_Gene_2023,
    second_sig = LLFS_Aging_Gene_2023
  )

  hyp_res <- SigRepo::runHypeR(
    omic_signature = sig_list,
    genesets = genesets,
    method = "hypergeo",
    plotting = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_true("multihyp" %in% class(hyp_res$result))
  expect_equal(base::sort(base::names(hyp_res$signatures)), c("first_sig", "second_sig"))
  expect_equal(base::sort(base::unique(hyp_res$metadata$signature_name)), c("first_sig", "second_sig"))
})

test_that("runHypeR accepts hypeR gsets objects directly", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  feature_hits <- base::unique(utils::head(LLFS_Aging_Gene_2023$signature$feature_name, 10))
  genesets <- hypeR::gsets$new(
    genesets = base::list(
      hit_set = feature_hits,
      miss_set = base::paste0("missing_", base::seq_len(10))
    ),
    name = "test_sets",
    version = "v1",
    quiet = TRUE
  )

  hyp_res <- SigRepo::runHypeR(
    omic_signature = LLFS_Aging_Gene_2023,
    genesets = genesets,
    method = "hypergeo",
    plotting = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_true("hyp" %in% class(hyp_res$result))
})

test_that("runHypeR can retrieve MSigDB genesets automatically", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("msigdbr")

  # This is the one test in the suite that needs MSigDB itself; every other
  # hypeR test builds its gene sets inline. msigdbr 26.x stopped bundling the
  # data and downloads it from zenodo.org on first use, so the test now depends
  # on a third-party service being reachable.
  #
  # Probe that service through the exact call resolveHypeRGenesets() makes, and
  # skip with the reason when it is down: an outage there is an environment
  # failure, not a regression in runHypeR, and it should not turn the suite red
  # for everyone. The probe also warms msigdbr's cache, so the call below does
  # not download twice. A probe that SUCCEEDS leaves both assertions in force.
  probe <- base::tryCatch(
    {
      base::suppressWarnings(
        hypeR::msigdb_gsets(species = "Homo sapiens", collection = "H")
      )
      TRUE
    },
    error = function(err) base::conditionMessage(err)
  )
  testthat::skip_if_not(
    base::isTRUE(probe),
    base::paste("MSigDB gene sets could not be downloaded:", probe)
  )

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  hyp_res <- base::suppressWarnings(
    SigRepo::runHypeR(
      omic_signature = LLFS_Aging_Gene_2023,
      msigdb_collection = "H",
      method = "hypergeo",
      plotting = FALSE,
      quiet = TRUE,
      verbose = FALSE
    )
  )

  expect_true("hyp" %in% class(hyp_res$result))
  expect_true(base::grepl("H", hyp_res$result$info[["Genesets"]], fixed = TRUE))
})
