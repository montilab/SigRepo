test_that("prepareHypeRSignatures keeps group labels but does not split by direction", {
  testthat::skip_if_not_installed("hypeR")

  sig <- OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = "group_only_signature",
      assay_type = "transcriptomics",
      phenotype = "test_phenotype",
      organism = "Homo sapiens",
      direction_type = "bi-directional",
      others = list()
    ),
    signature = data.frame(
      feature_name = c("GENE_A", "GENE_B", "GENE_C", "GENE_D"),
      score = c(2, -1.5, 3, -0.8),
      group_label = factor(c("treated", "treated", "control", "control")),
      stringsAsFactors = FALSE
    ),
    difexp = NULL
  )

  prepared <- SigRepo::prepareHypeRSignatures(
    omic_signature = sig,
    method = "hypergeo",
    split_by_group = TRUE,
    split_by_direction = TRUE,
    verbose = FALSE
  )

  expect_equal(
    sort(names(prepared$signatures)),
    sort(c("group_only_signature | control", "group_only_signature | treated"))
  )
  expect_false(any(grepl("_up|_dn", names(prepared$signatures), perl = TRUE)))
})

test_that("prepareHypeRSignatures falls back to reference gene symbols and warns when rows are dropped", {
  testthat::skip_if_not_installed("hypeR")

  sig <- OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = "mapped_symbol_signature",
      assay_type = "transcriptomics",
      phenotype = "test_phenotype",
      organism = "Homo sapiens",
      direction_type = "bi-directional",
      others = list()
    ),
    signature = data.frame(
      feature_name = c("feat_1", "feat_2", "feat_3", "feat_4"),
      feature_id = c(101, 102, 103, 104),
      group_label = factor(c("treated", "treated", "control", "control")),
      stringsAsFactors = FALSE
    ),
    difexp = data.frame(
      feature_name = c("feat_1", "feat_2", "feat_3", "feat_4"),
      score = c(2.2, -1.8, 2.5, -0.9),
      p_value = c(0.01, 0.02, 0.03, 0.04),
      group_label = factor(c("treated", "treated", "control", "control")),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    conn_init = function(conn_handler) list(),
    lookup_table_sql = function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table = TRUE, ...) {
      expect_equal(db_table_name, "transcriptomics_features")
      expect_equal(return_var, c("feature_name", "gene_symbol"))
      data.frame(
        feature_name = c("feat_1", "feat_2", "feat_3"),
        gene_symbol = c("GENE_A", "GENE_B", NA_character_),
        stringsAsFactors = FALSE
      )
    },
    .package = "SigRepo"
  )

  expect_warning(
    prepared <- SigRepo::prepareHypeRSignatures(
      conn_handler = list(),
      omic_signature = sig,
      method = "gsea",
      split_by_group = TRUE,
      verbose = FALSE
    ),
    "dropped"
  )

  expect_true("GENE_A" %in% names(prepared$signatures[[1]]))
  expect_true("GENE_B" %in% names(prepared$signatures[[1]]))
  expect_false("feat_3" %in% names(prepared$signatures[[1]]))
})

test_that("prepareHypeRSignatures resolves feature_name to symbol when feature_col is symbol", {
  testthat::skip_if_not_installed("hypeR")

  sig <- OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = "symbol_targeted_signature",
      assay_type = "transcriptomics",
      phenotype = "test_phenotype",
      organism = "Homo sapiens",
      direction_type = "bi-directional",
      others = list()
    ),
    signature = data.frame(
      feature_name = c("feat_1", "feat_2", "feat_3"),
      group_label = factor(c("treated", "treated", "control")),
      stringsAsFactors = FALSE
    ),
    difexp = NULL
  )

  testthat::local_mocked_bindings(
    conn_init = function(conn_handler) list(),
    lookup_table_sql = function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table = TRUE, ...) {
      expect_equal(db_table_name, "transcriptomics_features")
      expect_equal(return_var, c("feature_name", "gene_symbol"))
      data.frame(
        feature_name = c("feat_1", "feat_2", "feat_3"),
        gene_symbol = c("GENE_A", "GENE_B", "GENE_C"),
        stringsAsFactors = FALSE
      )
    },
    .package = "SigRepo"
  )

  prepared <- SigRepo::prepareHypeRSignatures(
    conn_handler = list(),
    omic_signature = sig,
    method = "hypergeo",
    feature_col = "symbol",
    split_by_group = TRUE,
    verbose = FALSE
  )

  expect_true("GENE_A" %in% unlist(prepared$signatures))
  expect_true("GENE_B" %in% unlist(prepared$signatures))
})

test_that("prepareHypeRSignatures accepts hypergeometric and ks aliases", {
  testthat::skip_if_not_installed("hypeR")

  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = environment())

  hyper <- SigRepo::prepareHypeRSignatures(
    omic_signature = LLFS_Aging_Gene_2023,
    method = "hypergeometric",
    verbose = FALSE
  )

  ks <- SigRepo::prepareHypeRSignatures(
    omic_signature = LLFS_Aging_Gene_2023,
    method = "ks",
    verbose = FALSE
  )

  expect_true(methods::is(hyper, "list"))
  expect_true(methods::is(ks, "list"))
  expect_true(base::length(hyper$signatures) > 0)
  expect_true(base::length(ks$signatures) > 0)
})

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

test_that("resolveHypeRGenesets falls back to direct msigdbr for mouse C2 collections", {
  testthat::skip_if_not_installed("msigdbr")

  gsets <- SigRepo::resolveHypeRGenesets(
    msigdb_species = "Mus musculus",
    msigdb_collection = "C2",
    msigdb_subcollection = "CP:KEGG_LEGACY"
  )

  expect_true(methods::is(gsets, "list"))
  expect_true(length(gsets) > 0)
  expect_true(any(grepl("KEGG", names(gsets), fixed = TRUE)))
})

test_that("runHypeR can retrieve MSigDB genesets automatically", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("msigdbr")

  # This is the one test in the suite that needs MSigDB itself; every other
  # hypeR test builds its gene sets inline. msigdbr 26.x stopped bundling the
  # data and downloads it from zenodo.org on first use, so the test now depends
  # on a third-party service being reachable.
  #
  # Probe that service through the direct msigdbr fetch used by SigRepo, and skip
  # with the reason when it is down: an outage there is an environment failure,
  # not a regression in runHypeR, and it should not turn the suite red for
  # everyone. The probe also warms msigdbr's cache, so the call below does not
  # download twice. A probe that SUCCEEDS leaves both assertions in force.
  probe <- base::tryCatch(
    {
      base::suppressWarnings(
        SigRepo:::fetchMsigdbGenesets(species = "Homo sapiens", collection = "H")
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
  expect_true("Genesets" %in% names(hyp_res$result$info))
  expect_true(!base::is.null(hyp_res$result$info[["Genesets"]]))
})
