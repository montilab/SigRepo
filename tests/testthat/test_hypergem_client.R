build_test_metabolomics_signature <- function() {
  metadata <- base::list(
    signature_name = "test_metabolomics_signature",
    assay_type = "metabolomics",
    phenotype = "test_phenotype",
    organism = "Homo sapiens",
    direction_type = "bi-directional",
    others = base::list(
      metabolomics_nomenclature = "refmet"
    )
  )

  signature_tbl <- base::data.frame(
    feature_name = c("glutamate", "pyruvate", "lactate", "citrate"),
    refmet_name = c("glutamate", "pyruvate", "lactate", "citrate"),
    hmdb_id = c("HMDB0000148", "HMDB0000243", "HMDB0000190", "HMDB0000094"),
    score = c(1.5, -2.1, 0.7, -0.3),
    group_label = base::factor(c("treated", "treated", "control", "control")),
    stringsAsFactors = FALSE
  )

  OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = signature_tbl,
    difexp = NULL
  )
}

test_that("prepareHyperGEMSignatures splits metabolomics signatures by group and direction", {
  omic_sig <- build_test_metabolomics_signature()

  prepared <- SigRepo:::prepareHyperGEMSignatures(
    omic_signature = omic_sig,
    verbose = FALSE
  )

  expect_true(methods::is(prepared, "list"))
  expect_true("signatures" %in% base::names(prepared))
  expect_true("metadata" %in% base::names(prepared))
  expect_equal(prepared$reference_key, "refmet_name")
  expect_equal(
    base::sort(base::names(prepared$signatures)),
    base::sort(c(
      "test_metabolomics_signature | control_dn",
      "test_metabolomics_signature | control_up",
      "test_metabolomics_signature | treated_dn",
      "test_metabolomics_signature | treated_up"
    ))
  )
  expect_true(methods::is(prepared$metadata, "data.frame"))
})

test_that("runHyperGEM rejects non-metabolomics signatures before calling hypeR.GEM", {
  transcript_sig <- OmicSignature::OmicSignature$new(
    metadata = base::list(
      signature_name = "test_transcriptomics_signature",
      assay_type = "transcriptomics",
      phenotype = "test_phenotype",
      organism = "Homo sapiens",
      direction_type = "bi-directional",
      others = base::list()
    ),
    signature = base::data.frame(
      probe_id = c("probe_1", "probe_2"),
      feature_name = c("TP53", "CDKN1A"),
      score = c(1, -1),
      group_label = base::factor(c("up", "dn")),
      stringsAsFactors = FALSE
    ),
    difexp = NULL
  )

  expect_error(
    SigRepo:::prepareHyperGEMSignatures(
      omic_signature = transcript_sig,
      verbose = FALSE
    ),
    "HyperGEM requires metabolomics signatures"
  )
})

test_that("runHyperGEM requires hypeR.GEM to be installed", {
  omic_sig <- build_test_metabolomics_signature()
  genesets <- base::list(pathway_a = c("G1", "G2"))

  if (!SigRepo:::hypergemAvailable()) {
    expect_error(
      SigRepo::runHyperGEM(
        omic_signature = omic_sig,
        genesets = genesets,
        verbose = FALSE
      ),
      "Package 'hypeR.GEM' is required"
    )
  } else {
    testthat::skip("hypeR.GEM is installed; the missing-package branch is not exercised.")
  }
})
