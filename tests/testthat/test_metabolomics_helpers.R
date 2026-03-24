# testing scripts for metabolomics helper functions in the SigRepo package

test_that("metabolomics helper resolves configured dictionaries", {
  helper_env <- new.env(parent = baseenv())
  sys.source(testthat::test_path("..", "..", "R", "metabolomicsHelpers.R"), envir = helper_env)
  
  hmdb_cfg <- helper_env$resolveMetabolomicsFeatureConfig(feature_database = "HMDB")
  expect_identical(hmdb_cfg$feature_database, "hmdb")
  expect_identical(hmdb_cfg$db_table_name, "hmdb_features")
  expect_identical(hmdb_cfg$maintenance_model, "curated")
  
  smiles_cfg <- helper_env$resolveMetabolomicsFeatureConfig(
    metadata = list(platform = "Untargeted SMILES panel")
  )
  expect_identical(smiles_cfg$feature_database, "smiles")
  expect_identical(smiles_cfg$maintenance_model, "growing")
})

test_that("metabolomics metadata normalization persists dictionary in others", {
  helper_env <- new.env(parent = baseenv())
  sys.source(testthat::test_path("..", "..", "R", "metabolomicsHelpers.R"), envir = helper_env)
  
  metadata <- helper_env$normalizeMetabolomicsMetadata(
    list(
      assay_type = "metabolomics",
      metabolomics_database = "refmet",
      others = list(existing = "value")
    )
  )
  
  expect_identical(metadata$metabolomics_database, "refmet")
  expect_identical(metadata$others$metabolomics_database, "refmet")
  expect_identical(metadata$others$existing, "value")
})
