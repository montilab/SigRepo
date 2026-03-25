# testing scripts for metabolomics helper functions in the SigRepo package

test_that("metabolomics helper resolves configured dictionaries", {
  helper_env <- new.env(parent = baseenv())
  sys.source(testthat::test_path("..", "..", "R", "metabolomicsHelpers.R"), envir = helper_env)
  
  hmdb_cfg <- helper_env$resolveMetabolomicsFeatureConfig(feature_database = "HMDB")
  expect_identical(hmdb_cfg$feature_database, "hmdb")
  expect_identical(hmdb_cfg$db_table_name, "hmdb_features")
  expect_identical(hmdb_cfg$maintenance_model, "curated")
  
  smiles_cfg <- helper_env$resolveMetabolomicsFeatureConfig(feature_database = "smiles")
  expect_identical(smiles_cfg$feature_database, "smiles")
  expect_identical(smiles_cfg$maintenance_model, "growing")
})

test_that("metabolomics nomenclature is persisted in others", {
  helper_env <- new.env(parent = baseenv())
  sys.source(testthat::test_path("..", "..", "R", "metabolomicsHelpers.R"), envir = helper_env)
  
  metadata <- helper_env$addMetabolomicsNomenclature(
    list(
      assay_type = "metabolomics",
      others = list(existing = "value")
    ),
    metabolomics_nomenclature = "refmet"
  )
  
  expect_identical(metadata$others$metabolomics_nomenclature, "refmet")
  expect_identical(metadata$others$existing, "value")
})
