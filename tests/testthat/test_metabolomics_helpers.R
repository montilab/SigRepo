test_that("normalize and build metabolomics rows support the reduced schema", {
  feature_tbl <- base::data.frame(
    refmet_id = "RM0001",
    refmet_name = "L-glutamate",
    hmdb_id = "HMDB0000148",
    smiles = "placeholder_smiles",
    inchi_key = "WHUUTDBJXJRKMK-VKHMYHEASA-N",
    is_current = 1,
    version = 20260428,
    stringsAsFactors = FALSE
  )

  normalized_tbl <- SigRepo:::normalizeMetabolomicsFeatureSet(
    feature_set = feature_tbl,
    feature_database = "hmdb"
  )

  expect_equal(normalized_tbl$feature_name, "HMDB0000148")
  expect_equal(normalized_tbl$refmet_name, "L-glutamate")
  expect_equal(normalized_tbl$smiles, "placeholder_smiles")

  reference_tbl <- SigRepo:::buildMetabolomicsReferenceRows(
    feature_set = feature_tbl,
    feature_database = "hmdb"
  )

  expect_true(all(c(
    "refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey", "is_current", "version"
  ) %in% base::colnames(reference_tbl)))
  expect_equal(reference_tbl$hmdb_id, "HMDB0000148")
})

test_that("metabolomics xref rows include all supported identifier mappings", {
  feature_tbl <- base::data.frame(
    refmet_id = "RM0001",
    refmet_name = "L-glutamate",
    hmdb_id = "HMDB0000148",
    smiles = "placeholder_smiles",
    inchikey = "WHUUTDBJXJRKMK-VKHMYHEASA-N",
    is_current = 1,
    version = 20260428,
    stringsAsFactors = FALSE
  )

  xref_tbl <- SigRepo:::buildMetabolomicsXrefRows(
    feature_set = feature_tbl,
    feature_database = "refmet"
  )

  expect_true(all(c(
    "refmet_id", "refmet_name", "hmdb_id", "smiles", "inchikey", "source_db", "source_value"
  ) %in% base::colnames(xref_tbl)))
  expect_true(all(c("refmet_id", "refmet", "hmdb", "smiles", "inchikey") %in% xref_tbl$source_db))
})

test_that("metabolomics feature database can be inferred from uploaded columns", {
  feature_tbl <- base::data.frame(
    refmet_id = "RM0001",
    refmet_name = "L-glutamate",
    hmdb_id = "HMDB0000148",
    is_current = 1,
    version = 20260428,
    stringsAsFactors = FALSE
  )

  expect_equal(
    SigRepo:::inferMetabolomicsFeatureDatabase(feature_tbl),
    "refmet_id"
  )

  feature_tbl_no_refmet_id <- feature_tbl |>
    dplyr::select(-"refmet_id")

  expect_equal(
    SigRepo:::inferMetabolomicsFeatureDatabase(feature_tbl_no_refmet_id),
    "refmet"
  )
})

test_that("refmet signature lookup prefers refmet_id when available", {
  signature_tbl <- base::data.frame(
    feature_name = "LPC 15:0/0:0*",
    refmet_id = "RM1234",
    probe_id = 1,
    score = 1.5,
    group_label = "group_a",
    stringsAsFactors = FALSE
  )

  expect_equal(
    SigRepo:::getMetabolomicsSignatureLookupValues(
      signature_set = signature_tbl,
      feature_database = "refmet"
    ),
    "RM1234"
  )

  expect_equal(
    SigRepo:::getMetabolomicsSignatureLookupValues(
      signature_set = signature_tbl,
      feature_database = "hmdb"
    ),
    "LPC 15:0/0:0*"
  )
})

test_that("refmet signature lookup treats RM-style feature names as refmet ids", {
  signature_tbl <- base::data.frame(
    feature_name = c("RM0159895", "RM0108718"),
    probe_id = c(1, 2),
    score = c(1.2, -0.5),
    group_label = c("group_a", "group_b"),
    stringsAsFactors = FALSE
  )

  lookup_spec <- SigRepo:::resolveMetabolomicsSignatureLookupSpec(
    signature_set = signature_tbl,
    feature_database = "refmet"
  )

  expect_equal(lookup_spec$feature_database, "refmet_id")
  expect_equal(lookup_spec$feature_values, c("RM0159895", "RM0108718"))
})
