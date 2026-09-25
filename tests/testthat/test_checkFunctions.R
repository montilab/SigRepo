# Offline checks for checkTableInput() and checkOmicSignature().

test_that("checkTableInput replaces every empty value with the NULL placeholder, not just the first (#208)", {
  testthat::local_mocked_bindings(getDBColNames = function(...) c("probe_id", "group_label", "score"))

  table <- base::data.frame(
    probe_id = c("p1", "p2", "p3"),
    group_label = c("", "", ""),
    score = c("NA", "NULL", NA),
    stringsAsFactors = FALSE
  )

  checked <- SigRepo::checkTableInput(conn = NULL, db_table_name = "signature_feature_set", table = table, check_db_table = FALSE)

  expect_equal(checked$group_label, base::rep("'NULL'", 3))
  expect_equal(checked$score, base::rep("'NULL'", 3))
  expect_equal(checked$probe_id, c("p1", "p2", "p3"))
})

test_that("checkOmicSignature fills a missing uni-directional group_label with 'All Features' (#208)", {
  fixture <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))

  metadata <- fixture$metadata
  metadata$direction_type <- "uni-directional"
  metadata$PMID <- base::as.character(metadata$PMID)
  metadata$year <- base::as.character(metadata$year)
  difexp <- fixture$difexp[, c("probe_id", "feature_name", "score", "p_value", "adj_p")]
  signature <- difexp[1:5, c("probe_id", "feature_name", "score")]

  sig <- OmicSignature::OmicSignature$new(metadata = metadata, signature = signature, difexp = difexp, print_message = FALSE)
  checked <- SigRepo::checkOmicSignature(omic_signature = sig)

  expect_true(base::all(checked$signature$group_label == "All Features"))
  expect_true(base::all(checked$difexp$group_label == "All Features"))
})

test_that("checkSignatureType() accepts valid types and rejects others", {
  expect_silent(checkSignatureType("uni-directional"))
  expect_silent(checkSignatureType("BI-DIRECTIONAL"))
  expect_error(checkSignatureType("sideways"), "'type' must be one of the following options")
})

test_that("signature_types exposes the three supported values", {
  expect_setequal(SigRepo::signature_types, c("uni-directional", "bi-directional", "categorical"))
})

test_that("checkOmicSignature() rejects an object built by a pre-rename OmicSignature", {
  ## Simulates an object deserialized from an RDS written before the metadata
  ## field was renamed. It never passes through OmicSignature's normalizer, so
  ## metadata$type is NULL and every downstream read would silently misbehave.
  stale <- structure(
    list(metadata = list(signature_name = "stale", direction_type = "uni-directional")),
    class = c("OmicSignature", "R6")
  )
  expect_error(
    checkOmicSignature(stale),
    "reinstall OmicSignature"
  )
})
