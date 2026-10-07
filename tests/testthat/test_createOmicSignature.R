# Offline checks for the helpers createOmicSignature() uses to rebuild a
# signature fetched from the database.

test_that("alignSignatureFeatureNames takes the difexp spelling when names differ only in case (#209)", {
  signature <- base::data.frame(
    probe_id = c("p1", "p2", "p3"),
    feature_name = c("ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003"),
    stringsAsFactors = FALSE
  )
  difexp <- base::data.frame(
    probe_id = c("p3", "p2", "p1", "p4"),
    feature_name = c("ENSMUSG00000000003", "ensmusg00000000002", "ensmusg00000000001", "ensmusg00000000004"),
    stringsAsFactors = FALSE
  )

  aligned <- SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = difexp)

  expect_equal(aligned$feature_name, c("ensmusg00000000001", "ensmusg00000000002", "ENSMUSG00000000003"))
})

test_that("alignSignatureFeatureNames leaves names that differ by more than case alone", {
  signature <- base::data.frame(probe_id = c("p1", "p2"), feature_name = c("GeneA", NA), stringsAsFactors = FALSE)
  difexp <- base::data.frame(probe_id = c("p1", "p2"), feature_name = c("GeneB", "geneb"), stringsAsFactors = FALSE)

  aligned <- SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = difexp)
  expect_equal(aligned$feature_name, c("GeneA", NA))

  expect_identical(SigRepo:::alignSignatureFeatureNames(signature = signature, difexp = NULL), signature)
})

# getSignature() admits viewers, but createOmicSignature() used to re-check the
# caller as an editor with INSERT, so a viewer account (production guest) found
# the signature row and then failed inside the loop (#260). The mocked
# checkPermissions() behaves like the real one does for a viewer: anything
# beyond SELECT as a viewer is refused.
test_that("createOmicSignature only asks for SELECT as a viewer, so a viewer can rebuild a signature (#260)", {
  permission_requests <- base::list()

  testthat::local_mocked_bindings(
    conn_init = function(conn_handler = NULL) "mock-conn",
    conn_close = function(conn) base::invisible(TRUE),
    checkPermissions = function(conn, action_type, required_role){
      permission_requests[[base::length(permission_requests) + 1]] <<- base::list(action_type = action_type, required_role = required_role)
      if (!base::all(action_type == "SELECT") || required_role != "viewer") {
        base::stop("User = 'viewer' does not have permission to perform this action in the database.\n")
      }
      base::data.frame(user = "viewer", user_role = "viewer", api_key = "viewer-key", active = 1, stringsAsFactors = FALSE)
    },
    lookup_table_sql = function(conn, db_table_name, ...){
      if (db_table_name == "signature_feature_set") {
        base::data.frame(
          signature_id = 1L, feature_id = c(10L, 11L), probe_id = c("p1", "p2"),
          score = c(2.5, -1.5), group_label = c("Up", "Dn"), stringsAsFactors = FALSE
        )
      } else if (db_table_name == "transcriptomics_features") {
        base::data.frame(feature_id = c(10L, 11L), feature_name = c("GeneA", "GeneB"), stringsAsFactors = FALSE)
      } else {
        base::stop("unexpected lookup of ", db_table_name)
      }
    },
    .package = "SigRepo"
  )
  testthat::local_mocked_bindings(dbDisconnect = function(conn, ...) TRUE, .package = "DBI")

  db_signature_tbl <- base::data.frame(
    signature_id = 1L, signature_name = "viewer_sig", organism = "Homo sapiens", type = "bi-directional",
    assay_type = "transcriptomics", phenotype = "test", platform = NA, sample_type = NA, covariates = NA,
    description = NA, score_cutoff = NA, logfc_cutoff = NA, p_value_cutoff = NA, adj_p_cutoff = NA,
    cutoff_description = NA, keywords = NA, PMID = NA, year = NA, others = NA, has_difexp = FALSE,
    signature_hashkey = "abc", stringsAsFactors = FALSE
  )

  oms <- NULL
  base::invisible(utils::capture.output(
    oms <- base::suppressWarnings(SigRepo:::createOmicSignature(conn_handler = NULL, db_signature_tbl = db_signature_tbl))
  ))

  expect_s3_class(oms, "OmicSignature")
  expect_setequal(oms$signature$feature_name, c("GeneA", "GeneB"))
  expect_length(permission_requests, 1)
  expect_equal(permission_requests[[1]], base::list(action_type = "SELECT", required_role = "viewer"))
})
