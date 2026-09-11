# Live round-trip for compareSignatures(): signatures are uploaded, fetched
# back through the database and the difexp API, compared, and removed again.
# The wrapper's own input handling is covered offline in
# test_compareSignatures.R; this file only proves the database path works
# end to end against SigRepo::test_conn_handler.

test_that("compareSignatures compares signatures fetched from the database", {
  test_conn <- SigRepo::test_conn_handler

  fixture <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))

  # Two uploads of the fixture under distinct names, so the comparison has a
  # pair of signatures the database resolves independently. ####
  make_named_copy <- function(new_name){
    sig <- fixture$clone(deep = TRUE)
    new_metadata <- sig$metadata
    new_metadata$signature_name <- new_name
    sig$metadata <- new_metadata
    sig
  }

  # addSignature() returns NULL (with a message, not an error) when the
  # database lacks the fixture's reference features, so guard each upload:
  # without both signatures there is nothing meaningful to compare. ####
  upload <- function(new_name){
    id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = make_named_copy(new_name),
      return_signature_id = TRUE,
      verbose = FALSE
    )
    if (base::is.null(id) || base::length(id) != 1) {
      testthat::skip("the test database could not accept the transcriptomics fixture (reference features missing)")
    }
    id
  }

  id_a <- upload("test_signature_compare_a")
  on.exit(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = id_a, verbose = FALSE), add = TRUE)

  id_b <- upload("test_signature_compare_b")
  on.exit(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = id_b, verbose = FALSE), add = TRUE)

  # Self-comparison by id ####
  res <- SigRepo::compareSignatures(
    conn_handler = test_conn,
    signature_ids = c(id_a, id_b),
    method = "overlap",
    min_features = 3,
    max_feature = 10,
    verbose = FALSE
  )

  expect_equal(res$method, "overlap")
  expect_named(res$comparisons, c("level1_vs_level1", "level2_vs_level2"))
  jaccard <- res$comparisons$level1_vs_level1$jaccard
  expect_equal(base::sort(base::rownames(jaccard)), c("test_signature_compare_a", "test_signature_compare_b"))
  expect_equal(base::rownames(jaccard), base::colnames(jaccard))
  expect_equal(base::unname(base::diag(jaccard)), c(1, 1))

  # Query-vs-reference by name on one side and id on the other ####
  res2 <- SigRepo::compareSignatures(
    conn_handler = test_conn,
    signature_names = "test_signature_compare_a",
    signature_ids2 = id_b,
    method = "overlap",
    min_features = 3,
    max_feature = 10,
    verbose = FALSE
  )

  jaccard2 <- res2$comparisons$level1_vs_level1$jaccard
  expect_equal(base::dim(jaccard2), c(1L, 1L))
  expect_equal(base::rownames(jaccard2), "test_signature_compare_a")
  expect_equal(base::colnames(jaccard2), "test_signature_compare_b")
  expect_equal(base::unname(jaccard2[1, 1]), 1)

  # An id that does not exist is reported, and the comparison still runs on
  # the signatures that do. ####
  expect_warning(
    res3 <- SigRepo::compareSignatures(
      conn_handler = test_conn,
      signature_ids = c(id_a, id_b, 0),
      method = "overlap",
      min_features = 3,
      max_feature = 10,
      verbose = FALSE
    ),
    "0"
  )
  expect_equal(base::dim(res3$comparisons$level1_vs_level1$jaccard), c(2L, 2L))
})
