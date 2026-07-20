# testing script for signature functions in the SigRepo package
# addSignature
# searchSignature

test_that("addSignature correctly adds a signature into the database", {
  test_conn <- SigRepo::test_conn_handler
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  expect_true(base::length(omic_signature_id) == 1)
  expect_type(omic_signature_id, "double") 
  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
  
})

test_that("searchSignature correctly searches for the desired signature", {
  test_conn <- SigRepo::test_conn_handler
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  
  signature_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_id = omic_signature_id,
    verbose = FALSE
  )
  #print(str(signature_search))
  
  expect_true(methods::is(signature_search, "data.frame"))
  expect_true(nrow(signature_search) > 0)
  expect_equal(signature_search$signature_name[1], "test_signature")
  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
  
  
})

test_that("getSignature returns the user-facing OmicSignature shape", {
  test_conn <- SigRepo::test_conn_handler

  expect_no_error({
    test_transcriptomics_sig <- base::readRDS(
      testthat::test_path("test_data", "test_data_transcriptomics.rds")
    )
  })

  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })

  expect_no_error({
    retrieved_signature <- SigRepo::getSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )[[1]]
  })

  internal_metadata_fields <- c(
    "signature_id", "signature_hashkey", "organism_id", "phenotype_id",
    "platform_id", "sample_type_id", "user_name", "visibility",
    "date_created", "has_difexp", "num_of_difexp", "num_up_regulated",
    "num_down_regulated", "platform_name"
  )
  expect_false(any(internal_metadata_fields %in% base::names(retrieved_signature$metadata)))
  expect_equal(
    base::sort(base::names(retrieved_signature$metadata)),
    base::sort(base::names(test_transcriptomics_sig$metadata))
  )
  expect_equal(retrieved_signature$metadata$covariates, "none")
  expect_equal(retrieved_signature$metadata$keywords, "Myc,KO,longevity")
  expect_equal(retrieved_signature$metadata$PMID, "25619689")
  expect_equal(retrieved_signature$metadata$year, "2015")

  internal_signature_fields <- c(
    "signature_id", "feature_id", "assay_type", "nomenclature_type",
    "match_status", "feature_hashkey", "sig_feature_hashkey"
  )
  expect_false(any(internal_signature_fields %in% base::colnames(retrieved_signature$signature)))
  expect_equal(
    base::colnames(retrieved_signature$signature),
    base::colnames(test_transcriptomics_sig$signature)
  )

  expected_signature_tbl <- test_transcriptomics_sig$signature |>
    dplyr::arrange(.data$probe_id) |>
    dplyr::mutate(group_label = base::as.character(.data$group_label))

  retrieved_signature_tbl <- retrieved_signature$signature |>
    dplyr::arrange(.data$probe_id) |>
    dplyr::mutate(group_label = base::as.character(.data$group_label))

  metadata_fields_to_match <- base::setdiff(
    base::names(test_transcriptomics_sig$metadata),
    c("covariates", "keywords", "PMID", "year")
  )
  purrr::walk(
    metadata_fields_to_match,
    function(field_name){
      expect_equal(
        retrieved_signature$metadata[[field_name]],
        test_transcriptomics_sig$metadata[[field_name]]
      )
    }
  )
  expect_equal(retrieved_signature_tbl, expected_signature_tbl)

  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
})

test_that("searchSignature returns all signatures when no filters provided", {
  test_conn <- SigRepo::test_conn_handler
  
  all_signatures <- SigRepo::searchSignature(
    conn_handler = test_conn,
    verbose = FALSE
  )
  
  #print("nrow(all_signatures)")
  
  expect_true(methods::is(all_signatures, "data.frame"))
  expect_true(nrow(all_signatures) >= 0)
})

test_that("searchSignature filters by organism", {
  test_conn <- SigRepo::test_conn_handler

  # These filter tests used to rely on a signature already sitting in
  # whatever database test_conn_handler pointed at (e.g. leftover data on a
  # long-lived shared instance). That's not true for a freshly initialized
  # database (nothing but schema + reference tables), so each test now
  # uploads and cleans up its own matching signature instead of assuming
  # one exists. Filtering on the fixture's own organism/phenotype/platform
  # (Mus musculus / Myc_reduce / transcriptomics by array) rather than
  # substituting different values, since the signature's feature IDs are
  # organism-specific (ENSMUSG*) and swapping just the metadata organism
  # would desync it from those feature IDs. ####
  test_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))$clone(deep = TRUE)
  new_metadata <- test_sig$metadata
  new_metadata$signature_name <- "test_signature_organism_filter"
  test_sig$metadata <- new_metadata

  sig_id <- SigRepo::addSignature(conn_handler = test_conn, omic_signature = test_sig, return_signature_id = TRUE, verbose = FALSE)
  on.exit(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = sig_id, verbose = FALSE), add = TRUE)

  organism_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Mus musculus",
    verbose = FALSE
  )

  #print(head(organism_search))

  expect_true(methods::is(organism_search, "data.frame"))
  expect_true(nrow(organism_search) > 0)

})

test_that("searchSignature filters by phenotype", {
  test_conn <- SigRepo::test_conn_handler

  test_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))$clone(deep = TRUE)
  new_metadata <- test_sig$metadata
  new_metadata$signature_name <- "test_signature_phenotype_filter"
  test_sig$metadata <- new_metadata

  sig_id <- SigRepo::addSignature(conn_handler = test_conn, omic_signature = test_sig, return_signature_id = TRUE, verbose = FALSE)
  on.exit(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = sig_id, verbose = FALSE), add = TRUE)

  phenotype_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    phenotype = "Myc_reduce",
    verbose = FALSE
  )

  expect_true(methods::is(phenotype_search, "data.frame"))
  expect_true(nrow(phenotype_search) > 0)
})

test_that("searchSignature filters by platform", {
  test_conn <- SigRepo::test_conn_handler

  # test_data_transcriptomics.rds already carries platform = "transcriptomics
  # by array"; it just needs a unique signature_name so it doesn't collide
  # with fixtures uploaded by other tests in this file. ####
  test_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))$clone(deep = TRUE)
  new_metadata <- test_sig$metadata
  new_metadata$signature_name <- "test_signature_platform_filter"
  test_sig$metadata <- new_metadata

  sig_id <- SigRepo::addSignature(conn_handler = test_conn, omic_signature = test_sig, return_signature_id = TRUE, verbose = FALSE)
  on.exit(SigRepo::deleteSignature(conn_handler = test_conn, signature_id = sig_id, verbose = FALSE), add = TRUE)

  platform_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    platform = "transcriptomics by array",
    verbose = FALSE
  )

  expect_true(methods::is(platform_search, "data.frame"))
  expect_true(nrow(platform_search) > 0)
})

test_that("searchSignature returns empty result for non-existent signature", {
  test_conn <- SigRepo::test_conn_handler
  
  signature_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_name = "non_existent_signature_xyz123",
    verbose = FALSE
  )
  
  expect_true(methods::is(signature_search, "data.frame"))
  expect_true(nrow(signature_search) == 0)
})

test_that("addSignature handles duplicate signatures", {
  test_conn <- SigRepo::test_conn_handler
  
  # Create test signature data
  expect_no_error({test_transcriptomics_sig <- base::readRDS(testthat::test_path("test_data", "test_data_transcriptomics.rds"))})
  
  
  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })
  
  # Try to add same signature again

  expect_message(
    SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = TRUE
    ),
    regexp = "You already uploaded a signature with the name = 'test_signature'"
  )
  
  
  

  
  # remove signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
  
  
})

test_that("addSignature validates input data frame", {
  test_conn <- SigRepo::test_conn_handler
  
  # Test with NULL
  expect_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      signature_tbl = NULL,
      verbose = FALSE
    )
  })
  
  # Test with empty data frame
  expect_error({
    SigRepo::addSignature(
      conn_handler = test_conn,
      signature_tbl = data.frame(),
      verbose = FALSE
    )
  })
  
})



test_that("searchSignature with multiple filters works correctly", {
  test_conn <- SigRepo::test_conn_handler
  
  multi_filter_search <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    platform = "transcriptomics by array",
    phenotype = "Aging",
    verbose = FALSE
  )
  
  # added an expect equal for the expected amount of columns. It should always be 27
  testthat::expect_true(methods::is(multi_filter_search, "data.frame"))
  testthat::expect_equal(ncol(multi_filter_search), 27)
})

test_that("searchSignature returns consistent results", {
  test_conn <- SigRepo::test_conn_handler
  
  # Run search twice with same parameters
  result1 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  
  result2 <- SigRepo::searchSignature(
    conn_handler = test_conn,
    organism = "Homo sapiens",
    verbose = FALSE
  )
  
  # Should return same results
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(ncol(result1), ncol(result2))
  
  
})

test_that("getSignatureFeatureSet returns raw signature_feature_set rows", {
  test_conn <- SigRepo::test_conn_handler

  expect_no_error({
    test_transcriptomics_sig <- base::readRDS(
      testthat::test_path("test_data", "test_data_transcriptomics.rds")
    )
  })

  expect_no_error({
    omic_signature_id <- SigRepo::addSignature(
      conn_handler = test_conn,
      omic_signature = test_transcriptomics_sig,
      return_signature_id = TRUE,
      verbose = FALSE
    )
  })

  expect_no_error({
    signature_feature_set <- SigRepo::getSignatureFeatureSet(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })

  expect_s3_class(signature_feature_set, "data.frame")
  expect_true(base::nrow(signature_feature_set) > 0)
  expect_true(base::all(signature_feature_set$signature_id == omic_signature_id))
  expect_true(base::all(c("signature_id", "sig_feature_hashkey", "score", "group_label") %in% base::colnames(signature_feature_set)))

  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = omic_signature_id,
      verbose = FALSE
    )
  })
})
