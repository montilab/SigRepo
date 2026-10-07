# Translation of a signature into the query shape the external engines share.
# No network and no database: the two symbol lookups are mocked.

# A tiny OmicSignature-shaped stand-in: buildExternalQuery() only touches
# $signature and $metadata, so a plain list is enough and keeps these tests
# independent of OmicSignature's constructor checks.
fake_signature <- function(feature_name, score = NULL, organism = "Homo sapiens",
                           name = "fake_sig", assay_type = "transcriptomics") {
  sig <- base::data.frame(
    probe_id = base::seq_along(feature_name),
    feature_name = feature_name,
    stringsAsFactors = FALSE
  )
  if (!base::is.null(score)) sig$score <- score
  base::list(
    signature = sig,
    metadata = base::list(signature_name = name, organism = organism, assay_type = assay_type)
  )
}

test_that("organismCode maps human and mouse case-insensitively", {
  expect_equal(organismCode("Homo sapiens"), "hsa")
  expect_equal(organismCode("Homo Sapiens"), "hsa")
  expect_equal(organismCode("mus musculus"), "mmu")
  expect_true(is.na(organismCode("Rattus norvegicus")))
  expect_true(is.na(organismCode(NULL)))
})

test_that("looksLikeGeneSymbol rejects Ensembl, Entrez and RefSeq ids", {
  expect_equal(
    looksLikeGeneSymbol(c("TP53", "ENSG00000141510", "ENSMUSG00000059552.7", "7157", "NM_000546", "Isg15")),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("mapFeaturesToSymbols passes symbols through and never calls lookups for them", {
  called <- FALSE
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(conn_handler, feature_names, organism) { called <<- TRUE; character() },
    lookupSymbolsInBiomart = function(feature_names, organism) { called <<- TRUE; character() }
  )
  m <- mapFeaturesToSymbols(c("TP53", "BRCA1", " TP53 "), "Homo sapiens")
  expect_false(called)
  expect_equal(unname(m), c("TP53", "BRCA1"))
})

test_that("mapFeaturesToSymbols uses SigRepo first, then biomaRt, and reports NA for the rest", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(conn_handler, feature_names, organism) c(ENSG00000141510 = "TP53"),
    lookupSymbolsInBiomart = function(feature_names, organism) c(ENSG00000012048 = "BRCA1")
  )
  m <- mapFeaturesToSymbols(c("ENSG00000141510", "ENSG00000012048", "ENSG00000000000"), "Homo sapiens", conn_handler = NULL)
  expect_equal(m[["ENSG00000141510"]], "TP53")
  expect_equal(m[["ENSG00000012048"]], "BRCA1")
  expect_true(is.na(m[["ENSG00000000000"]]))
})

test_that("lookupSymbolsInBiomart strips Ensembl versions and maps back to the original names", {
  testthat::local_mocked_bindings(
    biomartSymbolTable = function(ensembl_ids, organism) {
      base::data.frame(ensembl_gene_id = "ENSG00000126353", hgnc_symbol = "CCR7", stringsAsFactors = FALSE)
    }
  )
  m <- lookupSymbolsInBiomart(c("ENSG00000126353.12", "TP53"), "Homo sapiens")
  expect_equal(m, c(ENSG00000126353.12 = "CCR7"))
})

test_that("buildExternalQuery splits by score sign and drops NA scores", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("A1", "B1", "C1", "D1"), score = c(3, -2, NA, 1))
  up <- buildExternalQuery(sig, direction = "up", verbose = FALSE)
  expect_equal(up$genes, c("A1", "D1"))         # ordered by |score|
  expect_equal(up$direction, "up")
  # only B1 is negative: one gene is below the minimum of two
  expect_error(buildExternalQuery(sig, direction = "down", verbose = FALSE), "Could not map enough")
})

test_that("buildExternalQuery errors when direction splitting has no score", {
  sig <- fake_signature(c("A1", "B1"))
  expect_error(buildExternalQuery(sig, direction = "up", verbose = FALSE), "needs a 'score' column")
})

test_that("buildExternalQuery truncates to max_genes by |score| and flags it", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(paste0("G", 1:10), score = 10:1)
  q <- buildExternalQuery(sig, max_genes = 3, verbose = FALSE)
  expect_equal(q$genes, c("G1", "G2", "G3"))
  expect_true(q$truncated)
  expect_equal(q$n_input, 10)
})

test_that("buildExternalQuery errors when fewer than two symbols map", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("ENSG00000000001", "ENSG00000000002"))
  expect_error(buildExternalQuery(sig, verbose = FALSE), "Could not map enough")
})

test_that("buildExternalQuery refuses non-transcriptomics signatures", {
  sig <- fake_signature(c("P12345", "Q67890"), assay_type = "proteomics")
  expect_error(buildExternalQuery(sig, verbose = FALSE), "transcriptomics signatures only")
})

test_that("buildExternalQuery reports organism code and unmapped names", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) c(ENSG00000141510 = "TP53"),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("ENSG00000141510", "BRCA1", "ENSG00000000000"), organism = "Mus musculus")
  q <- buildExternalQuery(sig, verbose = FALSE)
  expect_equal(q$organism_code, "mmu")
  expect_setequal(q$genes, c("TP53", "BRCA1"))
  expect_equal(q$unmapped, "ENSG00000000000")
  expect_equal(q$n_unmapped, 1)
})
