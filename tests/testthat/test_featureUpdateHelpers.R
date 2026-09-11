# Pure helpers behind updateTranscriptomicsFeatureSet(); no database needed.

test_that("symbolAttributeForOrganism picks the BioMart attribute that actually carries symbols", {
  expect_equal(symbolAttributeForOrganism("Homo sapiens"), "hgnc_symbol")
  expect_equal(symbolAttributeForOrganism("Mus musculus"), "mgi_symbol")
  expect_equal(symbolAttributeForOrganism("Rattus norvegicus"), "external_gene_name")
  expect_equal(symbolAttributeForOrganism("Danio rerio"), "external_gene_name")
})

test_that("symbolAttributeForOrganism ignores case and surrounding whitespace", {
  expect_equal(symbolAttributeForOrganism("  mus musculus "), "mgi_symbol")
  expect_equal(symbolAttributeForOrganism("HOMO SAPIENS"), "hgnc_symbol")
})

db <- data.frame(
  feature_name = c("ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003", "ENSMUSG00000000004", "ENSMUSG00000000009"),
  gene_symbol  = c("Gnai3",              "Pbsn",               "",                   "Cdc45",              "Retired"),
  stringsAsFactors = FALSE
)
fetched <- data.frame(
  feature_name = c("ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003", "ENSMUSG00000000004", "ENSMUSG00000000005"),
  gene_symbol  = c("Gnai3",              "",                   "H19",                "Cdc45b",             "Xpo6"),
  stringsAsFactors = FALSE
)
parts <- partitionFeatureUpdates(db, fetched)

test_that("an unchanged symbol only needs its version refreshed", {
  expect_true("ENSMUSG00000000001" %in% parts$refresh$feature_name)
  expect_false("ENSMUSG00000000001" %in% parts$resymbol$feature_name)
})

test_that("a blank fetched symbol never overwrites a stored symbol", {
  expect_true("ENSMUSG00000000002" %in% parts$refresh$feature_name)
  expect_false("ENSMUSG00000000002" %in% parts$resymbol$feature_name)
})

test_that("a stored blank symbol is filled from the fetch", {
  row <- parts$resymbol[parts$resymbol$feature_name == "ENSMUSG00000000003", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$gene_symbol, "H19")
})

test_that("a genuinely changed symbol is rewritten with the fetched value", {
  row <- parts$resymbol[parts$resymbol$feature_name == "ENSMUSG00000000004", ]
  expect_equal(row$gene_symbol, "Cdc45b")
})

test_that("features only in the fetch are added and features only in the database are archived", {
  expect_equal(parts$add$feature_name, "ENSMUSG00000000005")
  expect_equal(parts$add$gene_symbol, "Xpo6")
  expect_equal(parts$archive$feature_name, "ENSMUSG00000000009")
})

test_that("every database feature lands in exactly one of refresh, resymbol, or archive", {
  placed <- c(parts$refresh$feature_name, parts$resymbol$feature_name, parts$archive$feature_name)
  expect_setequal(placed, db$feature_name)
  expect_equal(anyDuplicated(placed), 0)
})

test_that("feature names match case-insensitively and symbols compare case-insensitively", {
  p <- partitionFeatureUpdates(
    data.frame(feature_name = "ensmusg00000000010", gene_symbol = "ACTB", stringsAsFactors = FALSE),
    data.frame(feature_name = "ENSMUSG00000000010", gene_symbol = "Actb", stringsAsFactors = FALSE)
  )
  expect_equal(p$refresh$feature_name, "ensmusg00000000010")
  expect_equal(nrow(p$resymbol), 0)
  expect_equal(nrow(p$add), 0)
})

test_that("NA symbols are treated as blank on both sides", {
  p <- partitionFeatureUpdates(
    data.frame(feature_name = c("A", "B"), gene_symbol = c(NA, "Kept"), stringsAsFactors = FALSE),
    data.frame(feature_name = c("A", "B"), gene_symbol = c("Filled", NA), stringsAsFactors = FALSE)
  )
  expect_equal(p$resymbol$feature_name, "A")
  expect_equal(p$resymbol$gene_symbol, "Filled")
  expect_equal(p$refresh$feature_name, "B")
})

test_that("ensemblReleaseNumber extracts the release from biomaRt's listEnsembl() label", {
  expect_identical(ensemblReleaseNumber("Ensembl Genes 116"), 116L)
  expect_identical(ensemblReleaseNumber("116"), 116L)
  expect_identical(ensemblReleaseNumber(" 115 "), 115L)
})

test_that("ensemblReleaseNumber returns NA for values that carry no release", {
  expect_identical(ensemblReleaseNumber(""), NA_integer_)
  expect_identical(ensemblReleaseNumber(NA), NA_integer_)
  expect_identical(ensemblReleaseNumber("Ensembl Genes"), NA_integer_)
})
