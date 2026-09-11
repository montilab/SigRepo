# Pure helpers behind updateProteomicsFeatureSet(); no network or database.

test_that("uniprotGeneNameUrl asks UniProt's REST API for primary gene names of one organism", {
  url <- uniprotGeneNameUrl(10090)
  expect_match(url, "^https://rest\\.uniprot\\.org/uniprotkb/stream\\?")
  expect_match(url, "organism_id:10090")
  expect_match(url, "fields=accession,gene_primary")
  expect_match(url, "format=tsv")
  expect_match(url, "compressed=true")
})

tsv <- paste(
  "Entry\tGene Names (primary)",
  "P04637\tTP53",
  "Q0ZCI6\t",
  "P0DPB3\tGENE1; GENE2",
  " P31946 \t YWHAB ",
  "P04637\tTP53",
  sep = "\n"
)

test_that("parseUniprotGeneNames turns the REST TSV into feature_name / gene_symbol", {
  out <- parseUniprotGeneNames(tsv)
  expect_equal(names(out), c("feature_name", "gene_symbol"))
  expect_equal(out$gene_symbol[out$feature_name == "P04637"], "TP53")
  expect_equal(out$gene_symbol[out$feature_name == "P31946"], "YWHAB")
})

test_that("parseUniprotGeneNames leaves entries without a gene name blank, never the accession", {
  out <- parseUniprotGeneNames(tsv)
  expect_equal(out$gene_symbol[out$feature_name == "Q0ZCI6"], "")
})

test_that("parseUniprotGeneNames keeps only the first of several primary names and drops duplicate accessions", {
  out <- parseUniprotGeneNames(tsv)
  expect_equal(out$gene_symbol[out$feature_name == "P0DPB3"], "GENE1")
  expect_equal(sum(out$feature_name == "P04637"), 1)
  expect_equal(nrow(out), 4)
})

test_that("proteomicsSymbolIsPlaceholder flags entry names and accession copies but not real symbols", {
  flags <- proteomicsSymbolIsPlaceholder(
    symbols = c("P53_HUMAN", "1433B", "Q0ZCI6", "TP53", "", NA),
    feature_names = c("P04637", "P31946", "Q0ZCI6", "P04637", "A0A024R161", "A0A024R162"),
    organism_code = "HUMAN"
  )
  expect_equal(flags, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("proteomicsSymbolIsPlaceholder uses the organism's own UniProt code for the suffix", {
  expect_true(proteomicsSymbolIsPlaceholder("1433Z_MOUSE", "P63101", "MOUSE"))
  expect_false(proteomicsSymbolIsPlaceholder("1433Z_MOUSE", "P63101", "HUMAN"))
  expect_true(proteomicsSymbolIsPlaceholder("q0zci6", "Q0ZCI6", "HUMAN"))
})
