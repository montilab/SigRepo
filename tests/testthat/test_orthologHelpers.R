# Pure helpers behind mapOrthologs() and updateOrthologFeatureSet(); no
# database needed. babelgene itself is reached only through
# fetchBabelgeneOrthologs(), which the shaping tests replace.

test_that("classifyFeatureIdType classifies element-wise", {
  expect_equal(
    classifyFeatureIdType(c("ENSMUSG00000059552", "P02340", "22059", "Trp53", "trp53", "", NA, "Q60590-2", "ensg00000141510")),
    c("ensembl", "uniprot", "entrez", "symbol", "symbol", NA, NA, "uniprot", "ensembl")
  )
})

test_that("classifyFeatureIdType trims surrounding whitespace before classifying", {
  expect_equal(classifyFeatureIdType(c(" P02340 ", "  ")), c("uniprot", NA))
})

test_that("classifyFeatureIdType reads accession-shaped human symbols as accessions (the lookup falls back)", {
  # Documented ambiguity: these are real human symbols that fit the UniProt
  # pattern. lookupFeatureSymbols() turns them back into symbols when the
  # proteomics table does not know them.
  expect_equal(classifyFeatureIdType(c("H2AC11", "B3GAT1", "P2RY12")), rep("uniprot", 3))
})

test_that("fetchBabelgeneOrthologs ignores digit-only and Ensembl-shaped strings among the symbols", {
  # babelgene guesses the id type from the genes it is given; a stray "12345"
  # or ENSMUSG... value among thousands of reference symbols must not flip the
  # whole fetch into Entrez or Ensembl mode (which would archive most pairs).
  out <- fetchBabelgeneOrthologs(c("12345", "ENSMUSG00000059552", "Trp53"), "Mus musculus", human = FALSE)
  expect_true("Trp53" %in% out$symbol)
  expect_equal(unique(out$symbol), "Trp53")
})

fake_babelgene <- function() {
  data.frame(
    human_symbol = c("TP53", "IFNA1", "IFNA13"), human_entrez = c(7157L, 3439L, 3447L),
    human_ensembl = c("ENSG00000141510", "ENSG00000197919", "ENSG00000233816"),
    taxon_id = 10090L, symbol = c("Trp53", "Ifna1", "Ifna1"), entrez = c(22059L, 15962L, 15962L),
    ensembl = c("ENSMUSG00000059552", "ENSMUSG00000095498", "ENSMUSG00000095498"),
    support = c("Ensembl|HGNC", "Ensembl", "Ensembl"), support_n = c(2, 1, 1), stringsAsFactors = FALSE
  )
}

test_that("babelgeneOrthologTable shapes babelgene rows into the reference columns", {
  fake <- fake_babelgene()
  testthat::local_mocked_bindings(fetchBabelgeneOrthologs = function(genes, species, human) fake, .package = "SigRepo")
  out <- babelgeneOrthologTable(symbols = "Trp53", human_symbols = "TP53", organism = "Mus musculus", species = "Mus musculus")
  expect_equal(colnames(out), ORTHOLOG_COLUMNS)
  # both directions return the same three rows; the union is de-duplicated on the pair
  expect_equal(nrow(out), 3)
  expect_equal(out$organism, rep("Mus musculus", 3))
  expect_equal(out$version, rep(as.character(utils::packageVersion("babelgene")), 3))
  expect_type(out$entrez_id, "character")
  expect_equal(sum(out$gene_symbol == "Ifna1"), 2)
  expect_equal(out$human_ensembl_gene_id[out$gene_symbol == "Trp53"], "ENSG00000141510")
})

test_that("babelgeneOrthologTable drops pairs missing a symbol on either side", {
  fake <- fake_babelgene()
  fake$human_symbol[2] <- NA
  fake$symbol[3] <- ""
  testthat::local_mocked_bindings(fetchBabelgeneOrthologs = function(genes, species, human) fake, .package = "SigRepo")
  out <- babelgeneOrthologTable("Trp53", "TP53", "Mus musculus", "Mus musculus")
  expect_equal(out$gene_symbol, "Trp53")
})

test_that("babelgeneOrthologTable returns an empty frame when babelgene finds nothing", {
  # Real babelgene call: it throws "no orthologs found" for an unknown gene,
  # and the wrapper must turn that into zero rows, not an error.
  out <- babelgeneOrthologTable("NOT_A_GENE_XYZ", "NOT_A_GENE_XYZ", "Mus musculus", "Mus musculus")
  expect_equal(nrow(out), 0)
  expect_equal(colnames(out), ORTHOLOG_COLUMNS)
})

test_that("partitionOrthologUpdates places every stored pair once", {
  db <- data.frame(gene_symbol = c("Trp53", "Ifna1", "Old1"), human_symbol = c("TP53", "IFNA1", "OLD1"), stringsAsFactors = FALSE)
  fetched <- data.frame(gene_symbol = c("TRP53", "Ifna1", "Ifna1", "New1"), human_symbol = c("tp53", "IFNA1", "IFNA13", "NEW1"), stringsAsFactors = FALSE)
  parts <- partitionOrthologUpdates(db, fetched)
  expect_setequal(paste(parts$refresh$gene_symbol, parts$refresh$human_symbol), c("Trp53 TP53", "Ifna1 IFNA1"))
  expect_equal(paste(parts$archive$gene_symbol, parts$archive$human_symbol), "Old1 OLD1")
  expect_setequal(paste(parts$add$gene_symbol, parts$add$human_symbol), c("Ifna1 IFNA13", "New1 NEW1"))
  placed <- c(paste(parts$refresh$gene_symbol, parts$refresh$human_symbol), paste(parts$archive$gene_symbol, parts$archive$human_symbol))
  expect_setequal(placed, paste(db$gene_symbol, db$human_symbol))
  expect_equal(anyDuplicated(placed), 0)
})

test_that("babelgeneSpeciesName matches the organism name and falls back to the stored taxid", {
  # babelgene::orthologs() refuses a taxon id for `species`; it wants one of
  # the names babelgene::species() lists.
  expect_equal(babelgeneSpeciesName("Mus musculus", "10090"), "Mus musculus")
  expect_equal(babelgeneSpeciesName(" mus musculus ", NA), "Mus musculus")
  expect_equal(babelgeneSpeciesName("House mouse (lab strain)", "10090"), "Mus musculus")
  expect_true(is.na(babelgeneSpeciesName("Not a species", "1234")))
  expect_true(is.na(babelgeneSpeciesName("Not a species", NA)))
})
