# mapOrthologs() with the database replaced: the connection helpers, the
# organisms lookup and the two SQL steps (lookupFeatureSymbols(),
# lookupOrthologPairs()) are mocked, so these tests cover the argument
# checks, the id classification, the ordering and the ambiguity flag.
# test_orthologs_db.R runs the real joins against a stack.

mock_organisms <- function(conn, db_table_name, ..., filter_coln_val = NULL) {
  tbl <- data.frame(organism_id = c(1L, 2L), organism = c("Mus musculus", "Homo sapiens"), stringsAsFactors = FALSE)
  if (!is.null(filter_coln_val$organism)) {
    tbl <- tbl[tolower(tbl$organism) %in% tolower(trimws(filter_coln_val$organism)), , drop = FALSE]
  }
  tbl
}

with_mock_db <- function(code, pairs) {
  testthat::local_mocked_bindings(
    conn_init = function(...) structure(list(), class = "mock_conn"),
    conn_close = function(...) invisible(NULL),
    checkPermissions = function(...) data.frame(user = "v", user_role = "viewer", stringsAsFactors = FALSE),
    lookup_table_sql = mock_organisms,
    countCurrentOrthologs = function(conn, organism_id) nrow(pairs),
    lookupFeatureSymbols = function(conn, ids, id_types, organism_id) {
      data.frame(
        query = ids,
        query_symbol = ifelse(id_types == "uniprot" & ids == "P02340", "Trp53",
                       ifelse(id_types == "symbol", ids, NA_character_)),
        stringsAsFactors = FALSE
      )
    },
    lookupOrthologPairs = function(conn, symbols, entrez_ids, non_human_organism_id, from_is_human) {
      key <- if (from_is_human) pairs$human_symbol else pairs$gene_symbol
      hit <- pairs[tolower(key) %in% tolower(symbols), , drop = FALSE]
      if (from_is_human) {
        data.frame(query_symbol = hit$human_symbol, query_entrez = NA_character_, symbol = hit$gene_symbol,
                   ensembl = hit$ensembl_gene_id, entrez = hit$entrez_id, support_n = hit$support_n, stringsAsFactors = FALSE)
      } else {
        data.frame(query_symbol = hit$gene_symbol, query_entrez = NA_character_, symbol = hit$human_symbol,
                   ensembl = hit$human_ensembl_gene_id, entrez = hit$human_entrez_id, support_n = hit$support_n, stringsAsFactors = FALSE)
      }
    },
    .package = "SigRepo"
  )
  force(code)
}

pairs <- data.frame(
  gene_symbol = c("Trp53", "Ifna1", "Ifna1"), human_symbol = c("TP53", "IFNA1", "IFNA13"),
  ensembl_gene_id = "ENSMUSG0", entrez_id = "1",
  human_ensembl_gene_id = c("ENSG00000141510", "ENSG00000197919", "ENSG00000233816"),
  human_entrez_id = c("7157", "3439", "3447"), support_n = c(12, 3, 3), stringsAsFactors = FALSE
)

test_that("mapOrthologs trims and de-duplicates and keeps input order", {
  out <- with_mock_db(mapOrthologs(list(), c(" P02340", "Trp53", "P02340 "), from = "Mus musculus", verbose = FALSE), pairs)
  expect_equal(out$query, c("P02340", "Trp53"))
  expect_equal(out$query_type, c("uniprot", "symbol"))
  expect_equal(out$query_symbol, c("Trp53", "Trp53"))
  expect_equal(out$symbol, c("TP53", "TP53"))
  expect_equal(out$ensembl, rep("ENSG00000141510", 2))
  expect_equal(out$entrez, rep("7157", 2))
  expect_equal(out$organism, rep("Homo sapiens", 2))
  expect_equal(out$ambiguous, c(FALSE, FALSE))
})

test_that("mapOrthologs returns the documented columns in order", {
  out <- with_mock_db(mapOrthologs(list(), "Trp53", from = "Mus musculus", verbose = FALSE), pairs)
  expect_equal(
    colnames(out),
    c("query", "query_type", "query_symbol", "organism", "symbol", "ensembl", "entrez", "support_n", "ambiguous")
  )
  expect_type(out$support_n, "double")
  expect_type(out$ambiguous, "logical")
})

test_that("mapOrthologs flags ambiguous inputs and keeps unmapped ones as NA rows", {
  out <- with_mock_db(mapOrthologs(list(), c("Ifna1", "Nothing"), from = "Mus musculus", verbose = FALSE), pairs)
  expect_equal(out$query, c("Ifna1", "Ifna1", "Nothing"))
  expect_equal(out$symbol, c("IFNA1", "IFNA13", NA))
  expect_equal(out$ambiguous, c(TRUE, TRUE, FALSE))
  expect_true(is.na(out$ensembl[3]))
  expect_equal(out$query_symbol[3], "Nothing")   # the within-species step succeeded; only the hop failed
})

test_that("mapOrthologs maps human to mouse", {
  out <- with_mock_db(mapOrthologs(list(), "TP53", from = "Homo sapiens", to = "Mus musculus", verbose = FALSE), pairs)
  expect_equal(out$symbol, "Trp53")
  expect_equal(out$organism, "Mus musculus")
})

test_that("mapOrthologs refuses same-organism and non-human pairs before touching the database", {
  expect_error(mapOrthologs(list(), "Trp53", from = "Mus musculus", to = "Mus musculus", verbose = FALSE), "different organisms")
  expect_error(mapOrthologs(list(), "Trp53", from = "Mus musculus", to = "Rattus norvegicus", verbose = FALSE), "Homo sapiens")
  expect_error(mapOrthologs(list(), "Trp53", from = "", verbose = FALSE), "'from'")
})

test_that("mapOrthologs explains when no ortholog reference is loaded", {
  expect_error(
    with_mock_db(mapOrthologs(list(), "Trp53", from = "Mus musculus", verbose = FALSE), pairs[0, ]),
    "No ortholog reference is loaded"
  )
})

test_that("mapOrthologs returns an empty frame for empty input", {
  out <- with_mock_db(mapOrthologs(list(), c("", NA), from = "Mus musculus", verbose = FALSE), pairs)
  expect_equal(nrow(out), 0)
  expect_equal(colnames(out)[1], "query")
})
