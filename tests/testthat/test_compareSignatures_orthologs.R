# compareSignatures(map_orthologs = TRUE): the translation of signatures from
# another organism before the comparison. mapOrthologs() and the within-species
# symbol lookup are mocked, so this covers the wiring, the id-space choice,
# the renaming and the error paths. The bundled Myc signatures are mouse; a
# human copy is made by prefixing every feature id, so a mock mapping that
# adds the same prefix makes the two overlap again.

load_examples <- function() {
  env <- base::environment()
  utils::data(list = c("omic_signature_1", "omic_signature_2"), package = "SigRepo", envir = env)
  base::list(m1 = env$omic_signature_1, m2 = env$omic_signature_2)
}

# A "human" version of a mouse signature: ENSMUSG ids become ENSG ids with the
# same digits (so they still classify as Ensembl), or any other rewrite.
humanize <- function(x) sub("^ENSMUSG", "ENSG", x)

human_copy <- function(sig, rewrite = humanize) {
  md <- sig$metadata
  md$organism <- "Homo sapiens"
  md$signature_name <- paste0("human_", md$signature_name)
  s <- sig$signature
  s$feature_name <- rewrite(s$feature_name)
  d <- sig$difexp
  d$feature_name <- rewrite(d$feature_name)
  OmicSignature::OmicSignature$new(metadata = md, signature = s, difexp = d, print_message = FALSE)
}

# Every mouse id maps to the "human" id human_copy() would give it.
fake_map <- function(conn_handler, ids, from, to, id_type, verbose) {
  data.frame(
    query = ids, query_type = "ensembl", query_symbol = NA_character_, organism = to,
    symbol = paste0("S_", ids), ensembl = humanize(ids), entrez = NA_character_,
    support_n = 12, ambiguous = FALSE, stringsAsFactors = FALSE
  )
}

counts_of <- function(res) res$comparisons[[1]]$counts

test_that("map_orthologs requires a connection handler", {
  ex <- load_examples()
  expect_error(
    compareSignatures(omic_signatures = list(human_copy(ex$m2), ex$m1), method = "overlap", map_orthologs = TRUE, verbose = FALSE),
    "conn_handler"
  )
})

test_that("mouse signatures are translated onto the human Ensembl space before an overlap comparison", {
  ex <- load_examples()
  testthat::local_mocked_bindings(mapOrthologs = fake_map, .package = "SigRepo")
  human <- human_copy(ex$m2)

  # Quietly: no warning, and none of OmicSignature's "[Success] ... created"
  # chatter for the translated copies.
  expect_no_warning(
    output <- utils::capture.output(
      res <- compareSignatures(conn_handler = list(mock = TRUE), omic_signatures = list(human, ex$m1),
                               method = "overlap", map_orthologs = TRUE, verbose = FALSE)
    )
  )
  expect_false(any(grepl("created", output, fixed = TRUE)))

  counts <- counts_of(res)
  expect_true("Myc_reduce_mice_liver_24m_v1_to_Homo_sapiens" %in% rownames(counts))
  expect_true("human_Myc_reduce_mice_liver_24m_v2" %in% rownames(counts))
  expect_gt(counts[1, 2], 0)
})

test_that("the first signature's organism is the default target and ortholog_target overrides it", {
  ex <- load_examples()
  testthat::local_mocked_bindings(mapOrthologs = fake_map, .package = "SigRepo")
  human <- human_copy(ex$m2)

  res <- compareSignatures(conn_handler = list(mock = TRUE), omic_signatures = list(ex$m1, human),
                           method = "overlap", map_orthologs = TRUE, ortholog_target = "Homo sapiens", verbose = FALSE)
  expect_true("Myc_reduce_mice_liver_24m_v1_to_Homo_sapiens" %in% rownames(counts_of(res)))

  res2 <- compareSignatures(conn_handler = list(mock = TRUE), omic_signatures = list(ex$m1, human),
                            method = "overlap", map_orthologs = TRUE, verbose = FALSE)
  expect_true("human_Myc_reduce_mice_liver_24m_v2_to_Mus_musculus" %in% rownames(counts_of(res2)))
})

test_that("proteomics target signatures are translated to symbols", {
  ex <- load_examples()
  # Human copy whose ids look like accessions: the target id space is then
  # symbol, and the human side itself needs the within-species step.
  prot <- human_copy(ex$m2, rewrite = function(x) paste0("P0", x))
  calls <- new.env()
  calls$n <- 0
  testthat::local_mocked_bindings(
    mapOrthologs = function(conn_handler, ids, from, to, id_type, verbose) {
      out <- fake_map(conn_handler, ids, from, to, id_type, verbose)
      out$ensembl <- NA_character_
      out
    },
    lookupFeatureSymbols = function(conn, ids, id_types, organism_id) {
      calls$n <- calls$n + 1
      data.frame(query = ids, query_symbol = paste0("S_", sub("^P0", "", ids)), stringsAsFactors = FALSE)
    },
    classifyFeatureIdType = function(ids) ifelse(grepl("^P0", ids), "uniprot", ifelse(grepl("^ENS", ids), "ensembl", "symbol")),
    conn_init = function(...) structure(list(), class = "mock_conn"),
    conn_close = function(...) invisible(NULL),
    checkPermissions = function(...) data.frame(user = "v", user_role = "viewer", stringsAsFactors = FALSE),
    lookup_table_sql = function(...) data.frame(organism_id = 2L, organism = "Homo sapiens", stringsAsFactors = FALSE),
    .package = "SigRepo"
  )

  res <- compareSignatures(conn_handler = list(mock = TRUE), omic_signatures = list(prot, ex$m1),
                           method = "overlap", map_orthologs = TRUE, verbose = FALSE)

  expect_gt(calls$n, 0)
  expect_gt(counts_of(res)[1, 2], 0)
})

test_that("a signature none of whose features map is an error naming it", {
  ex <- load_examples()
  testthat::local_mocked_bindings(
    mapOrthologs = function(conn_handler, ids, from, to, id_type, verbose) {
      out <- fake_map(conn_handler, ids, from, to, id_type, verbose)
      out$symbol <- NA_character_
      out$ensembl <- NA_character_
      out
    },
    .package = "SigRepo"
  )
  expect_error(
    compareSignatures(conn_handler = list(mock = TRUE), omic_signatures = list(human_copy(ex$m2), ex$m1),
                      method = "overlap", map_orthologs = TRUE, verbose = FALSE),
    "Myc_reduce_mice_liver_24m_v1"
  )
})

test_that("map_orthologs = FALSE leaves mixed organisms untouched", {
  ex <- load_examples()
  res <- compareSignatures(omic_signatures = list(human_copy(ex$m2), ex$m1), method = "overlap", verbose = FALSE)
  counts <- counts_of(res)
  expect_false(any(grepl("_to_", rownames(counts))))
  expect_equal(counts[1, 2], 0)
})
