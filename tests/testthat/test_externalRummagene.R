# Rummagene adapter. Parsing runs against a saved response captured from
# rummagene.com on 2026-10-07 with a 12-gene interferon query; the network
# layer is mocked.

read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseRummagene flattens hits into the common frame", {
  out <- parseRummagene(read_fixture("rummagene_enrich.json"), ifn_query())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "rummagene")
  expect_equal(out$rank, 1:3)
  expect_equal(out$id[1], "PMC11278796-mmc3.docx-0-Target_gene")
  expect_match(out$title[1], "^Spontaneous NETosis")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC11278796/")
  expect_equal(out$score_label[1], "odds ratio")
  expect_equal(out$n_overlap[1], 12)
  expect_equal(out$n_set[1], 22)
  expect_equal(out$pmcid[1], "PMC11278796")
  expect_equal(out$year[1], 2023)
  expect_equal(attr(out, "total_count"), 21423)
  expect_equal(attr(out, "query")$signature_name, "ifn")
})

test_that("parseRummagene handles an empty result", {
  payload <- list(data = list(currentBackground = list(enrich = list(totalCount = 0, nodes = list()))))
  out <- parseRummagene(payload, ifn_query())
  expect_equal(nrow(out), 0)
  expect_equal(attr(out, "total_count"), 0)
})

test_that("externalRummagene upper-cases genes and passes limit", {
  seen <- NULL
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      seen <<- variables
      read_fixture("rummagene_enrich.json")
    }
  )
  q <- ifn_query("Mus musculus", "mmu"); q$genes <- c("Isg15", "Ifi6", "Mx1")
  out <- externalRummagene(q, limit = 7)
  expect_equal(unlist(seen$genes), c("ISG15", "IFI6", "MX1"))
  expect_equal(seen$first, 7L)
  expect_equal(nrow(out), 3)
})
