# RummaGEO adapter. Parsing runs against a saved response captured from
# rummageo.com on 2026-10-07 with a 12-gene interferon query; the network
# layer is mocked.

read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseRummageo flattens hits and derives GEO links", {
  out <- parseRummageo(read_fixture("rummageo_enrich.json"), ifn_query())
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "rummageo")
  expect_equal(out$id[1], "GSE218462,GSE218464-0-vs-1-human up")
  expect_equal(out$gse[1], "GSE218462,GSE218464")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE218462")
  expect_equal(out$regulation[1], "up")
  expect_equal(out$contrast[1], "0-vs-1")
  expect_equal(out$score_label[1], "odds ratio")
  expect_equal(out$n_overlap[1], 11)
  expect_equal(out$n_set[1], 34)
  expect_equal(attr(out, "total_count"), 18448)
})

test_that("rummageoBackgroundId picks the background by species", {
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      list(data = list(backgrounds = list(nodes = list(
        list(id = "mouse-id", species = "mouse"),
        list(id = "human-id", species = "human")
      ))))
    }
  )
  expect_equal(rummageoBackgroundId("hsa"), "human-id")
  expect_equal(rummageoBackgroundId("mmu"), "mouse-id")
})

test_that("externalRummageo refuses organisms RummaGEO does not index", {
  q <- ifn_query("Rattus norvegicus", NA_character_)
  expect_error(externalRummageo(q), "only supports Homo sapiens and Mus musculus")
})

test_that("externalRummageo sends mouse symbols unchanged with the mouse background", {
  seen <- list()
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      if (grepl("backgrounds", query, fixed = TRUE)) {
        return(list(data = list(backgrounds = list(nodes = list(list(id = "mouse-id", species = "mouse"))))))
      }
      seen <<- variables
      read_fixture("rummageo_enrich.json")
    }
  )
  q <- ifn_query("Mus musculus", "mmu"); q$genes <- c("Isg15", "Ifi6", "Mx1")
  out <- externalRummageo(q, limit = 5)
  expect_equal(seen$id, "mouse-id")
  expect_equal(unlist(seen$genes), c("Isg15", "Ifi6", "Mx1"))
  expect_equal(seen$first, 5L)
  expect_equal(nrow(out), 3)
})
