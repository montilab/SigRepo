# mapMetabolites(): metabolite identifier conversion through MetaboAnalyst's
# public mapcompounds endpoint. The fixture is a real reply captured on
# 2026-10-15 for five compound names, one of them bogus; METLIN came back as
# JSON null for one row and the string "NA" for the unmatched one, so both
# spellings of "missing" are covered. HTTP is mocked so the tests run offline.

read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
fixture_queries <- c("1,3-Diaminopropane", "2-Ketobutyric acid", "Citric acid", "notacompoundxyz", "D-Glucose")

test_that("parseMetaboanalystMapping turns the column-oriented reply into one row per query", {
  out <- parseMetaboanalystMapping(read_fixture("metaboanalyst_mapcompounds.json"), fixture_queries)
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), METABOANALYST_COLUMNS)
  expect_equal(nrow(out), 5)
  expect_equal(out$query, fixture_queries)
  expect_equal(out$match[1], "1,3-Diaminopropane")
  expect_equal(out$hmdb[5], "HMDB0000122")
  expect_equal(out$pubchem[3], "311")
  expect_equal(out$chebi[3], "30769")
  expect_equal(out$kegg[5], "C00031")
  expect_equal(out$metlin[1], "5081")
  expect_equal(out$smiles[1], "NCCCN")
  expect_type(out$matched, "logical")
  expect_equal(out$matched, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

test_that("parseMetaboanalystMapping reads both the string NA and JSON null as missing", {
  out <- parseMetaboanalystMapping(read_fixture("metaboanalyst_mapcompounds.json"), fixture_queries)
  expect_true(all(is.na(out[4, c("match", "hmdb", "pubchem", "chebi", "kegg", "metlin", "smiles")])))
  expect_true(is.na(out$metlin[2]))
})

test_that("parseMetaboanalystMapping keeps the caller's order and fills queries the service dropped", {
  payload <- read_fixture("metaboanalyst_mapcompounds.json")
  out <- parseMetaboanalystMapping(payload, c("D-Glucose", "never sent", "Citric acid"))
  expect_equal(out$query, c("D-Glucose", "never sent", "Citric acid"))
  expect_equal(out$hmdb, c("HMDB0000122", NA, "HMDB0000094"))
  expect_equal(out$matched, c(TRUE, FALSE, TRUE))
})

test_that("mapMetabolites posts the semicolon-joined list and the input type to the mapcompounds URL", {
  seen <- NULL
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      seen <<- list(url = url, body = body, timeout = timeout)
      read_fixture("metaboanalyst_mapcompounds.json")
    }
  )
  out <- mapMetabolites(fixture_queries, from = "name", timeout = 90)
  expect_equal(seen$url, "https://rest.xialab.ca/api/mapcompounds")
  expect_equal(seen$body$queryList, paste0(paste(fixture_queries, collapse = ";"), ";"))
  expect_equal(seen$body$inputType, "name")
  expect_equal(seen$timeout, 90)
  expect_equal(nrow(out), 5)
  expect_equal(out$kegg[5], "C00031")
})

test_that("mapMetabolites passes the other documented input types through unchanged", {
  seen <- character()
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      seen <<- c(seen, body$inputType)
      list(Query = list("x"), Match = list("NA"), HMDB = list("NA"), PubChem = list("NA"),
           ChEBI = list("NA"), KEGG = list("NA"), METLIN = list("NA"), SMILES = list("NA"), Comment = list("0"))
    }
  )
  for (type in c("hmdb", "pubchem", "chebi", "kegg", "metlin")) mapMetabolites("x", from = type)
  expect_equal(seen, c("hmdb", "pubchem", "chebi", "kegg", "metlin"))
})

test_that("mapMetabolites trims, drops empty values and de-duplicates before sending", {
  seen <- NULL
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      seen <<- body$queryList
      list(Query = list("Citric acid", "D-Glucose"), Match = list("Citric acid", "D-Glucose"),
           HMDB = list("HMDB0000094", "HMDB0000122"), PubChem = list("311", "5793"),
           ChEBI = list("30769", "15903"), KEGG = list("C00158", "C00031"),
           METLIN = list("124", "133"), SMILES = list("s1", "s2"), Comment = list("1", "1"))
    }
  )
  out <- mapMetabolites(c("  Citric acid ", "", NA, "D-Glucose", "Citric acid", "NA"))
  expect_equal(seen, "Citric acid;D-Glucose;")
  expect_equal(out$query, c("Citric acid", "D-Glucose"))
})

test_that("mapMetabolites returns an empty frame without calling the service when nothing is left to send", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) stop("must not be called")
  )
  out <- mapMetabolites(c("", NA, "  "))
  expect_equal(nrow(out), 0)
  expect_equal(colnames(out), METABOANALYST_COLUMNS)
  expect_type(out$matched, "logical")
})

test_that("mapMetabolites refuses a value containing the list separator", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) stop("must not be called")
  )
  expect_error(mapMetabolites(c("Citric acid", "a;b")), "semicolon")
})

test_that("mapMetabolites rejects an unknown input type", {
  expect_error(mapMetabolites("glucose", from = "inchikey"), "should be one of")
})

test_that("mapMetabolites sends long lists in chunks and keeps the input order", {
  sizes <- integer()
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      q <- strsplit(sub(";$", "", body$queryList), ";", fixed = TRUE)[[1]]
      sizes <<- c(sizes, length(q))
      list(Query = as.list(q), Match = as.list(toupper(q)), HMDB = as.list(rep("NA", length(q))),
           PubChem = as.list(rep("NA", length(q))), ChEBI = as.list(rep("NA", length(q))),
           KEGG = as.list(rep("NA", length(q))), METLIN = as.list(rep("NA", length(q))),
           SMILES = as.list(rep("NA", length(q))), Comment = as.list(rep("1", length(q))))
    }
  )
  ids <- sprintf("compound%03d", 1:250)
  out <- mapMetabolites(ids)
  expect_equal(sizes, c(200, 50))
  expect_equal(out$query, ids)
  expect_equal(out$match, toupper(ids))
})

test_that("externalUrl knows the MetaboAnalyst endpoint and honours its option", {
  expect_equal(externalUrl("metaboanalyst"), "https://rest.xialab.ca/api/mapcompounds")
  withr::local_options(list(sigrepo.metaboanalyst_url = "http://localhost:1/map"))
  expect_equal(externalUrl("metaboanalyst"), "http://localhost:1/map")
})
