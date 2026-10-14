# CORESH adapter. The ranking fixture is the head of a real get-ranking-result
# reply captured on 2026-10-07 for a 12-gene interferon query. HTTP is mocked
# so the submit / poll / fetch cycle runs offline.

read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseCoreshRanking maps GEO datasets into the common frame", {
  out <- parseCoreshRanking(read_fixture("coresh_ranking.json"), ifn_query(), limit = 3)
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "coresh")
  expect_equal(out$id[1], "GSE88731")
  expect_match(out$title[1], "hepatitis E")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE88731")
  expect_equal(out$score[1], 20.8949)
  expect_equal(out$score_label[1], "% variance")
  expect_true(is.na(out$pvalue[1]))
  expect_true(is.na(out$adj_pvalue[1]))        # log10Padj is the string "NA" without p-values
  expect_equal(out$n_set[1], 11)
  expect_equal(out$gpl[1], "GPL20301")
  expect_equal(attr(out, "total_count"), 5)
})

test_that("parseCoreshRanking converts log10Padj when present and tolerates limit > hits", {
  rows <- read_fixture("coresh_ranking.json")
  rows[[1]]$log10Padj <- -3
  out <- parseCoreshRanking(rows, ifn_query(), limit = 100)
  expect_equal(nrow(out), 5)
  expect_equal(out$adj_pvalue[1], 1e-3)
  expect_equal(out$log10_padj[1], -3)
})

test_that("externalCoresh submits, polls until done, and fetches the ranking", {
  calls <- character()
  progress <- c(40, 100)
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      calls <<- c(calls, "submit")
      expect_equal(body$organism, "hsa"); expect_equal(body$dbType, "hsa")
      expect_false(body$calculatePvalues)
      expect_equal(unlist(body$genes), c("ISG15", "IFI6", "MX1"))
      list(ID = "20261007_test")
    },
    externalGetJson = function(url, query = list(), timeout = 60) {
      if (grepl("check-job", url)) {
        calls <<- c(calls, "check-job")
        p <- progress[1]; progress <<- progress[-1]
        return(list(progress = p, nbatches = 100, ncompleted = p))
      }
      if (grepl("check-result-files", url)) { calls <<- c(calls, "files"); return(list(files_ready = TRUE)) }
      if (grepl("get-ranking-result", url)) {
        calls <<- c(calls, "ranking"); expect_equal(query$jobid, "20261007_test")
        return(read_fixture("coresh_ranking.json"))
      }
      stop("unexpected url ", url)
    }
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  out <- externalCoresh(ifn_query(), limit = 2)
  expect_equal(calls, c("submit", "check-job", "check-job", "files", "ranking"))
  expect_equal(nrow(out), 2)
  expect_equal(attr(out, "job_id"), "20261007_test")
  expect_equal(attr(out, "job_url"), "https://alserglab.wustl.edu/coresh/load/20261007_test")
})

test_that("externalCoresh stops when the job reports failure", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(ID = "bad_job"),
    externalGetJson = function(url, query = list(), timeout = 60) list(progress = 10, failed = "True")
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  expect_error(externalCoresh(ifn_query()), "CORESH job bad_job failed")
})

test_that("externalCoresh times out with the job url in the message", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(ID = "slow_job"),
    externalGetJson = function(url, query = list(), timeout = 60) list(progress = 10)
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  expect_error(externalCoresh(ifn_query(), timeout = 0), "coresh/load/slow_job")
})

test_that("externalCoresh refuses organisms CORESH does not index", {
  expect_error(externalCoresh(ifn_query("Danio rerio", NA_character_)), "only supports Homo sapiens and Mus musculus")
})
