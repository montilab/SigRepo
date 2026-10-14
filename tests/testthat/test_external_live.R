# Hits the real services. Off by default; run with
#   SIGREPO_LIVE_EXTERNAL=true Rscript -e 'devtools::test(filter = "external_live")'
skip_unless_live <- function() {
  testthat::skip_if_not(
    base::identical(base::tolower(base::Sys.getenv("SIGREPO_LIVE_EXTERNAL")), "true"),
    "SIGREPO_LIVE_EXTERNAL is not 'true'"
  )
}
ifn_query <- base::list(
  signature_name = "interferon_12", organism = "Homo sapiens", organism_code = "hsa", direction = "combined",
  genes = c("ISG15", "IFI6", "IFI27", "MX1", "OAS1", "IFIT1", "IFIT3", "RSAD2", "IFI44L", "STAT1", "IRF7", "OASL"),
  n_input = 12, n_mapped = 12, n_unmapped = 0, truncated = FALSE, unmapped = character()
)

test_that("live: Rummagene returns interferon papers", {
  skip_unless_live()
  out <- externalRummagene(ifn_query, limit = 5)
  expect_gt(nrow(out), 0)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_true(all(grepl("^PMC", out$pmcid[!is.na(out$pmcid)])))
})

test_that("live: RummaGEO returns human GEO contrasts", {
  skip_unless_live()
  out <- externalRummageo(ifn_query, limit = 5)
  expect_gt(nrow(out), 0)
  expect_true(all(out$species == "human"))
})

test_that("live: CORESH ranks datasets within the timeout", {
  skip_unless_live()
  out <- externalCoresh(ifn_query, limit = 5, timeout = 180)
  expect_gt(nrow(out), 0)
  expect_match(attr(out, "job_url"), "^https://alserglab.wustl.edu/coresh/load/")
  expect_equal(out$score_label[1], "% variance")
})
