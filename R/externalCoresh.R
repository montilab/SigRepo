############################################################
# CORESH: ranks ~40k human and ~40k mouse GEO expression
# datasets by how much variance a query gene set explains as
# a coregulated block (Sukhov et al., NAR 2025,
# doi 10.1093/nar/gkaf372). The web app's JSON backend has
# no auth: submit a job, poll it, fetch the ranking.
# Endpoints verified against the live service on 2026-10-07.
############################################################

#' @title coreshJobUrl
#' @description Shareable results page for a CORESH job id.
#' @keywords internal
#' @noRd
coreshJobUrl <- function(job_id) {
  base::sprintf("https://alserglab.wustl.edu/coresh/load/%s", job_id)
}

#' @title externalCoresh
#' @description Submit a translated query to CORESH, poll until the ranking is
#' ready, and return the top `limit` datasets. Attributes `job_id` and
#' `job_url` point at the shareable results page.
#' @keywords internal
#' @noRd
externalCoresh <- function(query, limit = 25, calculate_pvalues = FALSE, timeout = 120) {
  if (base::is.na(query$organism_code)) {
    base::stop(base::sprintf(
      "CORESH only supports Homo sapiens and Mus musculus; '%s' is '%s'.",
      query$signature_name, query$organism
    ))
  }
  base_url <- externalUrl("coresh")
  pv_flag <- if (base::isTRUE(calculate_pvalues)) "true" else "false"
  poll <- base::getOption("sigrepo.coresh_poll_seconds", 2)

  submitted <- externalPostJson(
    base::paste0(base_url, "/submit-genes"),
    base::list(
      organism = query$organism_code,
      dbType = query$organism_code,
      genes = base::as.list(query$genes),
      calculatePvalues = base::isTRUE(calculate_pvalues)
    ),
    timeout = 60
  )
  job_id <- submitted$ID
  if (base::is.null(job_id) || !base::nzchar(job_id)) {
    base::stop("CORESH did not return a job id.")
  }
  deadline <- base::Sys.time() + timeout
  timeout_msg <- base::sprintf(
    "CORESH job %s did not finish within %s s. Results will appear at %s when done.",
    job_id, timeout, coreshJobUrl(job_id)
  )

  repeat {
    status <- externalGetJson(
      base::paste0(base_url, "/check-job"),
      base::list(jobid = job_id, organism = query$organism_code, calculatePvalues = pv_flag)
    )
    if (base::identical(status$failed, "True") || base::isTRUE(status$failed)) {
      base::stop(base::sprintf("CORESH job %s failed.", job_id))
    }
    progress <- base::suppressWarnings(base::as.numeric(status$progress %||% 0))
    if (base::isTRUE(progress >= 100)) break
    if (base::Sys.time() > deadline) base::stop(timeout_msg)
    base::Sys.sleep(poll)
  }
  repeat {
    files <- externalGetJson(
      base::paste0(base_url, "/check-result-files"),
      base::list(jobid = job_id, calculatePvalues = pv_flag)
    )
    if (base::isTRUE(files$files_ready)) break
    if (base::Sys.time() > deadline) base::stop(timeout_msg)
    base::Sys.sleep(poll)
  }

  ranking <- externalGetJson(base::paste0(base_url, "/get-ranking-result"), base::list(jobid = job_id))
  out <- parseCoreshRanking(ranking, query, limit = limit)
  base::attr(out, "job_id") <- job_id
  base::attr(out, "job_url") <- coreshJobUrl(job_id)
  out
}

#' @title parseCoreshRanking
#' @description Head of a get-ranking-result reply -> the common frame.
#' `log10Padj` arrives as the string "NA" when p-values were not requested.
#' @keywords internal
#' @noRd
parseCoreshRanking <- function(rows, query, limit = 25) {
  rows <- rows %||% base::list()
  total <- base::length(rows)
  rows <- utils::head(rows, base::as.integer(limit))
  hits <- base::lapply(rows, function(r) {
    log10_padj <- base::suppressWarnings(base::as.numeric(r$log10Padj %||% NA))
    base::list(
      id = r$gseId,
      title = r$gseTitle,
      url = base::sprintf("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s", r$gseId),
      score = r$pctVar,
      score_label = "% variance",
      pvalue = NA_real_,
      adj_pvalue = if (base::is.na(log10_padj)) NA_real_ else 10^log10_padj,
      n_overlap = NA_integer_,
      n_set = r$size,
      gpl = r$gplId,
      summary = r$gseSummary,
      positive_words = r$positiveWords,
      negative_words = r$negativeWords,
      log10_padj = log10_padj
    )
  })
  out <- bindExternalHits("coresh", query$direction, hits)
  base::attr(out, "total_count") <- total
  base::attr(out, "query") <- query
  out
}
