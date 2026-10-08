############################################################
# Shared plumbing for the public signature search engines
# behind searchExternal(): Rummagene, RummaGEO and CORESH.
# Every network call goes through externalPostJson() /
# externalGetJson() so adapters can be tested offline by
# mocking those two functions.
############################################################

EXTERNAL_SOURCES <- c("rummagene", "rummageo", "coresh")

EXTERNAL_COLUMNS <- c(
  "source", "direction", "rank", "id", "title", "url",
  "score", "score_label", "pvalue", "adj_pvalue", "n_overlap", "n_set"
)

`%||%` <- function(a, b) if (base::is.null(a)) b else a

#' @title externalUrl
#' @description Base URL of an external search engine, overridable through
#' options so tests and a future dev instance can redirect it.
#' @param source One of "rummagene", "rummageo", "coresh".
#' @return A single URL string.
#' @keywords internal
#' @noRd
externalUrl <- function(source) {
  base::switch(
    source,
    rummagene = base::getOption("sigrepo.rummagene_url", "https://rummagene.com/graphql"),
    rummageo  = base::getOption("sigrepo.rummageo_url",  "https://rummageo.com/graphql"),
    coresh    = base::getOption("sigrepo.coresh_url",    "https://alserglab.wustl.edu/coresh-back"),
    base::stop(base::sprintf("Unknown external source '%s'.", source))
  )
}

#' @title externalPostJson
#' @description POST a JSON body and parse the JSON reply into nested lists.
#' @keywords internal
#' @noRd
externalPostJson <- function(url, body, timeout = 60) {
  res <- httr::POST(
    url, body = body, encode = "json",
    httr::content_type_json(), httr::timeout(timeout)
  )
  if (httr::status_code(res) >= 300) {
    base::stop(base::sprintf("Request to %s failed (HTTP %s).", url, httr::status_code(res)))
  }
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' @title externalGetJson
#' @description GET with query parameters and parse the JSON reply.
#' @keywords internal
#' @noRd
externalGetJson <- function(url, query = base::list(), timeout = 60) {
  res <- httr::GET(url, query = query, httr::timeout(timeout))
  if (httr::status_code(res) >= 300) {
    base::stop(base::sprintf("Request to %s failed (HTTP %s).", url, httr::status_code(res)))
  }
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' @title externalGraphql
#' @description Run one GraphQL document; a payload carrying `errors` is an R error.
#' @keywords internal
#' @noRd
externalGraphql <- function(url, query, variables = base::list(), timeout = 60) {
  payload <- externalPostJson(url, base::list(query = query, variables = variables), timeout = timeout)
  if (!base::is.null(payload$errors)) {
    msg <- base::tryCatch(payload$errors[[1]]$message, error = function(e) NULL) %||% "unknown error"
    base::stop(base::sprintf("GraphQL error from %s: %s", url, msg))
  }
  payload
}

#' @title emptyExternalFrame
#' @description A zero-row data frame with the common external columns.
#' @keywords internal
#' @noRd
emptyExternalFrame <- function(source, direction = "combined") {
  base::data.frame(
    source = base::character(), direction = base::character(), rank = base::integer(),
    id = base::character(), title = base::character(), url = base::character(),
    score = base::numeric(), score_label = base::character(),
    pvalue = base::numeric(), adj_pvalue = base::numeric(),
    n_overlap = base::integer(), n_set = base::integer(),
    stringsAsFactors = FALSE
  )
}

#' @title bindExternalHits
#' @description Turn a list of per-hit named lists into the common data frame:
#' common columns first (NA where a service has no value), service-specific
#' extras after, `rank` by position.
#' @keywords internal
#' @noRd
bindExternalHits <- function(source, direction, rows) {
  if (base::length(rows) == 0) {
    return(emptyExternalFrame(source, direction))
  }
  na_if_null <- function(x) if (base::is.null(x) || base::length(x) == 0) NA else x[[1]]
  all_cols <- base::unique(base::unlist(base::lapply(rows, base::names)))
  values <- base::lapply(all_cols, function(col) {
    base::unlist(base::lapply(rows, function(r) na_if_null(r[[col]])))
  })
  base::names(values) <- all_cols
  tbl <- base::as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
  for (col in base::setdiff(EXTERNAL_COLUMNS, c("source", "direction", "rank", base::colnames(tbl)))) {
    tbl[[col]] <- NA
  }
  tbl$source <- source
  tbl$direction <- direction
  tbl$rank <- base::seq_len(base::nrow(tbl))
  extras <- base::setdiff(base::colnames(tbl), EXTERNAL_COLUMNS)
  tbl <- tbl[, c(EXTERNAL_COLUMNS, extras), drop = FALSE]
  base::rownames(tbl) <- NULL
  tbl
}

#' @title rbindExternalFrames
#' @description rbind external frames whose extra columns may differ, filling
#' the gaps with NA.
#' @keywords internal
#' @noRd
rbindExternalFrames <- function(frames) {
  frames <- base::Filter(Negate(base::is.null), frames)
  if (base::length(frames) == 0) {
    return(emptyExternalFrame("external"))
  }
  all_cols <- base::unique(base::unlist(base::lapply(frames, base::colnames)))
  frames <- base::lapply(frames, function(f) {
    for (col in base::setdiff(all_cols, base::colnames(f))) f[[col]] <- base::rep(NA, base::nrow(f))
    f[, all_cols, drop = FALSE]
  })
  out <- base::do.call(base::rbind, frames)
  base::rownames(out) <- NULL
  out
}
