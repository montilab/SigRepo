#' @title searchExternal
#' @description Search public signature engines with a SigRepo signature.
#' The signature's features are translated into gene symbols (reference table
#' first, then biomaRt for Ensembl IDs) plus an organism code, and sent to one
#' or more of:
#' \itemize{
#'   \item \strong{Rummagene} (\url{https://rummagene.com}): ~1M gene sets mined
#'   from PMC supplementary tables. Fisher overlap, any organism.
#'   \item \strong{RummaGEO} (\url{https://rummageo.com}): up/down gene sets
#'   auto-computed from GEO RNA-seq studies. Fisher overlap, human and mouse.
#'   \item \strong{CORESH} (\url{https://alserglab.wustl.edu/coresh/}): GEO
#'   expression datasets ranked by how much variance the query explains as a
#'   coregulated block. Human and mouse; optional permutation p-values.
#' }
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied. Also enables the
#' reference-table symbol lookup.
#' @param signature_id A single SigRepo signature ID.
#' @param signature_name A single SigRepo signature name.
#' @param omic_signature An \code{OmicSignature} object, instead of fetching one.
#' @param source One or more of \code{"rummagene"}, \code{"rummageo"},
#' \code{"coresh"}. Defaults to all three.
#' @param direction Which features to send: \code{"combined"} (all),
#' \code{"up"} / \code{"down"} (by the sign of \code{score}), or \code{"both"}
#' (up and down as two separate queries, stacked with a \code{direction} column).
#' @param limit Maximum hits per source (per direction for \code{"both"}).
#' @param calculate_pvalues CORESH only: also compute permutation p-values
#' (about a minute instead of seconds).
#' @param timeout Seconds to wait for a CORESH job before giving up.
#' @param verbose Logical; print diagnostic messages. Default \code{TRUE}.
#'
#' @return For a single \code{source}, a data frame; for several, a named list of
#' data frames. Every data frame starts with the columns \code{source, direction,
#' rank, id, title, url, score, score_label, pvalue, adj_pvalue, n_overlap, n_set},
#' followed by service-specific extras, and carries attributes \code{query}
#' (the translated query that was sent) and \code{total_count}. CORESH frames
#' also carry \code{job_url}, a shareable results page.
#'
#' @export
#' @examples
#' \dontrun{
#' hits <- SigRepo::searchExternal(
#'   conn_handler = conn_handler,
#'   signature_name = "my_signature",
#'   source = "coresh"
#' )
#' attr(hits, "job_url")
#'
#' all_three <- SigRepo::searchExternal(omic_signature = my_omic_signature)
#' all_three$rummagene
#' }
searchExternal <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    source = c("rummagene", "rummageo", "coresh"),
    direction = c("combined", "up", "down", "both"),
    limit = 25,
    calculate_pvalues = FALSE,
    timeout = 120,
    verbose = TRUE
){

  source <- base::match.arg(source, choices = EXTERNAL_SOURCES, several.ok = TRUE)
  direction <- base::match.arg(direction)
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    base::stop("\n'limit' must be a positive integer.\n")
  }

  omic_signature <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    label = "signature",
    verbose = verbose
  )

  directions <- if (base::identical(direction, "both")) c("up", "down") else direction
  queries <- base::lapply(directions, function(d) {
    buildExternalQuery(omic_signature, direction = d, conn_handler = conn_handler, max_genes = 500, verbose = verbose)
  })
  base::names(queries) <- directions

  run_adapter <- function(src, query) {
    base::switch(
      src,
      rummagene = externalRummagene(query, limit = limit, timeout = timeout),
      rummageo  = externalRummageo(query, limit = limit, timeout = timeout),
      coresh    = externalCoresh(query, limit = limit, calculate_pvalues = calculate_pvalues, timeout = timeout)
    )
  }

  run_source <- function(src) {
    frames <- base::lapply(directions, function(d) {
      base::tryCatch(
        run_adapter(src, queries[[d]]),
        error = function(e) {
          if (base::length(source) == 1) {
            base::stop(e)
          }
          base::warning(base::sprintf("%s: %s", src, base::conditionMessage(e)), call. = FALSE)
          emptyExternalFrame(src, d)
        }
      )
    })
    base::names(frames) <- directions
    out <- rbindExternalFrames(frames)
    if (base::length(frames) == 1) {
      base::attr(out, "query") <- queries[[1]]
      base::attr(out, "total_count") <- base::attr(frames[[1]], "total_count")
      base::attr(out, "job_url") <- base::attr(frames[[1]], "job_url")
    } else {
      base::attr(out, "query") <- queries
      base::attr(out, "total_count") <- base::lapply(frames, base::attr, "total_count")
      base::attr(out, "job_url") <- base::lapply(frames, base::attr, "job_url")
    }
    out
  }

  results <- base::lapply(source, run_source)
  base::names(results) <- source
  if (base::length(source) == 1) results[[1]] else results

}
