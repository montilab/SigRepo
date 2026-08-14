#' Compare signatures from the SigRepo database
#'
#' @description Thin wrapper around
#' \code{OmicSignature::compare_omic_signatures()} that pulls signatures out of
#' the SigRepo database and runs a pairwise comparison across all of them at
#' once. Supply signatures by \code{signature_id} and/or \code{signature_name}
#' (fetched via \code{getSignature()}), or pass ready-made OmicSignature objects
#' directly through \code{omic_signatures}.
#'
#' The result is the matrix-based structure documented in
#' \code{?OmicSignature::compare_omic_signatures}: for \code{method =
#' "overlap"}, \code{jaccard}/\code{pvalue}/\code{counts} matrices per label
#' pairing; for the rank-based methods (\code{"ks_rank"}, \code{"ks_score"},
#' \code{"gsea"}), \code{score}/\code{pvalue} matrices.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#'   Required unless \code{omic_signatures} is supplied.
#' @param signature_ids Optional vector of database signature IDs to compare.
#' @param signature_names Optional vector of signature names to compare.
#' @param omic_signatures Optional named list of OmicSignature objects (or an
#'   OmicSignatureCollection) to compare directly, skipping the database fetch.
#' @param method Comparison method passed to
#'   \code{OmicSignature::compare_omic_signatures()}. One of \code{"overlap"},
#'   \code{"ks_rank"}, \code{"ks_score"}, or \code{"gsea"}. The rank-based
#'   methods require signatures that carry a difexp table.
#' @param verbose Logical; whether to print diagnostic messages while fetching
#'   signatures. Defaults to \code{FALSE}.
#' @param ... Additional arguments forwarded to
#'   \code{OmicSignature::compare_omic_signatures()} (e.g. \code{score_cutoff},
#'   \code{adj_p_cutoff}, \code{min_features}, \code{background},
#'   \code{label_pairing}).
#'
#' @return The list returned by
#'   \code{OmicSignature::compare_omic_signatures()}: \code{method},
#'   \code{comparisons}, \code{label_order}, and \code{background}.
#'
#' @examples
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "montilab", password = "sigrepo"
#' )
#' res <- SigRepo::compareSignatures(
#'   conn_handler = conn_handler,
#'   signature_ids = c(12, 34, 56),
#'   method = "overlap",
#'   min_features = 10
#' )
#' res$comparisons$level1_vs_level1$jaccard
#' }
#'
#' @export
compareSignatures <- function(
    conn_handler = NULL,
    signature_ids = NULL,
    signature_names = NULL,
    omic_signatures = NULL,
    method = c("overlap", "ks_rank", "ks_score", "gsea"),
    verbose = FALSE,
    ...) {

  method <- base::match.arg(method)

  if (base::is.null(omic_signatures)) {
    have_ids <- base::length(signature_ids) > 0 && !base::all(signature_ids %in% c("", NA))
    have_names <- base::length(signature_names) > 0 && !base::all(signature_names %in% c("", NA))
    if (base::is.null(conn_handler) || (!have_ids && !have_names)) {
      base::stop(
        "\nProvide 'conn_handler' and 'signature_ids' and/or 'signature_names', ",
        "or pass a list of OmicSignature objects via 'omic_signatures'.\n"
      )
    }
    omic_signatures <- getSignature(
      conn_handler = conn_handler,
      signature_id = signature_ids,
      signature_name = signature_names,
      verbose = verbose
    )
  }

  if (base::is.null(omic_signatures) || base::length(omic_signatures) < 2) {
    base::stop("\nAt least two signatures are required to compare.\n")
  }

  OmicSignature::compare_omic_signatures(
    sig_list1 = omic_signatures,
    method = method,
    ...
  )
}
