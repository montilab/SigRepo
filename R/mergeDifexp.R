#' Merge the difexp tables of several signatures into one matrix
#'
#' @description
#' Fetches signatures from the SigRepo database and/or takes OmicSignature
#' objects supplied directly, and lines their difexp tables up into a single
#' numeric matrix with one row per feature and one column per signature. The
#' value in each cell is that signature's \code{value_col} for that feature,
#' and \code{NA} where the signature's difexp table has no such feature.
#'
#' This is the "signature compendium" shape that signature-by-signature
#' correlation, clustering, PCA and reference databases for tools such as
#' signatureSearch expect. The thresholded signature tables are not used;
#' only the full difexp tables are, since those cover every tested feature.
#'
#' Signatures are assembled the same way as in \code{compareSignatures()}:
#' ids and names are resolved with \code{searchSignature()} and fetched with
#' \code{getSignature()}, supplied objects come after them, and requests that
#' cannot be honoured are reported in a warning and left out. Columns are
#' named by the list names of supplied objects (their metadata
#' \code{signature_name} when unnamed) and by the database
#' \code{signature_name} of fetched ones.
#'
#' A difexp table often lists one feature several times, once per probe.
#' Such rows are collapsed to one value per feature before merging, chosen by
#' \code{collapse}. Signatures without a difexp table, or whose difexp table
#' has no \code{value_col} or \code{feature_col} column, are dropped with a
#' warning naming them; it is an error when that leaves nothing to merge.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#'   Required whenever signatures are requested by id or name.
#' @param signature_ids Database signature ids to fetch.
#' @param signature_names Database signature names to fetch.
#' @param omic_signatures An OmicSignature object, a list of OmicSignature
#'   objects, or an OmicSignatureCollection to include directly.
#' @param value_col The difexp column whose values fill the matrix. Defaults
#'   to \code{"score"}; \code{"logfc"} is the other common choice.
#' @param feature_col The difexp column holding feature identifiers, used as
#'   row names.
#' @param features \code{"union"} (the default) keeps every feature found in
#'   any table, with \code{NA} where a table lacks it; \code{"intersect"} keeps
#'   only features present in every table.
#' @param collapse How to reduce several rows of the same feature within one
#'   table to a single value: \code{"max_abs"} (the default) keeps the value
#'   with the largest absolute size, \code{"mean"} averages them, and
#'   \code{"first"} keeps the first row as stored.
#' @param verbose Logical; whether to print diagnostic messages while
#'   resolving and fetching signatures. Defaults to \code{FALSE}.
#'
#' @return A numeric matrix with features as row names and signatures as
#'   column names, in the order the signatures were assembled.
#'
#' @seealso \code{compareSignatures()}, \code{getSignature()}.
#'
#' @examples
#' # The bundled example signatures need no database
#' utils::data("omic_signature_1", "omic_signature_2", "omic_signature_3", package = "SigRepo")
#'
#' m <- SigRepo::mergeDifexp(
#'   omic_signatures = list(v1 = omic_signature_1, v2 = omic_signature_2, v3 = omic_signature_3)
#' )
#' dim(m)
#' stats::cor(m, use = "pairwise.complete.obs")
#'
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "your_username", password = "your_password"
#' )
#'
#' m <- SigRepo::mergeDifexp(
#'   conn_handler = conn_handler,
#'   signature_ids = c(12, 34),
#'   signature_names = "my_signature",
#'   value_col = "logfc",
#'   features = "intersect"
#' )
#' }
#'
#' @export
mergeDifexp <- function(
    conn_handler = NULL,
    signature_ids = NULL,
    signature_names = NULL,
    omic_signatures = NULL,
    value_col = "score",
    feature_col = "feature_name",
    features = c("union", "intersect"),
    collapse = c("max_abs", "mean", "first"),
    verbose = FALSE) {

  features <- base::match.arg(features)
  collapse <- base::match.arg(collapse)

  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)

  signature_ids <- cleanCompareRequest(signature_ids)
  signature_names <- cleanCompareRequest(signature_names)

  needs_database <- base::length(c(signature_ids, signature_names)) > 0
  if (needs_database && base::is.null(conn_handler)) {
    base::stop("\n'conn_handler' is required to fetch signatures by 'signature_ids' or 'signature_names'.\n")
  }

  sig_list <- resolveCompareSignatureList(
    conn_handler = conn_handler,
    signature_ids = signature_ids,
    signature_names = signature_names,
    omic_signatures = omic_signatures,
    arg_suffix = "",
    verbose = verbose
  )
  if (base::length(sig_list) == 0) {
    base::stop("\nProvide signatures through 'signature_ids', 'signature_names' and/or 'omic_signatures'.\n")
  }

  # One named numeric vector per usable signature: feature -> value.
  columns <- base::list()
  dropped <- base::character()
  for (i in base::seq_along(sig_list)) {
    sig_name <- base::names(sig_list)[i]
    difexp <- sig_list[[i]]$difexp
    if (base::is.null(difexp)) {
      dropped <- c(dropped, base::sprintf("'%s' has no difexp table", sig_name))
      next
    }
    missing_cols <- base::setdiff(c(feature_col, value_col), base::colnames(difexp))
    if (base::length(missing_cols) > 0) {
      dropped <- c(dropped, base::sprintf(
        "'%s' has no %s column in its difexp table", sig_name,
        base::paste(base::sprintf("'%s'", missing_cols), collapse = " or ")
      ))
      next
    }
    columns[[sig_name]] <- collapseDifexpValues(
      feature = base::as.character(difexp[[feature_col]]),
      value = base::as.numeric(difexp[[value_col]]),
      collapse = collapse
    )
  }

  if (base::length(dropped) > 0) {
    if (base::length(columns) == 0) {
      base::stop(
        "\nNone of the signatures has a difexp table with '", feature_col, "' and '", value_col, "' columns:\n",
        base::paste(dropped, collapse = "\n"), "\n"
      )
    }
    base::warning(
      "Some signatures were left out of the merged matrix:\n",
      base::paste(dropped, collapse = "\n"),
      call. = FALSE
    )
  }

  feature_sets <- base::lapply(columns, base::names)
  all_features <- if (features == "union") {
    base::Reduce(base::union, feature_sets)
  } else {
    base::Reduce(base::intersect, feature_sets)
  }

  SigRepo::verbose(base::sprintf(
    "Merging %d signature(s) over %d feature(s) (%s) by '%s'.\n",
    base::length(columns), base::length(all_features), features, value_col
  ))

  merged <- base::vapply(columns, function(v) base::unname(v[all_features]), base::numeric(base::length(all_features)))
  merged <- base::matrix(
    merged,
    nrow = base::length(all_features),
    dimnames = base::list(all_features, base::names(columns))
  )
  merged
}


#' Reduce a difexp table to one value per feature
#'
#' @param feature Character vector of feature identifiers, one per row.
#' @param value Numeric vector of the values to keep, one per row.
#' @param collapse "max_abs", "mean" or "first".
#' @return A named numeric vector, one entry per distinct feature, in order of
#'   first appearance. Rows with an empty or missing feature are ignored.
#' @noRd
collapseDifexpValues <- function(feature, value, collapse) {
  keep <- !base::is.na(feature) & feature != ""
  feature <- feature[keep]
  value <- value[keep]
  groups <- base::split(value, base::factor(feature, levels = base::unique(feature)))
  reducer <- switch(collapse,
    max_abs = function(v) {
      v <- v[!base::is.na(v)]
      if (base::length(v) == 0) NA_real_ else v[base::which.max(base::abs(v))]
    },
    mean = function(v) if (base::all(base::is.na(v))) NA_real_ else base::mean(v, na.rm = TRUE),
    first = function(v) v[1]
  )
  base::vapply(groups, reducer, base::numeric(1))
}
