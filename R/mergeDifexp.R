#' Merge the difexp tables of several signatures into one matrix
#'
#' @description
#' Fetches signatures from the SigRepo database and/or takes OmicSignature
#' objects supplied directly, and lines their difexp tables up into a single
#' numeric matrix with one row per feature and one column per signature. The
#' value in each cell is that signature's \code{value_col} for that feature,
#' and \code{NA} where the signature's difexp table has no such feature.
#'
#' The matrix comes with a metadata table describing its columns: one row per
#' signature, in column order, built from each signature's metadata. The pair
#' plays the role of the assay and \code{colData} of a SummarizedExperiment,
#' but both are plain base R objects (a matrix and a data frame) so they can
#' be written out and used outside R without any Bioconductor dependency.
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
#'   with the largest absolute size, \code{"min"} keeps the smallest value,
#'   \code{"mean"} averages them, and \code{"first"} keeps the first row as
#'   stored. Use \code{"min"} with a p-value column such as \code{"p_value"}
#'   or \code{"adj_p"}, where the smallest value is the most significant;
#'   \code{"max_abs"} would keep the least significant probe there.
#' @param verbose Logical; whether to print diagnostic messages while
#'   resolving and fetching signatures. Defaults to \code{FALSE}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{matrix}}{A numeric matrix with features as row names and
#'       signatures as column names, in the order the signatures were
#'       assembled.}
#'     \item{\code{metadata}}{A data frame with one row per column of
#'       \code{matrix}, in the same order, whose row names are the column
#'       names of \code{matrix}. It starts with \code{signature_name} and
#'       \code{signature_id} (the database id, \code{NA} for objects supplied
#'       directly), followed by one column for every scalar field found in
#'       any signature's metadata (\code{organism}, \code{phenotype},
#'       \code{platform}, \code{PMID}, cutoffs, ...), \code{NA} where a
#'       signature lacks the field. Metadata fields holding several values,
#'       such as \code{keywords}, are collapsed into one string separated by
#'       \code{"; "}; nested list fields such as \code{others} are left out.
#'       The last column, \code{n_features}, counts the non-\code{NA} cells
#'       of that signature's column.}
#'   }
#'
#' @seealso \code{compareSignatures()}, \code{getSignature()}.
#'
#' @examples
#' # The bundled example signatures need no database
#' utils::data("omic_signature_1", "omic_signature_2", "omic_signature_3", package = "SigRepo")
#'
#' merged <- SigRepo::mergeDifexp(
#'   omic_signatures = list(v1 = omic_signature_1, v2 = omic_signature_2, v3 = omic_signature_3)
#' )
#' dim(merged$matrix)
#' merged$metadata[, c("signature_name", "organism", "phenotype", "n_features")]
#' stats::cor(merged$matrix, use = "pairwise.complete.obs")
#'
#' \dontrun{
#' conn_handler <- SigRepo::newConnHandler(
#'   dbname = "sigrepo", host = "localhost", port = 3306,
#'   user = "your_username", password = "your_password"
#' )
#'
#' merged <- SigRepo::mergeDifexp(
#'   conn_handler = conn_handler,
#'   signature_ids = c(12, 34),
#'   signature_names = "my_signature",
#'   value_col = "logfc",
#'   features = "intersect"
#' )
#' merged$metadata$signature_id
#'
#' # Adjusted p-values: keep the most significant probe of a duplicated feature
#' p <- SigRepo::mergeDifexp(
#'   conn_handler = conn_handler,
#'   signature_ids = c(12, 34),
#'   value_col = "adj_p",
#'   collapse = "min"
#' )$matrix
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
    collapse = c("max_abs", "min", "mean", "first"),
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

  resolved <- resolveCompareSignatureList(
    conn_handler = conn_handler,
    signature_ids = signature_ids,
    signature_names = signature_names,
    omic_signatures = omic_signatures,
    arg_suffix = "",
    verbose = verbose,
    with_ids = TRUE
  )
  sig_list <- resolved$signatures
  if (base::length(sig_list) == 0) {
    base::stop("\nProvide signatures through 'signature_ids', 'signature_names' and/or 'omic_signatures'.\n")
  }

  # One named numeric vector per usable signature: feature -> value. `kept`
  # remembers which signatures made it, so their metadata can be lined up.
  columns <- base::list()
  kept <- base::integer()
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
    columns[[base::length(columns) + 1L]] <- collapseDifexpValues(
      feature = base::as.character(difexp[[feature_col]]),
      value = base::as.numeric(difexp[[value_col]]),
      collapse = collapse
    )
    base::names(columns)[base::length(columns)] <- sig_name
    kept <- c(kept, i)
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

  metadata <- difexpSignatureMetadata(
    signatures = sig_list[kept],
    signature_ids = resolved$ids[kept],
    n_features = base::colSums(!base::is.na(merged))
  )
  base::rownames(metadata) <- base::make.unique(base::colnames(merged))

  base::list(matrix = merged, metadata = metadata)
}


#' Build the signature-level metadata table of a merged matrix
#'
#' Flattens each signature's metadata list into one row: atomic fields of
#' length one are kept as they are, longer atomic vectors are pasted with
#' "; ", empty fields become NA and anything else (nested lists such as
#' `others`) is left out. A column's type is numeric or logical when every
#' present value is, and character otherwise.
#'
#' @param signatures List of OmicSignature objects, one per matrix column.
#' @param signature_ids Character vector of database ids, NA where unknown.
#' @param n_features Integer vector of non-NA cells per matrix column.
#' @return A data frame with one row per signature, columns
#'   `signature_name`, `signature_id`, the flattened metadata fields in order
#'   of first appearance, and `n_features`.
#' @noRd
difexpSignatureMetadata <- function(signatures, signature_ids, n_features) {
  flat <- base::lapply(signatures, function(sig) {
    md <- sig$metadata
    if (!base::is.list(md)) {
      return(base::list())
    }
    md <- md[base::vapply(md, function(v) base::is.atomic(v) && !base::is.null(v), base::logical(1))]
    base::lapply(md, function(v) {
      if (base::length(v) == 0) NA else if (base::length(v) == 1) v else base::paste(v, collapse = "; ")
    })
  })

  fields <- base::unique(base::unlist(base::lapply(flat, base::names), use.names = FALSE))
  fields <- c("signature_name", base::setdiff(fields, c("signature_name", "signature_id")))

  columns <- base::lapply(fields, function(field) {
    values <- base::lapply(flat, function(row) if (field %in% base::names(row)) row[[field]] else NA)
    present <- values[!base::vapply(values, function(v) base::is.na(v), base::logical(1))]
    if (base::all(base::vapply(present, base::is.numeric, base::logical(1)))) {
      base::vapply(values, function(v) if (base::is.na(v)) NA_real_ else base::as.numeric(v), base::numeric(1))
    } else if (base::all(base::vapply(present, base::is.logical, base::logical(1)))) {
      base::vapply(values, function(v) if (base::is.na(v)) NA else base::as.logical(v), base::logical(1))
    } else {
      base::vapply(values, function(v) if (base::is.na(v)) NA_character_ else base::as.character(v), base::character(1))
    }
  })
  base::names(columns) <- fields

  metadata <- base::data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
  base::cbind(
    metadata[, "signature_name", drop = FALSE],
    signature_id = base::suppressWarnings(base::as.integer(signature_ids)),
    metadata[, base::setdiff(fields, "signature_name"), drop = FALSE],
    n_features = base::as.integer(n_features),
    stringsAsFactors = FALSE
  )
}


#' Reduce a difexp table to one value per feature
#'
#' @param feature Character vector of feature identifiers, one per row.
#' @param value Numeric vector of the values to keep, one per row.
#' @param collapse "max_abs", "min", "mean" or "first".
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
    min = function(v) if (base::all(base::is.na(v))) NA_real_ else base::min(v, na.rm = TRUE),
    mean = function(v) if (base::all(base::is.na(v))) NA_real_ else base::mean(v, na.rm = TRUE),
    first = function(v) v[1]
  )
  base::vapply(groups, reducer, base::numeric(1))
}
