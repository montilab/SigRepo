############################################################
# Ortholog translation behind compareSignatures(map_orthologs = TRUE)
############################################################

#' @title translateFeatureTable
#' @description Rewrite a feature table (a signature's `signature` data frame
#' or its `difexp`) onto another identifier space through a query to target
#' mapping. A feature with several targets becomes one row per target, with
#' every other column copied; features without a target are dropped. Rows
#' that then share a target inside one group (or in the whole table when
#' `group_col` is `NULL` or absent) collapse to a single row: the one with
#' the largest absolute `prefer_col` for `"max_abs"`, the smallest for
#' `"min"`, or simply the first when `prefer_col` is not in the table.
#' Queries match case-insensitively and ignore surrounding whitespace.
#' @param tbl Data frame with a `feature_col` column.
#' @param mapping Data frame with `query` and `target` columns, possibly
#'   several targets per query.
#' @param feature_col Name of the feature column in `tbl`.
#' @param group_col Name of the group column, or `NULL`.
#' @param prefer_col Column that decides which duplicate survives.
#' @param prefer `"max_abs"` (largest absolute value wins) or `"min"`
#'   (smallest value wins).
#' @return A list: `table` (the rewritten data frame, same columns as `tbl`),
#'   `n_input` (rows in), `n_unmapped` (input rows with no target) and
#'   `n_output` (rows out).
#' @keywords internal
#' @noRd
translateFeatureTable <- function(tbl, mapping, feature_col, group_col = NULL, prefer_col, prefer = c("max_abs", "min")) {
  prefer <- base::match.arg(prefer)
  norm <- function(x) base::tolower(base::trimws(base::as.character(x)))

  map <- base::data.frame(
    key = norm(mapping$query),
    target = base::as.character(mapping$target),
    stringsAsFactors = FALSE
  )
  map <- map[!base::is.na(map$key) & !base::is.na(map$target) & base::nzchar(map$target), , drop = FALSE]
  map <- map[!base::duplicated(base::paste(map$key, map$target, sep = "\r")), , drop = FALSE]

  rows <- base::data.frame(row = base::seq_len(base::nrow(tbl)), key = norm(tbl[[feature_col]]), stringsAsFactors = FALSE)
  n_unmapped <- base::sum(!(rows$key %in% map$key))

  joined <- base::merge(rows, map, by = "key")
  out <- tbl[joined$row, , drop = FALSE]
  out[[feature_col]] <- joined$target

  # Which rows compete: same target, same group.
  dup_key <- joined$target
  if (!base::is.null(group_col) && group_col %in% base::colnames(out)) {
    dup_key <- base::paste(dup_key, base::as.character(out[[group_col]]), sep = "\r")
  }

  # Order so the preferred row of each duplicate set comes first, then keep it.
  if (prefer_col %in% base::colnames(out)) {
    value <- base::suppressWarnings(base::as.numeric(out[[prefer_col]]))
    rank_value <- if (prefer == "max_abs") -base::abs(value) else value
    ord <- base::order(rank_value, joined$row, na.last = TRUE)
  } else {
    ord <- base::order(joined$row)
  }
  out <- out[ord, , drop = FALSE]
  dup_key <- dup_key[ord]
  source_row <- joined$row[ord]
  keep <- !base::duplicated(dup_key)
  out <- out[keep, , drop = FALSE]
  source_row <- source_row[keep]

  # Back to the input's order so the result reads like the original table.
  out <- out[base::order(source_row, out[[feature_col]]), , drop = FALSE]
  base::rownames(out) <- NULL

  base::list(
    table = out,
    n_input = base::nrow(tbl),
    n_unmapped = n_unmapped,
    n_output = base::nrow(out)
  )
}
