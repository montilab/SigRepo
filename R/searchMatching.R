#' Decide which values a set of search terms matches
#'
#' The rule searchSignature() applies to every text field, with nothing for the
#' caller to set: a term that is somebody's whole value matches only that
#' value; a term that is nobody's whole value matches every value containing
#' it. Issue #230 asked for partial matching; applying it unconditionally would
#' have been wrong, because 975 of the repository's 6,565 sample types contain
#' another sample type as a substring, so picking "M cell" from a dropdown
#' would have quietly returned "41-M cell" and "58CrPM cell" as well.
#'
#' Terms are compared literally. 97 sample types and 34 phenotypes contain a
#' regex metacharacter, so treating a term as a pattern would make
#' "BE(2)-M17 cell" match "BE2-M17 cell" and not itself: copy a value out of
#' the results table, search for it, find nothing.
#'
#' Values and terms are trimmed and lower-cased first, matching what
#' lookup_table_sql() has always done.
#'
#' @param values The column's values, as returned by the database.
#' @param terms The caller's search terms. Several are OR-ed: each decides on
#'   its own whether it was exact.
#'
#' @return A logical vector as long as `values`. NA values match nothing.
#'
#' @noRd
search_match_rows <- function(values, terms){

  matched <- base::rep(FALSE, base::length(values))
  if(base::length(terms) == 0 || base::length(values) == 0) return(matched)

  prepared <- base::trimws(base::tolower(base::as.character(values)))
  present <- !base::is.na(prepared)

  for(term in base::trimws(base::tolower(base::as.character(terms)))){
    if(base::is.na(term)) next

    exact <- present & prepared == term

    # An exact match wins for this term; only fall back to substring when the
    # term is nobody's whole value. ####
    hits <- if(base::any(exact)){
      exact
    }else{
      present & base::grepl(term, prepared, fixed = TRUE)
    }

    matched <- matched | hits
  }

  matched
}
