############################################################
# Helpers behind updateTranscriptomicsFeatureSet()
############################################################

#' @title symbolAttributeForOrganism
#' @description The BioMart attribute that actually carries gene symbols for an
#' organism's Ensembl dataset. `hgnc_symbol` is accepted by every dataset but
#' is only populated for human: for mouse it comes back empty for essentially
#' every gene, which is how a symbol refresh wiped the mouse reference table.
#' @param organism Organism name as stored in the `organisms` table.
#' @return A single attribute name for `biomaRt::getBM()`.
#' @keywords internal
#' @noRd
symbolAttributeForOrganism <- function(organism) {
  key <- base::tolower(base::trimws(base::as.character(organism)))
  base::switch(
    key,
    "homo sapiens" = "hgnc_symbol",
    "mus musculus" = "mgi_symbol",
    "external_gene_name"
  )
}

#' @title partitionFeatureUpdates
#' @description Decide what a reference-feature refresh should do with each
#' stored feature, given the freshly fetched (feature_name, gene_symbol) table.
#' Feature names match case-insensitively; symbols compare case-insensitively.
#' A blank fetched symbol never replaces a stored one -- the row is merely
#' refreshed -- so a source that lacks symbols cannot erase the ones we have.
#' @param db_tbl Data frame of stored features: `feature_name`, `gene_symbol`.
#' @param fetched_tbl Data frame of fetched features: `feature_name`, `gene_symbol`.
#' @return A list of data frames, each with `feature_name` and `gene_symbol`:
#'   `refresh` (stored rows to stamp with the new version, symbol unchanged),
#'   `resymbol` (stored rows whose symbol should become the fetched value),
#'   `add` (fetched rows not stored yet), and `archive` (stored rows absent
#'   from the fetch). Every stored row lands in exactly one of `refresh`,
#'   `resymbol`, or `archive`.
#' @keywords internal
#' @noRd
partitionFeatureUpdates <- function(db_tbl, fetched_tbl) {
  norm <- function(x) base::tolower(base::trimws(base::as.character(x)))
  is_blank <- function(x) base::is.na(x) | !base::nzchar(base::trimws(base::as.character(x)))

  db <- base::data.frame(
    feature_name = base::as.character(db_tbl$feature_name),
    gene_symbol = base::as.character(db_tbl$gene_symbol),
    stringsAsFactors = FALSE
  )
  fetched <- base::data.frame(
    feature_name = base::as.character(fetched_tbl$feature_name),
    gene_symbol = base::as.character(fetched_tbl$gene_symbol),
    stringsAsFactors = FALSE
  )
  db_key <- norm(db$feature_name)
  fetched_key <- norm(fetched$feature_name)
  fetched <- fetched[!base::duplicated(fetched_key), , drop = FALSE]
  fetched_key <- fetched_key[!base::duplicated(fetched_key)]

  hit <- base::match(db_key, fetched_key)
  in_fetch <- !base::is.na(hit)
  new_symbol <- fetched$gene_symbol[hit]
  fetched_blank <- is_blank(new_symbol)
  same_symbol <- !fetched_blank & !is_blank(db$gene_symbol) & norm(new_symbol) == norm(db$gene_symbol)
  same_symbol[base::is.na(same_symbol)] <- FALSE

  resymbol_idx <- in_fetch & !fetched_blank & !same_symbol
  refresh_idx <- in_fetch & !resymbol_idx
  archive_idx <- !in_fetch

  pick <- function(tbl, idx) {
    out <- tbl[idx, c("feature_name", "gene_symbol"), drop = FALSE]
    base::rownames(out) <- NULL
    out
  }

  base::list(
    refresh = pick(db, refresh_idx),
    resymbol = base::data.frame(
      feature_name = db$feature_name[resymbol_idx],
      gene_symbol = new_symbol[resymbol_idx],
      stringsAsFactors = FALSE
    ),
    add = pick(fetched, !(fetched_key %in% db_key)),
    archive = pick(db, archive_idx)
  )
}

#' @title ensemblReleaseNumber
#' @description Pull the numeric release out of a biomaRt version label such
#' as "Ensembl Genes 116", or out of the plain number stored in
#' `organisms.biomart_version`. The previous regex, `(.*?)([1-9]{1,})`, gave
#' "116" under R 4.4 but "" under R 4.5, and the releases were then compared
#' as strings ("116" < "99" is TRUE).
#' @param x Character vector of version labels.
#' @return Integer vector; NA where no release number is present.
#' @keywords internal
#' @noRd
ensemblReleaseNumber <- function(x) {
  x <- base::as.character(x)
  digits <- base::sub("^.*?([0-9]+)\\s*$", "\\1", x, perl = TRUE)
  out <- base::suppressWarnings(base::as.integer(digits))
  out[base::is.na(x) | !base::grepl("[0-9]", x)] <- NA_integer_
  out
}
