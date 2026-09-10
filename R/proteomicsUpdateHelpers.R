############################################################
# Helpers behind updateProteomicsFeatureSet()
############################################################

#' @title uniprotGeneNameUrl
#' @description The UniProt REST stream that lists every entry for one
#' organism with its primary gene name. This is the source of real gene
#' symbols: the id-mapping archive the updater used before has no gene-name
#' column at all, and its second column (UniProtKB-ID, e.g. `1433B_HUMAN`)
#' was being stored as if it were one.
#' @param taxid NCBI taxonomy id (`organisms.prot_organism_taxid`).
#' @return A single URL.
#' @keywords internal
#' @noRd
uniprotGeneNameUrl <- function(taxid) {
  base::sprintf(
    "https://rest.uniprot.org/uniprotkb/stream?query=organism_id:%s&fields=accession,gene_primary&format=tsv&compressed=true",
    base::trimws(base::as.character(taxid))
  )
}

#' @title parseUniprotGeneNames
#' @description Parse the TSV returned by [uniprotGeneNameUrl()] (columns
#' `Entry` and `Gene Names (primary)`) into the (feature_name, gene_symbol)
#' shape the reference tables use. Entries without a primary gene name get
#' a blank symbol -- never the accession. Where UniProt lists several
#' primary names ("GENE1; GENE2") the first is kept.
#' @param x Either a path to the (optionally gzipped) TSV file, or the TSV
#'   text itself.
#' @return A data frame with `feature_name` and `gene_symbol`, one row per
#'   accession.
#' @keywords internal
#' @noRd
parseUniprotGeneNames <- function(x) {
  is_path <- base::length(x) == 1 && !base::grepl("\t", x, fixed = TRUE) && base::file.exists(x)
  con <- if (is_path) base::gzfile(x, open = "rt") else base::textConnection(x)
  on.exit(base::close(con), add = TRUE)
  tbl <- utils::read.delim(
    con, sep = "\t", quote = "", header = TRUE, check.names = FALSE,
    na.strings = character(0), colClasses = "character", stringsAsFactors = FALSE
  )
  entry_col <- base::intersect(c("Entry", "Accession", "accession"), base::names(tbl))[1]
  name_col <- base::intersect(c("Gene Names (primary)", "gene_primary"), base::names(tbl))[1]
  if (base::is.na(entry_col) || base::is.na(name_col)) {
    base::stop("The UniProt download does not have the expected 'Entry' and 'Gene Names (primary)' columns.\n")
  }

  feature_name <- base::trimws(tbl[[entry_col]])
  gene_symbol <- base::trimws(base::sub(";.*$", "", tbl[[name_col]]))
  gene_symbol[base::is.na(gene_symbol)] <- ""

  keep <- base::nzchar(feature_name) & !base::duplicated(feature_name)
  base::data.frame(
    feature_name = feature_name[keep],
    gene_symbol = gene_symbol[keep],
    stringsAsFactors = FALSE
  )
}

#' @title proteomicsSymbolIsPlaceholder
#' @description TRUE where a stored proteomics "gene symbol" is really a
#' UniProt entry name (`1433Z_MOUSE`) or a copy of the accession itself --
#' the two things the previous updater and the shipped CSV wrote into the
#' column. Such values are not symbols, so a refresh may replace or clear
#' them; a genuine stored symbol is still never blanked.
#' @param symbols Stored gene_symbol values.
#' @param feature_names The matching UniProt accessions.
#' @param organism_code UniProt organism mnemonic (`organisms.prot_organism_code`,
#'   e.g. "HUMAN"), which forms the entry-name suffix.
#' @return Logical vector, FALSE for blank or NA symbols.
#' @keywords internal
#' @noRd
proteomicsSymbolIsPlaceholder <- function(symbols, feature_names, organism_code) {
  sym <- base::toupper(base::trimws(base::as.character(symbols)))
  acc <- base::toupper(base::trimws(base::as.character(feature_names)))
  suffix <- base::paste0("_", base::toupper(base::trimws(base::as.character(organism_code))))
  out <- !base::is.na(sym) & base::nzchar(sym) & (sym == acc | base::endsWith(sym, suffix))
  out[base::is.na(out)] <- FALSE
  out
}
