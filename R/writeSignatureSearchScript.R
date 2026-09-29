
# Fewest features per direction a LINCS query is written for. The LINCS
# connectivity score is a weighted KS statistic over an up and a down set, and
# on a handful of genes it is noise.
SIGNATURE_SEARCH_MIN_FEATURES <- 10L


#' Build the up and down query of a LINCS search from one signature
#'
#' Direction is the sign of \code{score}. \code{group_label} holds the
#' biological contrast (e.g. "Older", "Younger") and carries no direction.
#'
#' @param omic_signature A single \code{OmicSignature} object.
#'
#' @return A list with \code{signature_name}, \code{id_type} (the
#' \code{org.Hs.eg.db} keytype of the feature names) and the \code{up} and
#' \code{down} feature names, each ordered from the strongest score.
#'
#' @noRd
buildSignatureSearchQuery <- function(omic_signature) {

  metadata <- omic_signature$metadata
  signature_name <- base::as.character(metadata$signature_name)[1]

  clean_value <- function(value) {
    if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1])) {
      return("")
    }
    base::trimws(base::as.character(value[1]))
  }
  organism <- clean_value(metadata$organism)
  assay_type <- clean_value(metadata$assay_type)

  # LINCS profiles are of human cell lines. Mouse genes could be carried over
  # by orthology, but what that loses matters for a connectivity score.
  if (!base::identical(base::tolower(organism), "homo sapiens")) {
    base::stop(base::sprintf(
      "\nLINCS is built from human cell lines, so the signature must be of organism Homo sapiens. '%s' is %s.\n",
      signature_name,
      if (base::nzchar(organism)) organism else "of no recorded organism"
    ), call. = FALSE)
  }

  if (!base::identical(base::tolower(assay_type), "transcriptomics")) {
    base::stop(base::sprintf(
      "\nLINCS profiles are gene expression, so the signature must be of assay type transcriptomics. '%s' is %s.\n",
      signature_name,
      if (base::nzchar(assay_type)) assay_type else "of no recorded assay type"
    ), call. = FALSE)
  }

  signature_tbl <- omic_signature$signature
  missing_cols <- base::setdiff(c("feature_name", "score"), base::colnames(signature_tbl))
  if (base::length(missing_cols) > 0) {
    base::stop(base::sprintf(
      "\nSignature '%s' is missing the column(s) %s, which the query is built from.\n",
      signature_name,
      base::paste0("'", missing_cols, "'", collapse = ", ")
    ), call. = FALSE)
  }

  features <- base::trimws(base::as.character(signature_tbl$feature_name))
  scores <- base::suppressWarnings(base::as.numeric(signature_tbl$score))

  keep <- !base::is.na(features) & base::nzchar(features) & !base::is.na(scores) & scores != 0
  features <- features[keep]
  scores <- scores[keep]

  # Ensembl gene IDs may carry a version (ENSG00000126353.4) that
  # org.Hs.eg.db does not know.
  id_type <- "SYMBOL"
  if (base::length(features) > 0 && base::all(base::grepl("^ENSG[0-9]+(\\.[0-9]+)?$", features))) {
    id_type <- "ENSEMBL"
    features <- base::sub("\\.[0-9]+$", "", features)
  }

  strongest_first <- base::order(base::abs(scores), decreasing = TRUE)
  features <- features[strongest_first]
  scores <- scores[strongest_first]

  up <- base::unique(features[scores > 0])
  down <- base::unique(features[scores < 0])

  # A feature scored in both directions would push the score both ways.
  in_both <- base::intersect(up, down)
  up <- base::setdiff(up, in_both)
  down <- base::setdiff(down, in_both)

  if (base::length(up) < SIGNATURE_SEARCH_MIN_FEATURES || base::length(down) < SIGNATURE_SEARCH_MIN_FEATURES) {
    base::stop(base::sprintf(
      "\nA LINCS query needs at least %d features in each direction, by the sign of 'score'. '%s' has %d up and %d down.\n",
      SIGNATURE_SEARCH_MIN_FEATURES,
      signature_name,
      base::length(up),
      base::length(down)
    ), call. = FALSE)
  }

  base::list(
    signature_name = signature_name,
    id_type = id_type,
    up = up,
    down = down
  )

}


#' Write a character vector as the R code of an assignment
#'
#' \code{deparse()} quotes and escapes every value, so nothing taken from a
#' signature can end up in the script as code.
#'
#' @noRd
deparseAssignment <- function(name, value) {
  code <- base::deparse(value, width.cutoff = 80L)
  code[1] <- base::paste(name, "<-", code[1])
  code
}


#' @title writeSignatureSearchScript
#' @description Write an R script that runs a LINCS connectivity search for
#' one signature with Bioconductor's \code{signatureSearch}: which
#' perturbations produce the signature's expression pattern, and which reverse
#' it.
#'
#' SigRepo does not run the search, and does not depend on
#' \code{signatureSearch}, whose reference database is about 2.5 GB. The script
#' is run on a machine that has \code{signatureSearch},
#' \code{ExperimentHub}, \code{rhdf5}, \code{AnnotationDbi},
#' \code{org.Hs.eg.db} and \code{ggplot2} installed, and stops with the install command when one
#' of them is missing.
#'
#' The query is built from the signature's feature set: features with a
#' positive \code{score} are the up set and features with a negative
#' \code{score} the down set, each ordered from the strongest score. A feature
#' scored in both directions is left out. The script maps the features to
#' Entrez IDs, keeps the genes LINCS measured, and searches with the strongest
#' \code{n_genes} of each direction. It saves the result table, one row per
#' perturbation and cell line, as a CSV, and a plot of the 10 perturbations
#' that most resemble the signature and the 10 that most reverse it as a PDF.
#'
#' Only human transcriptomics signatures with at least 10 features in each
#' direction can be searched; any other signature is an error.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id A single SigRepo signature ID.
#' @param signature_name A single SigRepo signature name.
#' @param omic_signature A single \code{OmicSignature} object.
#' @param file Path of the script to write (required).
#' @param n_genes Number of genes of each direction to search with. Defaults
#' to \code{150}. It is a setting at the top of the script and can be changed
#' there.
#' @param overwrite Logical; whether to replace \code{file} when it exists.
#' Defaults to \code{FALSE}.
#' @param verbose Logical; whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'
#' @return The path of the script, invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' script <- SigRepo::writeSignatureSearchScript(
#'   conn_handler = conn_handler,
#'   signature_name = "LLFS_Aging_Gene_2023",
#'   file = "LLFS_Aging_Gene_2023_lincs.R"
#' )
#'
#' # Runs the search and saves LLFS_Aging_Gene_2023_lincs_results.csv and
#' # LLFS_Aging_Gene_2023_lincs_top_hits.pdf
#' source(script)
#' }
#'
writeSignatureSearchScript <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    file,
    n_genes = 150,
    overwrite = FALSE,
    verbose = TRUE
) {

  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)

  if (base::missing(file) || !base::is.character(file) || base::length(file) != 1L ||
      base::is.na(file) || !base::nzchar(base::trimws(file))) {
    base::stop("\n'file' is required: the path of the script to write, e.g. file = \"lincs_search.R\".\n", call. = FALSE)
  }

  if (!(base::is.numeric(n_genes) && base::length(n_genes) == 1L && !base::is.na(n_genes) &&
        n_genes >= SIGNATURE_SEARCH_MIN_FEATURES && n_genes == base::round(n_genes))) {
    base::stop(base::sprintf(
      "\n'n_genes' must be a single whole number of at least %d.\n",
      SIGNATURE_SEARCH_MIN_FEATURES
    ), call. = FALSE)
  }

  if (!base::dir.exists(base::dirname(file))) {
    base::stop(base::sprintf("\nThe directory of 'file' does not exist: %s\n", base::dirname(file)), call. = FALSE)
  }

  if (base::file.exists(file) && !base::isTRUE(overwrite)) {
    base::stop(base::sprintf("\n'%s' already exists. Use overwrite = TRUE to replace it.\n", file), call. = FALSE)
  }

  omic_signature <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    label = "the signature",
    verbose = verbose
  )

  query <- buildSignatureSearchQuery(omic_signature)

  # Not base::system.file(): devtools::load_all() can only redirect the
  # unqualified call to the source tree's inst/.
  template <- system.file("templates", "signatureSearch_lincs.R", package = "SigRepo")
  if (!base::nzchar(template)) {
    base::stop("\nThe script template is missing from the installed SigRepo package.\n", call. = FALSE)
  }

  file_stem <- base::gsub("[^A-Za-z0-9._-]+", "_", query$signature_name)
  output_file <- base::paste0(file_stem, "_lincs_results.csv")
  plot_file <- base::paste0(file_stem, "_lincs_top_hits.pdf")

  script <- c(
    "# LINCS connectivity search for a SigRepo signature, with signatureSearch",
    "# (https://bioconductor.org/packages/signatureSearch).",
    "#",
    base::sprintf(
      "# Written by SigRepo::writeSignatureSearchScript(), SigRepo %s, on %s.",
      base::as.character(utils::packageVersion("SigRepo")),
      base::format(base::Sys.Date())
    ),
    "#",
    "# Run it with source() or Rscript. The first run downloads the LINCS",
    "# reference database (about 2.5 GB); the search itself takes a few minutes.",
    "",
    "## The signature ----",
    "# Features by the sign of their score, strongest first.",
    deparseAssignment("signature_name", query$signature_name),
    deparseAssignment("id_type", query$id_type),
    deparseAssignment("up_ids", query$up),
    deparseAssignment("down_ids", query$down),
    "",
    "## Settings ----",
    "# Genes of each direction to search with.",
    deparseAssignment("n_genes", base::as.numeric(n_genes)),
    "# Where the result table and the plot of the strongest hits are saved,",
    "# relative to the working directory.",
    deparseAssignment("output_file", output_file),
    deparseAssignment("plot_file", plot_file),
    base::readLines(template, warn = FALSE)
  )

  base::writeLines(script, con = file)

  SigRepo::verbose(base::sprintf(
    "Script written to %s: %d up and %d down features of '%s'. Run it with source() where signatureSearch is installed.\n",
    file,
    base::length(query$up),
    base::length(query$down),
    query$signature_name
  ))

  base::invisible(file)

}
