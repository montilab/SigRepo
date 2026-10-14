
## Packages ----
# signatureSearch and its reference data are Bioconductor packages that SigRepo
# does not install. Nothing is installed from here either: the script stops
# and says what is missing.
required_packages <- c("signatureSearch", "ExperimentHub", "rhdf5", "AnnotationDbi", "org.Hs.eg.db", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages, then run this script again:\n",
    "  install.packages(\"BiocManager\")\n",
    "  BiocManager::install(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))",
    call. = FALSE
  )
}

# Attached, not only called as signatureSearch::, because the search annotates
# its hits with data sets that R finds only in an attached package.
suppressPackageStartupMessages(library(signatureSearch))

## LINCS reference database ----
# LINCS L1000 moderated z-scores (ExperimentHub EH3226). The first run
# downloads about 2.5 GB into the ExperimentHub cache; later runs reuse it.
lincs_db <- ExperimentHub::ExperimentHub()[["EH3226"]]
lincs_gene_ids <- as.character(rhdf5::h5read(lincs_db, "rownames"))

## Query ----
# LINCS is keyed on Entrez IDs. Map each direction, keep the genes LINCS
# measured, and take the strongest n_genes of what is left.
min_genes <- 10

to_lincs_entrez <- function(ids) {
  entrez <- suppressMessages(AnnotationDbi::mapIds(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = ids,
    column = "ENTREZID",
    keytype = id_type,
    multiVals = "first"
  ))
  entrez <- unique(unname(entrez[!is.na(entrez)]))
  entrez[entrez %in% lincs_gene_ids]
}

upset <- to_lincs_entrez(up_ids)
downset <- to_lincs_entrez(down_ids)

# Two features can map to the same gene. One that ends up in both directions
# is left out of both.
in_both <- intersect(upset, downset)
upset <- utils::head(setdiff(upset, in_both), n_genes)
downset <- utils::head(setdiff(downset, in_both), n_genes)

message(sprintf(
  "'%s': %d up and %d down features, of which %d up and %d down genes are in LINCS and used as the query.",
  signature_name, length(up_ids), length(down_ids), length(upset), length(downset)
))

if (length(upset) < min_genes || length(downset) < min_genes) {
  stop(
    "The LINCS score needs at least ", min_genes, " genes in each direction, and only ",
    length(upset), " up and ", length(downset), " down are in LINCS.",
    call. = FALSE
  )
}

## Search ----
# NCS is the normalized connectivity score of one perturbation in one cell
# line; NCSct summarizes a perturbation across cell lines. Positive scores
# mean the perturbation resembles the signature, negative that it reverses it.
query <- signatureSearch::qSig(
  query = list(upset = upset, downset = downset),
  gess_method = "LINCS",
  refdb = lincs_db
)
search <- signatureSearch::gess_lincs(query, sortby = "NCSct", tau = FALSE)
hits <- as.data.frame(signatureSearch::result(search))

## Results ----
utils::write.csv(hits, output_file, row.names = FALSE)
message(sprintf("%d LINCS signatures scored. Results saved to %s", nrow(hits), output_file))

print(utils::head(hits[, intersect(c("pert", "cell", "type", "NCS", "NCSct"), colnames(hits))], 20))

## Plot ----
# The 10 perturbations that most resemble the signature, then the 10 that most
# reverse it, by NCSct. Each coloured point is one cell line's NCS; the black
# points summarize the tumor and the normal cell lines. The results are
# already saved, so a plot that cannot be drawn is reported and not an error.
n_plotted <- 10
per_perturbation <- hits[!duplicated(hits$pert), ]
resembling <- utils::head(per_perturbation$pert[order(per_perturbation$NCSct, decreasing = TRUE)], n_plotted)
reversing <- utils::head(per_perturbation$pert[order(per_perturbation$NCSct)], n_plotted)

plotted <- tryCatch({
  top_hits_plot <- suppressWarnings(
    signatureSearch::gess_res_vis(hits, drugs = unique(c(resembling, reversing)), col = "NCS")
  ) +
    ggplot2::labs(
      title = signature_name,
      subtitle = "LINCS perturbations that most resemble the signature (left) and most reverse it (right)",
      x = "Perturbation",
      y = "Normalized connectivity score (NCS)"
    )
  ggplot2::ggsave(plot_file, top_hits_plot, width = 11, height = 7)
  TRUE
}, error = function(e) {
  message("The plot could not be drawn: ", conditionMessage(e))
  FALSE
})
if (plotted) {
  message(sprintf("Plot saved to %s", plot_file))
}
