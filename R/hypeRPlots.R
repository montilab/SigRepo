#' @importFrom rlang .data
NULL

#' Error unless ggplot2 is installed
#' @noRd
requireHypeRGgplot <- function(caller) {
  if (!base::requireNamespace("ggplot2", quietly = TRUE)) {
    base::stop(base::sprintf("\nPackage 'ggplot2' is required for %s(). Please install it first.\n", caller))
  }
  base::invisible(NULL)
}

#' Dot plot of runHypeR() results across queries
#'
#' @description One column per query and one row per geneset, for a
#' \code{hyp} or a \code{multihyp} of any length. Every query shows the same
#' \code{top} genesets (the best by \code{val} across all queries); a blank cell
#' means the geneset did not pass the cutoffs for that query. Significance is
#' drawn as -log10 values on a plain scale, so axes and legends stay readable
#' whatever the ggplot2 version. See \code{hypeRDotData()} for the data.
#'
#' @inheritParams hypeRDotData
#' @param color_by \code{"significance"} (default: -log10 of \code{val}) or
#' \code{"score"} (NES for fgsea, the enrichment score for kstest) on a
#' diverging scale centred at 0. \code{"score"} is an error for hypergeometric
#' results.
#' @param title Optional plot title.
#'
#' @details Save with \code{ggplot2::ggsave()}. A readable size is roughly
#' width = 5 + 0.085 x (longest geneset label, in characters) + 1.2 x (queries)
#' inches, clamped to 7-20, and height = 1.6 + 0.30 x (genesets) inches, clamped
#' to 3.2-22.
#'
#' @return A \code{ggplot}. When nothing passes the cutoffs, a blank plot that
#' says so.
#'
#' @examples
#' \dontrun{
#' SigRepo::plotHypeRDots(hyp, fdr = 0.05)
#' SigRepo::plotHypeRDots(gsea, color_by = "score")
#' }
#'
#' @export
plotHypeRDots <- function(
    hyp_obj,
    val = c("fdr", "pval"),
    pval = 1,
    fdr = 1,
    top = 20,
    color_by = c("significance", "score"),
    size_by = c("geneset", "overlap", "none"),
    query_labels = NULL,
    abrv = 50,
    title = NULL
) {
  requireHypeRGgplot("plotHypeRDots")
  val <- base::match.arg(val)
  color_by <- base::match.arg(color_by)
  size_by <- base::match.arg(size_by)

  hyps <- hypeRResultHyps(hyp_obj)
  tests <- base::vapply(hyps, hypeRInfoValue, base::character(1), key = "Test", USE.NAMES = FALSE)
  if (base::identical(color_by, "score") && !base::all(tests %in% c("kstest", "fgsea"))) {
    base::stop("\ncolor_by = \"score\" needs kstest or fgsea results.\n")
  }

  dots <- hypeRDotData(hyp_obj, val = val, pval = pval, fdr = fdr, top = top, size_by = size_by,
                       query_labels = query_labels, abrv = abrv)
  if (base::nrow(dots) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No genesets pass the cutoffs") +
        ggplot2::theme_void() +
        ggplot2::labs(title = title)
    )
  }

  point_aes <- if (base::identical(size_by, "none")) {
    ggplot2::aes(colour = .data[[if (base::identical(color_by, "score")) "score" else "significance"]])
  } else {
    ggplot2::aes(colour = .data[[if (base::identical(color_by, "score")) "score" else "significance"]], size = .data$size)
  }

  plot <- ggplot2::ggplot(dots, ggplot2::aes(x = .data$query_label, y = .data$label_abrv)) +
    (if (base::identical(size_by, "none")) ggplot2::geom_point(point_aes, size = 3) else ggplot2::geom_point(point_aes)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::theme_minimal()

  plot <- if (base::identical(color_by, "score")) {
    plot + ggplot2::scale_colour_gradient2(
      low = "#2166AC", mid = "#D9D9D9", high = "#B2182B", midpoint = 0,
      name = if (base::all(tests == "fgsea")) "NES" else "Score"
    )
  } else {
    plot + ggplot2::scale_colour_gradient(
      low = "#114357", high = "#E53935",
      name = if (base::identical(val, "fdr")) "-log10(FDR)" else "-log10(p)"
    )
  }
  if (!base::identical(size_by, "none")) {
    plot <- plot + ggplot2::scale_size_continuous(name = if (base::identical(size_by, "geneset")) "Geneset size" else "Overlap")
  }
  if (base::nlevels(dots$query_label) > 4) {
    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  plot
}
