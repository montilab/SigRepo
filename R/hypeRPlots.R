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

#' Format a number for a plot subtitle
#' @noRd
formatHypeRNumber <- function(x) base::format(base::signif(x, 2))

#' Enrichment figure for one geneset of a runHypeR() result
#'
#' @description For kstest and fgsea results, the running enrichment score
#' along the ranking with the geneset's hits marked below it (leading edge
#' highlighted). For hypergeometric results, hypeR's Venn diagram of the query
#' and the geneset. Both are drawn from the result itself, so they work for any
#' geneset without \code{runHypeR(plotting = TRUE)} storing every plot.
#'
#' @inheritParams hypeREnrichmentData
#' @param title Plot title; defaults to the geneset label.
#'
#' @return A \code{ggplot}.
#'
#' @examples
#' \dontrun{
#' SigRepo::plotHypeREnrichment(gsea, "HALLMARK_MYC_TARGETS_V1", query = "LLFS_Aging_Gene_2023 | up")
#' }
#'
#' @export
plotHypeREnrichment <- function(hyp_obj, geneset, query = NULL, title = NULL) {
  requireHypeRGgplot("plotHypeREnrichment")
  selected <- hypeRSelectHyp(hyp_obj, query)
  hyp <- selected$hyp
  plot_title <- if (base::is.null(title)) geneset else title

  if (base::identical(hypeRInfoValue(hyp, "Test"), "hypergeometric")) {
    members <- hypeRGenesetMembers(hyp, geneset)
    row <- hyp$data[hyp$data$label == geneset, , drop = FALSE]
    subtitle <- if (base::nrow(row) > 0) {
      base::sprintf("%s\noverlap = %s, p = %s, FDR = %s", selected$name, row$overlap[1],
                    formatHypeRNumber(row$pval[1]), formatHypeRNumber(row$fdr[1]))
    } else {
      base::sprintf("%s\nnot in results after cutoffs", selected$name)
    }
    return(hypeR::ggvenn(hyp$args$signature, members, "Query", "Geneset", plot_title) +
             ggplot2::labs(subtitle = subtitle))
  }

  enrichment <- hypeREnrichmentData(hyp_obj, geneset, query = selected$name)
  summary <- enrichment$summary
  subtitle <- base::paste0(selected$name, "\nES = ", formatHypeRNumber(summary$es))
  if (base::is.na(summary$pval)) {
    subtitle <- base::paste0(subtitle, ", not in results after cutoffs")
  } else {
    if (base::identical(summary$test, "fgsea")) {
      subtitle <- base::paste0(subtitle, ", NES = ", formatHypeRNumber(summary$nes))
    }
    subtitle <- base::paste0(subtitle, ", p = ", formatHypeRNumber(summary$pval), ", FDR = ", formatHypeRNumber(summary$fdr))
  }
  negated <- base::identical(summary$test, "kstest") && base::identical(summary$direction, "down")

  curve <- enrichment$curve
  ticks <- enrichment$ticks
  score_range <- base::range(c(curve$running_score, 0))
  tick_height <- base::diff(score_range) * 0.08
  ticks$y_start <- score_range[1] - tick_height * 0.5
  ticks$y_end <- score_range[1] - tick_height * 1.5
  ticks$hit_group <- base::factor(base::ifelse(ticks$leading_edge, "Leading edge", "Other hits"),
                                  levels = c("Leading edge", "Other hits"))

  plot <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$position, y = .data$running_score)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::geom_line(colour = "#1F4E79") +
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(x = .data$position, xend = .data$position, y = .data$y_start, yend = .data$y_end, colour = .data$hit_group),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("Leading edge" = "#D04A02", "Other hits" = "#6C8AA5"), name = NULL, drop = FALSE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = plot_title, subtitle = subtitle,
      x = if (negated) "Rank in ranking (negated scores)" else "Rank in ranking",
      y = "Running enrichment score"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")
  if (!base::is.na(summary$es_position)) {
    plot <- plot + ggplot2::geom_vline(xintercept = summary$es_position, linetype = "dashed", colour = "grey40")
  }
  plot
}
