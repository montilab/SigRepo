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

#' Default plot title: the test(s) behind the hyps, e.g. "KS test"
#' @noRd
hypeRTestTitle <- function(hyps) {
  tests <- base::vapply(hyps, hypeRInfoValue, base::character(1), key = "Test", USE.NAMES = FALSE)
  tests <- base::unique(tests[!base::is.na(tests)])
  if (base::length(tests) == 0) {
    return("Enrichment")
  }
  known <- c(hypergeometric = "Hypergeometric test", kstest = "KS test", fgsea = "GSEA (fgsea)")
  base::paste(base::ifelse(tests %in% base::names(known), known[tests], tests), collapse = ", ")
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
#' @param title Plot title; defaults to the test that was run
#' (\code{"Hypergeometric test"}, \code{"KS test"} or \code{"GSEA (fgsea)"}).
#'
#' @details With \code{signature_key = TRUE} (the default) and several
#' signatures, each signature's columns sit together under its code
#' (\code{S1}, \code{S2}, ...), labelled by group or direction at 45 degrees,
#' and the caption lists what each code stands for. A single signature is named
#' in the subtitle, with its group labels horizontal while they fit. With
#' \code{signature_key = FALSE} or \code{query_labels}, the labels are drawn
#' at 45 degrees and the left margin grows when they reach past the geneset
#' labels. Save with
#' \code{ggplot2::ggsave()}. A readable size is roughly
#' width = 5 + 0.085 x (longest geneset label, in characters) + 1.2 x (queries)
#' inches, clamped to 7-20, and height = 1.6 + 0.30 x (genesets) + 0.05 x
#' (longest query name, in characters) inches, clamped to 3.2-22.
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
    signature_key = TRUE,
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
  if (base::is.null(title)) {
    title <- hypeRTestTitle(hyps)
  }

  dots <- hypeRDotData(hyp_obj, val = val, pval = pval, fdr = fdr, top = top, size_by = size_by,
                       signature_key = signature_key, query_labels = query_labels, abrv = abrv)
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

  # With the signature key, each signature is a facet named by its code (listed
  # in the caption) and its queries are labelled by group; a single signature
  # is named in the subtitle instead. Otherwise the query labels are drawn as they are.
  info <- hypeRQueryInfo(hyps)
  use_key <- base::isTRUE(signature_key) && base::is.null(query_labels)
  signatures <- hypeRQuerySignatures(info)
  groups <- hypeRQueryGroups(info)
  query_levels <- base::levels(dots$query_label)
  facet <- use_key && base::nrow(signatures$key) > 1 && base::any(base::nzchar(groups))
  single_signature <- use_key && base::nrow(signatures$key) == 1
  axis_text <- if (facet || single_signature) {
    # The code (facet strip) or the subtitle already names the signature.
    stats::setNames(groups, query_levels)
  } else if (use_key) {
    stats::setNames(base::ifelse(base::nzchar(groups), groups, query_levels), query_levels)
  } else {
    stats::setNames(query_levels, query_levels)
  }

  dots$signature_code <- base::factor(dots$signature_code, levels = signatures$key$signature_code)
  plot <- ggplot2::ggplot(dots, ggplot2::aes(x = .data$query_label, y = .data$label_abrv)) +
    (if (base::identical(size_by, "none")) ggplot2::geom_point(point_aes, size = 3) else ggplot2::geom_point(point_aes)) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::theme_minimal()

  if (facet) {
    # Free x scales drop unused levels, so a blank layer keeps every query's column.
    all_queries <- base::data.frame(
      query_label = base::factor(query_levels, levels = query_levels),
      label_abrv = base::factor(base::levels(dots$label_abrv)[1], levels = base::levels(dots$label_abrv)),
      signature_code = base::factor(signatures$code, levels = signatures$key$signature_code)
    )
    plot <- plot +
      ggplot2::geom_blank(data = all_queries) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$signature_code), scales = "free_x", space = "free_x", switch = "x") +
      ggplot2::scale_x_discrete(labels = function(x) base::unname(axis_text[x]))
  } else {
    plot <- plot + ggplot2::scale_x_discrete(drop = FALSE, labels = function(x) base::unname(axis_text[x]))
  }
  if (use_key && base::nrow(signatures$key) > 1) {
    plot <- plot + ggplot2::labs(caption = base::paste(
      base::sprintf("%s = %s", signatures$key$signature_code, signatures$key$signature_name), collapse = "\n"
    ))
  } else if (use_key) {
    plot <- plot + ggplot2::labs(subtitle = signatures$key$signature_name)
  }

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

  # A single signature's group labels stay horizontal while they fit (about 40
  # characters across all columns, e.g. Old/Young or up/down); any other labels
  # are angled at 45 degrees, and the left margin grows by however far the
  # longest one reaches past the geneset labels (estimated at 0.6 em per
  # character) so it is not cut off. Legends hang from the top of the panel so a
  # tall stack is not clipped.
  horizontal <- single_signature &&
    base::length(axis_text) * (base::max(base::nchar(axis_text)) + 2) <= 40
  char_pt <- 0.6 * 8.8
  overhang <- if (horizontal) {
    0
  } else {
    base::max(base::nchar(axis_text)) * char_pt * base::sqrt(0.5) - base::max(base::nchar(base::levels(dots$label_abrv))) * char_pt
  }
  plot + ggplot2::theme(
    axis.text.x = if (horizontal) ggplot2::element_text() else ggplot2::element_text(angle = 45, hjust = 1),
    legend.justification = "top",
    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5 + base::max(0, overhang)),
    plot.caption = ggplot2::element_text(hjust = 0, lineheight = 1.1),
    plot.caption.position = "plot",
    strip.placement = "outside",
    strip.text = ggplot2::element_text(face = "bold"),
    panel.spacing.x = grid::unit(6, "pt")
  )
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
#' @param title Plot title; defaults to the test that was run
#' (\code{"Hypergeometric test"}, \code{"KS test"} or \code{"GSEA (fgsea)"}).
#' The geneset label is the first line of the subtitle.
#'
#' @return A \code{ggplot}.
#'
#' @examples
#' \dontrun{
#' SigRepo::plotHypeREnrichment(gsea, "HALLMARK_MYC_TARGETS_V1", query = "LLFS_Aging_Gene_2023")
#' }
#'
#' @export
plotHypeREnrichment <- function(hyp_obj, geneset, query = NULL, title = NULL) {
  requireHypeRGgplot("plotHypeREnrichment")
  selected <- hypeRSelectHyp(hyp_obj, query, geneset)
  hyp <- selected$hyp
  plot_title <- if (base::is.null(title)) hypeRTestTitle(base::list(hyp)) else title

  if (base::identical(hypeRInfoValue(hyp, "Test"), "hypergeometric")) {
    members <- hypeRGenesetMembers(hyp, geneset)
    row <- hyp$data[hyp$data$label == geneset, , drop = FALSE]
    subtitle <- if (base::nrow(row) > 0) {
      base::sprintf("%s\n%s\noverlap = %s, p = %s, FDR = %s", geneset, selected$name, row$overlap[1],
                    formatHypeRNumber(row$pval[1]), formatHypeRNumber(row$fdr[1]))
    } else {
      base::sprintf("%s\n%s\nnot in the result table", geneset, selected$name)
    }
    return(hypeR::ggvenn(hyp$args$signature, members, "Query", "Geneset", plot_title) +
             ggplot2::labs(subtitle = subtitle))
  }

  enrichment <- hypeREnrichmentData(hyp_obj, geneset, query = selected$name)
  summary <- enrichment$summary
  # fgsea's up and down queries share one ranking, so the subtitle names the
  # ranking; the sign of ES and NES gives the direction.
  subtitle <- base::paste0(geneset, "\n", summary$ranking, "\nES = ", formatHypeRNumber(summary$es))
  if (base::is.na(summary$pval)) {
    subtitle <- base::paste0(subtitle, ", not in the result table")
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

#' Whether any pair of genesets reaches the similarity cutoff, as hypeR computes it
#' @noRd
hypeRAnyMapEdge <- function(members, similarity_metric, similarity_cutoff) {
  if (base::length(members) < 2) {
    return(FALSE)
  }
  for (i in base::seq_len(base::length(members) - 1L)) {
    for (j in (i + 1L):base::length(members)) {
      shared <- base::length(base::intersect(members[[i]], members[[j]]))
      similarity <- if (base::identical(similarity_metric, "jaccard_similarity")) {
        shared / base::length(base::union(members[[i]], members[[j]]))
      } else {
        shared / base::min(base::length(members[[i]]), base::length(members[[j]]))
      }
      if (similarity > 0 && similarity >= similarity_cutoff) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Title a visNetwork map: the test (or `title`) above the query name
#' @noRd
titleHypeRMap <- function(map, name, hyp, title) {
  map$x$main <- base::list(
    text = if (base::is.null(title)) hypeRTestTitle(base::list(hyp)) else title,
    style = "font-family:Helvetica, Arial, sans-serif;font-weight:bold;font-size:18px;text-align:center;"
  )
  map$x$submain <- base::list(
    text = name,
    style = "font-family:Helvetica, Arial, sans-serif;font-size:13px;text-align:center;"
  )
  map
}

#' One query's map, or NULL with a warning when hypeR could not draw it
#' @noRd
plotHypeRMapOne <- function(name, hyp, type, val, pval, fdr, top, similarity_metric, similarity_cutoff, title = NULL) {
  if (base::identical(type, "hmap") && !methods::is(hyp$args$genesets, "rgsets")) {
    base::stop("\ntype = \"hmap\" needs rgsets genesets (e.g. hypeR::hyperdb_rgsets()).\n")
  }
  data <- hyp$data
  keep <- data$pval <= pval & data$fdr <= fdr
  data <- utils::head(data[!base::is.na(keep) & keep, , drop = FALSE], top)
  if (base::nrow(data) == 0) {
    base::warning(base::sprintf("No genesets pass the cutoffs for '%s'.", name), call. = FALSE)
    return(NULL)
  }
  if (base::nrow(data) < 2) {
    base::warning(base::sprintf(
      "Only one geneset passes the cutoffs for '%s'; a map needs at least two.", name
    ), call. = FALSE)
    return(NULL)
  }
  if (base::identical(type, "hmap")) {
    return(titleHypeRMap(hypeR::hyp_hmap(hyp, pval = pval, fdr = fdr, val = val, top = top), name, hyp, title))
  }
  members <- hyp$args$genesets$genesets[data$label]
  if (!hypeRAnyMapEdge(members, similarity_metric, similarity_cutoff)) {
    base::warning(base::sprintf(
      "No geneset pair in '%s' reaches similarity_cutoff = %s; lower it to draw a map.",
      name, similarity_cutoff
    ), call. = FALSE)
    return(NULL)
  }
  map <- hypeR::hyp_emap(hyp, similarity_metric = similarity_metric, similarity_cutoff = similarity_cutoff,
                         pval = pval, fdr = fdr, val = val, top = top)
  titleHypeRMap(map, name, hyp, title)
}

#' Enrichment or hierarchy map of runHypeR() results
#'
#' @description Draws hypeR's enrichment map (\code{hypeR::hyp_emap()}, genesets
#' linked by shared genes) or hierarchy map (\code{hypeR::hyp_hmap()}, needs
#' \code{rgsets} genesets), after checking the cases where hypeR would fail:
#' no geneset passing the cutoffs, or no pair of genesets reaching
#' \code{similarity_cutoff}. Those return \code{NULL} with a warning instead of
#' an error.
#'
#' @inheritParams hypeRDotData
#' @param val \code{"fdr"} (default) or \code{"pval"}: the value that colours
#' the map's nodes. Unlike \code{hypeRDotData()}, it does not rank genesets or
#' choose \code{top}.
#' @param type \code{"emap"} (default) or \code{"hmap"}.
#' @param query Query name; with \code{NULL} a \code{multihyp} of several queries
#' gives a named list with one map per query.
#' @param top The first \code{top} rows of each result table after the
#' cutoffs, in table order (not chosen by \code{val}). Default \code{25}.
#' @param similarity_metric,similarity_cutoff Passed to \code{hypeR::hyp_emap()}.
#' @param title Map title; defaults to the test that was run
#' (\code{"Hypergeometric test"}, \code{"KS test"} or \code{"GSEA (fgsea)"}),
#' with the query name below it.
#'
#' @return A \code{visNetwork} widget, \code{NULL} (with a warning) when there is
#' nothing to draw, or a named list of those for several queries.
#'
#' @examples
#' \dontrun{
#' SigRepo::plotHypeRMap(hyp, query = "LLFS_Aging_Gene_2023 | Group1", fdr = 0.05)
#' }
#'
#' @export
plotHypeRMap <- function(
    hyp_obj,
    type = c("emap", "hmap"),
    query = NULL,
    val = c("fdr", "pval"),
    pval = 1,
    fdr = 1,
    top = 25,
    similarity_metric = c("jaccard_similarity", "overlap_similarity"),
    similarity_cutoff = 0.2,
    title = NULL
) {
  type <- base::match.arg(type)
  val <- base::match.arg(val)
  similarity_metric <- base::match.arg(similarity_metric)
  checkHypeRNumber(top, "top", 1)
  checkHypeRNumber(similarity_cutoff, "similarity_cutoff", 0)
  checkHypeRNumber(pval, "pval", 0)
  checkHypeRNumber(fdr, "fdr", 0)

  hyps <- hypeRResultHyps(hyp_obj)
  draw <- function(name, hyp) {
    plotHypeRMapOne(name, hyp, type, val, pval, fdr, top, similarity_metric, similarity_cutoff, title)
  }
  if (!base::is.null(query) || base::length(hyps) == 1L) {
    selected <- hypeRSelectHyp(hyp_obj, query)
    return(draw(selected$name, selected$hyp))
  }
  stats::setNames(base::lapply(base::names(hyps), function(name) draw(name, hyps[[name]])), base::names(hyps))
}
