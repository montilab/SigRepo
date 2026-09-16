#' Hyps of a hypeR result as a named list, one element per query
#'
#' A multihyp keeps its names; a single hyp is named from its SigRepo
#' provenance (see hypeRSingleQueryName()).
#' @noRd
hypeRResultHyps <- function(hyp_obj) {
  if (methods::is(hyp_obj, "multihyp")) {
    return(hyp_obj$data)
  }
  if (methods::is(hyp_obj, "hyp")) {
    return(stats::setNames(base::list(hyp_obj), hypeRSingleQueryName(hyp_obj)))
  }
  base::stop("\n'hyp_obj' must be a hypeR hyp or multihyp object.\n")
}

#' One info value of a hyp as a string, or NA when absent or empty
#' @noRd
hypeRInfoValue <- function(hyp, key) {
  value <- hyp$info[[key]]
  if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1]) || !base::nzchar(value[1])) {
    return(NA_character_)
  }
  base::as.character(value[1])
}

#' Query name for a single hyp: its signature name, group label and direction
#' joined by " | ", or "query" when none is recorded
#' @noRd
hypeRSingleQueryName <- function(hyp) {
  parts <- c(
    hypeRInfoValue(hyp, "SigRepo Signature Name"),
    hypeRInfoValue(hyp, "Group Label"),
    hypeRInfoValue(hyp, "SigRepo Direction")
  )
  parts <- parts[!base::is.na(parts)]
  if (base::length(parts) == 0) "query" else base::paste(parts, collapse = " | ")
}

#' One row per query: name and SigRepo provenance
#' @noRd
hypeRQueryInfo <- function(hyps) {
  value <- function(key) base::vapply(hyps, hypeRInfoValue, base::character(1), key = key, USE.NAMES = FALSE)
  base::data.frame(
    query = base::names(hyps),
    signature_id = value("SigRepo Signature ID"),
    signature_name = value("SigRepo Signature Name"),
    group_label = value("Group Label"),
    direction = value("SigRepo Direction"),
    test = value("Test"),
    stringsAsFactors = FALSE
  )
}

#' Signature of each query (its SigRepo name, or the query name when none is
#' recorded) and its code, "S1", "S2", ... in order of first appearance
#' @noRd
hypeRQuerySignatures <- function(info) {
  signature <- base::ifelse(base::is.na(info$signature_name), info$query, info$signature_name)
  signatures <- base::unique(signature)
  base::list(
    code = base::paste0("S", base::match(signature, signatures)),
    key = base::data.frame(
      signature_code = base::paste0("S", base::seq_along(signatures)), signature_name = signatures,
      stringsAsFactors = FALSE
    )
  )
}

#' What sets a query apart within its signature: group label and direction,
#' e.g. "Old", "up" or "Subtype_A up"; "" when neither is recorded
#' @noRd
hypeRQueryGroups <- function(info) {
  base::vapply(base::seq_len(base::nrow(info)), function(i) {
    parts <- c(info$group_label[i], info$direction[i])
    base::paste(parts[!base::is.na(parts)], collapse = " ")
  }, base::character(1))
}

#' Display labels for queries: signature code and group ("S1 | Old"), the group
#' alone for a single signature, the full names, or a user function of the info
#' @noRd
hypeRQueryLabels <- function(info, query_labels, signature_key = TRUE) {
  if (base::is.null(query_labels)) {
    labels <- info$query
    if (base::isTRUE(signature_key)) {
      signatures <- hypeRQuerySignatures(info)
      groups <- hypeRQueryGroups(info)
      labels <- if (base::nrow(signatures$key) > 1) {
        base::ifelse(base::nzchar(groups), base::paste(signatures$code, groups, sep = " | "), signatures$code)
      } else {
        base::ifelse(base::nzchar(groups), groups, info$query)
      }
    }
  } else {
    if (!base::is.function(query_labels)) {
      base::stop("\n'query_labels' must be NULL or a function that takes the query info data frame and returns one label per row.\n")
    }
    labels <- query_labels(info)
    if (!base::is.character(labels) || base::length(labels) != base::nrow(info) ||
        base::any(base::is.na(labels) | !base::nzchar(labels))) {
      base::stop(base::sprintf(
        "\n'query_labels' must return a character vector of %d non-empty label(s), one per query.\n",
        base::nrow(info)
      ))
    }
  }
  disambiguateHypeRLabels(labels)
}

#' Truncate labels to `abrv` characters (adding "..."), keeping them unique
#' @noRd
hypeRAbbreviateLabels <- function(labels, abrv) {
  short <- base::ifelse(base::nchar(labels) > abrv, base::paste0(base::substr(labels, 1L, abrv), "..."), labels)
  disambiguateHypeRLabels(short)
}

#' Error unless x is a single number of at least `min`
#' @noRd
checkHypeRNumber <- function(x, name, min) {
  if (!(base::is.numeric(x) && base::length(x) == 1L && !base::is.na(x) && x >= min)) {
    base::stop(base::sprintf("\n'%s' must be a single number of at least %s.\n", name, min))
  }
  base::invisible(x)
}

#' 0-row hypeRDotData() frame, with the right columns and types
#' @noRd
emptyHypeRDotData <- function() {
  base::data.frame(
    query = base::character(), query_label = base::factor(), signature_code = base::character(),
    query_group = base::character(), signature_name = base::character(),
    group_label = base::character(), direction = base::character(), test = base::character(),
    label = base::character(), label_abrv = base::factor(), pval = base::numeric(), fdr = base::numeric(),
    significance = base::numeric(), score = base::numeric(), size = base::numeric(),
    stringsAsFactors = FALSE
  )
}

#' Dot-plot data for runHypeR() results
#'
#' @description Tidy data behind \code{plotHypeRDots()}: one row per query and
#' geneset that passes the cutoffs, restricted to the \code{top} genesets
#' shared by every query. Use it to draw your own figure or to serve the numbers
#' to another front end.
#'
#' @section Required and optional arguments:
#' Only \code{hyp_obj} is required. Every other argument is optional and has the
#' default listed with it: genesets ranked by FDR, no p-value or FDR cutoff
#' (\code{pval = 1}, \code{fdr = 1}), the 20 best genesets, sized by geneset
#' size, queries labelled with signature codes. \code{query_labels = NULL} means
#' "use the labels chosen by \code{signature_key}", not "no labels". Pass
#' \code{fdr} (e.g. \code{0.05}) to keep only significant genesets.
#'
#' @param hyp_obj A \code{hyp} or \code{multihyp}, usually from \code{runHypeR()}.
#' @param val \code{"fdr"} (default) or \code{"pval"}: the value used to rank
#' genesets and to compute \code{significance}.
#' @param pval,fdr Keep rows with p-value / FDR at or below these. Default \code{1}.
#' @param top Number of genesets to keep, chosen by their best (smallest)
#' \code{val} across all queries, so every query shows the same genesets.
#' Default \code{20}.
#' @param size_by \code{"geneset"} (geneset size, default), \code{"overlap"} or
#' \code{"none"}: what the \code{size} column holds.
#' @param signature_key \code{TRUE} (default) labels queries by a short
#' signature code and group, \code{"S1 | Old"}, with the codes listed by
#' \code{hypeRSignatureKey()}; for a single signature the label is the group
#' alone (\code{"Old"}). \code{FALSE} uses the full query names.
#' @param query_labels \code{NULL} (default) for the labels chosen by
#' \code{signature_key}, or a function that takes the query info data frame
#' (\code{query}, \code{signature_id}, \code{signature_name},
#' \code{group_label}, \code{direction}, \code{test}) and returns one non-empty
#' label per row. Duplicates become \code{"label (2)"}.
#' @param abrv Geneset labels longer than this many characters are truncated
#' with \code{"..."}. Default \code{50}.
#'
#' @return A data frame with \code{query}, \code{query_label} (factor in query
#' order), \code{signature_code} (\code{"S1"}, \code{"S2"}, ...; see
#' \code{hypeRSignatureKey()}), \code{query_group} (the group label and
#' direction that set the query apart within its signature, or \code{""}),
#' \code{signature_name}, \code{group_label}, \code{direction},
#' \code{test}, \code{label}, \code{label_abrv} (factor ordered so the most
#' significant geneset is the last level, i.e. the top row of a plot),
#' \code{pval}, \code{fdr}, \code{significance} (-log10 of \code{val}; values of
#' 0 are floored to a tenth of the smallest positive value, or 1e-300),
#' \code{score} (fgsea NES, kstest score, \code{NA} for hypergeometric) and
#' \code{size}. It has 0 rows when nothing passes the cutoffs. Ties for the
#' shared \code{top} genesets are broken by the best (smallest) \code{pval}
#' across queries, then by label name.
#'
#' @examples
#' \dontrun{
#' dots <- SigRepo::hypeRDotData(hyp, fdr = 0.05, top = 15)
#' }
#'
#' @export
hypeRDotData <- function(
    hyp_obj,
    val = c("fdr", "pval"),
    pval = 1,
    fdr = 1,
    top = 20,
    size_by = c("geneset", "overlap", "none"),
    signature_key = TRUE,
    query_labels = NULL,
    abrv = 50
) {
  val <- base::match.arg(val)
  size_by <- base::match.arg(size_by)
  checkHypeRNumber(top, "top", 1)
  checkHypeRNumber(abrv, "abrv", 1)
  checkHypeRNumber(pval, "pval", 0)
  checkHypeRNumber(fdr, "fdr", 0)

  hyps <- hypeRResultHyps(hyp_obj)
  info <- hypeRQueryInfo(hyps)
  query_label <- hypeRQueryLabels(info, query_labels, signature_key)
  signature_code <- hypeRQuerySignatures(info)$code
  query_group <- hypeRQueryGroups(info)

  rows <- base::lapply(base::seq_along(hyps), function(i) {
    data <- hyps[[i]]$data
    keep <- data$pval <= pval & data$fdr <= fdr
    data <- data[!base::is.na(keep) & keep, , drop = FALSE]
    if (base::nrow(data) == 0) {
      return(NULL)
    }
    if (size_by %in% c("geneset", "overlap") && !(size_by %in% base::colnames(data))) {
      base::stop(base::sprintf("\nsize_by = \"%s\" needs a '%s' column in the results.\n", size_by, size_by))
    }
    score <- if ("nes" %in% base::colnames(data)) {
      data$nes
    } else if ("score" %in% base::colnames(data)) {
      data$score
    } else {
      NA_real_
    }
    base::data.frame(
      query = info$query[i], query_label = query_label[i], signature_code = signature_code[i],
      query_group = query_group[i], signature_name = info$signature_name[i],
      group_label = info$group_label[i], direction = info$direction[i], test = info$test[i],
      label = base::as.character(data$label), pval = data$pval, fdr = data$fdr, value = data[[val]],
      score = base::as.numeric(score),
      size = base::switch(size_by, geneset = base::as.numeric(data$geneset), overlap = base::as.numeric(data$overlap), none = NA_real_),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!base::vapply(rows, base::is.null, base::logical(1))]
  if (base::length(rows) == 0) {
    return(emptyHypeRDotData())
  }
  dots <- base::do.call(base::rbind, rows)

  best <- base::tapply(dots$value, dots$label, base::min)
  best_p <- base::tapply(dots$pval, dots$label, base::min)
  ranked_labels <- base::names(best)[base::order(best, best_p[base::names(best)], base::names(best))]
  kept_labels <- ranked_labels[base::seq_len(base::min(top, base::length(ranked_labels)))]
  dots <- dots[dots$label %in% kept_labels, , drop = FALSE]

  value <- dots$value
  positive <- value[value > 0]
  value[value <= 0] <- if (base::length(positive) > 0) base::min(positive) / 10 else 1e-300
  dots$significance <- -base::log10(value)

  abbreviated <- stats::setNames(hypeRAbbreviateLabels(kept_labels, abrv), kept_labels)
  dots$label_abrv <- base::factor(abbreviated[dots$label], levels = base::rev(base::unname(abbreviated)))
  dots$query_label <- base::factor(dots$query_label, levels = query_label)

  dots <- dots[base::order(base::match(dots$query, info$query), base::match(dots$label, kept_labels)), , drop = FALSE]
  base::rownames(dots) <- NULL
  dots[, base::colnames(emptyHypeRDotData())]
}

#' Signature codes used in dot-plot labels
#'
#' @description The key behind \code{hypeRDotData()}'s and
#' \code{plotHypeRDots()}'s short query labels: one row per signature, coded
#' \code{"S1"}, \code{"S2"}, ... in the order the signatures first appear in
#' the result. A query without a recorded SigRepo signature name counts as its
#' own signature, named by the query.
#'
#' @section Required and optional arguments:
#' \code{hyp_obj} is the only argument, and it is required.
#'
#' @param hyp_obj A \code{hyp} or \code{multihyp}, usually from \code{runHypeR()}.
#'
#' @return A data frame with \code{signature_code} and \code{signature_name}.
#'
#' @examples
#' \dontrun{
#' SigRepo::hypeRSignatureKey(hyp)
#' }
#'
#' @export
hypeRSignatureKey <- function(hyp_obj) {
  hypeRQuerySignatures(hypeRQueryInfo(hypeRResultHyps(hyp_obj)))$key
}

#' Name of the ranking behind a query. fgsea's "<ranking> | up" and
#' "<ranking> | down" hyps come from one run on one ranking, so for them this is
#' the query name without the side; any other query is its own ranking
#' (a kstest "down" query ranks the negated scores).
#' @noRd
hypeRRankingName <- function(name, hyp) {
  direction <- hypeRInfoValue(hyp, "SigRepo Direction")
  if (!base::identical(hypeRInfoValue(hyp, "Test"), "fgsea") || !direction %in% c("up", "down")) {
    return(name)
  }
  suffix <- base::paste0(" | ", direction)
  if (base::endsWith(name, suffix)) base::substr(name, 1L, base::nchar(name) - base::nchar(suffix)) else name
}

#' The hyp for one query of a result, with its name. With `geneset`, an fgsea
#' ranking name (the query name without " | up"/" | down") also selects: the
#' side whose table holds the geneset, else the up side.
#' @noRd
hypeRSelectHyp <- function(hyp_obj, query, geneset = NULL) {
  hyps <- hypeRResultHyps(hyp_obj)
  if (base::is.null(query)) {
    if (base::length(hyps) == 1L) {
      return(base::list(name = base::names(hyps), hyp = hyps[[1]]))
    }
    base::stop(base::sprintf(
      "\n'query' is required for a result with %d queries: %s.\n",
      base::length(hyps), base::paste(base::sprintf("'%s'", base::names(hyps)), collapse = ", ")
    ))
  }
  if (!base::is.null(geneset) && base::is.character(query) && base::length(query) == 1L && !query %in% base::names(hyps)) {
    sides <- base::paste(query, c("up", "down"), sep = " | ")
    sides <- sides[sides %in% base::names(hyps)]
    sides <- sides[base::vapply(sides, function(side) base::identical(hypeRRankingName(side, hyps[[side]]), query), base::logical(1))]
    if (base::length(sides) > 0) {
      holding <- sides[base::vapply(sides, function(side) geneset %in% hyps[[side]]$data$label, base::logical(1))]
      query <- if (base::length(holding) > 0) holding[1] else sides[1]
    }
  }
  if (!(base::is.character(query) && base::length(query) == 1L && query %in% base::names(hyps))) {
    base::stop(base::sprintf(
      "\n'query' must be one of: %s.\n",
      base::paste(base::sprintf("'%s'", base::names(hyps)), collapse = ", ")
    ))
  }
  base::list(name = query, hyp = hyps[[query]])
}

#' Members of one geneset as the test used it (after background reduction)
#' @noRd
hypeRGenesetMembers <- function(hyp, geneset) {
  genesets <- hyp$args$genesets
  members <- if (methods::is(genesets, "gsets") || methods::is(genesets, "rgsets")) genesets$genesets else genesets
  if (!(base::is.character(geneset) && base::length(geneset) == 1L && geneset %in% base::names(members))) {
    base::stop(base::sprintf("\nGeneset '%s' is not in this result's genesets.\n", base::paste(geneset, collapse = ", ")))
  }
  members[[geneset]]
}

#' hypeR's weighted kstest running sum (.kstest with weights), same arithmetic
#' @noRd
hypeRWeightedCurve <- function(stats, members, power) {
  n <- base::length(stats)
  hit <- base::names(stats) %in% members
  n_hits <- base::sum(hit)
  weights <- base::abs(stats)^power
  weights[!hit] <- 0
  if (n_hits == n || base::sum(weights) == 0) {
    base::stop("\nThis geneset cannot be scored by hypeR's kstest (it covers every ranked gene, or all its hits score 0).\n")
  }
  p_hit <- base::cumsum(weights)
  p_hit <- p_hit / p_hit[n]
  p_mis <- base::cumsum(!hit)
  p_mis <- p_mis / (n - n_hits)
  running <- base::unname(p_hit - p_mis)
  es_position <- base::which.max(base::abs(running))
  base::list(positions = base::seq_len(n), running = running, es = running[es_position], es_position = es_position)
}

#' hypeR's unweighted (ranked) kstest statistic, same arithmetic as .kstest
#' @noRd
hypeRRankedCurve <- function(ranked_genes, members) {
  n <- base::length(ranked_genes)
  y <- base::match(members, ranked_genes)
  y <- base::sort(y[!base::is.na(y)])
  n_hits <- base::length(y)
  if (n_hits == n) {
    base::stop("\nThis geneset cannot be scored by hypeR's kstest (it covers every ranked gene).\n")
  }
  hit_step <- 1 / n_hits
  miss_step <- 1 / n
  # hypeR evaluates the statistic at each hit and the position just before it.
  Y <- base::sort(c(y - 1, y))
  Y <- Y[base::diff(Y) != 0]
  D <- base::rep(0, base::length(Y))
  D[base::match(y, Y)] <- base::seq_len(n_hits)
  zero <- base::which(D == 0)[-1]
  D[zero] <- D[zero - 1]
  z <- D * hit_step - Y * miss_step
  es_index <- base::which.max(base::abs(z))

  positions <- base::seq_len(n)
  hits_so_far <- base::cumsum(positions %in% y)
  base::list(positions = positions, running = hits_so_far * hit_step - positions * miss_step,
             es = z[es_index], es_position = Y[es_index])
}

#' fgsea's running sum and ES (calcGseaStat tie rule: an exact tie gives 0)
#' @noRd
hypeRFgseaCurve <- function(stats, members, power) {
  if (!base::requireNamespace("fgsea", quietly = TRUE)) {
    base::stop("\nPackage 'fgsea' is required to draw fgsea enrichment curves. Please install it first.\n")
  }
  plot_data <- fgsea::plotEnrichmentData(pathway = members, stats = stats, gseaParam = power)
  curve <- base::as.data.frame(plot_data$curve)
  es <- if (plot_data$posES > -plot_data$negES) {
    plot_data$posES
  } else if (plot_data$posES < -plot_data$negES) {
    plot_data$negES
  } else {
    0
  }
  es_position <- if (es == 0) NA_integer_ else base::as.integer(curve$rank[base::which(curve$ES == es)[1]])
  base::list(positions = curve$rank, running = curve$ES, es = es, es_position = es_position)
}

#' Running enrichment score data for one geneset of a kstest or fgsea result
#'
#' @description Recomputes the running-sum curve behind one geneset's
#' enrichment score from the result itself (the ranking and genesets stored in
#' \code{hyp$args}), so no per-geneset plots need to be kept.
#'
#' @section Required and optional arguments:
#' \code{hyp_obj} (a kstest or fgsea result) and \code{geneset} are required.
#' \code{query} is required only when \code{hyp_obj} holds more than one query;
#' for a single \code{hyp}, or a \code{multihyp} with one query, it can be left
#' out. For hypergeometric results use \code{plotHypeREnrichment()} instead.
#'
#' @param hyp_obj A \code{hyp} or \code{multihyp} from \code{runHypeR()} with
#' \code{test = "kstest"} or \code{"fgsea"}.
#' @param geneset Geneset label, e.g. \code{"HALLMARK_MYC_TARGETS_V1"}.
#' @param query Query name (\code{names(hyp_obj$data)}); required for a
#' \code{multihyp} with more than one query. For fgsea, whose
#' \code{"<ranking> | up"} and \code{"<ranking> | down"} queries come from one
#' run on one ranking, the ranking name alone also works: the side whose table
#' holds \code{geneset} is used.
#'
#' @details The curve reproduces each test's own arithmetic, so \code{es}
#' matches the result's \code{score} (kstest) or \code{es} (fgsea) to their 2
#' significant digits, ties included:
#' \itemize{
#'   \item kstest with weights: hypeR's weighted running sum; the first
#'   extremum is the enrichment score.
#'   \item kstest with \code{power = 0}: hypeR's unweighted statistic.
#'   \item fgsea: \code{fgsea::plotEnrichmentData()}; an exact tie between the
#'   positive and negative extremum gives 0, as fgsea does.
#' }
#' The ranking is the one tested: for a kstest \code{direction = "down"} query
#' it is the negated score. The leading edge follows the GSEA convention (hits
#' up to the extremum for a positive score, from it for a negative score); for
#' kstest with a negative score this can differ from hypeR's \code{hits}
#' column, which always counts from the top.
#'
#' \code{absolute = TRUE} kstest results are not supported: hypeR scores them
#' as \code{max(z) - min(z)}, which has no running-sum curve, so this errors
#' for them.
#'
#' @return A list with \code{curve} (data frame: \code{position},
#' \code{running_score}; for fgsea, \code{curve} holds fgsea's own step
#' points, positions \code{0} to \code{N+1}, not one row per ranked gene
#' \code{1..N}), \code{ticks} (data frame: \code{position},
#' \code{gene}, \code{score}, \code{leading_edge}) and \code{summary} (list:
#' \code{query}, \code{ranking} (the ranking the curve walks: for fgsea the
#' query name without \code{" | up"}/\code{" | down"}, otherwise the query),
#' \code{geneset}, \code{test}, \code{direction}, \code{n_ranked},
#' \code{n_hits}, \code{es}, \code{es_position}, \code{leading_edge_genes},
#' \code{pval}, \code{fdr}, \code{nes}, \code{score}; the last four are
#' \code{NA} when the geneset is not in the result table).
#'
#' @examples
#' \dontrun{
#' curve <- SigRepo::hypeREnrichmentData(gsea, "HALLMARK_MYC_TARGETS_V1", query = "LLFS_Aging_Gene_2023")
#' }
#'
#' @export
hypeREnrichmentData <- function(hyp_obj, geneset, query = NULL) {
  selected <- hypeRSelectHyp(hyp_obj, query, geneset)
  hyp <- selected$hyp
  test <- hypeRInfoValue(hyp, "Test")
  if (!test %in% c("kstest", "fgsea")) {
    base::stop("\nhypeREnrichmentData() needs kstest or fgsea results; use plotHypeREnrichment() for hypergeometric overlap.\n")
  }
  if (base::identical(test, "kstest") && base::isTRUE(hyp$args$absolute)) {
    base::stop("\nabsolute = TRUE kstest results have no running-sum curve; hypeR scores them as max - min.\n")
  }

  members <- hypeRGenesetMembers(hyp, geneset)
  stats <- hyp$args$signature
  if (base::identical(test, "fgsea")) {
    # fgsea::plotEnrichmentData() reorders the stats this way before it
    # computes ticks and the leading edge; match its coordinates.
    stats <- stats[base::order(base::rank(-stats))]
  }
  ranked_genes <- if (base::is.character(stats)) stats else base::names(stats)
  if (!base::any(ranked_genes %in% members)) {
    base::stop(base::sprintf("\n'%s' has no genes in this ranking.\n", geneset))
  }
  power <- if (base::is.null(hyp$args$power)) 1 else hyp$args$power

  curve <- if (base::identical(test, "fgsea")) {
    hypeRFgseaCurve(stats, members, power)
  } else if (base::is.character(stats)) {
    hypeRRankedCurve(stats, members)
  } else {
    hypeRWeightedCurve(stats, members, power)
  }

  hit_positions <- base::which(ranked_genes %in% members)
  leading_edge <- if (curve$es > 0) {
    hit_positions <= curve$es_position
  } else if (curve$es < 0) {
    hit_positions >= curve$es_position
  } else {
    base::rep(FALSE, base::length(hit_positions))
  }
  ticks <- base::data.frame(
    position = hit_positions,
    gene = ranked_genes[hit_positions],
    score = if (base::is.character(stats)) NA_real_ else base::unname(stats[hit_positions]),
    leading_edge = leading_edge,
    stringsAsFactors = FALSE
  )

  row <- hyp$data[hyp$data$label == geneset, , drop = FALSE]
  from_row <- function(column) if (base::nrow(row) > 0 && column %in% base::colnames(row)) row[[column]][1] else NA_real_

  base::list(
    curve = base::data.frame(position = curve$positions, running_score = curve$running),
    ticks = ticks,
    summary = base::list(
      query = selected$name, ranking = hypeRRankingName(selected$name, hyp), geneset = geneset, test = test,
      direction = hypeRInfoValue(hyp, "SigRepo Direction"),
      n_ranked = base::length(ranked_genes), n_hits = base::length(hit_positions),
      es = curve$es, es_position = curve$es_position,
      leading_edge_genes = ticks$gene[ticks$leading_edge],
      pval = from_row("pval"), fdr = from_row("fdr"), nes = from_row("nes"), score = from_row("score")
    )
  )
}
