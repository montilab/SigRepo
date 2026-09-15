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

#' Display labels for queries: wrapped names, or a user function of the info
#' @noRd
hypeRQueryLabels <- function(info, query_labels) {
  if (base::is.null(query_labels)) {
    labels <- base::vapply(
      info$query,
      function(query) base::paste(base::strwrap(query, width = 25), collapse = "\n"),
      base::character(1),
      USE.NAMES = FALSE
    )
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

emptyHypeRDotData <- function() {
  base::data.frame(
    query = base::character(), query_label = base::factor(), signature_name = base::character(),
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
#' @param hyp_obj A \code{hyp} or \code{multihyp}, usually from \code{runHypeR()}.
#' @param val \code{"fdr"} (default) or \code{"pval"}: the value used to rank
#' genesets and to compute \code{significance}.
#' @param pval,fdr Keep rows with p-value / FDR at or below these. Default \code{1}.
#' @param top Number of genesets to keep, chosen by their best (smallest)
#' \code{val} across all queries, so every query shows the same genesets.
#' Default \code{20}.
#' @param size_by \code{"geneset"} (geneset size, default), \code{"overlap"} or
#' \code{"none"}: what the \code{size} column holds.
#' @param query_labels \code{NULL} (default) for query names wrapped to 25
#' characters per line, or a function that takes the query info data frame
#' (\code{query}, \code{signature_id}, \code{signature_name},
#' \code{group_label}, \code{direction}, \code{test}) and returns one non-empty
#' label per row. Duplicates become \code{"label (2)"}.
#' @param abrv Geneset labels longer than this many characters are truncated
#' with \code{"..."}. Default \code{50}.
#'
#' @return A data frame with \code{query}, \code{query_label} (factor in query
#' order), \code{signature_name}, \code{group_label}, \code{direction},
#' \code{test}, \code{label}, \code{label_abrv} (factor ordered so the most
#' significant geneset is the last level, i.e. the top row of a plot),
#' \code{pval}, \code{fdr}, \code{significance} (-log10 of \code{val}; values of
#' 0 are floored to a tenth of the smallest positive value, or 1e-300),
#' \code{score} (fgsea NES, kstest score, \code{NA} for hypergeometric) and
#' \code{size}. It has 0 rows when nothing passes the cutoffs.
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
  query_label <- hypeRQueryLabels(info, query_labels)

  rows <- base::lapply(base::seq_along(hyps), function(i) {
    data <- hyps[[i]]$data
    keep <- data$pval <= pval & data$fdr <= fdr
    data <- data[!base::is.na(keep) & keep, , drop = FALSE]
    if (base::nrow(data) == 0) {
      return(NULL)
    }
    score <- if ("nes" %in% base::colnames(data)) {
      data$nes
    } else if ("score" %in% base::colnames(data)) {
      data$score
    } else {
      NA_real_
    }
    base::data.frame(
      query = info$query[i], query_label = query_label[i], signature_name = info$signature_name[i],
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
  ranked_labels <- base::names(best)[base::order(best, base::names(best))]
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
