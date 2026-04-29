#' Extract feature scores from an OmicSignature object
#'
#' @param omic_signature An OmicSignature R6 object.
#' @param data_source One of \code{"signature"} or \code{"difexp"}.
#' @param feature_col Column containing feature names.
#' @param score_col Column containing feature scores.
#'
#' @noRd
extractSignatureFeatureScores <- function(
    omic_signature,
    data_source = c("signature", "difexp"),
    feature_col = "feature_name",
    score_col = "score"
) {

  data_source <- base::match.arg(data_source)

  if (!methods::is(omic_signature, "OmicSignature")) {
    base::stop("\n'omic_signature' must be an OmicSignature object.\n")
  }

  feature_tbl <- omic_signature[[data_source]]

  if (base::is.null(feature_tbl)) {
    base::stop(base::sprintf("\n'omic_signature$%s' is NULL.\n", data_source))
  }

  if (!methods::is(feature_tbl, "data.frame") || base::nrow(feature_tbl) == 0) {
    base::stop(base::sprintf("\n'omic_signature$%s' must be a non-empty data frame.\n", data_source))
  }

  missing_cols <- base::setdiff(c(feature_col, score_col), base::colnames(feature_tbl))
  if (base::length(missing_cols) > 0) {
    base::stop(base::sprintf(
      "\n'omic_signature$%s' is missing required column(s): %s.\n",
      data_source,
      base::paste0(missing_cols, collapse = ", ")
    ))
  }

  feature_tbl |>
    dplyr::transmute(
      feature_name = base::as.character(.data[[feature_col]]),
      score = base::as.numeric(.data[[score_col]])
    ) |>
    dplyr::filter(!base::is.na(.data$feature_name), .data$feature_name != "", !base::is.na(.data$score)) |>
    dplyr::group_by(.data$feature_name) |>
    dplyr::summarise(
      score = .data$score[base::which.max(base::abs(.data$score))],
      .groups = "drop"
    )

}

#' Compare significant feature overlap between two signatures
#'
#' @description Returns the number of significant features unique to signature A,
#' unique to signature B, and shared by both signatures. Significant features are
#' read from the \code{signature} table of each OmicSignature object retrieved
#' from the SigRepo database. If you already have OmicSignature objects, pass
#' them with \code{omic_signature_a} and \code{omic_signature_b}.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature_a} and \code{omic_signature_b} are
#' supplied.
#' @param signature_id_a Database ID for signature A.
#' @param signature_name_a Name for signature A.
#' @param signature_id_b Database ID for signature B.
#' @param signature_name_b Name for signature B.
#' @param omic_signature_a Optional OmicSignature R6 object for signature A.
#' @param omic_signature_b Optional OmicSignature R6 object for signature B.
#' @param feature_col Column containing feature names. Defaults to
#' \code{"feature_name"}.
#' @param plot Logical; whether to draw a simple Venn diagram. Defaults to
#' \code{FALSE}.
#' @param label_a Label for signature A in the plot and output table.
#' @param label_b Label for signature B in the plot and output table.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{plotSignatureOverlap()} when
#' \code{plot = TRUE}.
#'
#' @return A data frame with counts for features unique to A, overlapping, and
#' unique to B.
#'
#' @export
compareSignatureFeatures <- function(
    conn_handler = NULL,
    signature_id_a = NULL,
    signature_name_a = NULL,
    signature_id_b = NULL,
    signature_name_b = NULL,
    omic_signature_a = NULL,
    omic_signature_b = NULL,
    feature_col = "feature_name",
    plot = FALSE,
    label_a = NULL,
    label_b = NULL,
    verbose = TRUE,
    ...
) {

  omic_signature_a <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_a,
    signature_name = signature_name_a,
    omic_signature = omic_signature_a,
    label = "signature A",
    verbose = verbose
  )
  omic_signature_b <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_b,
    signature_name = signature_name_b,
    omic_signature = omic_signature_b,
    label = "signature B",
    verbose = verbose
  )

  sig_a <- extractSignatureFeatureScores(
    omic_signature = omic_signature_a,
    data_source = "signature",
    feature_col = feature_col
  )
  sig_b <- extractSignatureFeatureScores(
    omic_signature = omic_signature_b,
    data_source = "signature",
    feature_col = feature_col
  )

  features_a <- base::unique(sig_a$feature_name)
  features_b <- base::unique(sig_b$feature_name)

  overlap_tbl <- base::data.frame(
    comparison = c(
      base::sprintf("unique_to_%s", cleanComparisonLabel(label_a, "A")),
      "overlap",
      base::sprintf("unique_to_%s", cleanComparisonLabel(label_b, "B"))
    ),
    n_features = c(
      base::length(base::setdiff(features_a, features_b)),
      base::length(base::intersect(features_a, features_b)),
      base::length(base::setdiff(features_b, features_a))
    ),
    stringsAsFactors = FALSE
  )

  if (base::isTRUE(plot)) {
    plotSignatureOverlap(
      omic_signature_a = omic_signature_a,
      omic_signature_b = omic_signature_b,
      feature_col = feature_col,
      label_a = label_a,
      label_b = label_b,
      ...
    )
  }

  overlap_tbl

}

#' Plot significant feature overlap between two signatures
#'
#' @description Draws a simple two-set Venn diagram for significant features in
#' two signatures retrieved from the SigRepo database.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature_a} and \code{omic_signature_b} are
#' supplied.
#' @param signature_id_a Database ID for signature A.
#' @param signature_name_a Name for signature A.
#' @param signature_id_b Database ID for signature B.
#' @param signature_name_b Name for signature B.
#' @param omic_signature_a Optional OmicSignature R6 object for signature A.
#' @param omic_signature_b Optional OmicSignature R6 object for signature B.
#' @param feature_col Column containing feature names. Defaults to
#' \code{"feature_name"}.
#' @param label_a Label for signature A.
#' @param label_b Label for signature B.
#' @param fill Fill colors for the two circles.
#' @param alpha Transparency value between 0 and 1.
#' @param main Plot title.
#' @param short_label_a Short label shown for signature A in the Venn diagram.
#' Defaults to \code{"S1"}.
#' @param short_label_b Short label shown for signature B in the Venn diagram.
#' Defaults to \code{"S2"}.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @return Invisibly returns the overlap count table from
#' \code{compareSignatureFeatures()}.
#'
#' @export
plotSignatureOverlap <- function(
    conn_handler = NULL,
    signature_id_a = NULL,
    signature_name_a = NULL,
    signature_id_b = NULL,
    signature_name_b = NULL,
    omic_signature_a = NULL,
    omic_signature_b = NULL,
    feature_col = "feature_name",
    label_a = NULL,
    label_b = NULL,
    fill = c("#3B82F6", "#F97316"),
    alpha = 0.35,
    main = "Significant feature overlap",
    short_label_a = "S1",
    short_label_b = "S2",
    verbose = TRUE
) {

  omic_signature_a <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_a,
    signature_name = signature_name_a,
    omic_signature = omic_signature_a,
    label = "signature A",
    verbose = verbose
  )
  omic_signature_b <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_b,
    signature_name = signature_name_b,
    omic_signature = omic_signature_b,
    label = "signature B",
    verbose = verbose
  )

  label_a <- resolveSignatureLabel(omic_signature_a, label_a, "Signature A")
  label_b <- resolveSignatureLabel(omic_signature_b, label_b, "Signature B")

  overlap_tbl <- compareSignatureFeatures(
    omic_signature_a = omic_signature_a,
    omic_signature_b = omic_signature_b,
    feature_col = feature_col,
    plot = FALSE,
    label_a = label_a,
    label_b = label_b,
    verbose = verbose
  )

  old_par <- graphics::par(no.readonly = TRUE)
  base::on.exit(graphics::par(old_par), add = TRUE)

  graphics::plot(
    NA,
    xlim = c(0, 10),
    ylim = c(0, 7),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    asp = 1
  )

  graphics::symbols(
    x = c(4, 6),
    y = c(3.5, 3.5),
    circles = c(2.3, 2.3),
    inches = FALSE,
    add = TRUE,
    bg = grDevices::adjustcolor(fill, alpha.f = alpha),
    fg = fill,
    lwd = 2
  )

  graphics::text(2.8, 6.15, labels = short_label_a, font = 2)
  graphics::text(7.2, 6.15, labels = short_label_b, font = 2)
  graphics::text(3.05, 3.5, labels = overlap_tbl$n_features[1], cex = 1.3)
  graphics::text(5.00, 3.5, labels = overlap_tbl$n_features[2], cex = 1.3, font = 2)
  graphics::text(6.95, 3.5, labels = overlap_tbl$n_features[3], cex = 1.3)
  graphics::legend(
    "bottom",
    legend = c(
      base::sprintf("%s: %s", short_label_a, label_a),
      base::sprintf("%s: %s", short_label_b, label_b)
    ),
    fill = grDevices::adjustcolor(fill, alpha.f = alpha),
    border = fill,
    bty = "n",
    horiz = FALSE,
    xpd = NA,
    inset = c(0, -0.08)
  )

  base::invisible(overlap_tbl)

}

#' Compare feature score correlation between two signatures
#'
#' @description Joins two signatures by feature name and optionally draws a
#' scatter plot comparing feature scores. Use \code{data_source = "signature"}
#' to compare significant features only, or \code{data_source = "difexp"} to
#' compare all overlapping features in the differential-expression tables.
#' Signatures are retrieved from the SigRepo database unless OmicSignature
#' objects are supplied directly.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature_a} and \code{omic_signature_b} are
#' supplied.
#' @param signature_id_a Database ID for signature A.
#' @param signature_name_a Name for signature A.
#' @param signature_id_b Database ID for signature B.
#' @param signature_name_b Name for signature B.
#' @param omic_signature_a Optional OmicSignature R6 object for signature A.
#' @param omic_signature_b Optional OmicSignature R6 object for signature B.
#' @param data_source One of \code{"signature"} or \code{"difexp"}. Defaults to
#' \code{"signature"}.
#' @param feature_col Column containing feature names. Defaults to
#' \code{"feature_name"}.
#' @param score_col Column containing feature scores. Defaults to \code{"score"}.
#' @param method Correlation method passed to \code{stats::cor.test()}.
#' @param plot Logical; whether to draw a scatter plot. Defaults to \code{TRUE}.
#' @param label_a Label for signature A.
#' @param label_b Label for signature B.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{graphics::plot()}.
#'
#' @return A list containing \code{overlap_scores}, \code{correlation}, and
#' \code{n_overlap}.
#'
#' @export
compareSignatureScores <- function(
    conn_handler = NULL,
    signature_id_a = NULL,
    signature_name_a = NULL,
    signature_id_b = NULL,
    signature_name_b = NULL,
    omic_signature_a = NULL,
    omic_signature_b = NULL,
    data_source = c("signature", "difexp"),
    feature_col = "feature_name",
    score_col = "score",
    method = c("pearson", "spearman", "kendall"),
    plot = TRUE,
    label_a = NULL,
    label_b = NULL,
    verbose = TRUE,
    ...
) {

  data_source <- base::match.arg(data_source)
  method <- base::match.arg(method)

  omic_signature_a <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_a,
    signature_name = signature_name_a,
    omic_signature = omic_signature_a,
    label = "signature A",
    verbose = verbose
  )
  omic_signature_b <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_b,
    signature_name = signature_name_b,
    omic_signature = omic_signature_b,
    label = "signature B",
    verbose = verbose
  )

  label_a <- resolveSignatureLabel(omic_signature_a, label_a, "Signature A")
  label_b <- resolveSignatureLabel(omic_signature_b, label_b, "Signature B")

  scores_a <- extractSignatureFeatureScores(
    omic_signature = omic_signature_a,
    data_source = data_source,
    feature_col = feature_col,
    score_col = score_col
  ) |>
    dplyr::rename(score_a = "score")

  scores_b <- extractSignatureFeatureScores(
    omic_signature = omic_signature_b,
    data_source = data_source,
    feature_col = feature_col,
    score_col = score_col
  ) |>
    dplyr::rename(score_b = "score")

  overlap_scores <- dplyr::inner_join(scores_a, scores_b, by = "feature_name")

  if (base::nrow(overlap_scores) < 2) {
    base::stop("\nAt least two overlapping features with non-missing scores are required.\n")
  }

  correlation <- stats::cor.test(
    x = overlap_scores$score_a,
    y = overlap_scores$score_b,
    method = method
  )

  if (base::isTRUE(plot)) {
    graphics::plot(
      overlap_scores$score_a,
      overlap_scores$score_b,
      xlab = base::sprintf("%s score", label_a),
      ylab = base::sprintf("%s score", label_b),
      main = base::sprintf("%s score correlation", data_source),
      pch = 19,
      col = grDevices::adjustcolor("#2563EB", alpha.f = 0.65),
      ...
    )
    graphics::abline(stats::lm(score_b ~ score_a, data = overlap_scores), col = "#DC2626", lwd = 2)
    graphics::abline(h = 0, v = 0, col = "gray75", lty = 2)
    graphics::legend(
      "topleft",
      legend = base::sprintf(
        "%s r = %.3f, p = %.3g, n = %s",
        method,
        base::unname(correlation$estimate),
        correlation$p.value,
        base::nrow(overlap_scores)
      ),
      bty = "n"
    )
  }

  base::list(
    overlap_scores = overlap_scores,
    correlation = correlation,
    n_overlap = base::nrow(overlap_scores)
  )

}

#' Test enrichment of one signature in another signature's ranked scores
#'
#' @description Treats significant features from signature B as a feature set
#' and tests whether their scores in signature A differ from the remaining
#' ranked scores in signature A using a two-sample Kolmogorov-Smirnov test.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature_a} and \code{omic_signature_b} are
#' supplied.
#' @param signature_id_a Database ID for signature A.
#' @param signature_name_a Name for signature A.
#' @param signature_id_b Database ID for signature B.
#' @param signature_name_b Name for signature B.
#' @param omic_signature_a Optional OmicSignature object containing the ranked scores.
#' Scores are taken from \code{omic_signature_a$difexp} by default.
#' @param omic_signature_b Optional OmicSignature object containing the significant
#' feature set. Features are taken from \code{omic_signature_b$signature}.
#' @param feature_col Column containing feature names. Defaults to
#' \code{"feature_name"}.
#' @param score_col Column containing feature scores. Defaults to \code{"score"}.
#' @param alternative Alternative hypothesis passed to \code{stats::ks.test()}.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @return A list containing the KS test result, feature counts, and ranked score
#' table for signature A.
#'
#' @export
signatureSetKsTest <- function(
    conn_handler = NULL,
    signature_id_a = NULL,
    signature_name_a = NULL,
    signature_id_b = NULL,
    signature_name_b = NULL,
    omic_signature_a = NULL,
    omic_signature_b = NULL,
    feature_col = "feature_name",
    score_col = "score",
    alternative = c("two.sided", "greater", "less"),
    verbose = TRUE
) {

  alternative <- base::match.arg(alternative)

  omic_signature_a <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_a,
    signature_name = signature_name_a,
    omic_signature = omic_signature_a,
    label = "signature A",
    verbose = verbose
  )
  omic_signature_b <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id_b,
    signature_name = signature_name_b,
    omic_signature = omic_signature_b,
    label = "signature B",
    verbose = verbose
  )

  ranked_scores <- extractSignatureFeatureScores(
    omic_signature = omic_signature_a,
    data_source = "difexp",
    feature_col = feature_col,
    score_col = score_col
  ) |>
    dplyr::arrange(dplyr::desc(.data$score))

  feature_set <- extractSignatureFeatureScores(
    omic_signature = omic_signature_b,
    data_source = "signature",
    feature_col = feature_col,
    score_col = score_col
  )$feature_name

  ranked_scores <- ranked_scores |>
    dplyr::mutate(
      in_feature_set = .data$feature_name %in% feature_set,
      rank = dplyr::row_number()
    )

  set_scores <- ranked_scores$score[ranked_scores$in_feature_set]
  background_scores <- ranked_scores$score[!ranked_scores$in_feature_set]

  if (base::length(set_scores) == 0) {
    base::stop("\nNo significant features from signature B were found in signature A's ranked scores.\n")
  }

  if (base::length(background_scores) == 0) {
    base::stop("\nSignature A has no background features outside signature B's significant feature set.\n")
  }

  ks_result <- stats::ks.test(
    x = set_scores,
    y = background_scores,
    alternative = alternative
  )

  base::list(
    ks_test = ks_result,
    n_feature_set = base::length(feature_set),
    n_feature_set_in_ranked_scores = base::length(set_scores),
    n_background = base::length(background_scores),
    ranked_scores = ranked_scores
  )

}

#' Resolve a signature from either an object or the SigRepo database
#'
#' @noRd
resolveComparisonSignature <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    label = "signature",
    verbose = TRUE
) {

  if (!base::is.null(omic_signature)) {
    if (!methods::is(omic_signature, "OmicSignature")) {
      base::stop(base::sprintf("\n'omic_signature' for %s must be an OmicSignature object.\n", label))
    }
    return(omic_signature)
  }

  if (base::is.null(conn_handler)) {
    base::stop(base::sprintf(
      "\nProvide 'conn_handler' and either 'signature_id' or 'signature_name' for %s, or pass an OmicSignature object.\n",
      label
    ))
  }

  if ((base::length(signature_id) == 0 || base::all(signature_id %in% c("", NA))) &&
      (base::length(signature_name) == 0 || base::all(signature_name %in% c("", NA)))) {
    base::stop(base::sprintf("\nProvide 'signature_id' or 'signature_name' for %s.\n", label))
  }

  omic_signature_list <- getSignature(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    verbose = verbose
  )

  if (base::is.null(omic_signature_list) || base::length(omic_signature_list) == 0) {
    base::stop(base::sprintf("\nNo %s was returned from the database.\n", label))
  }

  if (base::length(omic_signature_list) > 1) {
    base::stop(base::sprintf(
      "\nMore than one %s was returned from the database. Use a unique signature_id or signature_name.\n",
      label
    ))
  }

  omic_signature_list[[1]]

}

#' Resolve a default signature comparison label
#'
#' @noRd
resolveSignatureLabel <- function(omic_signature, label, fallback) {

  if (!base::is.null(label) && base::length(label) > 0 && !base::is.na(label[1]) && label[1] != "") {
    return(base::as.character(label[1]))
  }

  if (methods::is(omic_signature, "OmicSignature") &&
      "signature_name" %in% base::names(omic_signature$metadata) &&
      base::length(omic_signature$metadata$signature_name) > 0 &&
      !base::is.na(omic_signature$metadata$signature_name[1]) &&
      omic_signature$metadata$signature_name[1] != "") {
    return(base::as.character(omic_signature$metadata$signature_name[1]))
  }

  fallback

}

#' Clean a comparison label for table values
#'
#' @noRd
cleanComparisonLabel <- function(label, fallback) {

  if (base::is.null(label) || base::length(label) == 0 || base::is.na(label[1]) || label[1] == "") {
    label <- fallback
  }

  base::gsub("[^A-Za-z0-9]+", "_", base::tolower(base::as.character(label[1])))

}
