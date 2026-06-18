#' Resolve one or more signatures for client-side hypeR workflows
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id One or more SigRepo signature IDs.
#' @param signature_name One or more SigRepo signature names.
#' @param omic_signature A single \code{OmicSignature} object or a list of
#' \code{OmicSignature} objects.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @noRd
resolveHypeRSignatures <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    verbose = TRUE
){

  # validation of OmicSignature
  if (!base::is.null(omic_signature)) {
    if (methods::is(omic_signature, "OmicSignature")) {
      return(base::list(omic_signature))
    }

    if (!methods::is(omic_signature, "list") || base::length(omic_signature) == 0) {
      base::stop(
        "\n'omic_signature' must be either an OmicSignature object or a non-empty list of OmicSignature objects.\n"
      )
    }
    # Validating that the id is in the database
    invalid_idx <- base::which(!purrr::map_lgl(omic_signature, ~ methods::is(.x, "OmicSignature")))
    if (base::length(invalid_idx) > 0) {
      base::stop(
        base::sprintf(
          "\nAll elements in 'omic_signature' must be OmicSignature objects. Invalid indices: %s.\n",
          base::paste0(invalid_idx, collapse = ", ")
        )
      )
    }

    return(omic_signature)
  }

  if (base::is.null(conn_handler)) {
    base::stop(
      "\nProvide 'conn_handler' together with 'signature_id' or 'signature_name', or pass 'omic_signature'.\n"
    )
  }

  if (base::length(signature_id) > 0 && base::any(!signature_id %in% c("", NA))) {
    sig_ids <- base::unique(signature_id[!signature_id %in% c("", NA)])
    resolved <- 
      purrr::map(
        sig_ids,
        ~ resolveComparisonSignature(
          conn_handler = conn_handler,
          signature_id = .x,
          omic_signature = NULL,
          label = base::sprintf("signature_id = %s", .x),
          verbose = verbose
        )
      )
    base::names(resolved) <- base::as.character(sig_ids)
    return(resolved)
  }

  if (base::length(signature_name) > 0 && base::any(!signature_name %in% c("", NA))) {
    sig_names <- base::unique(signature_name[!signature_name %in% c("", NA)])
    resolved <-
      purrr::map(
        sig_names,
        ~ resolveComparisonSignature(
          conn_handler = conn_handler,
          signature_name = .x,
          omic_signature = NULL,
          label = base::sprintf("signature_name = %s", .x),
          verbose = verbose
        )
      )
    base::names(resolved) <- base::as.character(sig_names)
    return(resolved)
  }

  base::stop(
    "\nProvide at least one 'signature_id' or 'signature_name', or pass 'omic_signature'.\n"
  )
}

resolveHypeRSignatureLabels <- function(omic_signatures) {
  supplied_names <- base::names(omic_signatures)

  labels <- purrr::map_chr(
    base::seq_along(omic_signatures),
    function(idx) {
      supplied_label <- supplied_names[[idx]]
      if (!base::is.null(supplied_label) && !base::is.na(supplied_label) && base::nzchar(supplied_label)) {
        return(base::as.character(supplied_label))
      }

      resolveSignatureLabel(
        omic_signature = omic_signatures[[idx]],
        label = NULL,
        fallback = base::sprintf("sig_%s", idx)
      )
    }
  )

  base::make.unique(labels)
}

#' Build hypeR-ready signature vectors from SigRepo signatures
#'
#' @description Converts one or more SigRepo signatures into the query-vector
#' format expected by \code{hypeR::hypeR()}. This helper can resolve signatures
#' directly from the SigRepo database, so users do not need to call
#' \code{getSignature()} manually first.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id One or more SigRepo signature IDs.
#' @param signature_name One or more SigRepo signature names.
#' @param omic_signature A single \code{OmicSignature} object or a list of
#' \code{OmicSignature} objects.
#' @param method One of \code{"hypergeo"}, \code{"kstest"}, or \code{"gsea"}.
#' Hypergeometric enrichment uses the \code{signature} table. KS/GSEA-style
#' enrichment use ranked scores from \code{difexp}.
#' @param feature_col Column containing feature identifiers. Defaults to
#' \code{"feature_name"}.
#' @param score_col Column containing ranked scores for KS/GSEA-style enrichment.
#' Defaults to \code{"score"}.
#' @param split_by_group Logical; whether to create separate query vectors for
#' each \code{group_label}. Defaults to \code{FALSE}.
#' @param split_by_direction Logical; when \code{method = "hypergeo"}, whether
#' to split each group into up- and down-direction feature sets using the sign
#' of \code{score_col}. Defaults to \code{FALSE}.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{signatures}}{A named list of query vectors for \code{hypeR}.}
#'   \item{\code{metadata}}{A data frame describing each generated query vector.}
#'   \item{\code{omic_signatures}}{The resolved \code{OmicSignature} objects.}
#' }
#'
#' @export
prepareHypeRSignatures <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    method = c("hypergeo", "kstest", "gsea"),
    feature_col = "feature_name",
    score_col = "score",
    split_by_group = FALSE,
    split_by_direction = FALSE,
    verbose = TRUE
){

  method <- base::match.arg(method)

  omic_signatures <- resolveHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    verbose = verbose
  )
  signature_labels <- resolveHypeRSignatureLabels(omic_signatures)

  build_group_component <- function(group_label, suffix = NULL) {
    clean_group <- if (base::is.null(group_label) || base::is.na(group_label) || !base::nzchar(group_label)) {
      "all_features"
    } else {
      base::gsub("[^A-Za-z0-9]+", "_", base::tolower(base::as.character(group_label)))
    }

    if (base::is.null(suffix) || !base::nzchar(suffix)) {
      return(clean_group)
    }

    base::paste0(clean_group, "_", suffix)
  }

  vectors <- base::list()
  metadata <- base::list()
  metadata_idx <- 0

  for (sig_idx in base::seq_along(omic_signatures)) {
    sig_obj <- omic_signatures[[sig_idx]]
    sig_name <- signature_labels[[sig_idx]]

    if (identical(method, "hypergeo")) {
      signature_tbl <- sig_obj$signature

      if (!methods::is(signature_tbl, "data.frame") || base::nrow(signature_tbl) == 0) {
        base::stop(
          base::sprintf("\nSignature '%s' does not contain a non-empty 'signature' table.\n", sig_name)
        )
      }

      if (!feature_col %in% base::colnames(signature_tbl)) {
        base::stop(
          base::sprintf("\nSignature '%s' is missing feature column '%s' in 'signature'.\n", sig_name, feature_col)
        )
      }

      if (split_by_group && "group_label" %in% base::colnames(signature_tbl)) {
        signature_tbl$group_label <- base::as.character(signature_tbl$group_label)
      } else {
        signature_tbl$group_label <- "all_features"
      }

      if (split_by_direction) {
        if (!score_col %in% base::colnames(signature_tbl)) {
          base::stop(
            base::sprintf(
              "\nSignature '%s' is missing score column '%s', which is required for direction splitting.\n",
              sig_name,
              score_col
            )
          )
        }
        signature_tbl$score_value <- base::suppressWarnings(base::as.numeric(signature_tbl[[score_col]]))
      }

      for (group_label in base::unique(signature_tbl$group_label)) {
        group_tbl <- signature_tbl[signature_tbl$group_label == group_label, , drop = FALSE]

        if (split_by_direction) {
          direction_specs <- base::list(
            base::list(suffix = "up", keep = !base::is.na(group_tbl$score_value) & group_tbl$score_value >= 0),
            base::list(suffix = "dn", keep = !base::is.na(group_tbl$score_value) & group_tbl$score_value < 0)
          )

          for (direction in direction_specs) {
            query_features <- base::unique(base::as.character(group_tbl[[feature_col]][direction$keep]))
            query_features <- query_features[!base::is.na(query_features) & query_features != ""]
            if (base::length(query_features) == 0) {
              next
            }

            component <- build_group_component(group_label, direction$suffix)
            query_name <- base::sprintf("%s | %s", sig_name, component)
            vectors[[query_name]] <- query_features

            metadata_idx <- metadata_idx + 1
            metadata[[metadata_idx]] <- base::data.frame(
              query_name = query_name,
              signature_name = sig_name,
              method = method,
              group_label = component,
              n_features = base::length(query_features),
              stringsAsFactors = FALSE
            )
          }
        } else {
          query_features <- base::unique(base::as.character(group_tbl[[feature_col]]))
          query_features <- query_features[!base::is.na(query_features) & query_features != ""]
          if (base::length(query_features) == 0) {
            next
          }

          component <- build_group_component(group_label)
          query_name <- if (identical(component, "all_features")) sig_name else base::sprintf("%s | %s", sig_name, component)
          vectors[[query_name]] <- query_features

          metadata_idx <- metadata_idx + 1
          metadata[[metadata_idx]] <- base::data.frame(
            query_name = query_name,
            signature_name = sig_name,
            method = method,
            group_label = component,
            n_features = base::length(query_features),
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      difexp_tbl <- sig_obj$difexp

      if (!methods::is(difexp_tbl, "data.frame") || base::nrow(difexp_tbl) == 0) {
        base::stop(
          base::sprintf(
            "\nSignature '%s' does not contain a non-empty 'difexp' table, which is required for '%s'.\n",
            sig_name,
            method
          )
        )
      }

      missing_cols <- base::setdiff(c(feature_col, score_col), base::colnames(difexp_tbl))
      if (base::length(missing_cols) > 0) {
        base::stop(
          base::sprintf(
            "\nSignature '%s' is missing required difexp column(s): %s.\n",
            sig_name,
            base::paste0(missing_cols, collapse = ", ")
          )
        )
      }

      if (split_by_group && "group_label" %in% base::colnames(difexp_tbl)) {
        difexp_tbl$group_label <- base::as.character(difexp_tbl$group_label)
      } else {
        difexp_tbl$group_label <- "all_features"
      }

      difexp_tbl$feature_value <- base::as.character(difexp_tbl[[feature_col]])
      difexp_tbl$score_value <- base::suppressWarnings(base::as.numeric(difexp_tbl[[score_col]]))
      difexp_tbl <- difexp_tbl[
        !base::is.na(difexp_tbl$feature_value) & difexp_tbl$feature_value != "" &
          !base::is.na(difexp_tbl$score_value),
        ,
        drop = FALSE
      ]

      for (group_label in base::unique(difexp_tbl$group_label)) {
        group_tbl <- difexp_tbl[difexp_tbl$group_label == group_label, , drop = FALSE]
        if (base::nrow(group_tbl) == 0) {
          next
        }

        ranked_tbl <- group_tbl |>
          dplyr::group_by(.data$feature_value) |>
          dplyr::summarise(
            score_value = .data$score_value[base::which.max(base::abs(.data$score_value))],
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(.data$score_value))

        ranked_vector <- stats::setNames(ranked_tbl$score_value, ranked_tbl$feature_value)
        if (base::length(ranked_vector) == 0) {
          next
        }

        component <- build_group_component(group_label)
        query_name <- if (identical(component, "all_features")) sig_name else base::sprintf("%s | %s", sig_name, component)
        vectors[[query_name]] <- ranked_vector

        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- base::data.frame(
          query_name = query_name,
          signature_name = sig_name,
          method = method,
          group_label = component,
          n_features = base::length(ranked_vector),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (base::length(vectors) == 0) {
    base::stop(
      base::sprintf(
        "\nNo valid signatures were available to prepare '%s' enrichment inputs.\n",
        method
      )
    )
  }

  metadata_df <- if (metadata_idx > 0) {
    dplyr::bind_rows(metadata)
  } else {
    base::data.frame(
      query_name = character(),
      signature_name = character(),
      method = character(),
      group_label = character(),
      n_features = numeric(),
      stringsAsFactors = FALSE
    )
  }

  base::list(
    signatures = vectors,
    metadata = metadata_df,
    omic_signatures = omic_signatures
  )
}

#' Run hypeR enrichment directly from SigRepo signatures
#'
#' @description Resolves one or more signatures from SigRepo and runs
#' \code{hypeR::hypeR()} on them without requiring the user to call
#' \code{getSignature()} manually first.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id One or more SigRepo signature IDs.
#' @param signature_name One or more SigRepo signature names.
#' @param omic_signature A single \code{OmicSignature} object or a list of
#' \code{OmicSignature} objects.
#' @param genesets A named list of genesets supplied to \code{hypeR}.
#' @param method One of \code{"hypergeo"}, \code{"kstest"}, or \code{"gsea"}.
#' @param feature_col Column containing feature identifiers. Defaults to
#' \code{"feature_name"}.
#' @param score_col Column containing ranked scores for KS/GSEA-style enrichment.
#' Defaults to \code{"score"}.
#' @param split_by_group Logical; whether to create separate query vectors for
#' each \code{group_label}. Defaults to \code{FALSE}.
#' @param split_by_direction Logical; when \code{method = "hypergeo"}, whether
#' to split each group into up- and down-direction feature sets using the sign
#' of \code{score_col}. Defaults to \code{FALSE}.
#' @param background Optional background size passed to \code{hypeR}.
#' @param fdr FDR threshold passed to \code{hypeR}. Defaults to \code{0.05}.
#' @param plotting Logical; whether to let \code{hypeR} generate plots.
#' Defaults to \code{FALSE}.
#' @param quiet Logical; whether to suppress \code{hypeR} messages.
#' Defaults to \code{TRUE}.
#' @param absolute Passed to \code{hypeR} for GSEA-style ranked enrichment.
#' Defaults to \code{FALSE}.
#' @param power Passed to \code{hypeR} for GSEA-style ranked enrichment.
#' Defaults to \code{1}.
#' @param ... Additional arguments passed directly to \code{hypeR::hypeR()}.
#' Use this for extra tuning parameters such as \code{background} or other
#' engine-specific options.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{result}}{A \code{hyp} or \code{multihyp} object returned by \code{hypeR}.}
#'   \item{\code{signatures}}{The query vectors passed into \code{hypeR}.}
#'   \item{\code{metadata}}{A data frame describing each query vector.}
#' }
#'
#' @examples
#' \dontrun{
#' genesets <- list(
#'   pathway_a = c("ENSG000001", "ENSG000002"),
#'   pathway_b = c("ENSG000003", "ENSG000004")
#' )
#'
#' hyp_res <- SigRepo::runHypeR(
#'   conn_handler = conn_handler,
#'   signature_name = "example_signature",
#'   genesets = genesets,
#'   method = "hypergeo",
#'   background = 20000
#' )
#' }
#'
#' @export
runHypeR <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    genesets,
    method = c("hypergeo", "kstest", "gsea"),
    feature_col = "feature_name",
    score_col = "score",
    split_by_group = FALSE,
    split_by_direction = FALSE,
    background = NULL,
    fdr = 0.05,
    plotting = FALSE,
    quiet = TRUE,
    absolute = FALSE,
    power = 1,
    ...,
    verbose = TRUE
){

  if (!requireNamespace("hypeR", quietly = TRUE)) {
    base::stop(
      "\nPackage 'hypeR' is required for runHypeR(). Please install it first.\n"
    )
  }

  if (!methods::is(genesets, "list") || base::length(genesets) == 0) {
    base::stop("\n'genesets' must be a non-empty named list.\n")
  }

  method <- base::match.arg(method)
  hype_test <- if (identical(method, "hypergeo")) "hypergeometric" else "ks"
  extra_args <- base::list(...)

  reserved_args <- c("signature", "genesets", "test")
  duplicate_reserved <- base::intersect(base::names(extra_args), reserved_args)
  if (base::length(duplicate_reserved) > 0) {
    base::stop(
      base::sprintf(
        "\nThe following hypeR arguments are handled internally by runHypeR() and cannot be supplied in '...': %s.\n",
        base::paste0(duplicate_reserved, collapse = ", ")
      )
    )
  }

  enrichment_inputs <- prepareHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    method = method,
    feature_col = feature_col,
    score_col = score_col,
    split_by_group = split_by_group,
    split_by_direction = split_by_direction,
    verbose = verbose
  )

  signature_vectors <- enrichment_inputs$signatures

  hype_signature <- if (base::length(signature_vectors) == 1) {
    signature_vectors[[1]]
  } else {
    signature_vectors
  }

  hype_args <- base::list(
    signature = hype_signature,
    genesets = genesets,
    test = hype_test,
    fdr = fdr,
    plotting = plotting,
    quiet = quiet
  )

  if (!base::is.null(background)) {
    hype_args$background <- background
  }

  if (identical(method, "gsea")) {
    hype_args$absolute <- absolute
    hype_args$power <- power
  }

  if (base::length(extra_args) > 0) {
    hype_args[base::names(extra_args)] <- extra_args
  }

  hyp_result <- do.call(hypeR::hypeR, hype_args)

  base::list(
    result = hyp_result,
    signatures = signature_vectors,
    metadata = enrichment_inputs$metadata
  )
}
