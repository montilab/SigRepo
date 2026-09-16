#' Pick a display label for a signature
#'
#' Uses an explicit label when given, otherwise the signature's own
#' \code{signature_name}, otherwise the supplied fallback.
#'
#' Restored alongside \code{resolveComparisonSignature()}: both lived in
#' R/compareSignature.R and were removed with the pairwise comparison helpers,
#' but the hypeR and hypeR-GEM clients still depend on them.
#'
#' @param omic_signature An \code{OmicSignature} object.
#' @param label An explicit label, or NULL.
#' @param fallback Label to use when neither is available.
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


#' Resolve a single signature for a client-side workflow
#'
#' Returns \code{omic_signature} when one is supplied, otherwise fetches
#' exactly one signature from the database by id or name.
#'
#' This lived in R/compareSignature.R alongside the pairwise comparison
#' helpers. Those were replaced by \code{compareSignatures()}, which resolves
#' its own inputs, but \code{runHypeR()} still depends on this, so it moved
#' here to sit with its only caller.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id A single SigRepo signature ID.
#' @param signature_name A single SigRepo signature name.
#' @param omic_signature A single \code{OmicSignature} object.
#' @param label A human-readable label used in error messages.
#' @param verbose Logical; whether to print diagnostic messages.
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
      "\nProvide the signatures to test: 'conn_handler' together with 'signature_id' or 'signature_name', 'omic_signature', or 'signature' (gene symbols or a named score vector).\n"
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


# A full transcriptomics difexp covers the measured transcriptome; on production
# (2026-09) every transcriptomics difexp has either > 10,000 rows or <= 5,000.
HYPER_MIN_TRANSCRIPTOME_DIFEXP_ROWS <- 10000L

#' Why a difexp looks filtered rather than a full list of measured genes, or NULL
#'
#' The default background uses a signature's difexp genes as the measured
#' universe (BS831: "the number of annotated genes in the dataset"), which is
#' only right when the difexp is complete. A truncated one shrinks the
#' background and hides real enrichment.
#' @noRd
hypeRDifexpTruncation <- function(omic_signature) {
  difexp <- omic_signature$difexp
  n_difexp <- base::nrow(difexp)
  n_signature <- if (methods::is(omic_signature$signature, "data.frame")) base::nrow(omic_signature$signature) else 0L
  assay <- base::tolower(base::as.character(omic_signature$metadata$assay_type)[1])

  if (base::identical(assay, "transcriptomics") && n_difexp < HYPER_MIN_TRANSCRIPTOME_DIFEXP_ROWS) {
    return(base::sprintf("%d rows, fewer than a transcriptome's %d", n_difexp, HYPER_MIN_TRANSCRIPTOME_DIFEXP_ROWS))
  }
  if (n_signature > 0 && n_difexp <= 2L * n_signature) {
    return(base::sprintf("%d rows for a %d-feature signature", n_difexp, n_signature))
  }
  for (col in base::intersect(c("p_value", "pvalue", "adj_p"), base::colnames(difexp))) {
    p <- base::suppressWarnings(base::as.numeric(difexp[[col]]))
    if (base::any(!base::is.na(p)) && base::max(p, na.rm = TRUE) <= 0.05) {
      return(base::sprintf("every %s <= 0.05", col))
    }
  }
  NULL
}

#' Background for each query vector
#'
#' A number or gene vector is passed through for every query. "difexp" gives
#' each query its own signature's measured genes; signatures without difexp
#' symbols fall back to hypeR's default with one warning. A named list gives
#' each signature its own background (any of those three forms), looked up by
#' signature label, then by signature ID.
#'
#' @param inputs list(signatures, ids, labels) for every supplied signature,
#'   including ones that built no query.
#' NULL (the default) means "difexp" where a signature has a complete difexp
#' with gene symbols and hypeR's 23467 otherwise: silently when there is no
#' difexp, with a warning when the difexp looks filtered
#' (hypeRDifexpTruncation()). An explicit "difexp" uses any difexp.
#'
#' @return list(values = unnamed list, one background per row of `info`;
#'   sources = chr per row: "number", "genes", "difexp", "difexp-fallback" or
#'   "difexp-truncated")
#' @noRd
resolveQueryBackgrounds <- function(background, info, inputs, resolve_symbols) {
  if (base::is.list(background)) {
    keys <- base::names(background)
    unused <- keys[!keys %in% c(inputs$labels, inputs$ids[!base::is.na(inputs$ids)])]
    if (base::length(unused) > 0) {
      base::stop(base::sprintf(
        "\nbackground names %s match no signature label or ID (labels: %s).\n",
        base::paste(base::sprintf("'%s'", unused), collapse = ", "),
        base::paste(base::sprintf("'%s'", inputs$labels), collapse = ", ")
      ))
    }
    row_keys <- base::ifelse(
      info$signature_name %in% keys, info$signature_name,
      base::ifelse(!base::is.na(info$signature_id) & info$signature_id %in% keys, info$signature_id, NA_character_)
    )
    if (base::anyNA(row_keys)) {
      base::stop(base::sprintf(
        "\nThe background list has no entry for %s; name each element by signature label or ID.\n",
        base::paste(base::sprintf("'%s'", base::unique(info$signature_name[base::is.na(row_keys)])), collapse = ", ")
      ))
    }
    specs <- base::unname(background[row_keys])
  } else {
    specs <- base::rep(base::list(background), base::nrow(info))
  }

  # NULL for hypeR-native input, which cannot use "difexp".
  signatures_by_label <- if (base::is.null(inputs$signatures)) base::list() else stats::setNames(inputs$signatures, inputs$labels)
  universes <- base::list()
  fell_back <- base::character()
  values <- base::vector("list", base::nrow(info))
  sources <- base::character(base::nrow(info))

  warn_fallback <- base::character()
  truncated <- base::character()
  truncation_notes <- base::character()
  for (i in base::seq_len(base::nrow(info))) {
    spec <- specs[[i]]
    is_default <- base::is.null(spec)
    if (is_default && base::is.null(inputs$signatures)) {
      # hypeR-native input has no difexp to default to.
      is_default <- FALSE
      spec <- HYPER_DEFAULT_BACKGROUND
    }
    if (!is_default && !base::identical(spec, "difexp")) {
      values[i] <- base::list(spec)
      sources[i] <- if (base::is.character(spec)) "genes" else "number"
      next
    }

    label <- info$signature_name[i]
    sig <- signatures_by_label[[label]]
    has_difexp <- methods::is(sig$difexp, "data.frame") && base::nrow(sig$difexp) > 0
    if (base::is.null(universes[[label]]) && is_default && has_difexp) {
      note <- hypeRDifexpTruncation(sig)
      if (!base::is.null(note)) {
        truncated <- c(truncated, label)
        truncation_notes <- c(truncation_notes, base::sprintf("'%s' (%s)", label, note))
        universes[[label]] <- HYPER_DEFAULT_BACKGROUND
      }
    }
    if (base::is.null(universes[[label]])) {
      symbols <- if (has_difexp) {
        resolve_symbols(sig, "difexp", label)$symbols
      } else {
        NA_character_
      }
      symbols <- base::unique(symbols[!base::is.na(symbols)])
      if (base::length(symbols) == 0) {
        fell_back <- c(fell_back, label)
        if (!is_default) {
          warn_fallback <- c(warn_fallback, label)
        }
        universes[[label]] <- HYPER_DEFAULT_BACKGROUND
      } else {
        universes[[label]] <- symbols
      }
    }
    values[i] <- base::list(universes[[label]])
    sources[i] <- if (label %in% truncated) "difexp-truncated" else if (label %in% fell_back) "difexp-fallback" else "difexp"
  }

  if (base::length(truncation_notes) > 0) {
    base::warning(
      base::sprintf(
        "Default background: the difexp looks filtered for %s, so it is not the measured gene universe; used background = %s instead. Pass background = \"difexp\" to use it anyway.",
        base::paste(truncation_notes, collapse = ", "), HYPER_DEFAULT_BACKGROUND
      ),
      call. = FALSE
    )
  }

  if (base::length(warn_fallback) > 0) {
    base::warning(
      base::sprintf(
        "background = \"difexp\": no difexp gene symbols for %s; used background = %s instead.",
        base::paste(base::sprintf("'%s'", base::unique(warn_fallback)), collapse = ", "),
        HYPER_DEFAULT_BACKGROUND
      ),
      call. = FALSE
    )
  }

  base::list(values = values, sources = sources)
}

# Excel caps a cell at 32767 characters; the dropped-geneset list goes into
# hyp_to_excel()'s versioning sheet.
HYPER_INFO_MAX_CHARS <- 32000L

#' Append SigRepo provenance to a hyp object's info, in a fixed order
#'
#' hypeR::hyp_to_excel() stacks info across a multihyp with mapply(), so every
#' hyp must carry the same keys in the same order. Keys that do not apply are "".
#'
#' @param info_row One row of the prepared info data frame.
#' @param run list(ranked_table, score_col, split (all chr, "" when not
#'   applicable), background_source, query_genes_removed, genesets_dropped = chr,
#'   fdr_scope).
#' @noRd
appendHypeRProvenance <- function(hyp_obj, info_row, run) {
  blank_na <- function(x) if (base::is.na(x)) "" else base::as.character(x)
  dropped_list <- base::paste(run$genesets_dropped, collapse = "; ")
  if (base::nchar(dropped_list) > HYPER_INFO_MAX_CHARS) {
    dropped_list <- base::paste0(base::substr(dropped_list, 1L, HYPER_INFO_MAX_CHARS), " ...")
  }

  hyp_obj$info <- c(hyp_obj$info, base::list(
    "SigRepo Signature ID" = blank_na(info_row$signature_id),
    "SigRepo Signature Name" = base::as.character(info_row$signature_name),
    "Group Label" = blank_na(info_row$group_label),
    "Symbol Source" = blank_na(info_row$symbol_source),
    "SigRepo Direction" = blank_na(info_row$direction),
    "SigRepo Ranked Table" = run$ranked_table,
    "SigRepo Score Column" = run$score_col,
    "SigRepo Split" = run$split,
    "SigRepo Background Source" = run$background_source,
    "SigRepo Features Unmapped" = base::as.character(info_row$n_dropped),
    "SigRepo Query Genes Removed" = base::as.character(run$query_genes_removed),
    "SigRepo Genesets Dropped" = base::as.character(base::length(run$genesets_dropped)),
    "SigRepo Genesets Dropped List" = dropped_list,
    "SigRepo FDR Scope" = run$fdr_scope
  ))
  hyp_obj
}

HYPER_PROVENANCE_KEYS <- c(
  "SigRepo Signature ID", "SigRepo Signature Name", "Group Label", "Symbol Source",
  "SigRepo Direction", "SigRepo Ranked Table", "SigRepo Score Column", "SigRepo Split",
  "SigRepo Background Source", "SigRepo Features Unmapped", "SigRepo Query Genes Removed",
  "SigRepo Genesets Dropped", "SigRepo Genesets Dropped List", "SigRepo FDR Scope"
)

isHypeRBackgroundValue <- function(background) {
  is_number <- base::is.numeric(background) && base::length(background) == 1L &&
    base::is.finite(background) && background > 0
  is_genes <- base::is.character(background) && base::length(background) >= 2L
  is_number || is_genes || base::identical(background, "difexp")
}

#' Error unless background is NULL, a positive number, a gene vector, "difexp",
#' or a named list of those
#' @noRd
checkHypeRBackground <- function(background) {
  if (base::is.null(background)) {
    return(base::invisible(background))
  }
  if (base::is.list(background)) {
    keys <- base::names(background)
    if (base::length(background) == 0 || base::is.null(keys) || base::any(base::is.na(keys) | !base::nzchar(keys)) ||
        base::anyDuplicated(keys)) {
      base::stop("\nA background list must name every element uniquely, by signature label or signature ID.\n")
    }
    bad <- keys[!base::vapply(background, isHypeRBackgroundValue, base::logical(1))]
    if (base::length(bad) > 0) {
      base::stop(base::sprintf(
        "\nbackground must be a number, a gene vector, or \"difexp\" for every list element; not for %s.\n",
        base::paste(base::sprintf("'%s'", bad), collapse = ", ")
      ))
    }
    return(base::invisible(background))
  }
  if (!isHypeRBackgroundValue(background)) {
    base::stop("\nbackground must be a number, a gene vector, or \"difexp\" (or a named list of those, one per signature).\n")
  }
  base::invisible(background)
}

#' Drop genesets hypeR's kstest cannot score
#'
#' hypeR's kstest breaks on two kinds of geneset, erroring or silently shifting
#' scores onto other genesets:
#' \itemize{
#'   \item (weighted kstest, `power != 0` only) every member present in `query`
#'   has score exactly 0, so the hit weights sum to 0;
#'   \item (any power) the members present in `query` number exactly
#'   `length(query)`, so there are no misses and the miss step divides by 0.
#' }
#' Genesets with no members in `query` are kept.
#'
#' hypeR reduces genesets to a gene-vector background before the kstest, so
#' when `background` is a character vector the checks use each geneset's
#' members within it. The returned genesets are not reduced; hypeR still does
#' that itself.
#'
#' @param genesets Named list, `hypeR::gsets` or `hypeR::rgsets`.
#' @param query Named numeric vector (gene -> score).
#' @param background The query's background; only a character vector matters.
#' @param power hypeR's `power`; the zero-weight check runs only when it is not 0.
#' @return list(genesets = same kind as the input, unchanged when nothing is
#'   dropped, or NULL when every geneset is dropped (hypeR cannot build an
#'   empty gsets); dropped = chr labels)
#' @noRd
dropZeroWeightGenesets <- function(genesets, query, background = NULL, power = 1) {
  unchanged <- base::list(genesets = genesets, dropped = base::character())
  query_genes <- base::unique(base::names(query))
  check_zero_weight <- power != 0
  zero_genes <- base::names(query)[query == 0]
  nonzero_genes <- base::names(query)[query != 0]

  is_hyper_gsets <- methods::is(genesets, "gsets") || methods::is(genesets, "rgsets")
  members <- if (is_hyper_gsets) genesets$genesets else genesets
  reduce_to_background <- base::is.character(background)
  drop <- base::vapply(members, function(genes) {
    if (reduce_to_background) {
      genes <- genes[genes %in% background]
    }
    genes <- base::unique(genes)
    zero_weight <- check_zero_weight && base::length(zero_genes) > 0 &&
      base::any(genes %in% zero_genes) && !base::any(genes %in% nonzero_genes)
    full_coverage <- base::sum(genes %in% query_genes) == base::length(query_genes)
    zero_weight || full_coverage
  }, base::logical(1))
  if (!base::any(drop)) {
    return(unchanged)
  }

  kept_labels <- base::names(members)[!drop]
  reduced <- if (base::length(kept_labels) == 0) {
    NULL
  } else if (methods::is(genesets, "rgsets")) {
    genesets$subset(kept_labels)
  } else if (methods::is(genesets, "gsets")) {
    hypeR::gsets$new(members[!drop], name = genesets$name, version = genesets$version, quiet = TRUE)
  } else {
    genesets[!drop]
  }

  base::list(genesets = reduced, dropped = base::names(members)[drop])
}

#' Number of genesets in a named list, `hypeR::gsets` or `hypeR::rgsets` (0 for NULL)
#' @noRd
countHypeRGenesets <- function(genesets) {
  if (methods::is(genesets, "gsets") || methods::is(genesets, "rgsets")) {
    return(base::length(genesets$genesets))
  }
  base::length(genesets)
}

warnSkippedHypeRSignatures <- function(skipped) {
  if (base::nrow(skipped) == 0) {
    return(base::invisible(NULL))
  }
  base::warning(
    base::sprintf(
      "Skipped %d signature(s): %s",
      base::nrow(skipped),
      base::paste(base::sprintf("'%s' (%s: %s)", skipped$signature, skipped$reason, skipped$message), collapse = "; ")
    ),
    call. = FALSE
  )
}

#' Apply pval/fdr cutoffs to hyp results and record them in args and info
#' @noRd
filterHypeRResults <- function(results, pval, fdr) {
  for (k in base::seq_along(results)) {
    data <- results[[k]]$data
    results[[k]]$data <- data[data$pval <= pval & data$fdr <= fdr, , drop = FALSE]
    results[[k]]$args$pval <- pval
    results[[k]]$args$fdr <- fdr
    results[[k]]$info[["P-Value"]] <- base::as.character(pval)
    results[[k]]$info[["FDR"]] <- base::as.character(fdr)
  }
  results
}

#' FDR across every query of a run, then hypeR's pval/fdr filters
#'
#' BS831's hyperEnrichment() (mht = TRUE) adjusts p-values across all
#' signatures x genesets tested, not per signature. hypeR keeps only p-values
#' rounded to 2 significant digits, so the pooled FDR is computed from those,
#' as hyperEnrichment() does. With a single query the FDR hypeR computed is
#' kept. hypeR ran with pval = fdr = 1, so the filters are applied here and
#' the hyp's args and info record the requested cutoffs.
#' @noRd
applyRunHypeRFdr <- function(results, pval, fdr) {
  if (base::length(results) > 1L) {
    n_rows <- base::vapply(results, function(h) base::nrow(h$data), base::integer(1))
    pooled <- stats::p.adjust(base::unlist(base::lapply(results, function(h) h$data$pval), use.names = FALSE), method = "fdr")
    ends <- base::cumsum(n_rows)
    for (k in base::seq_along(results)) {
      if (n_rows[k] > 0) {
        results[[k]]$data$fdr <- base::signif(pooled[(ends[k] - n_rows[k] + 1L):ends[k]], 2)
      }
    }
  }
  filterHypeRResults(results, pval, fdr)
}

#' FDR scope, filters and return type shared by every test in runHypeR()
#'
#' @param results Named list of hyp objects, one per query (or fgsea side).
#' @return A hyp when the input is single and the call yields one hyp by
#'   construction, otherwise a multihyp.
#' @noRd
finishHypeRRun <- function(results, fdr_scope, pval, fdr, test, direction, split, inputs, native) {
  if (base::identical(test, "fgsea")) {
    # runFgseaQueries() already pooled the FDR (fdr_scope = "run", across
    # every up/down side of every ranking, before direction dropped any) or
    # kept fgsea's own per-ranking padj (fdr_scope = "query"); only the
    # pval/fdr cutoffs remain to apply here, for both scopes.
    results <- filterHypeRResults(results, pval = pval, fdr = fdr)
  } else if (fdr_scope == "run") {
    results <- applyRunHypeRFdr(results, pval = pval, fdr = fdr)
  }

  categorical <- !native && base::length(inputs$signatures) == 1L && isCategoricalHypeRSignature(inputs$signatures[[1]])
  one_by_construction <- if (native) {
    !(base::identical(test, "fgsea") && base::identical(direction, "both"))
  } else if (test %in% c("kstest", "fgsea")) {
    !base::identical(direction, "both") && !categorical
  } else {
    !split
  }
  if (inputs$single && one_by_construction) {
    return(results[[1]])
  }
  hypeR::multihyp$new(data = results)
}

#' Run hypeR enrichment on SigRepo signatures
#'
#' @description Builds hypeR query vectors from SigRepo signatures (see
#' \code{prepareHypeRSignatures()}) and runs \code{hypeR::hypeR()} on them. The
#' arguments after \code{query_names} are hypeR's own, with hypeR's defaults,
#' and the result is hypeR's own object. Plot it with \code{plotHypeRDots()},
#' \code{plotHypeREnrichment()} and \code{plotHypeRMap()}; hypeR's own
#' \code{hypeR::hyp_dots()},
#' \code{hyp_emap()}, \code{hyp_to_rmd()}, \code{hyp_to_table()} and
#' \code{rctbl_build()} work on it directly, and \code{hyp_hmap()} does when
#' \code{genesets} is a \code{hypeR::rgsets}. \code{hyp_show()} takes a single
#' \code{hyp}, so pass one element of a \code{multihyp}, e.g.
#' \code{hypeR::hyp_show(res$data[[1]])}; \code{hyp_to_graph()} also needs
#' \code{rgsets} genesets.
#'
#' \code{hypeR::hyp_to_excel()} uses each query name as an Excel sheet name,
#' which must be at most 31 characters and cannot contain
#' \code{: \\ / ? * [ ]}; most SigRepo signature names are longer. Use
#' \code{hypeRToExcel()}, which makes the sheet names safe and adds an index
#' sheet, or shorten the names with \code{query_names}.
#'
#' @section Required and optional arguments:
#' Only two things are required: the signatures to test, and the genesets to
#' test them against.
#' \itemize{
#'   \item \strong{Signatures}, as exactly one of: \code{signature_id} or
#'   \code{signature_name} (each with \code{conn_handler}),
#'   \code{omic_signature}, or \code{signature} (hypeR-native gene symbols or
#'   a named score vector). They cannot be combined.
#'   \item \strong{\code{genesets}}: \code{"msigdb"} together with
#'   \code{msigdb_collection}, or your own genesets (a named list,
#'   \code{hypeR::gsets} or \code{hypeR::rgsets}).
#' }
#'
#' Every other argument is optional: each has a default, listed with it below,
#' and leaving it alone runs the lab's conventions (see the Details of
#' \code{background}, \code{min_query_genes} and \code{fdr_scope}). The
#' arguments that take \code{NULL} treat it as "choose for me" rather than
#' "nothing": \code{background = NULL} picks the background per signature,
#' \code{query_names = NULL} builds the default query names,
#' \code{seed = NULL} leaves fgsea on the session's random-number state, and
#' the \code{msigdb_*} arguments apply only with \code{genesets = "msigdb"}.
#'
#' @inheritParams prepareHypeRSignatures
#' @inheritParams getHypeRGenesets
#' @param seed \code{test = "fgsea"} only: fgsea's p-values come from random
#' sampling, so each ranking's run starts from \code{set.seed(seed)} and the
#' caller's random-number state is restored afterwards. Defaults to \code{1};
#' \code{NULL} uses the session's random-number state. Recorded as \code{Seed}
#' in \code{info}.
#' @param fgsea_args \code{test = "fgsea"} only: a named list of
#' \code{fgsea::fgseaMultilevel()} arguments merged over the defaults
#' \code{sampleSize = 101, minSize = 1, maxSize = Inf}, e.g.
#' \code{list(minSize = 15, maxSize = 500, nproc = 4)}. \code{stats},
#' \code{pathways} and \code{gseaParam} (set by \code{power}) cannot be given.
#' @param min_query_genes Hypergeometric only: skip, with a warning, a query
#' with fewer than this many genes found in the genesets. Defaults to \code{4},
#' the \code{min.drawsize} of BS831's \code{hyperEnrichment()}.
#' @param fdr_scope \code{"run"} (default) adjusts p-values for multiple testing
#' across every query and geneset in the call, as BS831's
#' \code{hyperEnrichment()} does across signatures; \code{"query"} adjusts
#' within each query, as \code{hypeR::hypeR()} does for a named list. With one
#' query both are the same. The pooled FDR is computed from hypeR's p-values,
#' which are rounded to 2 significant digits. \code{pval} and \code{fdr} filter
#' on the FDR of the chosen scope. For \code{test = "fgsea"}, \code{"run"}
#' pools the up and down sides of every ranking in the call, regardless of
#' \code{direction} (also with a single ranking); \code{"query"} keeps
#' fgsea's own per-ranking \code{padj}, which already covers both sides.
#' @param background \code{NULL} (default) uses each signature's measured genes
#' from its difexp table when it has difexp gene symbols, and \code{23467}
#' otherwise (always \code{23467} for \code{signature} input). A difexp that
#' looks filtered is not a measured-gene universe, so the default also uses
#' \code{23467}, with a warning, when a transcriptomics difexp has fewer than
#' 10,000 rows, a difexp has no more than twice its signature's rows, or every
#' \code{p_value}, \code{pvalue} or \code{adj_p} is at most 0.05. For
#' \code{test = "kstest"} the default is \code{23467}: the ranked list is the
#' universe, so the background does not enter the test. \code{test = "fgsea"}
#' defaults to \code{23467} as well and for the same reason (the ranking is
#' the universe): a number has no effect on the fgsea result, while a gene
#' vector or \code{"difexp"} still reduces the genesets to those genes (the
#' \code{geneset} column) as hypeR does. Otherwise
#' hypeR's background: a single number (population size), a
#' character vector of background genes, or \code{"difexp"} to use each
#' signature's measured genes from its difexp table. With an explicit
#' \code{"difexp"}, signatures without difexp
#' symbols fall back to \code{23467} with a warning. For a different background
#' per signature, pass a named list of those forms, one element per signature,
#' named by signature label (see \code{prepareHypeRSignatures()$info$signature_name};
#' with \code{signature}, the list names) or by signature ID; arms of a split
#' signature share its background. A name that matches no signature, or a
#' signature with no entry, is an error. With a gene vector or
#' \code{"difexp"}, a hypergeometric query is first reduced to the background
#' genes, with a warning naming each query that lost genes: hypeR reduces only
#' the genesets, and query genes outside the background would distort the
#' p-values. Anything else (e.g. \code{"Difexp"}) is an error.
#' @param power Exponent for the score weights: fgsea's \code{gseaParam} with
#' \code{test = "fgsea"}, where it does change the p-values. For kstest it changes
#' only the enrichment \code{score}. \code{1} (default) weights hits by
#' |score|. \code{0} runs hypeR's ranked (unweighted) signature: the query is
#' passed as gene names in rank order, so \code{score} and
#' \code{Signature Type = "ranked"} match hypeR's documented ranked test. The
#' \code{pval} and \code{fdr} come from hypeR's unweighted, one-sided KS test
#' and do not depend on \code{power} or \code{absolute}. That test finds
#' genesets enriched toward the top of the ranking; use \code{direction} to
#' test the other end. hypeR's kstest also cannot score a geneset whose hits in
#' a query all have score 0 (\code{power != 0}) or that contains every query
#' gene (any power); such genesets are dropped for that query with a warning,
#' which also removes them from the FDR adjustment. They are listed in the
#' result's \code{info} (see Value).
#' @param absolute Passed to \code{hypeR::hypeR()} (kstest only). hypeR marks
#' it as not fully implemented; \code{TRUE} is an error with
#' \code{test = "fgsea"}. Defaults to \code{FALSE}.
#' @param pval Keep results with p-value at or below this. Defaults to \code{1}.
#' @param fdr Keep results with FDR at or below this. Defaults to \code{1}.
#' @param plotting Logical; generate hypeR's per-geneset plots. Defaults to
#' \code{FALSE}, in which case the empty placeholder plots hypeR stores anyway
#' are removed (they are most of the object's size). fgsea makes no plots, so
#' \code{TRUE} with \code{test = "fgsea"} warns and is ignored. To plot one
#' geneset without storing every plot, use \code{plotHypeREnrichment()}.
#' @param quiet Logical; suppress hypeR's logs. Defaults to \code{TRUE}. With
#' \code{test = "fgsea"} this also suppresses fgsea's own console output
#' (its progress bars).
#'
#' @return A \code{hyp} when the input is a single vector in \code{signature},
#' or a single signature (one \code{OmicSignature}, or one id or name) and the
#' test builds one query by construction (\code{split = FALSE}, or
#' \code{test = "kstest"} or \code{"fgsea"} with \code{direction} \code{"up"}
#' or \code{"down"} on a signature that is not categorical; a single
#' \code{signature} vector gives a \code{multihyp} only for fgsea with
#' \code{"both"}). Otherwise a \code{multihyp} named
#' by query, even when only one query is left. This mirrors hypeR, where a
#' vector gives a \code{hyp} and a named list a \code{multihyp}.
#'
#' With \code{test = "fgsea"}, each ranking runs
#' \code{fgsea::fgseaMultilevel()} once, and its pathways are split by the sign
#' of their enrichment score into \code{"<query> | up"} (ES > 0) and
#' \code{"<query> | down"} (ES < 0) hyps (just \code{"<query>"} for
#' \code{direction} \code{"up"} or \code{"down"}); pathways with ES = 0 join
#' neither. Their \code{data} has \code{label}, \code{pval}, \code{fdr},
#' \code{lte} (log2err), \code{es}, \code{nes}, \code{signature},
#' \code{geneset}, \code{overlap}, \code{le} and \code{hits} (the leading
#' edge), and \code{info} adds \code{fgsea}, \code{Sample Size},
#' \code{Min Size}, \code{Max Size}, \code{Seed} and \code{fgsea Args}
#' before the SigRepo keys. fgsea's own FDR covers every pathway of the
#' ranking, both sides.
#'
#' Each \code{hyp$info} ends with these keys, in this order (\code{""} when a
#' key does not apply): \code{SigRepo Signature ID},
#' \code{SigRepo Signature Name}, \code{Group Label}, \code{Symbol Source},
#' \code{SigRepo Direction}, \code{SigRepo Ranked Table},
#' \code{SigRepo Score Column}, \code{SigRepo Split},
#' \code{SigRepo Background Source} (\code{number}, \code{genes},
#' \code{difexp}, \code{difexp-fallback} or \code{difexp-truncated}),
#' \code{SigRepo Features Unmapped}, \code{SigRepo Query Genes Removed},
#' \code{SigRepo Genesets Dropped}, \code{SigRepo Genesets Dropped List} and
#' \code{SigRepo FDR Scope}.
#'
#' Signatures that cannot produce a query are skipped with a warning; if none
#' can, an error is raised. A query left with no genesets or no genes (after
#' the drops described under \code{power} and \code{background}), or a
#' hypergeometric query below \code{min_query_genes}, is skipped with a
#' warning; if no query is left, an error is raised.
#'
#' @examples
#' \dontrun{
#' utils::data("LLFS_Aging_Gene_2023", package = "SigRepo")
#' hallmark <- SigRepo::getHypeRGenesets("msigdb", msigdb_collection = "H")
#'
#' hyp <- SigRepo::runHypeR(
#'   omic_signature = LLFS_Aging_Gene_2023,
#'   genesets = hallmark,
#'   fdr = 0.05
#' )
#' hypeR::hyp_dots(hyp)
#' SigRepo::hypeRToExcel(hyp, file_path = "llfs_hallmark.xlsx")
#'
#' ranked <- SigRepo::runHypeR(
#'   omic_signature = LLFS_Aging_Gene_2023,
#'   genesets = hallmark,
#'   test = "kstest",
#'   direction = "both"
#' )
#' }
#'
#' @export
runHypeR <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    signature = NULL,
    genesets,
    msigdb_species = NULL,
    msigdb_collection = NULL,
    msigdb_subcollection = NULL,
    msigdb_clean = FALSE,
    test = c("hypergeometric", "kstest", "fgsea"),
    split = TRUE,
    direction = c("up", "down", "both"),
    ks_source = c("difexp", "signature"),
    score_col = "score",
    query_names = NULL,
    min_query_genes = 4,
    fdr_scope = c("run", "query"),
    seed = 1,
    fgsea_args = base::list(),
    background = NULL,
    power = 1,
    absolute = FALSE,
    pval = 1,
    fdr = 1,
    plotting = FALSE,
    quiet = TRUE,
    verbose = TRUE
) {
  if (!base::requireNamespace("hypeR", quietly = TRUE)) {
    base::stop("\nPackage 'hypeR' is required for runHypeR(). Please install it first.\n")
  }

  direction_missing <- base::missing(direction)
  test <- base::match.arg(test)
  direction <- if (base::identical(test, "fgsea") && direction_missing) "both" else base::match.arg(direction)
  ks_source <- base::match.arg(ks_source)
  fdr_scope <- base::match.arg(fdr_scope)
  if (base::identical(test, "fgsea")) {
    checkHypeRFgseaArgs(absolute = absolute, seed = seed, fgsea_args = fgsea_args, plotting = plotting)
  }
  checkHypeRBackground(background)
  checkHypeRSplit(split)
  checkHypeRKstestArgs(test, direction, ks_source)
  checkHypeRQueryNames(query_names)
  if (!(base::is.numeric(min_query_genes) && base::length(min_query_genes) == 1L && !base::is.na(min_query_genes) && min_query_genes >= 1)) {
    base::stop("\n'min_query_genes' must be a single number of at least 1.\n")
  }

  if (base::missing(genesets)) {
    genesets <- NULL
  }
  resolved_genesets <- getHypeRGenesets(
    genesets = genesets,
    msigdb_species = msigdb_species,
    msigdb_collection = msigdb_collection,
    msigdb_subcollection = msigdb_subcollection,
    msigdb_clean = msigdb_clean
  )

  if (!base::is.null(signature) &&
      (base::identical(background, "difexp") || (base::is.list(background) && base::any(base::vapply(background, base::identical, base::logical(1), "difexp"))))) {
    base::stop("\nbackground = \"difexp\" needs SigRepo signatures; 'signature' input has no difexp. Pass a number or gene vector.\n")
  }

  resolve_symbols <- newHypeRSymbolResolver(conn_handler)
  collected <- collectAndBuildHypeRQueries(
    conn_handler = conn_handler, signature_id = signature_id, signature_name = signature_name,
    omic_signature = omic_signature, signature = signature, test = test, split = split,
    direction = direction, ks_source = ks_source, score_col = score_col, query_names = query_names,
    resolve_symbols = resolve_symbols, verbose = verbose
  )
  inputs <- collected$inputs
  prepared <- collected$prepared

  warnSkippedHypeRSignatures(prepared$skipped)
  if (base::length(prepared$signatures) == 0) {
    base::stop("\nNo signature produced a hypeR query vector; see the warning for each signature's reason.\n")
  }

  # The default difexp universe matters for the hypergeometric test only; a
  # ranked test's universe is its ranked list, so it keeps hypeR's number.
  if (base::is.null(background) && test %in% c("kstest", "fgsea")) {
    background <- HYPER_DEFAULT_BACKGROUND
  }
  backgrounds <- resolveQueryBackgrounds(background, prepared$info, inputs, resolve_symbols)

  if (base::identical(test, "fgsea")) {
    results <- runFgseaQueries(
      prepared = prepared, backgrounds = backgrounds, genesets = resolved_genesets, direction = direction,
      power = power, seed = seed, fgsea_args = fgsea_args, quiet = quiet, native = collected$native,
      ks_source = ks_source, score_col = score_col, fdr_scope = fdr_scope
    )
    return(finishHypeRRun(results, fdr_scope, pval, fdr, test, direction, split, inputs, collected$native))
  }

  queries <- prepared$signatures
  query_names_out <- base::names(queries)

  # hypeR reduces genesets to a gene-vector background but not the query, so a
  # hypergeometric query gene outside the background would distort the
  # p-values. Keep each query inside its background.
  removed <- base::integer(base::length(queries))
  if (base::identical(test, "hypergeometric")) {
    for (i in base::seq_along(queries)) {
      if (base::is.character(backgrounds$values[[i]])) {
        measured <- queries[[i]] %in% backgrounds$values[[i]]
        removed[i] <- base::sum(!measured)
        queries[[i]] <- queries[[i]][measured]
      }
    }
    # One warning per background kind, so each names what the genes were missing from.
    for (kind in c("difexp", "genes")) {
      hit <- removed > 0 & base::startsWith(backgrounds$sources, kind)
      if (base::any(hit)) {
        base::warning(
          base::sprintf(
            "%s: removed %s query gene(s) not %s from: %s",
            if (kind == "genes") "background" else if (base::identical(background, "difexp")) "background = \"difexp\"" else "background (difexp)",
            base::sum(removed[hit]),
            if (kind == "difexp") "measured in difexp" else "in the background gene vector",
            base::paste(base::sprintf("'%s' (%d)", query_names_out[hit], removed[hit]), collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  }

  # hypeR's kstest cannot score some genesets (all hits score 0 under
  # power != 0, or a geneset covering every query gene); it errors or
  # misaligns scores, so drop those per query before calling hypeR.
  query_genesets <- base::lapply(base::seq_along(queries), function(i) {
    if (base::identical(test, "kstest")) {
      dropZeroWeightGenesets(resolved_genesets, queries[[i]], background = backgrounds$values[[i]], power = power)
    } else {
      base::list(genesets = resolved_genesets, dropped = base::character())
    }
  })
  dropped <- base::unique(base::unlist(base::lapply(query_genesets, `[[`, "dropped"), use.names = FALSE))
  if (base::length(dropped) > 0) {
    base::warning(
      base::sprintf(
        "Dropped %d geneset(s) that hypeR's kstest cannot score (all hits score 0, or the geneset covers every query gene): %s",
        base::length(dropped),
        base::paste(c(dropped[base::seq_len(base::min(10L, base::length(dropped)))], if (base::length(dropped) > 10) "..."), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # A query with no genesets or no genes left cannot run; skip it rather than
  # letting hypeR abort the whole batch.
  runnable <- base::vapply(base::seq_along(queries), function(i) {
    base::length(queries[[i]]) > 0 && countHypeRGenesets(query_genesets[[i]]$genesets) > 0
  }, base::logical(1))
  if (!base::all(runnable)) {
    base::warning(
      base::sprintf(
        "Skipped %d query(ies) with nothing left to test after removing zero-weight genesets or unmeasured genes: %s",
        base::sum(!runnable),
        base::paste(base::sprintf("'%s'", query_names_out[!runnable]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # BS831's hyperEnrichment() skips a draw with fewer than min.drawsize (4)
  # genes found among the genesets: a p-value from one or two hits says little.
  if (base::identical(test, "hypergeometric")) {
    members <- if (methods::is(resolved_genesets, "gsets") || methods::is(resolved_genesets, "rgsets")) {
      resolved_genesets$genesets
    } else {
      resolved_genesets
    }
    annotated <- base::unique(base::unlist(members, use.names = FALSE))
    n_found <- base::vapply(queries, function(q) base::sum(q %in% annotated), base::integer(1))
    too_small <- runnable & n_found < min_query_genes
    if (base::any(too_small)) {
      base::warning(
        base::sprintf(
          "Skipped %d query(ies) with fewer than min_query_genes = %s genes found in the genesets: %s",
          base::sum(too_small), min_query_genes,
          base::paste(base::sprintf("'%s' (%d)", query_names_out[too_small], n_found[too_small]), collapse = ", ")
        ),
        call. = FALSE
      )
      runnable <- runnable & !too_small
    }
  }
  if (!base::any(runnable)) {
    base::stop("\nNo query had genesets left to test; see the warnings for the dropped genesets and skipped queries.\n")
  }
  run_idx <- base::which(runnable)
  is_kstest <- base::identical(test, "kstest")

  results <- base::lapply(run_idx, function(i) {
    if (!quiet && base::length(run_idx) > 1) {
      base::cat(base::sprintf("\n%s\n", query_names_out[i]))
    }
    # A character vector in rank order takes hypeR's ranked (unweighted)
    # branch; a named numeric vector at power 0 would take the weighted branch
    # with all weights 1, which scores differently.
    query <- if (is_kstest && power == 0) base::names(queries[[i]]) else queries[[i]]
    hyp_obj <- hypeR::hypeR(
      signature = query,
      genesets = query_genesets[[i]]$genesets,
      test = test,
      background = backgrounds$values[[i]],
      power = power,
      absolute = absolute,
      # With fdr_scope = "run" the FDR is recomputed across every query below,
      # so hypeR must keep every row until then.
      pval = if (fdr_scope == "run") 1 else pval,
      fdr = if (fdr_scope == "run") 1 else fdr,
      plotting = plotting,
      quiet = quiet
    )
    if (!plotting) {
      # hypeR stores one empty ggplot per geneset even with plotting = FALSE
      # (about 1.4 MB each serialized); nothing downstream reads them.
      hyp_obj$plots <- base::list()
    }
    appendHypeRProvenance(hyp_obj, prepared$info[i, , drop = FALSE], base::list(
      ranked_table = if (is_kstest && !collected$native) ks_source else "",
      score_col = if (is_kstest && !collected$native) score_col else "",
      split = if (is_kstest || collected$native) "" else base::as.character(split),
      background_source = backgrounds$sources[i],
      query_genes_removed = removed[i],
      genesets_dropped = query_genesets[[i]]$dropped,
      fdr_scope = fdr_scope
    ))
  })
  base::names(results) <- query_names_out[run_idx]

  finishHypeRRun(results, fdr_scope, pval, fdr, test, direction, split, inputs, collected$native)
}

#' Excel-safe, case-insensitively unique sheet names
#'
#' Excel sheet names are at most 31 characters, cannot contain
#' \code{: \\ / ? * [ ]}, cannot start or end with an apostrophe, and are
#' compared without regard to case.
#' @noRd
hypeRSheetNames <- function(x, reserved = base::character()) {
  clean <- function(s) base::gsub("^'+|'+$", "", s)
  x <- clean(base::trimws(base::gsub("[][*?/\\\\:]", "_", base::as.character(x))))
  x[base::is.na(x) | !base::nzchar(x)] <- "sheet"

  used <- base::tolower(reserved)
  out <- base::character(base::length(x))
  for (i in base::seq_along(x)) {
    candidate <- clean(base::substr(x[i], 1L, 31L))
    k <- 2L
    while (!base::nzchar(candidate) || base::tolower(candidate) %in% used) {
      suffix <- base::sprintf(" (%d)", k)
      candidate <- base::paste0(clean(base::substr(x[i], 1L, 31L - base::nchar(suffix))), suffix)
      k <- k + 1L
    }
    out[i] <- candidate
    used <- c(used, base::tolower(candidate))
  }
  out
}

#' Write runHypeR() results to Excel with safe sheet names
#'
#' @description Writes a \code{hyp} or \code{multihyp} with
#' \code{hypeR::hyp_to_excel()} after turning query names into valid Excel
#' sheet names (at most 31 characters, none of \code{: \\ / ? * [ ]}, unique
#' regardless of case). An \code{index} sheet, placed first, maps each sheet
#' back to its full query name and SigRepo provenance. The object you pass is
#' not modified.
#'
#' @section Required and optional arguments:
#' \code{hyp_obj} and \code{file_path} are required. \code{cols},
#' \code{versioning} and \code{index} are optional: by default every column is
#' written, with hypeR's \code{versioning} sheet and the SigRepo \code{index}
#' sheet. \code{cols = NULL} means "all columns", not "none". The hypeR and
#' openxlsx packages must be installed.
#'
#' @param hyp_obj A \code{hyp} or \code{multihyp}, usually from \code{runHypeR()}.
#' @param file_path Path of the \code{.xlsx} file to write (overwritten).
#' @param cols Passed to \code{hypeR::hyp_to_excel()}: columns of each result
#' table to write. Defaults to all.
#' @param versioning Passed to \code{hypeR::hyp_to_excel()}: add hypeR's
#' \code{versioning} sheet with each query's \code{info}. Defaults to \code{TRUE}.
#' @param index Logical; add the \code{index} sheet. Defaults to \code{TRUE}.
#'
#' @return Invisibly, the index as a data frame: \code{sheet}, \code{query},
#' \code{signature_id}, \code{signature_name}, \code{group_label},
#' \code{direction}.
#'
#' @examples
#' \dontrun{
#' hyp <- SigRepo::runHypeR(omic_signature = sig, genesets = "msigdb", msigdb_collection = "H")
#' SigRepo::hypeRToExcel(hyp, file_path = "results.xlsx")
#' }
#'
#' @export
hypeRToExcel <- function(hyp_obj, file_path, cols = NULL, versioning = TRUE, index = TRUE) {
  for (pkg in c("hypeR", "openxlsx")) {
    if (!base::requireNamespace(pkg, quietly = TRUE)) {
      base::stop(base::sprintf("\nPackage '%s' is required for hypeRToExcel(). Please install it first.\n", pkg))
    }
  }

  info_value <- function(h, key) {
    value <- h$info[[key]]
    if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1])) "" else base::as.character(value[1])
  }

  if (methods::is(hyp_obj, "multihyp")) {
    data <- hyp_obj$data
  } else if (methods::is(hyp_obj, "hyp")) {
    parts <- c(info_value(hyp_obj, "SigRepo Signature Name"), info_value(hyp_obj, "Group Label"), info_value(hyp_obj, "SigRepo Direction"))
    parts <- parts[base::nzchar(parts)]
    data <- stats::setNames(base::list(hyp_obj), if (base::length(parts) > 0) base::paste(parts, collapse = " | ") else "results")
  } else {
    base::stop("\n'hyp_obj' must be a hypeR hyp or multihyp object.\n")
  }

  reserved <- c(if (index) "index", if (versioning) "versioning")
  sheets <- hypeRSheetNames(base::names(data), reserved = reserved)
  hypeR::hyp_to_excel(
    hypeR::multihyp$new(data = stats::setNames(data, sheets)),
    file_path = file_path, cols = cols, versioning = versioning
  )

  index_df <- base::data.frame(
    sheet = sheets,
    query = base::names(data),
    signature_id = base::vapply(data, info_value, base::character(1), key = "SigRepo Signature ID", USE.NAMES = FALSE),
    signature_name = base::vapply(data, info_value, base::character(1), key = "SigRepo Signature Name", USE.NAMES = FALSE),
    group_label = base::vapply(data, info_value, base::character(1), key = "Group Label", USE.NAMES = FALSE),
    direction = base::vapply(data, info_value, base::character(1), key = "SigRepo Direction", USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )

  if (index) {
    wb <- openxlsx::loadWorkbook(file_path)
    openxlsx::addWorksheet(wb, sheetName = "index")
    openxlsx::writeData(wb, sheet = "index", x = index_df, colNames = TRUE, rowNames = FALSE)
    n_sheets <- base::length(base::names(wb))
    openxlsx::worksheetOrder(wb) <- c(n_sheets, base::seq_len(n_sheets - 1L))
    openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
  }

  base::invisible(index_df)
}
