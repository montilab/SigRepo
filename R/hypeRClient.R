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


#' Background for each query vector
#'
#' A number or gene vector is passed through for every query. "difexp" gives
#' each query its own signature's measured genes; signatures without difexp
#' symbols fall back to hypeR's default with one warning.
#'
#' @return Unnamed list, one background per row of `info`.
#' @noRd
resolveQueryBackgrounds <- function(background, info, inputs, conn_handler) {
  if (!(base::is.character(background) && base::length(background) == 1L && base::identical(background, "difexp"))) {
    return(base::rep(base::list(background), base::nrow(info)))
  }

  signatures_by_label <- stats::setNames(inputs$signatures, inputs$labels)
  universes <- base::list()
  fell_back <- base::character()

  for (label in base::unique(info$signature_name)) {
    sig <- signatures_by_label[[label]]
    symbols <- if (methods::is(sig$difexp, "data.frame") && base::nrow(sig$difexp) > 0) {
      resolveSignatureSymbols(sig, "difexp", conn_handler)$symbols
    } else {
      NA_character_
    }
    symbols <- base::unique(symbols[!base::is.na(symbols)])

    if (base::length(symbols) == 0) {
      fell_back <- c(fell_back, label)
      universes[[label]] <- HYPER_DEFAULT_BACKGROUND
    } else {
      universes[[label]] <- symbols
    }
  }

  if (base::length(fell_back) > 0) {
    base::warning(
      base::sprintf(
        "background = \"difexp\": no difexp gene symbols for %s; used background = %s instead.",
        base::paste(base::sprintf("'%s'", fell_back), collapse = ", "),
        HYPER_DEFAULT_BACKGROUND
      ),
      call. = FALSE
    )
  }

  base::unname(universes[info$signature_name])
}

#' Append SigRepo provenance to a hyp object's info, in a fixed order
#'
#' hypeR::hyp_to_excel() stacks info across a multihyp with mapply(), so every
#' hyp must carry the same keys in the same order.
#' @noRd
appendHypeRProvenance <- function(hyp_obj, info_row) {
  hyp_obj$info <- c(hyp_obj$info, base::list(
    "SigRepo Signature ID" = if (base::is.na(info_row$signature_id)) "" else base::as.character(info_row$signature_id),
    "SigRepo Signature Name" = base::as.character(info_row$signature_name),
    "Group Label" = if (base::is.na(info_row$group_label)) "" else base::as.character(info_row$group_label),
    "Symbol Source" = if (base::is.na(info_row$symbol_source)) "" else base::as.character(info_row$symbol_source)
  ))
  hyp_obj
}

#' Error unless background is a single positive number, a gene vector, or "difexp"
#' @noRd
checkHypeRBackground <- function(background) {
  is_number <- base::is.numeric(background) && base::length(background) == 1L &&
    base::is.finite(background) && background > 0
  is_genes <- base::is.character(background) && base::length(background) >= 2L
  is_difexp <- base::identical(background, "difexp")
  if (!(is_number || is_genes || is_difexp)) {
    base::stop("\nbackground must be a number, a gene vector, or \"difexp\".\n")
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

#' Run hypeR enrichment on SigRepo signatures
#'
#' @description Builds hypeR query vectors from SigRepo signatures (see
#' \code{prepareHypeRSignatures()}) and runs \code{hypeR::hypeR()} on them. The
#' arguments after \code{score_col} are hypeR's own, with hypeR's defaults, and
#' the result is hypeR's own object, so \code{hypeR::hyp_dots()},
#' \code{hyp_show()}, \code{hyp_emap()}, \code{hyp_to_excel()} and
#' \code{hyp_to_rmd()} work on it directly.
#'
#' \code{hyp_to_excel()} uses each query name as an Excel sheet name, which
#' must be at most 31 characters and cannot contain \code{: \\ / ? * [ ]}.
#' Query names built from long signature names break that, so make the names
#' sheet-safe first. This keeps the group label (the part after \code{" | "}),
#' so arms stay distinguishable, and numbers the sheets so they stay unique:
#' \preformatted{
#' n <- names(hyp$data)
#' names(hyp$data) <- sprintf("\%02d_\%s", seq_along(n), substr(gsub("[][\\\\\\\\/?*:]", "_", sub(".* \\\\| ", "", n)), 1, 27))
#' hypeR::hyp_to_excel(hyp, file_path = "results.xlsx")
#' }
#'
#' @inheritParams prepareHypeRSignatures
#' @inheritParams getHypeRGenesets
#' @param background hypeR's background: a single number (population size), a
#' character vector of background genes, or \code{"difexp"} to use each
#' signature's measured genes from its difexp table. Signatures without difexp
#' symbols fall back to \code{23467} with a warning. With \code{"difexp"}, a
#' hypergeometric query is first reduced to its measured genes (hypeR reduces
#' only the genesets), with a warning naming each query that lost genes.
#' Anything else (e.g. \code{"Difexp"}) is an error. Defaults to \code{23467}.
#' @param power Exponent for the kstest score weights (kstest only). It changes
#' only the enrichment \code{score}: \code{1} (default) weights hits by
#' |score|, \code{0} is unweighted. The \code{pval} and \code{fdr} come from
#' hypeR's unweighted, one-sided KS test and do not depend on \code{power} or
#' \code{absolute}. That test finds genesets enriched toward the top of the
#' ranking (the highest scores); genesets concentrated among the most negative
#' scores cannot reach significance. To test that end, rank by a negated score
#' column: add \code{difexp$neg_score <- -difexp$score} to your OmicSignature
#' copy and pass \code{score_col = "neg_score"}. hypeR's kstest also cannot
#' score a geneset whose hits in a query all have score 0 (\code{power != 0})
#' or that contains every query gene (any power); such genesets are dropped for
#' that query with a warning, which also removes them from the FDR adjustment.
#' @param absolute Passed to \code{hypeR::hypeR()} (kstest only). Defaults to \code{FALSE}.
#' @param pval Keep results with p-value at or below this. Defaults to \code{1}.
#' @param fdr Keep results with FDR at or below this. Defaults to \code{1}.
#' @param plotting Logical; generate hypeR's per-geneset plots. Defaults to \code{FALSE}.
#' @param quiet Logical; suppress hypeR's logs. Defaults to \code{TRUE}.
#'
#' @return A \code{hypeR} \code{hyp} object when one query vector is produced,
#' otherwise a \code{multihyp} named by query. Each \code{hyp$info} ends with
#' \code{SigRepo Signature ID}, \code{SigRepo Signature Name},
#' \code{Group Label} and \code{Symbol Source}. Signatures that cannot produce a
#' query are skipped with a warning; if none can, an error is raised. A query
#' left with no genesets or no genes (after the drops described under
#' \code{power} and \code{background}) is skipped with a warning; if no query
#' is left, an error is raised.
#' Query names are unique but can be long; rename them as shown in the
#' description before \code{hypeR::hyp_to_excel()}.
#'
#' @examples
#' \dontrun{
#' utils::data("LLFS_Aging_Gene_2023", package = "SigRepo")
#'
#' hyp <- SigRepo::runHypeR(
#'   omic_signature = LLFS_Aging_Gene_2023,
#'   genesets = "msigdb",
#'   msigdb_collection = "H",
#'   fdr = 0.05
#' )
#' hypeR::hyp_dots(hyp)
#'
#' ranked <- SigRepo::runHypeR(
#'   omic_signature = LLFS_Aging_Gene_2023,
#'   genesets = "msigdb",
#'   msigdb_collection = "H",
#'   test = "kstest",
#'   power = 0
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
    msigdb_species = NULL,
    msigdb_collection = NULL,
    msigdb_subcollection = NULL,
    msigdb_clean = FALSE,
    test = c("hypergeometric", "kstest"),
    split = TRUE,
    score_col = "score",
    background = 23467,
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

  test <- base::match.arg(test)
  checkHypeRBackground(background)
  checkHypeRSplit(split)

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

  inputs <- collectHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    verbose = verbose
  )
  prepared <- buildHypeRQueries(inputs, test = test, split = split, score_col = score_col, conn_handler = conn_handler)

  warnSkippedHypeRSignatures(prepared$skipped)
  if (base::length(prepared$signatures) == 0) {
    base::stop("\nNo signature produced a hypeR query vector; see the warning for each signature's reason.\n")
  }

  backgrounds <- resolveQueryBackgrounds(background, prepared$info, inputs, conn_handler)
  queries <- prepared$signatures
  query_names <- base::names(queries)

  # hypeR reduces genesets to a gene-vector background but not the query, so a
  # hypergeometric query gene outside its difexp background would distort the
  # p-values. Keep each query inside its own measured genes.
  if (base::identical(background, "difexp") && base::identical(test, "hypergeometric")) {
    removed <- base::integer(base::length(queries))
    for (i in base::seq_along(queries)) {
      if (base::is.character(backgrounds[[i]])) {
        measured <- queries[[i]] %in% backgrounds[[i]]
        removed[i] <- base::sum(!measured)
        queries[[i]] <- queries[[i]][measured]
      }
    }
    if (base::any(removed > 0)) {
      base::warning(
        base::sprintf(
          "background = \"difexp\": removed %s query gene(s) not measured in difexp from: %s",
          base::sum(removed),
          base::paste(base::sprintf("'%s' (%d)", query_names[removed > 0], removed[removed > 0]), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # hypeR's kstest cannot score some genesets (all hits score 0 under
  # power != 0, or a geneset covering every query gene); it errors or
  # misaligns scores, so drop those per query before calling hypeR.
  query_genesets <- base::lapply(base::seq_along(queries), function(i) {
    if (base::identical(test, "kstest")) {
      dropZeroWeightGenesets(resolved_genesets, queries[[i]], background = backgrounds[[i]], power = power)
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
        base::paste(base::sprintf("'%s'", query_names[!runnable]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!base::any(runnable)) {
    base::stop("\nNo query had genesets left to test; see the warnings for the dropped genesets and skipped queries.\n")
  }
  run_idx <- base::which(runnable)

  results <- base::lapply(run_idx, function(i) {
    if (!quiet && base::length(run_idx) > 1) {
      base::cat(base::sprintf("\n%s\n", query_names[i]))
    }
    hyp_obj <- hypeR::hypeR(
      signature = queries[[i]],
      genesets = query_genesets[[i]]$genesets,
      test = test,
      background = backgrounds[[i]],
      power = power,
      absolute = absolute,
      pval = pval,
      fdr = fdr,
      plotting = plotting,
      quiet = quiet
    )
    appendHypeRProvenance(hyp_obj, prepared$info[i, , drop = FALSE])
  })
  base::names(results) <- query_names[run_idx]

  if (base::length(results) == 1L) {
    return(results[[1]])
  }

  hypeR::multihyp$new(data = results)
}
