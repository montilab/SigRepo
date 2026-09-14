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

fetchMsigdbGenesets <- function(
    species = "Homo sapiens",
    collection = "H",
    subcollection = NULL,
    clean = FALSE
) {
  if (!requireNamespace("msigdbr", quietly = TRUE)) {
    base::stop(
      "\nPackage 'msigdbr' is required to retrieve MSigDB genesets. Please install it first.\n"
    )
  }

  species <- base::as.character(species[[1]])
  collection <- base::as.character(collection[[1]])
  subcollection <- if (
    base::is.null(subcollection) ||
    base::length(subcollection) == 0 ||
    base::all(subcollection %in% c("", NA))
  ) {
    NULL
  } else {
    base::as.character(subcollection[[1]])
  }

  call_args <- base::list(
    species = species,
    collection = collection
  )
  if (!base::is.null(subcollection)) {
    call_args$subcollection <- subcollection
  }

  # Some msigdbr builds support a direct "species = mouse" + "collection = ..."
  # query, while others require the ortholog-mapped human database for non-human
  # sets. Use the direct human mapping as the safest fallback instead of relying
  # on the upstream hypeR wrapper, which has been inconsistent across recent
  # releases.
  response <- tryCatch(
    do.call(msigdbr::msigdbr, call_args),
    error = function(e) {
      if (!base::identical(species, "Homo sapiens")) {
        fallback_args <- call_args
        fallback_args$species <- "Homo sapiens"
        fallback_args$db_species <- if (base::identical(species, "Mus musculus")) "MM" else "HS"
        return(do.call(msigdbr::msigdbr, fallback_args))
      }
      stop(e)
    }
  )

  if (base::nrow(response) == 0L) {
    base::stop(
      "\nNo MSigDB genesets were returned for the requested species/collection combination.\n"
    )
  }

  mdf <- response[, c("gs_name", "gene_symbol")]
  mdf <- stats::na.omit(mdf)
  mdf <- unique(mdf)

  gsets <- split(mdf$gene_symbol, mdf$gs_name)
  if (isTRUE(clean)) {
    names(gsets) <- base::gsub("^HALLMARK_|^KEGG_|^GO_|^CP:|^C[0-9]+\\.", "", names(gsets))
  }

  gsets
}

#' Resolve genesets for hypeR, including direct MSigDB fetches
#'
#' @description Resolves a named list of genesets directly or retrieves a
#' collection from MSigDB when requested.
#'
#' @param genesets A named list of genesets, a hypeR gsets object, or a hypeR
#' rgsets object.
#' @param msigdb_species Species name for MSigDB lookup.
#' @param msigdb_collection MSigDB collection id.
#' @param msigdb_subcollection Optional MSigDB subcollection id.
#' @param msigdb_clean Logical; whether to simplify geneset names.
#'
#' @return A named list of genesets or a hypeR gsets object.
#'
#' @export
resolveHypeRGenesets <- function(
    genesets = NULL,
    msigdb_species = NULL,
    msigdb_collection = NULL,
    msigdb_subcollection = NULL,
    msigdb_clean = FALSE
) {
  msigdb_requested <- !base::is.null(msigdb_collection) &&
    base::length(msigdb_collection) > 0 &&
    !base::all(msigdb_collection %in% c("", NA))

  if (!base::is.null(genesets) && msigdb_requested) {
    base::stop(
      "\nSupply either 'genesets' or 'msigdb_collection', but not both.\n"
    )
  }

  if (msigdb_requested) {
    msigdb_species <- if (
      base::is.null(msigdb_species) ||
      base::length(msigdb_species) == 0 ||
      base::all(msigdb_species %in% c("", NA))
    ) {
      "Homo sapiens"
    } else {
      base::as.character(msigdb_species[[1]])
    }

    msigdb_collection <- base::as.character(msigdb_collection[[1]])
    msigdb_subcollection <- if (
      base::is.null(msigdb_subcollection) ||
      base::length(msigdb_subcollection) == 0 ||
      base::all(msigdb_subcollection %in% c("", NA))
    ) {
      NULL
    } else {
      base::as.character(msigdb_subcollection[[1]])
    }

    return(
      fetchMsigdbGenesets(
        species = msigdb_species,
        collection = msigdb_collection,
        subcollection = msigdb_subcollection,
        clean = msigdb_clean
      )
    )
  }

  if (base::is.null(genesets)) {
    base::stop(
      "\nProvide either 'genesets' or specify an MSigDB collection with 'msigdb_collection'.\n"
    )
  }

  if (
    methods::is(genesets, "gsets") ||
    methods::is(genesets, "rgsets")
  ) {
    return(genesets)
  }

  if (!methods::is(genesets, "list") || base::length(genesets) == 0) {
    base::stop(
      "\n'genesets' must be a non-empty named list, a hypeR gsets object, or a hypeR rgsets object.\n"
    )
  }

  if (base::is.null(base::names(genesets)) || base::any(base::names(genesets) %in% c("", NA))) {
    base::stop("\n'genesets' must be named.\n")
  }

  genesets
}

resolveHypeRFeatureSymbols <- function(
    data_tbl = NULL,
    feature_col = "feature_name",
    assay_type = NULL,
    sig_name = NULL,
    conn_handler = NULL,
    verbose = TRUE
) {
  if (base::is.null(data_tbl) || !methods::is(data_tbl, "data.frame") || base::nrow(data_tbl) == 0) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  symbol_like <- c("symbol", "gene_symbol", "gene", "gene_name")
  existing_symbol <- symbol_like[symbol_like %in% base::colnames(data_tbl)]
  if (base::length(existing_symbol) > 0) {
    return(list(
      data = data_tbl,
      feature_col = existing_symbol[[1]],
      n_dropped = 0L,
      mapped = FALSE
    ))
  }

  if (!feature_col %in% base::colnames(data_tbl)) {
    fallback_cols <- c("feature_name", "feature_id", "probe_id", "probe", "feature")
    fallback_cols <- fallback_cols[fallback_cols %in% base::colnames(data_tbl)]
    if (base::length(fallback_cols) == 0L) {
      return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
    }
    feature_col <- fallback_cols[[1]]
  }

  if (feature_col %in% c("symbol", "gene_symbol", "gene", "gene_name")) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  ref_lookup <- switch(
    base::tolower(base::as.character(assay_type)),
    transcriptomics = "transcriptomics_features",
    proteomics = "proteomics_features",
    genetic_variants = "genetic_variants_features",
    NULL
  )

  if (base::is.null(ref_lookup) || base::is.null(conn_handler)) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  feature_values <- base::as.character(data_tbl[[feature_col]])
  valid_idx <- !base::is.na(feature_values) & nzchar(feature_values)
  if (!base::any(valid_idx)) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  mapped_tbl <- tryCatch(
    SigRepo::lookup_table_sql(
      conn = SigRepo::conn_init(conn_handler),
      db_table_name = ref_lookup,
      return_var = c("feature_name", "gene_symbol"),
      filter_coln_var = "feature_name",
      filter_coln_val = base::list("feature_name" = base::unique(feature_values[valid_idx])),
      check_db_table = TRUE
    ),
    error = function(e) NULL
  )

  if (base::is.null(mapped_tbl) || base::nrow(mapped_tbl) == 0) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  mapped_tbl <- mapped_tbl[
    !base::is.na(mapped_tbl$feature_name) & nzchar(mapped_tbl$feature_name) &
      !base::is.na(mapped_tbl$gene_symbol) & nzchar(mapped_tbl$gene_symbol),
    ,
    drop = FALSE
  ]

  if (base::nrow(mapped_tbl) == 0) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  symbol_map <- stats::setNames(
    base::as.character(mapped_tbl$gene_symbol),
    base::as.character(mapped_tbl$feature_name)
  )

  matched_idx <- valid_idx & feature_values %in% base::names(symbol_map)
  mapped_values <- symbol_map[feature_values[matched_idx]]

  if (base::sum(valid_idx) == 0 || base::sum(matched_idx) == 0) {
    return(list(data = data_tbl, feature_col = feature_col, n_dropped = 0L, mapped = FALSE))
  }

  dropped_count <- base::sum(valid_idx) - base::sum(matched_idx)
  if (dropped_count > 0L) {
    base::warning(
      base::sprintf(
        "Signature '%s' used reference MySQL feature mapping to resolve %s to gene symbols; %s row(s) were dropped because they could not be mapped.",
        if (is.null(sig_name) || !nzchar(sig_name)) "hypeR_input" else sig_name,
        feature_col,
        dropped_count
      ),
      call. = FALSE
    )
  }

  out_tbl <- data_tbl[matched_idx, , drop = FALSE]
  out_tbl$symbol <- unname(mapped_values)

  list(
    data = out_tbl,
    feature_col = "symbol",
    n_dropped = dropped_count,
    mapped = TRUE
  )
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
#' @param split_by_direction Logical; retained for compatibility with other
#' SigRepo wrappers, but ignored by the hypeR wrapper because HypeR queries are
#' prepared as one feature vector per group label and should not be further
#' split by positive/negative direction. Defaults to \code{FALSE}.
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
    method = c("hypergeo", "hypergeometric", "kstest", "ks", "gsea"),
    feature_col = "feature_name",
    score_col = "score",
    split_by_group = FALSE,
    split_by_direction = FALSE,
    verbose = TRUE
){

  method <- base::match.arg(method, choices = c("hypergeo", "hypergeometric", "kstest", "ks", "gsea"))
  if (identical(method, "hypergeo") || identical(method, "hypergeometric")) {
    method <- "hypergeometric"
  } else if (identical(method, "ks") || identical(method, "kstest")) {
    method <- "kstest"
  }

  if (isTRUE(split_by_direction)) {
    base::warning(
      "split_by_direction is ignored by SigRepo::prepareHypeRSignatures(); hypeR queries are split by group_label only.",
      call. = FALSE
    )
  }
  split_by_direction <- FALSE

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

    if (identical(method, "hypergeometric")) {
      signature_tbl <- sig_obj$signature

      if (!methods::is(signature_tbl, "data.frame") || base::nrow(signature_tbl) == 0) {
        base::stop(
          base::sprintf("\nSignature '%s' does not contain a non-empty 'signature' table.\n", sig_name)
        )
      }

      if (!feature_col %in% base::colnames(signature_tbl) ||
          feature_col %in% c("feature_name", "feature_id", "probe_id", "probe", "feature")) {
        resolved_features <- resolveHypeRFeatureSymbols(
          data_tbl = signature_tbl,
          feature_col = feature_col,
          assay_type = sig_obj$metadata$assay_type[1],
          sig_name = sig_name,
          conn_handler = conn_handler,
          verbose = verbose
        )
        if (resolved_features$mapped) {
          signature_tbl <- resolved_features$data
          feature_col <- resolved_features$feature_col
        } else if (!feature_col %in% base::colnames(signature_tbl)) {
          base::stop(
            base::sprintf("\nSignature '%s' is missing feature column '%s' in 'signature'.\n", sig_name, feature_col)
          )
        }
      }

      if (split_by_group && "group_label" %in% base::colnames(signature_tbl)) {
        signature_tbl$group_label <- base::as.character(signature_tbl$group_label)
      } else {
        signature_tbl$group_label <- "all_features"
      }

      for (group_label in base::unique(signature_tbl$group_label)) {
        group_tbl <- signature_tbl[signature_tbl$group_label == group_label, , drop = FALSE]

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

      if (feature_col %in% c("feature_name", "feature_id", "probe_id", "probe", "feature") ||
          !feature_col %in% base::colnames(difexp_tbl)) {
        resolved_features <- resolveHypeRFeatureSymbols(
          data_tbl = difexp_tbl,
          feature_col = feature_col,
          assay_type = sig_obj$metadata$assay_type[1],
          sig_name = sig_name,
          conn_handler = conn_handler,
          verbose = verbose
        )
        if (resolved_features$mapped) {
          difexp_tbl <- resolved_features$data
          feature_col <- resolved_features$feature_col
        } else if (!feature_col %in% base::colnames(difexp_tbl)) {
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
        }
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
#' @param genesets A named list of genesets, a \code{hypeR} \code{gsets}
#' object, or a \code{hypeR} \code{rgsets} object supplied to \code{hypeR}.
#' Alternatively, leave this as \code{NULL} and specify
#' \code{msigdb_collection} to fetch genesets from MSigDB automatically.
#' @param msigdb_species Species passed to \code{hypeR::msigdb_gsets()} when
#' \code{msigdb_collection} is supplied. Defaults to \code{"Homo sapiens"}.
#' @param msigdb_collection MSigDB collection identifier, such as \code{"H"}
#' or \code{"C2"}. When supplied, \code{runHypeR()} retrieves genesets
#' automatically via \code{hypeR::msigdb_gsets()}.
#' @param msigdb_subcollection Optional MSigDB subcollection identifier, such as
#' \code{"CP:KEGG_LEGACY"}.
#' @param msigdb_clean Logical; whether to clean MSigDB geneset labels using
#' \code{hypeR::msigdb_gsets(clean = ...)}. Defaults to \code{FALSE}.
#' @param method One of \code{"hypergeo"}, \code{"kstest"}, or \code{"gsea"}.
#' @param feature_col Column containing feature identifiers. Defaults to
#' \code{"feature_name"}.
#' @param score_col Column containing ranked scores for KS/GSEA-style enrichment.
#' Defaults to \code{"score"}.
#' @param split_by_group Logical; whether to create separate query vectors for
#' each \code{group_label}. Defaults to \code{FALSE}.
#' @param split_by_direction Logical; retained for backward compatibility, but
#' ignored by the wrapper because HypeR signatures should be split only by
#' \code{group_label}, not by positive/negative direction. Defaults to
#' \code{FALSE}.
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
#' hyp_res <- SigRepo::runHypeR(
#'   conn_handler = conn_handler,
#'   signature_name = "example_signature",
#'   msigdb_collection = "H",
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
    genesets = NULL,
    msigdb_species = "Homo sapiens",
    msigdb_collection = NULL,
    msigdb_subcollection = NULL,
    msigdb_clean = FALSE,
    method = c("hypergeo", "hypergeometric", "kstest", "ks", "gsea"),
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

  method <- base::match.arg(method, choices = c("hypergeo", "hypergeometric", "kstest", "ks", "gsea"))
  if (identical(method, "hypergeo") || identical(method, "hypergeometric")) {
    method <- "hypergeometric"
  } else if (identical(method, "ks") || identical(method, "kstest")) {
    method <- "kstest"
  }
  if (isTRUE(split_by_direction)) {
    base::warning(
      "split_by_direction is ignored by SigRepo::runHypeR(); hypeR queries are split by group_label only.",
      call. = FALSE
    )
  }
  split_by_direction <- FALSE
  hype_test <- if (identical(method, "hypergeometric")) "hypergeometric" else "ks"
  extra_args <- base::list(...)
  resolved_genesets <- resolveHypeRGenesets(
    genesets = genesets,
    msigdb_species = msigdb_species,
    msigdb_collection = msigdb_collection,
    msigdb_subcollection = msigdb_subcollection,
    msigdb_clean = msigdb_clean
  )

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
    genesets = resolved_genesets,
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
