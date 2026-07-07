#' Resolve whether hypeR.GEM is available
#'
#' @keywords internal
hypergemAvailable <- function() {
  "hypeR.GEM" %in% loadedNamespaces() ||
    requireNamespace("hypeR.GEM", quietly = TRUE)
}

#' Retrieve an exported function from hypeR.GEM
#'
#' @keywords internal
hypergemExport <- function(name) {
  getExportedValue("hypeR.GEM", name)
}

resolveHyperGEMReferenceKey <- function(
    omic_signatures,
    reference_key = NULL
) {
  if (!base::is.null(reference_key) &&
      base::length(reference_key) > 0 &&
      !base::all(reference_key %in% c("", NA))) {
    return(base::as.character(reference_key[[1]]))
  }

  first_signature <- omic_signatures[[1]]
  metabolomics_config <- tryCatch(
    resolveMetabolomicsFeatureConfig(metadata = first_signature$metadata),
    error = function(e) NULL
  )

  if (base::is.null(metabolomics_config)) {
    return("refmet_name")
  }

  switch(
    metabolomics_config$feature_database,
    "refmet_id" = "refmet_name",
    "refmet" = "refmet_name",
    "hmdb" = "hmdb_id",
    "smiles" = "smiles",
    "inchikey" = "inchikey",
    "refmet_name"
  )
}

prepareHyperGEMSignatures <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    reference_key = NULL,
    score_col = "score",
    split_by_group = TRUE,
    split_by_direction = TRUE,
    verbose = TRUE
) {
  omic_signatures <- resolveHypeRSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    verbose = verbose
  )
  signature_labels <- resolveHypeRSignatureLabels(omic_signatures)
  reference_key <- resolveHyperGEMReferenceKey(
    omic_signatures = omic_signatures,
    reference_key = reference_key
  )

  signatures <- base::list()
  metadata <- base::list()
  metadata_idx <- 0

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

  for (sig_idx in base::seq_along(omic_signatures)) {
    sig_obj <- omic_signatures[[sig_idx]]
    sig_name <- signature_labels[[sig_idx]]

    if (!identical(sig_obj$metadata$assay_type[[1]], "metabolomics")) {
      base::stop(
        base::sprintf(
          "\nHyperGEM requires metabolomics signatures. Signature '%s' has assay_type '%s'.\n",
          sig_name,
          sig_obj$metadata$assay_type[[1]]
        )
      )
    }

    signature_tbl <- sig_obj$signature
    if (!methods::is(signature_tbl, "data.frame") || base::nrow(signature_tbl) == 0) {
      base::stop(
        base::sprintf("\nSignature '%s' does not contain a non-empty 'signature' table.\n", sig_name)
      )
    }

    if (!reference_key %in% base::colnames(signature_tbl)) {
      base::stop(
        base::sprintf(
          "\nSignature '%s' is missing metabolomics reference column '%s'.\n",
          sig_name,
          reference_key
        )
      )
    }

    signature_tbl <- signature_tbl[
      !base::is.na(signature_tbl[[reference_key]]) &
        base::trimws(base::as.character(signature_tbl[[reference_key]])) != "",
      ,
      drop = FALSE
    ]

    if (base::nrow(signature_tbl) == 0) {
      next
    }

    if (split_by_group && "group_label" %in% base::colnames(signature_tbl)) {
      signature_tbl$group_label <- base::as.character(signature_tbl$group_label)
      signature_tbl$group_label[base::is.na(signature_tbl$group_label) | !base::nzchar(signature_tbl$group_label)] <- "all_features"
    } else {
      signature_tbl$group_label <- "all_features"
    }

    has_score <- score_col %in% base::colnames(signature_tbl)
    if (has_score) {
      signature_tbl$score_value <- base::suppressWarnings(base::as.numeric(signature_tbl[[score_col]]))
      has_score <- base::any(!base::is.na(signature_tbl$score_value))
    }

    for (group_label in base::unique(signature_tbl$group_label)) {
      group_tbl <- signature_tbl[signature_tbl$group_label == group_label, , drop = FALSE]

      if (split_by_direction && has_score) {
        direction_specs <- base::list(
          base::list(suffix = "up", keep = !base::is.na(group_tbl$score_value) & group_tbl$score_value >= 0),
          base::list(suffix = "dn", keep = !base::is.na(group_tbl$score_value) & group_tbl$score_value < 0)
        )

        for (direction in direction_specs) {
          split_tbl <- group_tbl[direction$keep, , drop = FALSE]
          if (base::nrow(split_tbl) == 0) {
            next
          }

          component <- build_group_component(group_label, direction$suffix)
          query_name <- if (identical(component, "all_features")) sig_name else base::sprintf("%s | %s", sig_name, component)
          signatures[[query_name]] <- split_tbl

          metadata_idx <- metadata_idx + 1
          metadata[[metadata_idx]] <- base::data.frame(
            query_name = query_name,
            signature_name = sig_name,
            reference_key = reference_key,
            group_label = component,
            n_features = base::nrow(split_tbl),
            stringsAsFactors = FALSE
          )
        }
      } else {
        component <- build_group_component(group_label)
        query_name <- if (identical(component, "all_features")) sig_name else base::sprintf("%s | %s", sig_name, component)
        signatures[[query_name]] <- group_tbl

        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- base::data.frame(
          query_name = query_name,
          signature_name = sig_name,
          reference_key = reference_key,
          group_label = component,
          n_features = base::nrow(group_tbl),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (base::length(signatures) == 0) {
    base::stop(
      base::sprintf(
        "\nNo valid metabolomics signatures were available for HyperGEM using reference column '%s'.\n",
        reference_key
      )
    )
  }

  metadata_df <- if (metadata_idx > 0) {
    dplyr::bind_rows(metadata)
  } else {
    base::data.frame(
      query_name = character(),
      signature_name = character(),
      reference_key = character(),
      group_label = character(),
      n_features = numeric(),
      stringsAsFactors = FALSE
    )
  }

  base::list(
    signatures = signatures,
    metadata = metadata_df,
    omic_signatures = omic_signatures,
    reference_key = reference_key
  )
}

#' Run hypeR.GEM enrichment directly from SigRepo metabolomics signatures
#'
#' @description Resolves one or more metabolomics signatures from SigRepo and
#' runs \code{hypeR.GEM} without requiring the user to call
#' \code{getSignature()} manually first.
#'
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied.
#' @param signature_id One or more SigRepo signature IDs.
#' @param signature_name One or more SigRepo signature names.
#' @param omic_signature A single \code{OmicSignature} object or a list of
#' \code{OmicSignature} objects.
#' @param genesets A named list of genesets, a \code{hypeR} \code{gsets}
#' object, or a \code{hypeR} \code{rgsets} object.
#' @param reference_key Optional metabolite identifier column to use for GEM
#' mapping. When omitted, SigRepo defaults to \code{refmet_name} or infers a
#' reasonable metabolomics reference column from the signature metadata.
#' @param species Species passed to \code{hypeR.GEM::signature2gene()}.
#' Defaults to \code{"human"}.
#' @param directional Logical; whether to use directional mapping in
#' \code{hypeR.GEM::signature2gene()}. Defaults to \code{TRUE}.
#' @param merge Logical; whether to merge mapped metabolites in
#' \code{hypeR.GEM::signature2gene()}. Defaults to \code{TRUE}.
#' @param promiscuous_threshold Numeric threshold passed to
#' \code{hypeR.GEM::signature2gene()}. Defaults to \code{10}.
#' @param ensemble_id Logical; passed to \code{hypeR.GEM::signature2gene()}.
#' Defaults to \code{FALSE}.
#' @param background Optional background passed to \code{hypeR.GEM}.
#' @param method One of \code{"weighted"} or \code{"unweighted"}.
#' @param weighted_by Weighting field passed to \code{hypeR.GEM::enrichment()}
#' for weighted GEM analyses. Defaults to \code{"one_minus_fdr"}.
#' @param min_metabolite Minimum metabolite count passed to GEM enrichment.
#' Defaults to \code{1}.
#' @param score_col Column containing directional scores in the metabolomics
#' signature table. Defaults to \code{"score"}.
#' @param split_by_group Logical; whether to split GEM signatures by
#' \code{group_label}. Defaults to \code{TRUE}.
#' @param split_by_direction Logical; whether to split GEM signatures into up
#' and down subsets when a numeric score column is available. Defaults to
#' \code{TRUE}.
#' @param verbose Logical; whether or not to print diagnostic messages while
#' retrieving signatures. Defaults to \code{TRUE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{result}}{A GEM enrichment result object returned by \code{hypeR.GEM}.}
#'   \item{\code{signatures}}{The prepared metabolomics signature tables passed into GEM mapping.}
#'   \item{\code{metadata}}{A data frame describing each prepared signature subset.}
#'   \item{\code{gem_object}}{The intermediate object returned by \code{hypeR.GEM::signature2gene()}.}
#'   \item{\code{reference_key}}{The metabolite identifier column used for GEM mapping.}
#' }
#'
#' @examples
#' \dontrun{
#' gem_res <- SigRepo::runHyperGEM(
#'   conn_handler = conn_handler,
#'   signature_name = "example_metabolomics_signature",
#'   genesets = genesets,
#'   method = "weighted"
#' )
#' }
#'
#' @export
runHyperGEM <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    genesets,
    reference_key = NULL,
    species = "human",
    directional = TRUE,
    merge = TRUE,
    promiscuous_threshold = 10,
    ensemble_id = FALSE,
    background = NULL,
    method = c("weighted", "unweighted"),
    weighted_by = "one_minus_fdr",
    min_metabolite = 1,
    score_col = "score",
    split_by_group = TRUE,
    split_by_direction = TRUE,
    verbose = TRUE
) {
  if (!hypergemAvailable()) {
    base::stop(
      "\nPackage 'hypeR.GEM' is required for runHyperGEM(). Please install it first.\n"
    )
  }

  resolved_genesets <- resolveHypeRGenesets(genesets = genesets)
  method <- base::match.arg(method)

  enrichment_inputs <- prepareHyperGEMSignatures(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    reference_key = reference_key,
    score_col = score_col,
    split_by_group = split_by_group,
    split_by_direction = split_by_direction,
    verbose = verbose
  )

  signature2gene_fn <- hypergemExport("signature2gene")
  gem_enrichment_fn <- hypergemExport("enrichment")

  gem_object <- signature2gene_fn(
    signatures = enrichment_inputs$signatures,
    species = species,
    directional = directional,
    merge = merge,
    reference_key = enrichment_inputs$reference_key,
    promiscuous_threshold = promiscuous_threshold,
    ensemble_id = ensemble_id,
    background = NULL
  )

  gem_result <- gem_enrichment_fn(
    hypeR_GEM_obj = gem_object,
    genesets = resolved_genesets,
    genesets_name = if (
      methods::is(resolved_genesets, "gsets") ||
      methods::is(resolved_genesets, "rgsets")
    ) {
      resolved_genesets$name
    } else {
      "Selected Genesets"
    },
    method = method,
    weighted_by = weighted_by,
    min_metabolite = min_metabolite,
    background = background
  )

  base::list(
    result = gem_result,
    signatures = enrichment_inputs$signatures,
    metadata = enrichment_inputs$metadata,
    gem_object = gem_object,
    reference_key = enrichment_inputs$reference_key
  )
}
