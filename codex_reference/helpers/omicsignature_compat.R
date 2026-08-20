# OmicSignature compatibility helper for SigRepo ingestion workflows

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

sigrepo_get_export <- function(candidates, required = TRUE) {
  if (!requireNamespace("OmicSignature", quietly = TRUE)) {
    stop("OmicSignature is not installed.", call. = FALSE)
  }
  exports <- getNamespaceExports("OmicSignature")
  hit <- candidates[candidates %in% exports]
  if (length(hit) == 0L) {
    if (required) {
      stop("Required OmicSignature export not found: ",
           paste(candidates, collapse = ", "), call. = FALSE)
    }
    return(NULL)
  }
  getExportedValue("OmicSignature", hit[[1L]])
}

sigrepo_package_provenance <- function() {
  desc <- utils::packageDescription("OmicSignature")
  list(
    package = "OmicSignature",
    version = as.character(utils::packageVersion("OmicSignature")),
    library_path = normalizePath(dirname(find.package("OmicSignature")),
                                 winslash = "/", mustWork = TRUE),
    remote_type = unname(desc[["RemoteType"]] %||% NA_character_),
    remote_host = unname(desc[["RemoteHost"]] %||% NA_character_),
    remote_username = unname(desc[["RemoteUsername"]] %||% NA_character_),
    remote_repo = unname(desc[["RemoteRepo"]] %||% NA_character_),
    remote_ref = unname(desc[["RemoteRef"]] %||% NA_character_),
    remote_sha = unname(desc[["RemoteSha"]] %||% NA_character_),
    built = unname(desc[["Built"]] %||% NA_character_)
  )
}

sigrepo_extract_terms <- function(x) {
  if (is.null(x)) return(character())
  out <- if (is.character(x)) x else unlist(x, use.names = FALSE)
  out <- trimws(as.character(out))
  unique(out[!is.na(out) & nzchar(out)])
}

sigrepo_sample_type_terms <- function(query = "") {
  result <-
    OmicSignature::OmicS_searchSampleType(query)
  
  if (!is.data.frame(result) ||
      !"Name" %in% names(result)) {
    stop(
      paste0(
        "OmicS_searchSampleType() did not return ",
        "the expected Name column."
      ),
      call. = FALSE
    )
  }
  
  terms <- trimws(
    as.character(result$Name)
  )
  
  unique(
    terms[
      !is.na(terms) &
        nzchar(terms)
    ]
  )
}

sigrepo_platform_terms <- function(query = "") {
  fn <- sigrepo_get_export(c("OmicS_searchPlatform", "searchPlatform"))
  sigrepo_extract_terms(fn(query))
}

sigrepo_assert_sample_type <- function(value) {
  terms <- unique(c(sigrepo_sample_type_terms(value),
                    sigrepo_sample_type_terms("")))
  if (!value %in% terms) {
    stop("sample_type is not an exact approved term: ", value, call. = FALSE)
  }
  invisible(value)
}

sigrepo_assert_platform <- function(value) {
  terms <- unique(c(sigrepo_platform_terms(value),
                    sigrepo_platform_terms("")))
  if (!value %in% terms) {
    stop("platform is not an exact approved term: ", value, call. = FALSE)
  }
  invisible(value)
}

sigrepo_keywords <- function(x) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x) == 0L) return(NULL)
  paste(x, collapse = ", ")
}

sigrepo_validate_probe_ids <- function(x, context = "signature") {
  if (!"probe_id" %in% names(x)) {
    stop(context, " has no probe_id column.", call. = FALSE)
  }
  x$probe_id <- as.character(x$probe_id)
  if (anyNA(x$probe_id) || any(!nzchar(x$probe_id))) {
    stop(context, " has missing or empty probe_id.", call. = FALSE)
  }
  if (anyDuplicated(x$probe_id)) {
    stop(context, " has duplicated probe_id values.", call. = FALSE)
  }
  x
}

sigrepo_assign_surrogate_probe_ids <- function(
    x,
    context = "signature"
) {
  if ("probe_id" %in% names(x)) {
    return(
      sigrepo_validate_probe_ids(
        x,
        paste(
          context,
          "before standardization"
        )
      )
    )
  }
  
  if (nrow(x) == 0L) {
    stop(
      context,
      " contains no rows.",
      call. = FALSE
    )
  }
  
  ## The installed OmicSignature version fails when standardizeSigDF()
  ## receives no probe_id. Reproduce the repository/package convention
  ## explicitly for list-only signatures with no genuine probe IDs.
  x$probe_id <- paste0(
    "feature_",
    seq_len(nrow(x))
  )
  
  x <- x[
    ,
    c(
      "probe_id",
      setdiff(
        names(x),
        "probe_id"
      )
    ),
    drop = FALSE
  ]
  
  sigrepo_validate_probe_ids(
    x,
    paste(
      context,
      "after automatic feature-number probe-ID assignment"
    )
  )
}


sigrepo_standardize_signature <- function(
    x,
    context = "signature"
) {
  if (
    !is.data.frame(x) ||
    !"feature_name" %in% names(x)
  ) {
    stop(
      context,
      " must contain feature_name.",
      call. = FALSE
    )
  }
  
  x$feature_name <- trimws(
    as.character(
      x$feature_name
    )
  )
  
  if (
    anyNA(x$feature_name) ||
    any(!nzchar(x$feature_name))
  ) {
    stop(
      context,
      " has missing or empty feature_name.",
      call. = FALSE
    )
  }
  
  input_n <- nrow(x)
  
  group_levels <- if (
    "group_label" %in% names(x) &&
    is.factor(x$group_label)
  ) {
    levels(x$group_label)
  } else {
    NULL
  }
  
  ## Assign deterministic repository surrogate IDs only when
  ## the source supplies no genuine probe or assay identifiers.
  x <- sigrepo_assign_surrogate_probe_ids(
    x,
    context
  )
  
  input_probe_ids <- as.character(
    x$probe_id
  )
  
  input_features <- as.character(
    x$feature_name
  )
  
  input_probe_feature_pairs <- paste(
    input_probe_ids,
    input_features,
    sep = "\r"
  )
  
  fn <- sigrepo_get_export(
    "standardizeSigDF"
  )
  
  out <- tryCatch(
    fn(x),
    error = function(e) {
      stop(
        "Standardization failed for ",
        context,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  if (!is.data.frame(out)) {
    stop(
      "Standardization did not return a data.frame for ",
      context,
      ".",
      call. = FALSE
    )
  }
  
  if (nrow(out) != input_n) {
    stop(
      "Standardization changed the row count for ",
      context,
      ": ",
      input_n,
      " to ",
      nrow(out),
      ".",
      call. = FALSE
    )
  }
  
  if (
    !all(
      c(
        "probe_id",
        "feature_name"
      ) %in% names(out)
    )
  ) {
    stop(
      "Standardization did not retain probe_id and feature_name for ",
      context,
      ".",
      call. = FALSE
    )
  }
  
  out$probe_id <- as.character(
    out$probe_id
  )
  
  out$feature_name <- trimws(
    as.character(
      out$feature_name
    )
  )
  
  out <- sigrepo_validate_probe_ids(
    out,
    paste(
      context,
      "after standardization"
    )
  )
  
  if (
    !identical(
      sort(out$feature_name),
      sort(input_features)
    )
  ) {
    stop(
      "Feature membership changed during standardization for ",
      context,
      ".",
      call. = FALSE
    )
  }
  
  if (
    !identical(
      sort(out$probe_id),
      sort(input_probe_ids)
    )
  ) {
    stop(
      "Repository surrogate probe IDs changed during standardization for ",
      context,
      ".",
      call. = FALSE
    )
  }
  
  output_probe_feature_pairs <- paste(
    out$probe_id,
    out$feature_name,
    sep = "\r"
  )
  
  if (
    !identical(
      sort(output_probe_feature_pairs),
      sort(input_probe_feature_pairs)
    )
  ) {
    stop(
      "Probe-to-feature relationships changed during standardization for ",
      context,
      ".",
      call. = FALSE
    )
  }
  
  if (
    !is.null(group_levels) &&
    "group_label" %in% names(out)
  ) {
    out$group_label <- factor(
      as.character(
        out$group_label
      ),
      levels = group_levels
    )
  }
  
  out
}

sigrepo_collection_members <- function(x) {
  members <- tryCatch(x$OmicSigList, error = function(e) NULL)
  if (!is.null(members)) return(members)
  if (methods::isS4(x) && "OmicSigList" %in% methods::slotNames(x)) {
    return(methods::slot(x, "OmicSigList"))
  }
  stop("Could not retrieve collection members through OmicSigList.",
       call. = FALSE)
}

sigrepo_assert_compatible_api <- function() {
  if (!requireNamespace("OmicSignature", quietly = TRUE)) {
    stop("OmicSignature is not installed.", call. = FALSE)
  }
  exports <- getNamespaceExports("OmicSignature")
  required <- c("createMetadata", "standardizeSigDF",
                "OmicSignature", "OmicSignatureCollection")
  missing <- setdiff(required, exports)
  sample_search <- intersect(c("OmicS_searchSampleType", "searchSampleType"),
                             exports)
  platform_search <- intersect(c("OmicS_searchPlatform", "searchPlatform"),
                               exports)

  if (length(missing) > 0L ||
      length(sample_search) == 0L ||
      length(platform_search) == 0L) {
    stop("The installed OmicSignature API is incompatible.",
         call. = FALSE)
  }

  list(
    compatibility_status = "core API discovery passed",
    contract_name = "sigrepo-omicsignature-contract-v3",
    provenance = sigrepo_package_provenance(),
    resolved_exports = list(
      sample_type_search = sample_search[[1L]],
      platform_search = platform_search[[1L]]
    )
  )
}
