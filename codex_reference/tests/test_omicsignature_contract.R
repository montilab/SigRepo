#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  normalizePath(getwd())
}

source(normalizePath(
  file.path(SCRIPT_DIR, "..", "helpers", "omicsignature_compat.R"),
  winslash = "/",
  mustWork = TRUE
))

RESULTS_DIR <- file.path(SCRIPT_DIR, "results_v4")
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

capture_check <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        structure(
          list(message = conditionMessage(e)),
          class = "contract_error"
        )
      }
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    passed = !inherits(value, "contract_error"),
    value = value,
    warnings = unique(warnings)
  )
}

nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

results <- list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version = R.version.string
)

results$provenance <- capture_check(
  sigrepo_package_provenance()
)

results$core_api <- capture_check(
  sigrepo_assert_compatible_api()
)

results$sample_type <- capture_check(
  sigrepo_assert_sample_type("blood plasma")
)

results$platform <- capture_check(
  sigrepo_assert_platform("transcriptomics by bulk RNA-seq")
)

metadata <- OmicSignature::createMetadata(
  signature_name = "SigRepo_API_Contract_v4",
  organism = "Homo sapiens",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "API contract test",
  platform = "transcriptomics by bulk RNA-seq",
  sample_type = "blood plasma",
  author = "SigRepo contract test",
  keywords = sigrepo_keywords(
    c("contract", "probe ID", "difexp")
  )
)

explicit_unique <- data.frame(
  probe_id = c("SRC_001", "SRC_002"),
  feature_name = c("ENSG000001", "ENSG000002"),
  score = c(1.25, -1.50),
  group_label = factor(
    c("Higher_A", "Higher_B"),
    levels = c("Higher_A", "Higher_B")
  ),
  stringsAsFactors = FALSE
)

explicit_repeated <- data.frame(
  probe_id = c("SRC_A", "SRC_B", "SRC_C"),
  feature_name = c(
    "ENSG000001",
    "ENSG000001",
    "ENSG000002"
  ),
  score = c(1, 2, -1),
  group_label = factor(
    c("Higher_A", "Higher_A", "Higher_B"),
    levels = c("Higher_A", "Higher_B")
  ),
  stringsAsFactors = FALSE
)

missing_probe <- explicit_unique[
  ,
  setdiff(names(explicit_unique), "probe_id"),
  drop = FALSE
]

results$explicit_probe_preservation <- capture_check({
  out <- sigrepo_standardize_signature(
    explicit_unique,
    "explicit genuine probe-ID test"
  )

  stopifnot(
    nrow(out) == nrow(explicit_unique),
    identical(
      sort(as.character(out$probe_id)),
      sort(explicit_unique$probe_id)
    )
  )

  TRUE
})

results$repeated_feature_preservation <- capture_check({
  out <- sigrepo_standardize_signature(
    explicit_repeated,
    "repeated feature-name test"
  )

  stopifnot(
    nrow(out) == 3L,
    sum(out$feature_name == "ENSG000001") == 2L,
    identical(
      sort(as.character(out$probe_id)),
      sort(explicit_repeated$probe_id)
    )
  )

  TRUE
})

results$deterministic_missing_probe <- capture_check({
  out <- sigrepo_standardize_signature(
    missing_probe,
    "missing genuine probe-ID test"
  )

  expected_pairs <- paste(
    c("feature_1", "feature_2"),
    missing_probe$feature_name,
    sep = "\r"
  )

  observed_pairs <- paste(
    as.character(out$probe_id),
    as.character(out$feature_name),
    sep = "\r"
  )

  stopifnot(
    identical(
      sort(as.character(out$probe_id)),
      sort(c("feature_1", "feature_2"))
    ),
    identical(
      sort(observed_pairs),
      sort(expected_pairs)
    ),
    !anyDuplicated(out$probe_id)
  )

  TRUE
})

results$parent_signature_synchronization <- capture_check({
  parent_raw <- data.frame(
    feature_name = c(
      "ENSG000001",
      "ENSG000001",
      "ENSG000002",
      "ENSG000003"
    ),
    score = c(1.5, 0.2, -2.0, 0.8),
    p_value = c(0.001, 0.20, 0.002, 0.03),
    adj_p_value = c(0.01, 0.40, 0.02, 0.07),
    group_label = factor(
      c(
        "Higher_A",
        "Higher_A",
        "Higher_B",
        "Higher_A"
      ),
      levels = c("Higher_A", "Higher_B")
    ),
    stringsAsFactors = FALSE
  )

  parent <- sigrepo_standardize_signature(
    parent_raw,
    "parent difexp synchronization test"
  )

  signature <- parent[
    parent$adj_p_value <= 0.05 &
      abs(parent$score) >= 1,
    ,
    drop = FALSE
  ]

  expected_parent_pairs <- paste(
    paste0("feature_", 1:4),
    parent_raw$feature_name,
    sep = "\r"
  )

  observed_parent_pairs <- paste(
    as.character(parent$probe_id),
    as.character(parent$feature_name),
    sep = "\r"
  )

  stopifnot(
    nrow(parent) == 4L,
    identical(
      sort(as.character(parent$probe_id)),
      sort(paste0("feature_", 1:4))
    ),
    identical(
      sort(observed_parent_pairs),
      sort(expected_parent_pairs)
    ),
    nrow(signature) == 2L,
    setequal(
      as.character(signature$probe_id),
      c("feature_1", "feature_3")
    ),
    all(signature$probe_id %in% parent$probe_id)
  )

  parent_pairs <- paste(
    parent$probe_id,
    parent$feature_name,
    parent$score,
    sep = "\r"
  )

  signature_pairs <- paste(
    signature$probe_id,
    signature$feature_name,
    signature$score,
    sep = "\r"
  )

  stopifnot(
    all(signature_pairs %in% parent_pairs)
  )

  TRUE
})

results$constructor_helper_path <- capture_check({
  sig <- sigrepo_standardize_signature(
    missing_probe,
    "constructor helper-path test"
  )

  obj <- OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = sig,
    difexp = NULL,
    print_message = FALSE
  )

  stopifnot(
    !is.null(obj$signature),
    nrow(obj$signature) == 2L,
    all(nonempty(obj$signature$probe_id)),
    !anyDuplicated(obj$signature$probe_id),
    all(
      as.character(obj$signature$group_label) %in%
        c("Higher_A", "Higher_B")
    )
  )

  TRUE
})

results$constructor_with_difexp <- capture_check({
  parent_raw <- data.frame(
    feature_name = c(
      "ENSG000011",
      "ENSG000012",
      "ENSG000013"
    ),
    score = c(1.8, -1.4, 0.2),
    p_value = c(0.001, 0.002, 0.40),
    adj_p_value = c(0.01, 0.02, 0.60),
    group_label = factor(
      c("Higher_A", "Higher_B", "Higher_A"),
      levels = c("Higher_A", "Higher_B")
    ),
    stringsAsFactors = FALSE
  )

  parent <- sigrepo_standardize_signature(
    parent_raw,
    "constructor difexp parent"
  )

  signature <- parent[
    parent$adj_p_value <= 0.05 &
      abs(parent$score) >= 1,
    ,
    drop = FALSE
  ]

  obj <- OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = signature,
    difexp = parent,
    print_message = FALSE
  )

  stopifnot(
    !is.null(obj$signature),
    !is.null(obj$difexp),
    nrow(obj$signature) == 2L,
    nrow(obj$difexp) == 3L,
    all(obj$signature$probe_id %in% obj$difexp$probe_id)
  )

  TRUE
})

results$collection_accessor <- capture_check({
  sig <- sigrepo_standardize_signature(
    missing_probe,
    "collection member signature"
  )

  obj1 <- OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = sig,
    difexp = NULL,
    print_message = FALSE
  )

  metadata2 <- metadata
  metadata2$signature_name <- "SigRepo_API_Contract_v4_2"

  obj2 <- OmicSignature::OmicSignature$new(
    metadata = metadata2,
    signature = sig,
    difexp = NULL,
    print_message = FALSE
  )

  collection <- OmicSignature::OmicSignatureCollection$new(
    OmicSigList = list(
      first = obj1,
      second = obj2
    ),
    metadata = list(
      collection_name = "SigRepo_API_Contract_v4_Collection",
      description = "In-memory behavioral contract"
    ),
    print_message = FALSE
  )

  members <- sigrepo_collection_members(collection)

  stopifnot(
    length(members) == 2L,
    all(
      vapply(
        members,
        function(x) {
          !is.null(x$signature) &&
            nrow(x$signature) == 2L
        },
        logical(1)
      )
    )
  )

  TRUE
})

checks <- c(
  provenance = results$provenance$passed,
  core_api = results$core_api$passed,
  sample_type = results$sample_type$passed,
  platform = results$platform$passed,
  explicit_probe_preservation =
    results$explicit_probe_preservation$passed,
  repeated_feature_preservation =
    results$repeated_feature_preservation$passed,
  deterministic_missing_probe =
    results$deterministic_missing_probe$passed,
  parent_signature_synchronization =
    results$parent_signature_synchronization$passed,
  constructor_helper_path =
    results$constructor_helper_path$passed,
  constructor_with_difexp =
    results$constructor_with_difexp$passed,
  collection_accessor =
    results$collection_accessor$passed
)

results$checks <- checks
results$passed <- all(checks)

txt <- file.path(
  RESULTS_DIR,
  "omicsignature_api_contract_v4_results.txt"
)

rds <- file.path(
  RESULTS_DIR,
  "omicsignature_api_contract_v4_results.rds"
)

saveRDS(results, rds)

sink(txt)
cat("# OmicSignature API contract v4 results\n\n")
cat("Generated:", results$generated_at_utc, "\n")
cat("R:", results$r_version, "\n\n")
cat("## Checks\n\n")
print(results$checks)
cat("\nPassed:", results$passed, "\n")
cat("\n## Failures\n\n")

for (nm in names(results)) {
  x <- results[[nm]]

  if (
    is.list(x) &&
    !is.null(x$passed) &&
    !isTRUE(x$passed)
  ) {
    cat(
      nm,
      ": ",
      x$value$message,
      "\n",
      sep = ""
    )
  }
}

cat("\n## Warnings\n\n")

for (nm in names(results)) {
  x <- results[[nm]]

  if (
    is.list(x) &&
    !is.null(x$warnings) &&
    length(x$warnings) > 0L
  ) {
    cat(
      nm,
      ": ",
      paste(x$warnings, collapse = " | "),
      "\n",
      sep = ""
    )
  }
}

sink()

cat("API contract passed:", results$passed, "\n")
cat(
  "Result:",
  normalizePath(
    txt,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

if (!results$passed) {
  quit(status = 2L)
}
