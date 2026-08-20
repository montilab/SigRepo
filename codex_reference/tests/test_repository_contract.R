#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  normalizePath(getwd())
}

ROOT <- normalizePath(
  file.path(SCRIPT_DIR, ".."),
  winslash = "/",
  mustWork = TRUE
)

RESULTS_DIR <- file.path(SCRIPT_DIR, "results_v4")
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

checks <- logical()
details <- character()

record_check <- function(name, condition, detail) {
  checks[[name]] <<- isTRUE(condition)

  if (!isTRUE(condition)) {
    details[[length(details) + 1L]] <<- paste0(
      name,
      ": ",
      detail
    )
  }

  invisible(condition)
}

read_text <- function(path) {
  paste(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

approved_root <- file.path(ROOT, "approved_examples")

expected_examples <- c(
  "LLFS_Sebastiani2024",
  "EMT_Youssef2024",
  "Ding2025",
  "AhR_CYP1B1_MDA_SUM149"
)

record_check(
  "approved_root_exists",
  dir.exists(approved_root),
  approved_root
)

actual_examples <- list.dirs(
  approved_root,
  full.names = FALSE,
  recursive = FALSE
)

actual_examples <- setdiff(
  actual_examples,
  ""
)

record_check(
  "expected_approved_examples",
  setequal(
    actual_examples,
    expected_examples
  ),
  paste(
    "Expected",
    paste(expected_examples, collapse = ", "),
    "but found",
    paste(actual_examples, collapse = ", ")
  )
)

expected_reference_files <- list(
  LLFS_Sebastiani2024 = c(
    "LLFS_Sebastiani2024_refmet_rescue_2023_07.rds"
  ),
  EMT_Youssef2024 = c(
    "Youssef2024_HGNC_to_Ensembl_v114.rds",
    "Youssef2024_mapping_review_decisions.csv"
  ),
  Ding2025 = c(
    "Ding2025_HGNC_to_UniProt_2025-04-01.rds",
    "Ding2025_final_mapping_summary.csv",
    "Ding2025_mapping_review_decisions.csv",
    "Ding2025_reviewed_mapping_audit.csv"
  ),
  AhR_CYP1B1_MDA_SUM149 = c(
    "AhR_CYP1B1_Ensembl_v114_reviewed_mapping.rds",
    "AhR_CYP1B1_Ensembl_v114_mapping_review_decisions.csv",
    "AhR_CYP1B1_Ensembl_v114_mapping_summary.csv",
    "AhR_CYP1B1_Ensembl_v114_reviewed_mapping_audit.csv"
  )
)

for (example in expected_examples) {
  example_dir <- file.path(
    approved_root,
    example
  )

  rmd_files <- list.files(
    example_dir,
    pattern = "\\.[Rr]md$",
    full.names = TRUE,
    recursive = FALSE
  )

  html_files <- list.files(
    example_dir,
    pattern = "\\.html$",
    full.names = TRUE,
    recursive = FALSE
  )

  record_check(
    paste0(example, "_one_rmd"),
    length(rmd_files) == 1L,
    paste(
      "Found",
      length(rmd_files),
      "Rmd files."
    )
  )

  record_check(
    paste0(example, "_one_html"),
    length(html_files) == 1L,
    paste(
      "Found",
      length(html_files),
      "HTML files."
    )
  )

  reference_dir <- file.path(
    example_dir,
    "reference_data"
  )

  record_check(
    paste0(example, "_reference_dir"),
    dir.exists(reference_dir),
    reference_dir
  )

  required_files <- file.path(
    reference_dir,
    expected_reference_files[[example]]
  )

  record_check(
    paste0(example, "_reference_files"),
    all(file.exists(required_files)),
    paste(
      "Missing:",
      paste(
        basename(
          required_files[
            !file.exists(required_files)
          ]
        ),
        collapse = ", "
      )
    )
  )

  all_files <- list.files(
    example_dir,
    recursive = TRUE,
    full.names = FALSE
  )

  record_check(
    paste0(example, "_no_unreviewed_files"),
    !any(
      grepl(
        "UNREVIEWED",
        all_files,
        fixed = TRUE
      )
    ),
    "Approved example contains an UNREVIEWED filename."
  )

  if (length(rmd_files) == 1L) {
    rmd_text <- read_text(rmd_files[[1L]])

    record_check(
      paste0(example, "_uses_helper"),
      grepl(
        "omicsignature_compat.R",
        rmd_text,
        fixed = TRUE
      ),
      "Shared compatibility helper was not referenced."
    )

    record_check(
      paste0(example, "_approval_text"),
      grepl(
        "No unresolved items remain for this approved reference workflow",
        rmd_text,
        fixed = TRUE
      ),
      "Approval-status sentence is missing."
    )

    record_check(
      paste0(example, "_no_development_status"),
      !grepl(
        "# Development status",
        rmd_text,
        fixed = TRUE
      ),
      "Obsolete development-status section remains."
    )

    record_check(
      paste0(example, "_save_disabled"),
      grepl(
        "\\{r[^\\n]*save[^\\n]*eval\\s*=\\s*FALSE",
        rmd_text,
        perl = TRUE
      ),
      "A save chunk with eval=FALSE was not found."
    )
  }
}

development_root <- file.path(
  ROOT,
  "development_examples"
)

development_files <- list.files(
  development_root,
  recursive = TRUE,
  full.names = FALSE
)

record_check(
  "development_examples_empty",
  setequal(
    development_files,
    "README.md"
  ),
  paste(
    "Unexpected files:",
    paste(
      setdiff(
        development_files,
        "README.md"
      ),
      collapse = ", "
    )
  )
)

required_docs <- c(
  "README_for_codex.md",
  "README_SKILL_FOUNDATION.md",
  "NEXT_STEPS.md",
  file.path(
    "references",
    "README_SigRepo_core_conventions.md"
  ),
  file.path(
    "references",
    "PROBE_ID_POLICY.md"
  ),
  file.path(
    "references",
    "DIFFEXP_POLICY.md"
  ),
  file.path(
    "references",
    "APPROVED_EXAMPLES_MANIFEST.md"
  ),
  file.path(
    "regression",
    "scenario1_manifest.md"
  )
)

record_check(
  "required_documents",
  all(
    file.exists(
      file.path(
        ROOT,
        required_docs
      )
    )
  ),
  "One or more reconciled documents are missing."
)

core_text <- read_text(
  file.path(
    ROOT,
    "references",
    "README_SigRepo_core_conventions.md"
  )
)

record_check(
  "feature_n_policy_documented",
  grepl(
    "feature_1",
    core_text,
    fixed = TRUE
  ) &&
    grepl(
      "sigrepo_standardize_signature",
      core_text,
      fixed = TRUE
    ),
  "Core conventions do not document the approved feature_n helper path."
)

main_docs <- paste(
  vapply(
    c(
      "README_for_codex.md",
      "README_SKILL_FOUNDATION.md",
      "NEXT_STEPS.md"
    ),
    function(x) {
      read_text(
        file.path(
          ROOT,
          x
        )
      )
    },
    character(1)
  ),
  collapse = "\n"
)

record_check(
  "no_stale_foundation_status",
  !grepl(
    "This folder should remain empty for now",
    main_docs,
    fixed = TRUE
  ) &&
    !grepl(
      "EMT remains blocked",
      main_docs,
      fixed = TRUE
    ) &&
    !grepl(
      "Ding.*development example",
      main_docs,
      perl = TRUE
    ),
  "Stale pre-approval status remains in active foundation documents."
)

scenario_default <- file.path(
  dirname(ROOT),
  "codex_tests",
  "scenario1_diff_table"
)

scenario_dir <- Sys.getenv(
  "SIGREPO_SCENARIO1_DIR",
  unset = scenario_default
)

scenario_dir <- normalizePath(
  scenario_dir,
  winslash = "/",
  mustWork = FALSE
)

record_check(
  "scenario1_external_directory",
  dir.exists(scenario_dir),
  paste(
    "Expected visible regression corpus at",
    scenario_dir
  )
)

expected_scenarios <- c(
  "1_selman2009_s6k1",
  "2_chakraborty2026_sp2509",
  "3_Liang2016_EMT_MultiSystem",
  "6_Kanfi2012_SIRT6_KO_vs_WT",
  "7_MontiLab2023_HNSCC_Hs_OralMucosa_PML",
  "8_AhR_CYP1B1_MDA_Sum149",
  "9_GHRH_KO_Sun2013",
  "Hofmann_MycWT",
  "Sebastiani2021_Centenarian_Proteomics"
)

if (dir.exists(scenario_dir)) {
  actual_scenarios <- list.dirs(
    scenario_dir,
    full.names = FALSE,
    recursive = FALSE
  )

  actual_scenarios <- setdiff(
    actual_scenarios,
    ""
  )

  record_check(
    "scenario1_expected_folders",
    all(expected_scenarios %in% actual_scenarios),
    paste(
      "Missing:",
      paste(
        setdiff(
          expected_scenarios,
          actual_scenarios
        ),
        collapse = ", "
      )
    )
  )

  scenario_rmds <- list.files(
    scenario_dir,
    pattern = "\\.[Rr]md$",
    recursive = TRUE,
    full.names = TRUE
  )

  record_check(
    "scenario1_has_drafts",
    length(scenario_rmds) >= 8L,
    paste(
      "Found only",
      length(scenario_rmds),
      "Rmd drafts."
    )
  )
}

passed <- all(checks)

txt <- file.path(
  RESULTS_DIR,
  "repository_contract_results.txt"
)

rds <- file.path(
  RESULTS_DIR,
  "repository_contract_results.rds"
)

saveRDS(
  list(
    generated_at_utc = format(
      Sys.time(),
      tz = "UTC",
      usetz = TRUE
    ),
    root = ROOT,
    scenario_dir = scenario_dir,
    checks = checks,
    passed = passed,
    failures = details
  ),
  rds
)

sink(txt)
cat("# SigRepo repository contract results\n\n")
cat(
  "Generated:",
  format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  ),
  "\n"
)
cat("Root:", ROOT, "\n")
cat("Scenario directory:", scenario_dir, "\n\n")
cat("## Checks\n\n")
print(checks)
cat("\nPassed:", passed, "\n")
cat("\n## Failures\n\n")

if (length(details) == 0L) {
  cat("None\n")
} else {
  cat(
    paste0(
      "- ",
      details,
      collapse = "\n"
    ),
    "\n"
  )
}

sink()

cat(
  "Repository contract passed:",
  passed,
  "\n"
)
cat(
  "Result:",
  normalizePath(
    txt,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

if (!passed) {
  quit(status = 2L)
}
