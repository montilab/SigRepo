# searchExternal() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One exported R function, `searchExternal()`, that takes a SigRepo signature, translates its features into gene symbols plus an organism code, and queries Rummagene, RummaGEO and/or CORESH, returning tidy data frames with a shared column set.

**Architecture:** A translation layer (`externalQuery.R`) turns an `OmicSignature` into a plain list (`genes`, `organism_code`, counts). Three adapters, one file each, own one service's HTTP and parsing and return the common data frame via shared helpers in `externalCommon.R`. `searchExternal.R` resolves the signature, builds one query per direction, dispatches to adapters inside `tryCatch`, and shapes the return value. All network I/O goes through two small helpers (`externalPostJson`, `externalGetJson`) so every adapter is testable offline by mocking them.

**Tech Stack:** R >= 4.4, httr, jsonlite, biomaRt, testthat 3 (`local_mocked_bindings`), roxygen2 via `devtools::document()`.

**Spec:** `docs/superpowers/specs/2026-10-07-search-external-design.md`

## Global Constraints

- Package style: every non-base call is namespace-qualified (`base::`, `httr::`, `jsonlite::`), as in the rest of `R/`. Internal functions carry `#' @keywords internal` + `#' @noRd`.
- Only one new export: `searchExternal`. Everything else is internal.
- No new `Imports`. `httr`, `jsonlite`, `biomaRt`, `rlang`, `methods` are already declared. `stats`/`utils` are already used elsewhere via `::` without declaration, so follow suit.
- Gene cap is 500 (CORESH limit), applied to all sources.
- Organism codes: `"hsa"` for Homo sapiens, `"mmu"` for Mus musculus, `NA` otherwise.
- Common columns, in order: `source, direction, rank, id, title, url, score, score_label, pvalue, adj_pvalue, n_overlap, n_set`.
- Endpoints come from options with defaults: `sigrepo.rummagene_url`, `sigrepo.rummageo_url`, `sigrepo.coresh_url`, `sigrepo.coresh_poll_seconds`.
- Tests must pass with no network and no database. Live tests run only when `SIGREPO_LIVE_EXTERNAL=true`.
- Commits end with `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.
- Work in the worktree `~/Documents/GitHub/SigRepo/wt/searchexternal`, branch `265-add-searchexternal-rummagene-rummageo-coresh`. Run all commands from there.

## Review Focus

1. Versioned Ensembl IDs (`ENSG00000126353.12`) must map: strip the version before the biomaRt filter and map back to the original name. (Task 2 test.)
2. A bi-directional signature whose `score` has NA rows: `direction = "up"` must drop NA rows silently, not error or include them. (Task 2 test.)
3. Mouse symbols are mixed case (`Isg15`): Rummagene receives upper case, RummaGEO receives them unchanged. (Task 3 and Task 4 tests.)
4. CORESH reports failure as the string `"True"` in `failed`; the adapter must stop with the job id rather than poll until timeout. (Task 5 test.)
5. `limit` larger than the number of hits returns the hits available with no error. (Task 5 test.)

---

### Task 1: Shared plumbing (`externalCommon.R`)

**Files:**
- Create: `R/externalCommon.R`
- Test: `tests/testthat/test_externalCommon.R`

**Interfaces:**
- Produces:
  - `EXTERNAL_SOURCES` (character), `EXTERNAL_COLUMNS` (character)
  - `` `%||%`(a, b) ``
  - `externalUrl(source)` -> character URL
  - `externalPostJson(url, body, timeout = 60)` -> parsed list
  - `externalGetJson(url, query = list(), timeout = 60)` -> parsed list
  - `externalGraphql(url, query, variables = list(), timeout = 60)` -> parsed list, errors on GraphQL `errors`
  - `emptyExternalFrame(source, direction = "combined")` -> 0-row data frame with common columns
  - `bindExternalHits(source, direction, rows)` -> data frame; `rows` is a list of named lists
  - `rbindExternalFrames(frames)` -> one data frame, missing columns filled with NA

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_externalCommon.R
test_that("externalUrl returns defaults and honours options", {
  expect_equal(externalUrl("rummagene"), "https://rummagene.com/graphql")
  expect_equal(externalUrl("rummageo"), "https://rummageo.com/graphql")
  expect_equal(externalUrl("coresh"), "https://alserglab.wustl.edu/coresh-back")
  withr::with_options(list(sigrepo.coresh_url = "http://localhost:8000"), {
    expect_equal(externalUrl("coresh"), "http://localhost:8000")
  })
  expect_error(externalUrl("nope"), "Unknown external source")
})

test_that("emptyExternalFrame has the common columns and no rows", {
  f <- emptyExternalFrame("coresh")
  expect_s3_class(f, "data.frame")
  expect_equal(nrow(f), 0)
  expect_equal(colnames(f), EXTERNAL_COLUMNS)
})

test_that("bindExternalHits orders common columns first, fills gaps, ranks", {
  rows <- list(
    list(id = "a", title = "A", score = 2, pvalue = 0.01, extra = "x"),
    list(id = "b", title = "B", score = 1, pvalue = NULL, extra = "y")
  )
  f <- bindExternalHits("rummagene", "up", rows)
  expect_equal(nrow(f), 2)
  expect_equal(colnames(f)[seq_along(EXTERNAL_COLUMNS)], EXTERNAL_COLUMNS)
  expect_equal(f$source, c("rummagene", "rummagene"))
  expect_equal(f$direction, c("up", "up"))
  expect_equal(f$rank, 1:2)
  expect_equal(f$extra, c("x", "y"))
  expect_true(is.na(f$pvalue[2]))
  expect_true(all(is.na(f$url)))
})

test_that("bindExternalHits with no rows returns the empty frame", {
  f <- bindExternalHits("coresh", "combined", list())
  expect_equal(nrow(f), 0)
  expect_equal(colnames(f), EXTERNAL_COLUMNS)
})

test_that("rbindExternalFrames unions columns", {
  a <- bindExternalHits("coresh", "up", list(list(id = "a", gpl = "GPL1")))
  b <- emptyExternalFrame("coresh", "down")
  f <- rbindExternalFrames(list(a, b))
  expect_equal(nrow(f), 1)
  expect_true("gpl" %in% colnames(f))
})

test_that("externalGraphql surfaces GraphQL errors", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(errors = list(list(message = "boom")))
  )
  expect_error(externalGraphql("http://x", "{ q }", list()), "boom")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "externalCommon")'`
Expected: FAIL, `could not find function "externalUrl"` (and friends).

- [ ] **Step 3: Write the implementation**

```r
# R/externalCommon.R
############################################################
# Shared plumbing for the public signature search engines
# behind searchExternal(): Rummagene, RummaGEO and CORESH.
# Every network call goes through externalPostJson() /
# externalGetJson() so adapters can be tested offline by
# mocking those two functions.
############################################################

EXTERNAL_SOURCES <- c("rummagene", "rummageo", "coresh")

EXTERNAL_COLUMNS <- c(
  "source", "direction", "rank", "id", "title", "url",
  "score", "score_label", "pvalue", "adj_pvalue", "n_overlap", "n_set"
)

`%||%` <- function(a, b) if (base::is.null(a)) b else a

#' @title externalUrl
#' @description Base URL of an external search engine, overridable through
#' options so tests and a future dev instance can redirect it.
#' @keywords internal
#' @noRd
externalUrl <- function(source) {
  base::switch(
    source,
    rummagene = base::getOption("sigrepo.rummagene_url", "https://rummagene.com/graphql"),
    rummageo  = base::getOption("sigrepo.rummageo_url",  "https://rummageo.com/graphql"),
    coresh    = base::getOption("sigrepo.coresh_url",    "https://alserglab.wustl.edu/coresh-back"),
    base::stop(base::sprintf("Unknown external source '%s'.", source))
  )
}

#' @keywords internal
#' @noRd
externalPostJson <- function(url, body, timeout = 60) {
  res <- httr::POST(
    url, body = body, encode = "json",
    httr::content_type_json(), httr::timeout(timeout)
  )
  if (httr::status_code(res) >= 300) {
    base::stop(base::sprintf("Request to %s failed (HTTP %s).", url, httr::status_code(res)))
  }
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' @keywords internal
#' @noRd
externalGetJson <- function(url, query = base::list(), timeout = 60) {
  res <- httr::GET(url, query = query, httr::timeout(timeout))
  if (httr::status_code(res) >= 300) {
    base::stop(base::sprintf("Request to %s failed (HTTP %s).", url, httr::status_code(res)))
  }
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' @keywords internal
#' @noRd
externalGraphql <- function(url, query, variables = base::list(), timeout = 60) {
  payload <- externalPostJson(url, base::list(query = query, variables = variables), timeout = timeout)
  if (!base::is.null(payload$errors)) {
    msg <- base::tryCatch(payload$errors[[1]]$message, error = function(e) NULL) %||% "unknown error"
    base::stop(base::sprintf("GraphQL error from %s: %s", url, msg))
  }
  payload
}

#' @keywords internal
#' @noRd
emptyExternalFrame <- function(source, direction = "combined") {
  base::data.frame(
    source = base::character(), direction = base::character(), rank = base::integer(),
    id = base::character(), title = base::character(), url = base::character(),
    score = base::numeric(), score_label = base::character(),
    pvalue = base::numeric(), adj_pvalue = base::numeric(),
    n_overlap = base::integer(), n_set = base::integer(),
    stringsAsFactors = FALSE
  )
}

#' @title bindExternalHits
#' @description Turn a list of per-hit named lists into the common data frame:
#' common columns first (NA where a service has no value), service-specific
#' extras after, `rank` by position.
#' @keywords internal
#' @noRd
bindExternalHits <- function(source, direction, rows) {
  if (base::length(rows) == 0) {
    return(emptyExternalFrame(source, direction))
  }
  na_if_null <- function(x) if (base::is.null(x) || base::length(x) == 0) NA else x[[1]]
  all_cols <- base::unique(base::unlist(base::lapply(rows, base::names)))
  values <- base::lapply(all_cols, function(col) {
    base::unlist(base::lapply(rows, function(r) na_if_null(r[[col]])))
  })
  base::names(values) <- all_cols
  tbl <- base::as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
  for (col in base::setdiff(EXTERNAL_COLUMNS, c("source", "direction", "rank", base::colnames(tbl)))) {
    tbl[[col]] <- NA
  }
  tbl$source <- source
  tbl$direction <- direction
  tbl$rank <- base::seq_len(base::nrow(tbl))
  extras <- base::setdiff(base::colnames(tbl), EXTERNAL_COLUMNS)
  tbl <- tbl[, c(EXTERNAL_COLUMNS, extras), drop = FALSE]
  base::rownames(tbl) <- NULL
  tbl
}

#' @keywords internal
#' @noRd
rbindExternalFrames <- function(frames) {
  frames <- base::Filter(Negate(base::is.null), frames)
  if (base::length(frames) == 0) {
    return(emptyExternalFrame("external"))
  }
  all_cols <- base::unique(base::unlist(base::lapply(frames, base::colnames)))
  frames <- base::lapply(frames, function(f) {
    for (col in base::setdiff(all_cols, base::colnames(f))) f[[col]] <- NA
    f[, all_cols, drop = FALSE]
  })
  out <- base::do.call(base::rbind, frames)
  base::rownames(out) <- NULL
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "externalCommon")'`
Expected: all PASS. If `withr` is missing, it is already in `Suggests`; install it.

- [ ] **Step 5: Commit**

```bash
git add R/externalCommon.R tests/testthat/test_externalCommon.R
git commit -m "Add shared HTTP and data-frame helpers for external signature searches

Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 2: Translation layer (`externalQuery.R`)

**Files:**
- Create: `R/externalQuery.R`
- Test: `tests/testthat/test_externalQuery.R`

**Interfaces:**
- Consumes: `` `%||%` `` (Task 1); `symbolAttributeForOrganism(organism)` (exists in `R/featureUpdateHelpers.R`); `searchTranscriptomicsFeatureSet(conn_handler, feature_name, organism, verbose)` (exists).
- Produces:
  - `organismCode(organism)` -> `"hsa"`, `"mmu"` or `NA_character_`
  - `looksLikeGeneSymbol(x)` -> logical vector
  - `lookupSymbolsInSigRepo(conn_handler, feature_names, organism)` -> named character (feature_name -> symbol)
  - `lookupSymbolsInBiomart(feature_names, organism)` -> named character
  - `mapFeaturesToSymbols(feature_names, organism, conn_handler = NULL)` -> named character, NA where unmapped
  - `buildExternalQuery(omic_signature, direction = "combined", conn_handler = NULL, max_genes = 500, verbose = TRUE)` -> list with `signature_name, organism, organism_code, direction, genes, n_input, n_mapped, n_unmapped, truncated, unmapped`

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_externalQuery.R

# A tiny OmicSignature-shaped stand-in: buildExternalQuery() only touches
# $signature and $metadata, so a plain list is enough and keeps these tests
# independent of OmicSignature's constructor checks.
fake_signature <- function(feature_name, score = NULL, organism = "Homo sapiens",
                           name = "fake_sig", assay_type = "transcriptomics") {
  sig <- base::data.frame(
    probe_id = base::seq_along(feature_name),
    feature_name = feature_name,
    stringsAsFactors = FALSE
  )
  if (!base::is.null(score)) sig$score <- score
  base::list(
    signature = sig,
    metadata = base::list(signature_name = name, organism = organism, assay_type = assay_type)
  )
}

test_that("organismCode maps human and mouse case-insensitively", {
  expect_equal(organismCode("Homo sapiens"), "hsa")
  expect_equal(organismCode("Homo Sapiens"), "hsa")
  expect_equal(organismCode("mus musculus"), "mmu")
  expect_true(is.na(organismCode("Rattus norvegicus")))
  expect_true(is.na(organismCode(NULL)))
})

test_that("looksLikeGeneSymbol rejects Ensembl, Entrez and RefSeq ids", {
  expect_equal(
    looksLikeGeneSymbol(c("TP53", "ENSG00000141510", "ENSMUSG00000059552.7", "7157", "NM_000546", "Isg15")),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("mapFeaturesToSymbols passes symbols through and never calls lookups for them", {
  called <- FALSE
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(conn_handler, feature_names, organism) { called <<- TRUE; character() },
    lookupSymbolsInBiomart = function(feature_names, organism) { called <<- TRUE; character() }
  )
  m <- mapFeaturesToSymbols(c("TP53", "BRCA1", " TP53 "), "Homo sapiens")
  expect_false(called)
  expect_equal(unname(m), c("TP53", "BRCA1"))
})

test_that("mapFeaturesToSymbols uses SigRepo first, then biomaRt, and reports NA for the rest", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(conn_handler, feature_names, organism) c(ENSG00000141510 = "TP53"),
    lookupSymbolsInBiomart = function(feature_names, organism) c(ENSG00000012048 = "BRCA1")
  )
  m <- mapFeaturesToSymbols(c("ENSG00000141510", "ENSG00000012048", "ENSG00000000000"), "Homo sapiens", conn_handler = NULL)
  expect_equal(m[["ENSG00000141510"]], "TP53")
  expect_equal(m[["ENSG00000012048"]], "BRCA1")
  expect_true(is.na(m[["ENSG00000000000"]]))
})

test_that("lookupSymbolsInBiomart strips Ensembl versions and maps back to the original names", {
  testthat::local_mocked_bindings(
    biomartSymbolTable = function(ensembl_ids, organism) {
      base::data.frame(ensembl_gene_id = "ENSG00000126353", hgnc_symbol = "CCR7", stringsAsFactors = FALSE)
    }
  )
  m <- lookupSymbolsInBiomart(c("ENSG00000126353.12", "TP53"), "Homo sapiens")
  expect_equal(m, c(ENSG00000126353.12 = "CCR7"))
})

test_that("buildExternalQuery splits by score sign and drops NA scores", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("A1", "B1", "C1", "D1"), score = c(3, -2, NA, 1))
  up <- buildExternalQuery(sig, direction = "up", verbose = FALSE)
  expect_equal(up$genes, c("A1", "D1"))         # ordered by |score|
  expect_equal(up$direction, "up")
  # only B1 is negative: one gene is below the minimum of two
  expect_error(buildExternalQuery(sig, direction = "down", verbose = FALSE), "Could not map enough")
})

test_that("buildExternalQuery errors when direction splitting has no score", {
  sig <- fake_signature(c("A1", "B1"))
  expect_error(buildExternalQuery(sig, direction = "up", verbose = FALSE), "needs a 'score' column")
})

test_that("buildExternalQuery truncates to max_genes by |score| and flags it", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(paste0("G", 1:10), score = 10:1)
  q <- buildExternalQuery(sig, max_genes = 3, verbose = FALSE)
  expect_equal(q$genes, c("G1", "G2", "G3"))
  expect_true(q$truncated)
  expect_equal(q$n_input, 10)
})

test_that("buildExternalQuery errors when fewer than two symbols map", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) character(),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("ENSG00000000001", "ENSG00000000002"))
  expect_error(buildExternalQuery(sig, verbose = FALSE), "Could not map enough")
})

test_that("buildExternalQuery refuses non-transcriptomics signatures", {
  sig <- fake_signature(c("P12345", "Q67890"), assay_type = "proteomics")
  expect_error(buildExternalQuery(sig, verbose = FALSE), "transcriptomics signatures only")
})

test_that("buildExternalQuery reports organism code and unmapped names", {
  testthat::local_mocked_bindings(
    lookupSymbolsInSigRepo = function(...) c(ENSG00000141510 = "TP53"),
    lookupSymbolsInBiomart = function(...) character()
  )
  sig <- fake_signature(c("ENSG00000141510", "BRCA1", "ENSG00000000000"), organism = "Mus musculus")
  q <- buildExternalQuery(sig, verbose = FALSE)
  expect_equal(q$organism_code, "mmu")
  expect_setequal(q$genes, c("TP53", "BRCA1"))
  expect_equal(q$unmapped, "ENSG00000000000")
  expect_equal(q$n_unmapped, 1)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "externalQuery")'`
Expected: FAIL, `could not find function "organismCode"`.

- [ ] **Step 3: Write the implementation**

```r
# R/externalQuery.R
############################################################
# Translate a SigRepo signature into the one query shape the
# external engines share: gene symbols + an organism code.
############################################################

#' @keywords internal
#' @noRd
organismCode <- function(organism) {
  key <- base::tolower(base::trimws(base::as.character(organism %||% "")))
  if (base::length(key) == 0 || base::is.na(key[1])) {
    return(NA_character_)
  }
  if (key[1] %in% c("homo sapiens", "human", "hsa")) return("hsa")
  if (key[1] %in% c("mus musculus", "mouse", "mmu")) return("mmu")
  NA_character_
}

#' @keywords internal
#' @noRd
looksLikeGeneSymbol <- function(x) {
  x <- base::as.character(x)
  is_ensembl <- base::grepl("^ENS[A-Z]*[GTP][0-9]{6,}(\\.[0-9]+)?$", x)
  is_entrez  <- base::grepl("^[0-9]+$", x)
  is_refseq  <- base::grepl("^(NM|NR|XM|XR|NP|XP)_[0-9]+", x)
  !(is_ensembl | is_entrez | is_refseq)
}

#' @title lookupSymbolsInSigRepo
#' @description feature_name -> gene_symbol from the transcriptomics
#' reference table. Any failure (no connection, viewer without access,
#' missing table) yields an empty mapping so the caller can fall back.
#' @keywords internal
#' @noRd
lookupSymbolsInSigRepo <- function(conn_handler, feature_names, organism) {
  if (base::is.null(conn_handler) || base::length(feature_names) == 0) {
    return(base::character())
  }
  tbl <- base::tryCatch(
    searchTranscriptomicsFeatureSet(
      conn_handler = conn_handler, feature_name = feature_names, organism = NULL, verbose = FALSE
    ),
    error = function(e) NULL
  )
  if (base::is.null(tbl) || !base::all(c("feature_name", "gene_symbol") %in% base::colnames(tbl))) {
    return(base::character())
  }
  sym <- base::trimws(base::as.character(tbl$gene_symbol))
  keep <- !base::is.na(sym) & base::nzchar(sym)
  tbl <- tbl[keep, , drop = FALSE]
  tbl <- tbl[!base::duplicated(tbl$feature_name), , drop = FALSE]
  stats::setNames(base::trimws(base::as.character(tbl$gene_symbol)), base::as.character(tbl$feature_name))
}

#' @title biomartSymbolTable
#' @description The raw biomaRt call, isolated so tests can mock it.
#' @keywords internal
#' @noRd
biomartSymbolTable <- function(ensembl_ids, organism) {
  dataset <- base::switch(
    organismCode(organism),
    hsa = "hsapiens_gene_ensembl",
    mmu = "mmusculus_gene_ensembl",
    NA_character_
  )
  if (base::is.na(dataset)) {
    return(NULL)
  }
  attribute <- symbolAttributeForOrganism(organism)
  mart <- biomaRt::useEnsembl(biomart = "genes", dataset = dataset)
  biomaRt::getBM(
    attributes = c("ensembl_gene_id", attribute),
    filters = "ensembl_gene_id",
    values = base::unique(ensembl_ids),
    mart = mart
  )
}

#' @keywords internal
#' @noRd
lookupSymbolsInBiomart <- function(feature_names, organism) {
  ensembl_names <- feature_names[base::grepl("^ENS[A-Z]*G[0-9]+", feature_names)]
  if (base::length(ensembl_names) == 0) {
    return(base::character())
  }
  ids <- base::sub("\\.[0-9]+$", "", ensembl_names)
  bm <- base::tryCatch(biomartSymbolTable(ids, organism), error = function(e) NULL)
  if (base::is.null(bm) || base::nrow(bm) == 0) {
    return(base::character())
  }
  attribute <- symbolAttributeForOrganism(organism)
  sym <- base::trimws(base::as.character(bm[[attribute]]))
  bm <- bm[!base::is.na(sym) & base::nzchar(sym), , drop = FALSE]
  bm <- bm[!base::duplicated(bm$ensembl_gene_id), , drop = FALSE]
  by_id <- stats::setNames(base::trimws(base::as.character(bm[[attribute]])), bm$ensembl_gene_id)
  out <- by_id[ids]
  base::names(out) <- ensembl_names
  out[!base::is.na(out)]
}

#' @keywords internal
#' @noRd
mapFeaturesToSymbols <- function(feature_names, organism, conn_handler = NULL) {
  feature_names <- base::unique(base::trimws(base::as.character(feature_names)))
  feature_names <- feature_names[!base::is.na(feature_names) & base::nzchar(feature_names)]
  symbol <- stats::setNames(base::rep(NA_character_, base::length(feature_names)), feature_names)
  is_symbol <- looksLikeGeneSymbol(feature_names)
  symbol[is_symbol] <- feature_names[is_symbol]

  pending <- base::names(symbol)[base::is.na(symbol)]
  if (base::length(pending) > 0) {
    hits <- lookupSymbolsInSigRepo(conn_handler, pending, organism)
    hits <- hits[base::names(hits) %in% pending]
    if (base::length(hits) > 0) symbol[base::names(hits)] <- base::unname(hits)
  }
  pending <- base::names(symbol)[base::is.na(symbol)]
  if (base::length(pending) > 0) {
    hits <- lookupSymbolsInBiomart(pending, organism)
    hits <- hits[base::names(hits) %in% pending]
    if (base::length(hits) > 0) symbol[base::names(hits)] <- base::unname(hits)
  }
  symbol
}

#' @title buildExternalQuery
#' @description Turn an OmicSignature into the query all three engines accept.
#' @param omic_signature An OmicSignature (or anything with `$signature` and `$metadata`).
#' @param direction "combined", "up" or "down". Up/down use the sign of `score`.
#' @param conn_handler Optional; enables the reference-table symbol lookup.
#' @param max_genes Cap on genes sent (CORESH refuses more than 500).
#' @keywords internal
#' @noRd
buildExternalQuery <- function(omic_signature, direction = "combined", conn_handler = NULL,
                               max_genes = 500, verbose = TRUE) {
  sig <- omic_signature$signature
  meta <- omic_signature$metadata
  name <- base::as.character(meta$signature_name %||% "signature")[1]
  organism <- base::as.character(meta$organism %||% NA_character_)[1]
  assay <- base::tolower(base::as.character(meta$assay_type %||% "transcriptomics")[1])

  if (!base::identical(assay, "transcriptomics")) {
    base::stop(base::sprintf(
      "searchExternal() supports transcriptomics signatures only; '%s' has assay_type '%s'.", name, assay
    ))
  }
  if (base::is.null(sig) || base::nrow(sig) == 0 || !"feature_name" %in% base::colnames(sig)) {
    base::stop(base::sprintf("Signature '%s' has no features to search with.", name))
  }

  has_score <- "score" %in% base::colnames(sig) && base::any(!base::is.na(sig$score))
  if (!base::identical(direction, "combined")) {
    if (!has_score) {
      base::stop(base::sprintf("Direction splitting needs a 'score' column; '%s' has none.", name))
    }
    keep <- if (base::identical(direction, "up")) sig$score > 0 else sig$score < 0
    sig <- sig[keep %in% TRUE, , drop = FALSE]
  }
  if (has_score) {
    sig <- sig[base::order(-base::abs(sig$score), na.last = TRUE), , drop = FALSE]
  }

  feature_names <- base::as.character(sig$feature_name)
  mapping <- mapFeaturesToSymbols(feature_names, organism, conn_handler)
  symbols <- base::unname(mapping[base::trimws(feature_names)])
  genes <- base::unique(symbols[!base::is.na(symbols) & base::nzchar(symbols)])

  n_input <- base::length(base::unique(feature_names))
  n_mapped <- base::length(genes)
  unmapped <- base::names(mapping)[base::is.na(mapping)]
  truncated <- n_mapped > max_genes
  if (truncated) {
    genes <- genes[base::seq_len(max_genes)]
  }
  if (base::length(genes) < 2) {
    base::stop(base::sprintf(
      "Could not map enough of the %d features of '%s' to gene symbols (organism '%s'): %d mapped, need at least 2.",
      n_input, name, organism, n_mapped
    ))
  }
  if (base::isTRUE(verbose)) {
    base::message(base::sprintf(
      "[%s] %s: %d of %d features mapped to gene symbols%s%s.",
      direction, name, n_mapped, n_input,
      if (base::length(unmapped) > 0) base::sprintf(" (%d unmapped)", base::length(unmapped)) else "",
      if (truncated) base::sprintf("; sending the top %d by |score|", max_genes) else ""
    ))
  }

  base::list(
    signature_name = name,
    organism = organism,
    organism_code = organismCode(organism),
    direction = direction,
    genes = genes,
    n_input = n_input,
    n_mapped = n_mapped,
    n_unmapped = base::length(unmapped),
    truncated = truncated,
    unmapped = unmapped
  )
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "externalQuery")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/externalQuery.R tests/testthat/test_externalQuery.R
git commit -m "Translate a signature into gene symbols and an organism code for external searches

Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 3: Rummagene adapter

**Files:**
- Create: `R/externalRummagene.R`
- Test: `tests/testthat/test_externalRummagene.R`
- Fixture (already present): `tests/testthat/test_data/external/rummagene_enrich.json`

**Interfaces:**
- Consumes: `externalGraphql`, `externalUrl`, `bindExternalHits`, `` `%||%` `` (Task 1); query list from Task 2.
- Produces:
  - `externalRummagene(query, limit = 25, timeout = 60)` -> data frame
  - `parseRummagene(payload, query)` -> data frame with extras `pmcid, year, doi, description, n_papers`

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_externalRummagene.R
read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseRummagene flattens hits into the common frame", {
  out <- parseRummagene(read_fixture("rummagene_enrich.json"), ifn_query())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "rummagene")
  expect_equal(out$rank, 1:3)
  expect_equal(out$id[1], "PMC11278796-mmc3.docx-0-Target_gene")
  expect_match(out$title[1], "^Spontaneous NETosis")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC11278796/")
  expect_equal(out$score_label[1], "odds ratio")
  expect_equal(out$n_overlap[1], 12)
  expect_equal(out$n_set[1], 22)
  expect_equal(out$pmcid[1], "PMC11278796")
  expect_equal(out$year[1], 2023)
  expect_equal(attr(out, "total_count"), 21423)
  expect_equal(attr(out, "query")$signature_name, "ifn")
})

test_that("parseRummagene handles an empty result", {
  payload <- list(data = list(currentBackground = list(enrich = list(totalCount = 0, nodes = list()))))
  out <- parseRummagene(payload, ifn_query())
  expect_equal(nrow(out), 0)
  expect_equal(attr(out, "total_count"), 0)
})

test_that("externalRummagene upper-cases genes and passes limit", {
  seen <- NULL
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      seen <<- variables
      read_fixture("rummagene_enrich.json")
    }
  )
  q <- ifn_query("Mus musculus", "mmu"); q$genes <- c("Isg15", "Ifi6", "Mx1")
  out <- externalRummagene(q, limit = 7)
  expect_equal(unlist(seen$genes), c("ISG15", "IFI6", "MX1"))
  expect_equal(seen$first, 7L)
  expect_equal(nrow(out), 3)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "externalRummagene")'`
Expected: FAIL, `could not find function "parseRummagene"`.

- [ ] **Step 3: Write the implementation**

```r
# R/externalRummagene.R
############################################################
# Rummagene: ~1M gene sets mined from PMC supplementary
# tables (Communications Biology 2024, PMID 38643247).
# Public GraphQL endpoint, no auth. Same document the SigRepo
# server's Annotate tab uses.
############################################################

RUMMAGENE_ENRICH_QUERY <- "query enrich($genes:[String]!, $first:Int, $overlapGe:Int, $pvalueLe:Float){
  currentBackground {
    enrich(genes:$genes, first:$first, overlapGe:$overlapGe, pvalueLe:$pvalueLe){
      totalCount
      nodes {
        pvalue adjPvalue oddsRatio nOverlap
        geneSets { nodes { term description nGeneIds
          geneSetPmcsById { nodes { pmcInfoByPmcid { pmcid title yr doi } } } } }
      }
    }
  }
}"

#' @keywords internal
#' @noRd
externalRummagene <- function(query, limit = 25, timeout = 60) {
  variables <- base::list(
    genes = base::as.list(base::toupper(query$genes)),
    first = base::as.integer(limit),
    overlapGe = 2L,
    pvalueLe = 0.05
  )
  payload <- externalGraphql(externalUrl("rummagene"), RUMMAGENE_ENRICH_QUERY, variables, timeout = timeout)
  parseRummagene(payload, query)
}

#' @keywords internal
#' @noRd
parseRummagene <- function(payload, query) {
  enrich <- base::tryCatch(payload$data$currentBackground$enrich, error = function(e) NULL)
  rows <- base::lapply(enrich$nodes %||% base::list(), function(node) {
    gs_nodes <- base::tryCatch(node$geneSets$nodes, error = function(e) NULL) %||% base::list()
    if (base::length(gs_nodes) == 0) {
      return(NULL)
    }
    gs <- gs_nodes[[1]]
    pmc <- NULL
    for (n in gs$geneSetPmcsById$nodes %||% base::list()) {
      if (!base::is.null(n$pmcInfoByPmcid)) { pmc <- n$pmcInfoByPmcid; break }
    }
    term <- gs$term %||% NA_character_
    pmcid <- pmc$pmcid
    if (base::is.null(pmcid) && !base::is.na(term) && base::grepl("^PMC[0-9]+", term)) {
      pmcid <- base::regmatches(term, base::regexpr("^PMC[0-9]+", term))
    }
    pmcid <- pmcid %||% NA_character_
    base::list(
      id = term,
      title = pmc$title %||% term,
      url = if (base::is.na(pmcid)) NA_character_ else base::sprintf("https://www.ncbi.nlm.nih.gov/pmc/articles/%s/", pmcid),
      score = node$oddsRatio,
      score_label = "odds ratio",
      pvalue = node$pvalue,
      adj_pvalue = node$adjPvalue,
      n_overlap = node$nOverlap,
      n_set = gs$nGeneIds,
      pmcid = pmcid,
      year = pmc$yr,
      doi = pmc$doi,
      description = gs$description,
      n_papers = base::length(gs_nodes)
    )
  })
  rows <- base::Filter(Negate(base::is.null), rows)
  out <- bindExternalHits("rummagene", query$direction, rows)
  base::attr(out, "total_count") <- enrich$totalCount %||% base::nrow(out)
  base::attr(out, "query") <- query
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "externalRummagene")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/externalRummagene.R tests/testthat/test_externalRummagene.R tests/testthat/test_data/external/rummagene_enrich.json
git commit -m "Add the Rummagene adapter for searchExternal()

Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 4: RummaGEO adapter

**Files:**
- Create: `R/externalRummageo.R`
- Test: `tests/testthat/test_externalRummageo.R`
- Fixture (already present): `tests/testthat/test_data/external/rummageo_enrich.json`

**Interfaces:**
- Consumes: `externalGraphql`, `externalUrl`, `bindExternalHits`, `` `%||%` `` (Task 1).
- Produces:
  - `rummageoBackgroundId(organism_code, timeout = 60)` -> UUID string
  - `externalRummageo(query, limit = 25, timeout = 60)` -> data frame
  - `parseRummageo(payload, query)` -> data frame with extras `gse, species, contrast, regulation`

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_externalRummageo.R
read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseRummageo flattens hits and derives GEO links", {
  out <- parseRummageo(read_fixture("rummageo_enrich.json"), ifn_query())
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "rummageo")
  expect_equal(out$id[1], "GSE218462,GSE218464-0-vs-1-human up")
  expect_equal(out$gse[1], "GSE218462,GSE218464")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE218462")
  expect_equal(out$regulation[1], "up")
  expect_equal(out$contrast[1], "0-vs-1")
  expect_equal(out$score_label[1], "odds ratio")
  expect_equal(out$n_overlap[1], 11)
  expect_equal(out$n_set[1], 34)
  expect_equal(attr(out, "total_count"), 18448)
})

test_that("rummageoBackgroundId picks the background by species", {
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      list(data = list(backgrounds = list(nodes = list(
        list(id = "mouse-id", species = "mouse"),
        list(id = "human-id", species = "human")
      ))))
    }
  )
  expect_equal(rummageoBackgroundId("hsa"), "human-id")
  expect_equal(rummageoBackgroundId("mmu"), "mouse-id")
})

test_that("externalRummageo refuses organisms RummaGEO does not index", {
  q <- ifn_query("Rattus norvegicus", NA_character_)
  expect_error(externalRummageo(q), "only supports Homo sapiens and Mus musculus")
})

test_that("externalRummageo sends mouse symbols unchanged with the mouse background", {
  seen <- list()
  testthat::local_mocked_bindings(
    externalGraphql = function(url, query, variables = list(), timeout = 60) {
      if (grepl("backgrounds", query, fixed = TRUE)) {
        return(list(data = list(backgrounds = list(nodes = list(list(id = "mouse-id", species = "mouse"))))))
      }
      seen <<- variables
      read_fixture("rummageo_enrich.json")
    }
  )
  q <- ifn_query("Mus musculus", "mmu"); q$genes <- c("Isg15", "Ifi6", "Mx1")
  out <- externalRummageo(q, limit = 5)
  expect_equal(seen$id, "mouse-id")
  expect_equal(unlist(seen$genes), c("Isg15", "Ifi6", "Mx1"))
  expect_equal(seen$first, 5L)
  expect_equal(nrow(out), 3)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "externalRummageo")'`
Expected: FAIL, `could not find function "parseRummageo"`.

- [ ] **Step 3: Write the implementation**

```r
# R/externalRummageo.R
############################################################
# RummaGEO: up/down gene sets auto-computed from GEO RNA-seq
# (ARCHS4). Human and mouse live in separate "backgrounds";
# we look the right one up by species, then enrich against it.
############################################################

RUMMAGEO_BACKGROUNDS_QUERY <- "{ backgrounds { nodes { id species } } }"

RUMMAGEO_ENRICH_QUERY <- "query enrich($id:UUID!, $genes:[String]!, $first:Int, $overlapGe:Int){
  background(id:$id) {
    species
    enrich(genes:$genes, first:$first, overlapGe:$overlapGe){
      totalCount
      nodes {
        pvalue adjPvalue oddsRatio nOverlap
        geneSet { term species nGeneIds geneSetGsesById { nodes { gse } } }
      }
    }
  }
}"

#' @keywords internal
#' @noRd
rummageoBackgroundId <- function(organism_code, timeout = 60) {
  species <- base::switch(organism_code, hsa = "human", mmu = "mouse",
                          base::stop("RummaGEO only supports Homo sapiens and Mus musculus."))
  payload <- externalGraphql(externalUrl("rummageo"), RUMMAGEO_BACKGROUNDS_QUERY, base::list(), timeout = timeout)
  for (n in payload$data$backgrounds$nodes %||% base::list()) {
    if (base::identical(n$species, species)) {
      return(n$id)
    }
  }
  base::stop(base::sprintf("RummaGEO has no %s background.", species))
}

#' @keywords internal
#' @noRd
externalRummageo <- function(query, limit = 25, timeout = 60) {
  if (base::is.na(query$organism_code)) {
    base::stop(base::sprintf(
      "RummaGEO only supports Homo sapiens and Mus musculus; '%s' is '%s'.",
      query$signature_name, query$organism
    ))
  }
  background_id <- rummageoBackgroundId(query$organism_code, timeout = timeout)
  variables <- base::list(
    id = background_id,
    genes = base::as.list(query$genes),
    first = base::as.integer(limit),
    overlapGe = 2L
  )
  payload <- externalGraphql(externalUrl("rummageo"), RUMMAGEO_ENRICH_QUERY, variables, timeout = timeout)
  parseRummageo(payload, query)
}

#' @keywords internal
#' @noRd
parseRummageo <- function(payload, query) {
  bg <- base::tryCatch(payload$data$background, error = function(e) NULL) %||%
    base::tryCatch(payload$data$currentBackground, error = function(e) NULL)
  enrich <- bg$enrich
  rows <- base::lapply(enrich$nodes %||% base::list(), function(node) {
    gs <- node$geneSet
    if (base::is.null(gs)) {
      return(NULL)
    }
    term <- gs$term %||% NA_character_
    gses <- base::vapply(gs$geneSetGsesById$nodes %||% base::list(),
                         function(n) base::as.character(n$gse %||% NA_character_), character(1))
    gse <- if (base::length(gses) > 0 && !base::is.na(gses[1])) gses[1] else base::sub("-.*$", "", term)
    first_gse <- base::strsplit(gse, ",", fixed = TRUE)[[1]][1]
    # term looks like "GSE123,GSE124-0-vs-1-human up" / "GSE1-0-vs-6-mouse.tsv up"
    rest <- base::sub("^[^-]*-", "", term)
    regulation <- if (base::grepl(" up$", rest)) "up" else if (base::grepl(" (dn|down)$", rest)) "down" else NA_character_
    contrast <- base::sub("-(human|mouse)(\\.tsv)? (up|dn|down)$", "", rest)
    base::list(
      id = term,
      title = term,
      url = base::sprintf("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s", first_gse),
      score = node$oddsRatio,
      score_label = "odds ratio",
      pvalue = node$pvalue,
      adj_pvalue = node$adjPvalue,
      n_overlap = node$nOverlap,
      n_set = gs$nGeneIds,
      gse = gse,
      species = gs$species,
      contrast = contrast,
      regulation = regulation
    )
  })
  rows <- base::Filter(Negate(base::is.null), rows)
  out <- bindExternalHits("rummageo", query$direction, rows)
  base::attr(out, "total_count") <- enrich$totalCount %||% base::nrow(out)
  base::attr(out, "query") <- query
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "externalRummageo")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/externalRummageo.R tests/testthat/test_externalRummageo.R tests/testthat/test_data/external/rummageo_enrich.json
git commit -m "Add the RummaGEO adapter for searchExternal()

Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 5: CORESH adapter

**Files:**
- Create: `R/externalCoresh.R`
- Test: `tests/testthat/test_externalCoresh.R`
- Fixtures (already present): `tests/testthat/test_data/external/coresh_ranking.json`, `coresh_words.json` (words unused for now; kept for a later enriched-words feature)

**Interfaces:**
- Consumes: `externalPostJson`, `externalGetJson`, `externalUrl`, `bindExternalHits`, `` `%||%` `` (Task 1).
- Produces:
  - `coreshJobUrl(job_id)` -> `https://alserglab.wustl.edu/coresh/load/<id>`
  - `externalCoresh(query, limit = 25, calculate_pvalues = FALSE, timeout = 120)` -> data frame with attributes `job_id`, `job_url`
  - `parseCoreshRanking(rows, query, limit = 25)` -> data frame with extras `gpl, summary, positive_words, negative_words, log10_padj`

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_externalCoresh.R
read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("test_data", "external", name), simplifyVector = FALSE)
}
ifn_query <- function(organism = "Homo sapiens", code = "hsa") {
  base::list(signature_name = "ifn", organism = organism, organism_code = code, direction = "combined",
             genes = c("ISG15", "IFI6", "MX1"), n_input = 3, n_mapped = 3, n_unmapped = 0,
             truncated = FALSE, unmapped = character())
}

test_that("parseCoreshRanking maps GEO datasets into the common frame", {
  out <- parseCoreshRanking(read_fixture("coresh_ranking.json"), ifn_query(), limit = 3)
  expect_equal(nrow(out), 3)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_equal(out$source[1], "coresh")
  expect_equal(out$id[1], "GSE88731")
  expect_match(out$title[1], "hepatitis E")
  expect_equal(out$url[1], "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE88731")
  expect_equal(out$score[1], 20.8949)
  expect_equal(out$score_label[1], "% variance")
  expect_true(is.na(out$pvalue[1]))
  expect_true(is.na(out$adj_pvalue[1]))        # log10Padj is the string "NA" without p-values
  expect_equal(out$n_set[1], 11)
  expect_equal(out$gpl[1], "GPL20301")
  expect_equal(attr(out, "total_count"), 5)
})

test_that("parseCoreshRanking converts log10Padj when present and tolerates limit > hits", {
  rows <- read_fixture("coresh_ranking.json")
  rows[[1]]$log10Padj <- -3
  out <- parseCoreshRanking(rows, ifn_query(), limit = 100)
  expect_equal(nrow(out), 5)
  expect_equal(out$adj_pvalue[1], 1e-3)
  expect_equal(out$log10_padj[1], -3)
})

test_that("externalCoresh submits, polls until done, and fetches the ranking", {
  calls <- character()
  progress <- c(40, 100)
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) {
      calls <<- c(calls, "submit")
      expect_equal(body$organism, "hsa"); expect_equal(body$dbType, "hsa")
      expect_false(body$calculatePvalues)
      expect_equal(unlist(body$genes), c("ISG15", "IFI6", "MX1"))
      list(ID = "20261007_test")
    },
    externalGetJson = function(url, query = list(), timeout = 60) {
      if (grepl("check-job", url)) {
        calls <<- c(calls, "check-job")
        p <- progress[1]; progress <<- progress[-1]
        return(list(progress = p, nbatches = 100, ncompleted = p))
      }
      if (grepl("check-result-files", url)) { calls <<- c(calls, "files"); return(list(files_ready = TRUE)) }
      if (grepl("get-ranking-result", url)) {
        calls <<- c(calls, "ranking"); expect_equal(query$jobid, "20261007_test")
        return(read_fixture("coresh_ranking.json"))
      }
      stop("unexpected url ", url)
    }
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  out <- externalCoresh(ifn_query(), limit = 2)
  expect_equal(calls, c("submit", "check-job", "check-job", "files", "ranking"))
  expect_equal(nrow(out), 2)
  expect_equal(attr(out, "job_id"), "20261007_test")
  expect_equal(attr(out, "job_url"), "https://alserglab.wustl.edu/coresh/load/20261007_test")
})

test_that("externalCoresh stops when the job reports failure", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(ID = "bad_job"),
    externalGetJson = function(url, query = list(), timeout = 60) list(progress = 10, failed = "True")
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  expect_error(externalCoresh(ifn_query()), "CORESH job bad_job failed")
})

test_that("externalCoresh times out with the job url in the message", {
  testthat::local_mocked_bindings(
    externalPostJson = function(url, body, timeout = 60) list(ID = "slow_job"),
    externalGetJson = function(url, query = list(), timeout = 60) list(progress = 10)
  )
  withr::local_options(list(sigrepo.coresh_poll_seconds = 0))
  expect_error(externalCoresh(ifn_query(), timeout = 0), "coresh/load/slow_job")
})

test_that("externalCoresh refuses organisms CORESH does not index", {
  expect_error(externalCoresh(ifn_query("Danio rerio", NA_character_)), "only supports Homo sapiens and Mus musculus")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "externalCoresh")'`
Expected: FAIL, `could not find function "parseCoreshRanking"`.

- [ ] **Step 3: Write the implementation**

```r
# R/externalCoresh.R
############################################################
# CORESH: ranks ~40k human and ~40k mouse GEO expression
# datasets by how much variance a query gene set explains as
# a coregulated block (Sukhov et al., NAR 2025,
# doi 10.1093/nar/gkaf372). The web app's JSON backend has
# no auth: submit a job, poll it, fetch the ranking.
# Endpoints verified against the live service on 2026-10-07.
############################################################

#' @keywords internal
#' @noRd
coreshJobUrl <- function(job_id) {
  base::sprintf("https://alserglab.wustl.edu/coresh/load/%s", job_id)
}

#' @keywords internal
#' @noRd
externalCoresh <- function(query, limit = 25, calculate_pvalues = FALSE, timeout = 120) {
  if (base::is.na(query$organism_code)) {
    base::stop(base::sprintf(
      "CORESH only supports Homo sapiens and Mus musculus; '%s' is '%s'.",
      query$signature_name, query$organism
    ))
  }
  base_url <- externalUrl("coresh")
  pv_flag <- if (base::isTRUE(calculate_pvalues)) "true" else "false"
  poll <- base::getOption("sigrepo.coresh_poll_seconds", 2)

  submitted <- externalPostJson(
    base::paste0(base_url, "/submit-genes"),
    base::list(
      organism = query$organism_code,
      dbType = query$organism_code,
      genes = base::as.list(query$genes),
      calculatePvalues = base::isTRUE(calculate_pvalues)
    ),
    timeout = 60
  )
  job_id <- submitted$ID
  if (base::is.null(job_id) || !base::nzchar(job_id)) {
    base::stop("CORESH did not return a job id.")
  }
  deadline <- base::Sys.time() + timeout
  timeout_msg <- base::sprintf(
    "CORESH job %s did not finish within %s s. Results will appear at %s when done.",
    job_id, timeout, coreshJobUrl(job_id)
  )

  repeat {
    status <- externalGetJson(
      base::paste0(base_url, "/check-job"),
      base::list(jobid = job_id, organism = query$organism_code, calculatePvalues = pv_flag)
    )
    if (base::identical(status$failed, "True") || base::isTRUE(status$failed)) {
      base::stop(base::sprintf("CORESH job %s failed.", job_id))
    }
    progress <- base::suppressWarnings(base::as.numeric(status$progress %||% 0))
    if (base::isTRUE(progress >= 100)) break
    if (base::Sys.time() > deadline) base::stop(timeout_msg)
    base::Sys.sleep(poll)
  }
  repeat {
    files <- externalGetJson(
      base::paste0(base_url, "/check-result-files"),
      base::list(jobid = job_id, calculatePvalues = pv_flag)
    )
    if (base::isTRUE(files$files_ready)) break
    if (base::Sys.time() > deadline) base::stop(timeout_msg)
    base::Sys.sleep(poll)
  }

  ranking <- externalGetJson(base::paste0(base_url, "/get-ranking-result"), base::list(jobid = job_id))
  out <- parseCoreshRanking(ranking, query, limit = limit)
  base::attr(out, "job_id") <- job_id
  base::attr(out, "job_url") <- coreshJobUrl(job_id)
  out
}

#' @keywords internal
#' @noRd
parseCoreshRanking <- function(rows, query, limit = 25) {
  rows <- rows %||% base::list()
  total <- base::length(rows)
  rows <- utils::head(rows, base::as.integer(limit))
  hits <- base::lapply(rows, function(r) {
    log10_padj <- base::suppressWarnings(base::as.numeric(r$log10Padj %||% NA))
    base::list(
      id = r$gseId,
      title = r$gseTitle,
      url = base::sprintf("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s", r$gseId),
      score = r$pctVar,
      score_label = "% variance",
      pvalue = NA_real_,
      adj_pvalue = if (base::is.na(log10_padj)) NA_real_ else 10^log10_padj,
      n_overlap = NA_integer_,
      n_set = r$size,
      gpl = r$gplId,
      summary = r$gseSummary,
      positive_words = r$positiveWords,
      negative_words = r$negativeWords,
      log10_padj = log10_padj
    )
  })
  out <- bindExternalHits("coresh", query$direction, hits)
  base::attr(out, "total_count") <- total
  base::attr(out, "query") <- query
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "externalCoresh")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/externalCoresh.R tests/testthat/test_externalCoresh.R tests/testthat/test_data/external/coresh_ranking.json tests/testthat/test_data/external/coresh_words.json
git commit -m "Add the CORESH adapter for searchExternal()

Submit, poll and fetch against the coresh-back JSON endpoints. Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 6: `searchExternal()` dispatcher, docs and NEWS

**Files:**
- Create: `R/searchExternal.R`
- Modify: `NAMESPACE` (generated), `man/searchExternal.Rd` (generated), `NEWS.Rmd`, `NEWS.md` (regenerated)
- Test: `tests/testthat/test_searchExternal.R`

**Interfaces:**
- Consumes: `resolveComparisonSignature(conn_handler, signature_id, signature_name, omic_signature, label, verbose)` (exists in `R/hypeRClient.R`); `buildExternalQuery` (Task 2); `externalRummagene`, `externalRummageo`, `externalCoresh` (Tasks 3-5); `emptyExternalFrame`, `rbindExternalFrames` (Task 1).
- Produces: exported `searchExternal(...)`.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test_searchExternal.R
fake_signature <- function(feature_name, score = NULL, organism = "Homo sapiens", name = "fake_sig") {
  sig <- base::data.frame(probe_id = base::seq_along(feature_name), feature_name = feature_name, stringsAsFactors = FALSE)
  if (!base::is.null(score)) sig$score <- score
  base::list(signature = sig, metadata = base::list(signature_name = name, organism = organism, assay_type = "transcriptomics"))
}
one_hit <- function(source, query) {
  bindExternalHits(source, query$direction, list(list(id = paste0(source, "-hit"), title = "t", score = 1)))
}

test_that("searchExternal with one source returns a data frame", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) one_hit("rummagene", query)
  )
  out <- searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "rummagene", verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(out$id, "rummagene-hit")
  expect_equal(out$direction, "combined")
})

test_that("searchExternal with several sources returns a named list and survives one failure", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) one_hit("rummagene", query),
    externalRummageo = function(query, limit, timeout) stop("rummageo is down"),
    externalCoresh = function(query, limit, calculate_pvalues, timeout) one_hit("coresh", query)
  )
  expect_warning(
    out <- searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), verbose = FALSE),
    "rummageo: rummageo is down"
  )
  expect_type(out, "list")
  expect_equal(names(out), c("rummagene", "rummageo", "coresh"))
  expect_equal(nrow(out$rummageo), 0)
  expect_equal(out$coresh$id, "coresh-hit")
})

test_that("searchExternal with a single failing source errors instead of warning", {
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalCoresh = function(query, limit, calculate_pvalues, timeout) stop("CORESH job x failed.")
  )
  expect_error(
    searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "coresh", verbose = FALSE),
    "CORESH job x failed"
  )
})

test_that("searchExternal direction = 'both' stacks up and down with a direction column", {
  seen <- character()
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalRummagene = function(query, limit, timeout) { seen <<- c(seen, query$genes); one_hit("rummagene", query) }
  )
  sig <- fake_signature(c("UP1", "UP2", "DN1", "DN2"), score = c(2, 1, -1, -2))
  out <- searchExternal(omic_signature = sig, source = "rummagene", direction = "both", verbose = FALSE)
  expect_equal(out$direction, c("up", "down"))
  expect_setequal(seen, c("UP1", "UP2", "DN1", "DN2"))
  expect_equal(names(attr(out, "query")), c("up", "down"))
})

test_that("searchExternal passes limit and calculate_pvalues through to CORESH", {
  seen <- list()
  testthat::local_mocked_bindings(
    resolveComparisonSignature = function(conn_handler, signature_id, signature_name, omic_signature, label, verbose) omic_signature,
    externalCoresh = function(query, limit, calculate_pvalues, timeout) {
      seen <<- list(limit = limit, pv = calculate_pvalues, timeout = timeout)
      one_hit("coresh", query)
    }
  )
  searchExternal(omic_signature = fake_signature(c("TP53", "BRCA1")), source = "coresh",
                 limit = 7, calculate_pvalues = TRUE, timeout = 33, verbose = FALSE)
  expect_equal(seen, list(limit = 7L, pv = TRUE, timeout = 33))
})

test_that("searchExternal validates its arguments", {
  expect_error(searchExternal(omic_signature = fake_signature(c("A", "B")), source = "enrichr"), "should be one of")
  expect_error(searchExternal(omic_signature = fake_signature(c("A", "B")), source = "coresh", limit = 0), "positive integer")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "searchExternal")'`
Expected: FAIL, `could not find function "searchExternal"`.

- [ ] **Step 3: Write the implementation**

```r
# R/searchExternal.R
#' @title searchExternal
#' @description Search public signature engines with a SigRepo signature.
#' The signature's features are translated into gene symbols (reference table
#' first, then biomaRt for Ensembl IDs) plus an organism code, and sent to one
#' or more of:
#' \itemize{
#'   \item \strong{Rummagene} (\url{https://rummagene.com}): ~1M gene sets mined
#'   from PMC supplementary tables. Fisher overlap, any organism.
#'   \item \strong{RummaGEO} (\url{https://rummageo.com}): up/down gene sets
#'   auto-computed from GEO RNA-seq studies. Fisher overlap, human and mouse.
#'   \item \strong{CORESH} (\url{https://alserglab.wustl.edu/coresh/}): GEO
#'   expression datasets ranked by how much variance the query explains as a
#'   coregulated block. Human and mouse; optional permutation p-values.
#' }
#' @param conn_handler An R object obtained from \code{SigRepo::newConnHandler()}.
#' Required unless \code{omic_signature} is supplied. Also enables the
#' reference-table symbol lookup.
#' @param signature_id A single SigRepo signature ID.
#' @param signature_name A single SigRepo signature name.
#' @param omic_signature An \code{OmicSignature} object, instead of fetching one.
#' @param source One or more of \code{"rummagene"}, \code{"rummageo"},
#' \code{"coresh"}. Defaults to all three.
#' @param direction Which features to send: \code{"combined"} (all),
#' \code{"up"} / \code{"down"} (by the sign of \code{score}), or \code{"both"}
#' (up and down as two separate queries, stacked with a \code{direction} column).
#' @param limit Maximum hits per source (per direction for \code{"both"}).
#' @param calculate_pvalues CORESH only: also compute permutation p-values
#' (about a minute instead of seconds).
#' @param timeout Seconds to wait for a CORESH job before giving up.
#' @param verbose Logical; print diagnostic messages. Default \code{TRUE}.
#'
#' @return For a single \code{source}, a data frame; for several, a named list of
#' data frames. Every data frame starts with the columns \code{source, direction,
#' rank, id, title, url, score, score_label, pvalue, adj_pvalue, n_overlap, n_set},
#' followed by service-specific extras, and carries attributes \code{query}
#' (the translated query that was sent) and \code{total_count}. CORESH frames
#' also carry \code{job_url}, a shareable results page.
#'
#' @export
#' @examples
#' \dontrun{
#' hits <- SigRepo::searchExternal(
#'   conn_handler = conn_handler,
#'   signature_name = "my_signature",
#'   source = "coresh"
#' )
#' attr(hits, "job_url")
#'
#' all_three <- SigRepo::searchExternal(omic_signature = my_omic_signature)
#' all_three$rummagene
#' }
searchExternal <- function(
    conn_handler = NULL,
    signature_id = NULL,
    signature_name = NULL,
    omic_signature = NULL,
    source = c("rummagene", "rummageo", "coresh"),
    direction = c("combined", "up", "down", "both"),
    limit = 25,
    calculate_pvalues = FALSE,
    timeout = 120,
    verbose = TRUE
){

  source <- base::match.arg(source, choices = EXTERNAL_SOURCES, several.ok = TRUE)
  direction <- base::match.arg(direction)
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    base::stop("\n'limit' must be a positive integer.\n")
  }

  omic_signature <- resolveComparisonSignature(
    conn_handler = conn_handler,
    signature_id = signature_id,
    signature_name = signature_name,
    omic_signature = omic_signature,
    label = "signature",
    verbose = verbose
  )

  directions <- if (base::identical(direction, "both")) c("up", "down") else direction
  queries <- base::lapply(directions, function(d) {
    buildExternalQuery(omic_signature, direction = d, conn_handler = conn_handler, max_genes = 500, verbose = verbose)
  })
  base::names(queries) <- directions

  run_adapter <- function(src, query) {
    base::switch(
      src,
      rummagene = externalRummagene(query, limit = limit, timeout = timeout),
      rummageo  = externalRummageo(query, limit = limit, timeout = timeout),
      coresh    = externalCoresh(query, limit = limit, calculate_pvalues = calculate_pvalues, timeout = timeout)
    )
  }

  run_source <- function(src) {
    frames <- base::lapply(directions, function(d) {
      base::tryCatch(
        run_adapter(src, queries[[d]]),
        error = function(e) {
          if (base::length(source) == 1) {
            base::stop(e)
          }
          base::warning(base::sprintf("%s: %s", src, base::conditionMessage(e)), call. = FALSE)
          emptyExternalFrame(src, d)
        }
      )
    })
    base::names(frames) <- directions
    out <- rbindExternalFrames(frames)
    if (base::length(frames) == 1) {
      base::attr(out, "query") <- queries[[1]]
      base::attr(out, "total_count") <- base::attr(frames[[1]], "total_count")
      base::attr(out, "job_url") <- base::attr(frames[[1]], "job_url")
    } else {
      base::attr(out, "query") <- queries
      base::attr(out, "total_count") <- base::lapply(frames, base::attr, "total_count")
      base::attr(out, "job_url") <- base::lapply(frames, base::attr, "job_url")
    }
    out
  }

  results <- base::lapply(source, run_source)
  base::names(results) <- source
  if (base::length(source) == 1) results[[1]] else results

}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "searchExternal")'`
Expected: all PASS.

- [ ] **Step 5: Regenerate NAMESPACE and man pages**

Run: `Rscript -e 'devtools::document()'`
Expected: `NAMESPACE` gains `export(searchExternal)`; `man/searchExternal.Rd` is created; no other `man/` files change. Verify with `git status --short`.

- [ ] **Step 6: Add the NEWS entry and regenerate NEWS.md**

Insert as the first bullet under `# SigRepo 1.0.0` in `NEWS.Rmd`:

```markdown
* New `searchExternal()`: give it a signature (id, name, or `OmicSignature`) and it translates the features to gene symbols and queries Rummagene, RummaGEO and/or CORESH, returning data frames with a shared column set. CORESH results carry the shareable job page as `attr(x, "job_url")` (#265, serves #82).
```

Run: `Rscript -e 'rmarkdown::render("NEWS.Rmd", output_file = "NEWS.md", quiet = TRUE)'`
Expected: `NEWS.md` updated with the same bullet.

- [ ] **Step 7: Run the whole external test set and the package tests**

Run: `Rscript -e 'devtools::test(filter = "external|searchExternal")'`
Expected: all PASS, 0 failures.

Run: `Rscript -e 'devtools::test()'`
Expected: no new failures. Database tests skip without `SIGREPO_TEST_DB_USER`.

- [ ] **Step 8: Commit**

```bash
git add R/searchExternal.R tests/testthat/test_searchExternal.R NAMESPACE man/searchExternal.Rd NEWS.Rmd NEWS.md
git commit -m "Add searchExternal(): one call to query Rummagene, RummaGEO and CORESH

Closes #265. Serves #82.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 7: Opt-in live tests and a check pass

**Files:**
- Create: `tests/testthat/test_external_live.R`

**Interfaces:**
- Consumes: `externalRummagene`, `externalRummageo`, `externalCoresh` (Tasks 3-5).

- [ ] **Step 1: Write the live tests (skipped by default)**

```r
# tests/testthat/test_external_live.R
# Hits the real services. Run with SIGREPO_LIVE_EXTERNAL=true Rscript -e 'devtools::test(filter="external_live")'
skip_unless_live <- function() {
  testthat::skip_if_not(
    base::identical(base::tolower(base::Sys.getenv("SIGREPO_LIVE_EXTERNAL")), "true"),
    "SIGREPO_LIVE_EXTERNAL is not 'true'"
  )
}
ifn_query <- base::list(
  signature_name = "interferon_12", organism = "Homo sapiens", organism_code = "hsa", direction = "combined",
  genes = c("ISG15", "IFI6", "IFI27", "MX1", "OAS1", "IFIT1", "IFIT3", "RSAD2", "IFI44L", "STAT1", "IRF7", "OASL"),
  n_input = 12, n_mapped = 12, n_unmapped = 0, truncated = FALSE, unmapped = character()
)

test_that("live: Rummagene returns interferon papers", {
  skip_unless_live()
  out <- externalRummagene(ifn_query, limit = 5)
  expect_gt(nrow(out), 0)
  expect_equal(colnames(out)[1:12], EXTERNAL_COLUMNS)
  expect_true(all(grepl("^PMC", out$pmcid[!is.na(out$pmcid)])))
})

test_that("live: RummaGEO returns human GEO contrasts", {
  skip_unless_live()
  out <- externalRummageo(ifn_query, limit = 5)
  expect_gt(nrow(out), 0)
  expect_true(all(out$species == "human"))
})

test_that("live: CORESH ranks datasets within the timeout", {
  skip_unless_live()
  out <- externalCoresh(ifn_query, limit = 5, timeout = 180)
  expect_gt(nrow(out), 0)
  expect_match(attr(out, "job_url"), "^https://alserglab.wustl.edu/coresh/load/")
  expect_equal(out$score_label[1], "% variance")
})
```

- [ ] **Step 2: Run them live once**

Run: `SIGREPO_LIVE_EXTERNAL=true Rscript -e 'devtools::test(filter = "external_live")'`
Expected: 3 PASS. If a service is down, note which and move on; the offline suite is the gate.

- [ ] **Step 3: Run them without the flag**

Run: `Rscript -e 'devtools::test(filter = "external_live")'`
Expected: 3 SKIP.

- [ ] **Step 4: Package check**

Run: `Rscript -e 'devtools::check(document = FALSE, vignettes = FALSE, args = c("--no-manual", "--no-tests"))'`
Expected: 0 errors, 0 warnings. A NOTE about `no visible global function definition` for `%||%` means a file forgot to use it from `externalCommon.R`; a NOTE about undeclared `stats`/`utils` is pre-existing and acceptable.

- [ ] **Step 5: Commit and push**

```bash
git add tests/testthat/test_external_live.R
git commit -m "Add opt-in live tests for the external search adapters

Part of #265.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
git push -u origin 265-add-searchexternal-rummagene-rummageo-coresh
```

---

## Self-review notes

- Spec coverage: public interface (Task 6), translation incl. DB-then-biomaRt and 500 cap (Task 2), three adapters with common columns and attributes (Tasks 3-5), error messages (Tasks 2, 4, 5), offline + live tests (all tasks + Task 7), NEWS and rollout (Task 6/7). Out-of-scope items untouched.
- Names used across tasks: `externalPostJson`, `externalGetJson`, `externalGraphql`, `externalUrl`, `bindExternalHits`, `rbindExternalFrames`, `emptyExternalFrame`, `EXTERNAL_COLUMNS`, `EXTERNAL_SOURCES`, `buildExternalQuery`, `externalRummagene`, `externalRummageo`, `externalCoresh`, `parseRummagene`, `parseRummageo`, `parseCoreshRanking`, `coreshJobUrl`, `rummageoBackgroundId`, `biomartSymbolTable`, `lookupSymbolsInSigRepo`, `lookupSymbolsInBiomart`, `mapFeaturesToSymbols`, `organismCode`, `looksLikeGeneSymbol` — spelled identically in every task.
- Review Focus 1-5 each have a named test: Task 2 (versioned Ensembl; NA scores), Task 3 and 4 (mouse casing), Task 5 (failed job; limit > hits).
