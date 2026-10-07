# searchExternal(): query Rummagene, RummaGEO and CORESH from a SigRepo signature

Date: 2026-10-07
Serves idea montilab/SigRepo#82 (Connect to CORESH).

## Goal

A SigRepo user names a signature and asks "what in the public world looks like
this?" One exported function answers from three public engines that each index
GEO or the literature in a different way:

| Source | What it indexes | Statistic | Species | Protocol |
|---|---|---|---|---|
| Rummagene | ~1M gene sets mined from PMC supplementary tables | Fisher overlap | any (symbol keyed) | GraphQL, `https://rummagene.com/graphql` |
| RummaGEO | ~380k up/down gene sets auto-computed from GEO RNA-seq (ARCHS4) | Fisher overlap | human, mouse (separate backgrounds) | GraphQL, `https://rummageo.com/graphql` |
| CORESH | ~40k human + ~40k mouse GEO expression matrices (array + RNA-seq) | % variance explained by the coregulated query (optional permutation p) | human, mouse | JSON, `https://alserglab.wustl.edu/coresh-back/` submit / poll / fetch |

The user calls exactly one function. Everything else in this spec is internal.

## Public interface

```r
searchExternal(
  conn_handler     = NULL,
  signature_id     = NULL,
  signature_name   = NULL,
  omic_signature   = NULL,
  source           = c("rummagene", "rummageo", "coresh"),
  direction        = c("combined", "up", "down", "both"),
  limit            = 25,
  calculate_pvalues = FALSE,
  timeout          = 120,
  verbose          = TRUE
)
```

- Signature resolution is the same as `runHypeR()`: pass an `OmicSignature`, or a
  `conn_handler` plus exactly one of `signature_id` / `signature_name`. Reuses the
  internal `resolveComparisonSignature()`.
- `source` accepts one or more of the three names. Default is all three.
- `direction` picks which genes are sent. `combined` sends every feature.
  `up` / `down` send the features with positive / negative `score`. `both` runs
  the up set and the down set as two separate queries. Signatures without a
  `score` column (uni-directional without scores) only support `combined`; any
  other value errors with a clear message.
- `limit` caps hits per source (per direction when `direction = "both"`).
- `calculate_pvalues` is CORESH only (slower, ~1 min). Ignored by the others.
- `timeout` is the CORESH polling budget in seconds.

### Return value

- One source: a data frame.
- Several sources: a named list of data frames, names = source.
- `direction = "both"`: each data frame carries a `direction` column with
  `"up"` / `"down"`, rows stacked.

Every data frame has these columns first, in this order:

| column | meaning |
|---|---|
| `source` | `"rummagene"`, `"rummageo"`, `"coresh"` |
| `direction` | `"combined"`, `"up"`, `"down"` |
| `rank` | 1-based within source and direction |
| `id` | service-native id: Rummagene term, RummaGEO term, CORESH GSE id |
| `title` | human-readable: PMC paper title, RummaGEO term (GSE + contrast), GEO series title |
| `url` | link out: PMC article, GEO accession page, GEO accession page |
| `score` | the service's primary ranking number |
| `score_label` | `"odds ratio"`, `"odds ratio"`, `"% variance"` |
| `pvalue` | raw p (NA for CORESH without `calculate_pvalues`) |
| `adj_pvalue` | adjusted p (NA when not provided) |
| `n_overlap` | overlapping genes (NA for CORESH, which reports `size` = query genes present) |
| `n_set` | size of the matched set (gene set size, or CORESH `size`) |

Service-specific extras follow: Rummagene `pmcid, year, doi, description`;
RummaGEO `gse, species, contrast`; CORESH `gpl, summary, positive_words,
negative_words, log10_padj`.

Attributes on each data frame:

- `query`: the translated query actually sent (`list(organism, organism_code,
  genes, n_input, n_mapped, n_unmapped, truncated)`).
- `total_count`: the service's total hit count where it reports one.
- `job_url` (CORESH only): `https://alserglab.wustl.edu/coresh/load/<ID>`, the
  shareable results page.

## Internal design

### 1. Translation (`R/externalQuery.R`, not exported)

`buildExternalQuery(omic_signature, conn_handler, direction, max_genes = 500, verbose)`

1. Take `omic_signature$signature` (`probe_id, feature_name, score?, group_label?`).
2. Split by `direction` using `sign(score)`; `combined` keeps all rows.
3. Map `feature_name` to a gene symbol:
   - If the name already looks like a symbol (no `ENS[A-Z]*G\d+`, no bare
     integer), keep it.
   - Else, when `conn_handler` is given, look up `gene_symbol` in the
     transcriptomics reference table via `searchTranscriptomicsFeatureSet()`.
   - Anything still unmapped goes to `biomaRt` keyed on the organism
     (`symbolAttributeForOrganism()` already exists in `featureUpdateHelpers.R`).
   - Failures of either lookup are caught and reported; they never abort the call.
4. Drop blanks and duplicates. Count `n_unmapped`.
5. If more than `max_genes` remain, keep the top by `abs(score)` (or the first
   `max_genes` when no score) and set `truncated = TRUE`. CORESH refuses >500;
   the same cap is applied to all sources for consistency.
6. Organism: `"Homo sapiens"` → `"hsa"`, `"Mus musculus"` → `"mmu"`, matched
   case-insensitively and tolerant of `"Homo Sapiens"`. Anything else →
   `organism_code = NA`.

Result: `list(organism, organism_code, genes, n_input, n_mapped, n_unmapped,
truncated, signature_name)`. Fewer than 2 mapped genes is an error.

### 2. Adapters (one file each, not exported)

Each adapter takes a translated query and `limit`, performs its HTTP calls with
`httr`, parses with `jsonlite`, and returns the common data frame. Parsing is
split from fetching so it can be tested offline:

- `R/externalRummagene.R`: `rummageneEnrich(query, limit)` + `parseRummagene(payload)`.
  Same GraphQL document SigRepo_Server uses (`currentBackground.enrich`).
- `R/externalRummageo.R`: `rummageoEnrich(query, limit)` + `parseRummageo(payload)`.
  Picks the background by species: query `backgrounds { nodes { id species } }`
  once per call, choose the one matching `hsa`→`human`, `mmu`→`mouse`, then
  `background(id:) { enrich(...) }`. Errors when `organism_code` is NA.
- `R/externalCoresh.R`: `coreshSearch(query, limit, calculate_pvalues, timeout)`
  + `parseCoreshRanking(payload)`:
  1. `POST submit-genes` `{organism, dbType, genes, calculatePvalues}` → `{ID}`.
  2. Poll `GET check-job` every 2 s until `progress == 100` or `failed == "True"`
     or `timeout`.
  3. Poll `GET check-result-files` until `files_ready`.
  4. `GET get-ranking-result` (5000 rows) → head(limit).
  Errors when `organism_code` is NA. `log10Padj` of `"NA"` becomes `NA_real_`.

Endpoints are read from options with defaults, so tests and a future dev
instance can redirect them: `getOption("sigrepo.rummagene_url", ...)`, etc.

### 3. Dispatcher (`R/searchExternal.R`, exported)

Validate args → resolve signature → for each direction build the query → for
each source call the adapter inside `tryCatch` → when several sources are
requested, a failing source yields a zero-row data frame with the common columns
and a warning naming the source and the error, so one outage does not lose the
others' results.

## Error messages (user-facing)

- No symbols could be mapped: "Could not map any of the N features of
  '<name>' to gene symbols (organism '<org>'). ..."
- Non human/mouse with rummageo or coresh: "<source> only supports Homo sapiens
  and Mus musculus; '<name>' is '<org>'."
- `direction != "combined"` without a score column: "Direction splitting needs
  a 'score' column; '<name>' has none."
- CORESH timeout: "CORESH job <ID> did not finish within <timeout> s. Results
  will appear at <job_url> when done."

## Testing

- `tests/testthat/test_externalQuery.R`: translation on the bundled
  `LLFS_Aging_Gene_2023` with a stubbed symbol lookup (no network); direction
  splitting; truncation; organism mapping; uni-directional guard.
- `tests/testthat/test_external_parsers.R`: each `parse*()` against saved JSON in
  `tests/testthat/test_data/external/` (captured 2026-10-07 from the live
  services with a 12-gene interferon query).
- `tests/testthat/test_searchExternal.R`: dispatcher with adapters stubbed via
  `testthat::local_mocked_bindings()`: single source → data frame; several →
  named list; one adapter erroring → warning plus empty frame; `both` → stacked
  with `direction` column.
- Live tests, skipped unless `SIGREPO_LIVE_EXTERNAL=true`: one small query per
  service, asserting shape and at least one row.

## Out of scope

- Server API route, MCP tool, React panel (a later issue in SigRepo_Server can
  wrap the same adapters).
- Caching of results.
- Non-transcriptomics assay types: proteomics signatures are refused with a
  clear message in this version.

## Rollout

Branch from the tracking issue off `dev`. PR into `dev` with `Closes #<issue>`
and a NEWS entry. Idea #82 is closed by hand once the PR merges, per
CONTRIBUTING.
