# SigRepo (development version)

## Breaking changes

`runHypeR()` and `prepareHypeRSignatures()` now follow hypeR's own interface.

- `runHypeR()` returns hypeR's `hyp` or `multihyp` instead of
  `list(result, signatures, metadata)`, and hypeR's functions (`hyp_dots()`,
  `hyp_emap()`, `hyp_to_rmd()`, ...) work on it directly. The type follows the
  input, as in hypeR: a `hyp` for a single signature (one `OmicSignature`, id
  or name) with `split = FALSE` or a one-direction kstest, otherwise a
  `multihyp`, even when only one query is left. `hyp_show()` takes a single
  `hyp`, e.g. `hyp_show(res$data[[1]])`.
- Each `hyp$info` ends with SigRepo provenance in a fixed order:
  `SigRepo Signature ID`, `SigRepo Signature Name`, `Group Label`,
  `Symbol Source`, `SigRepo Direction`, `SigRepo Ranked Table`,
  `SigRepo Score Column`, `SigRepo Split`, `SigRepo Background Source`,
  `SigRepo Features Unmapped`, `SigRepo Query Genes Removed`,
  `SigRepo Genesets Dropped`, `SigRepo Genesets Dropped List`, `SigRepo FDR Scope`.
- `method` is now `test = c("hypergeometric", "kstest")`. The `"hypergeo"`,
  `"ks"` and `"gsea"` aliases are gone; use `test = "kstest"` for a ranked test.
  `power` changes only the kstest `score`: `1`, the default, weights hits by
  |score|; `0` runs hypeR's ranked (unweighted) signature, so its `score` and
  `Signature Type = "ranked"` match hypeR's documentation. The p-value and FDR
  come from hypeR's unweighted, one-sided KS test and do not depend on `power`
  or `absolute`; it finds genesets enriched toward the top of the ranking, and
  `direction` chooses which end that is.
- `fdr = 1` (was 0.05) and `pval = 1` (new) match `hypeR::hypeR()`.
- Hypergeometric runs follow the BS831 `hyperEnrichment()` conventions
  (montilab.github.io/BS831), which differ from plain hypeR:
  - `background = NULL` (the default) uses each signature's measured genes from
    its difexp, "the number of annotated genes in the dataset". It uses 23467
    instead when there is no difexp (silently) or when the difexp looks filtered
    (with a warning): a transcriptomics difexp under 10,000 rows, a difexp no
    more than twice its signature, or every `p_value`/`pvalue`/`adj_p` at most
    0.05. kstest keeps 23467, since the ranked list is its universe. Pass
    `background = 23467` for hypeR's behaviour or `"difexp"` to force the difexp.
  - `min_query_genes = 4` skips, with a warning, a query with fewer than 4 genes
    found in the genesets (`min.drawsize`).
  - `fdr_scope = "run"` adjusts p-values across every query and geneset in the
    call (`mht = TRUE`); `fdr_scope = "query"` adjusts within each query, as
    hypeR does. `pval`/`fdr` filter on the chosen FDR.
  With a gene-vector or `"difexp"` background, a hypergeometric query is first
  reduced to the background genes (hypeR reduces only the genesets), with a
  warning, so a gene-vector background can give different p-values than a
  plain `hypeR::hypeR()` call on the unreduced query.
- `genesets` is required: `genesets = "msigdb"` with `msigdb_collection`
  (and optional `msigdb_species`, `msigdb_subcollection`), or your own named
  list / `hypeR::gsets` / `hypeR::rgsets`. `msigdb_*` without `"msigdb"` is an error.
- `split_by_group` is now `split` and defaults to `TRUE` (one hypergeometric
  vector per `group_label`). Categorical signatures with scores are split by
  category and score sign (`"<label> | <group> | up"`/`"| down"`), and kstest
  ranks each category's own difexp rows; a single categorical signature
  therefore returns a `multihyp` for kstest.
- kstest skips a ranking whose scores are all equal (`constant_score`) or all
  one sign (`unsigned_score`), per category for categorical signatures: such a
  ranking cannot say which class a geneset is enriched in. `split_by_direction`, `feature_col` and `...` are removed.
- `prepareHypeRSignatures()` returns `list(signatures, info, skipped)`.
- Signatures that cannot produce a query are skipped with a warning instead of
  aborting the run.

## New

- Plotting for `runHypeR()` results, drawn from the result objects:
  `plotHypeRDots()` (one column per query with the same top genesets for every
  query, a readable -log10 significance scale, one-query results supported, and
  `color_by = "score"` for NES/score), `plotHypeREnrichment()` (running
  enrichment score with the leading edge for kstest and fgsea, Venn diagram for
  hypergeometric, for any geneset without `plotting = TRUE`) and
  `plotHypeRMap()` (hypeR's enrichment/hierarchy maps, returning `NULL` with a
  warning where hypeR would error). `hypeRDotData()` and
  `hypeREnrichmentData()` return the underlying data. `ggplot2` is in Suggests.

- `runHypeR(test = "fgsea")` runs GSEA with `fgsea::fgseaMultilevel()`, ported
  from hypeR's fgsea vignette wrapper. Unlike hypeR's kstest, whose p-values
  ignore the score weights and test only the top of the ranking, fgsea gives
  weighted permutation p-values, NES and the leading edge for both tails in one
  run. Pathways are split by the sign of their enrichment score into
  `"<query> | up"` and `"<query> | down"` hyps (`direction`, default `"both"`
  for fgsea). `seed = 1` makes the p-values reproducible without touching the
  session's random-number state; `fgsea_args` passes fgsea options such as
  `minSize`/`maxSize`. The results work with `hyp_dots()`, `hyp_emap()`,
  `rctbl_build()`, `hyp_to_rmd()` and `hypeRToExcel()`.

- `getHypeRGenesets()` returns a named, versioned `hypeR::gsets` for MSigDB.
  Mouse requests for human collections (e.g. C2, C5) now use ortholog mapping.
- `hypeRToExcel()` writes a `hyp`/`multihyp` with Excel-safe, unique sheet
  names (hypeR's `hyp_to_excel()` fails on names over 31 characters, which
  most SigRepo signature names are) and an `index` sheet mapping each sheet to
  its query and signature.
- kstest `direction = c("up", "down", "both")`: `"down"` ranks by the negated
  score so genesets at the low end can reach significance; `"both"` returns
  `"<label> | up"` and `"<label> | down"` queries.
- kstest `ks_source = c("difexp", "signature")`: `"signature"` ranks the
  signature table, for signatures stored without a difexp.
- `query_names`: a function of the query info table that returns the query
  names, e.g. `function(info) paste(info$signature_id, info$group_label, sep = "_")`.
- `prepareHypeRSignatures()$info` gains a `direction` column.
- `signature =` takes hypeR-native input (a character vector, a named numeric
  vector, or a named list of those) instead of SigRepo signatures, and still
  gets backgrounds, the kstest guards, provenance and the return-type rule.
- `getHypeRDifexp()` returns each signature's difexp with `resolved_symbol`,
  so you can apply your own cutoffs and pass the genes to `signature =`.
- `background` can be a named list giving each signature its own background
  (number, gene vector or `"difexp"`), keyed by signature label or ID.
- `hypeR (>= 2.0.0)` in Suggests.

## Fixes

- Gene symbols resolve from the table's symbol column, then (hypergeometric)
  difexp symbols joined on `probe_id`, then the reference table by
  `feature_name` and organism. `probe_id` is no longer read as an identifier,
  and one signature's symbol column no longer leaks into the next.
- The `probe_id` join to difexp symbols matches on (`probe_id`, `feature_name`),
  so a `probe_id` repeated in difexp no longer hands a row another feature's
  symbol. Without `feature_name` it joins on `probe_id` only when that is unique.
- Query names are always unique (`"X | Up"`, `"X | Up (2)"`).
- The kstest ranked list keeps the positive score when a gene's scores tie on
  `|score|` with opposite signs, so it no longer depends on difexp row order.
- `runHypeR()` errors on an invalid `background` (e.g. `"Difexp"`); both
  functions error on a `split` that is not `TRUE`/`FALSE` and on
  `omic_signature` supplied together with `signature_id`/`signature_name`.
- kstest drops, with a warning, genesets hypeR cannot score: those whose hits
  all have score 0 (`power != 0`) and those containing every query gene (any
  power). hypeR would otherwise error or put scores on the wrong genesets.
  Dropped genesets are also left out of the FDR adjustment, and are listed in
  each `hyp$info`.
- With `plotting = FALSE`, the empty placeholder plots hypeR stores for every
  geneset are removed. They were most of the object's size (about 46 MB for a
  Hallmark `hyp`).
- A signature's gene symbols are resolved once per call, so a kstest with
  `background = "difexp"` no longer looks up the reference table twice.
- With `background = "difexp"`, a hypergeometric query is reduced to the genes
  its difexp measured, with a warning; hypeR reduces only the genesets.
- A query left with no genesets or no genes is skipped with a warning instead of
  aborting the whole run.

# SigRepo 1.0.0

- Documentation: <https://montilab.github.io/SigRepo/>
- GitHub: <https://github.com/montilab/SigRepo/>
