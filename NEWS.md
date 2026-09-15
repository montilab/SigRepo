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
  `SigRepo Genesets Dropped`, `SigRepo Genesets Dropped List`.
- `method` is now `test = c("hypergeometric", "kstest")`. The `"hypergeo"`,
  `"ks"` and `"gsea"` aliases are gone; use `test = "kstest"` for a ranked test.
  `power` changes only the kstest `score`: `1`, the default, weights hits by
  |score|; `0` runs hypeR's ranked (unweighted) signature, so its `score` and
  `Signature Type = "ranked"` match hypeR's documentation. The p-value and FDR
  come from hypeR's unweighted, one-sided KS test and do not depend on `power`
  or `absolute`; it finds genesets enriched toward the top of the ranking, and
  `direction` chooses which end that is.
- Defaults match `hypeR::hypeR()`: `fdr = 1` (was 0.05), `pval = 1` (new),
  `background = 23467`. `background = "difexp"` uses each signature's measured genes.
  With a gene-vector or `"difexp"` background, a hypergeometric query is first
  reduced to the background genes (hypeR reduces only the genesets), with a
  warning, so a gene-vector background can give different p-values than a
  plain `hypeR::hypeR()` call on the unreduced query.
- `genesets` is required: `genesets = "msigdb"` with `msigdb_collection`
  (and optional `msigdb_species`, `msigdb_subcollection`), or your own named
  list / `hypeR::gsets` / `hypeR::rgsets`. `msigdb_*` without `"msigdb"` is an error.
- `split_by_group` is now `split` and defaults to `TRUE` (one hypergeometric
  vector per `group_label`). `split_by_direction`, `feature_col` and `...` are removed.
- `prepareHypeRSignatures()` returns `list(signatures, info, skipped)`.
- Signatures that cannot produce a query are skipped with a warning instead of
  aborting the run.

## New

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
