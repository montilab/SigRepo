# SigRepo (development version)

## Breaking changes

`runHypeR()` and `prepareHypeRSignatures()` now follow hypeR's own interface.

- `runHypeR()` returns hypeR's `hyp` (one query vector) or `multihyp` (several)
  instead of `list(result, signatures, metadata)`. Use `hypeR::hyp_dots()`,
  `hyp_to_excel()` etc. on it directly. Each `hyp$info` now ends with
  `SigRepo Signature ID`, `SigRepo Signature Name`, `Group Label`, `Symbol Source`.
- `method` is now `test = c("hypergeometric", "kstest")`. The `"hypergeo"`,
  `"ks"` and `"gsea"` aliases are gone: GSEA-style weighting is
  `test = "kstest"` with `power = 1` (the default); `power = 0` is the classic KS test.
- Defaults match `hypeR::hypeR()`: `fdr = 1` (was 0.05), `pval = 1` (new),
  `background = 23467`. `background = "difexp"` uses each signature's measured genes.
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

## Fixes

- Gene symbols resolve from the table's symbol column, then (hypergeometric)
  difexp symbols joined on `probe_id`, then the reference table by
  `feature_name` and organism. `probe_id` is no longer read as an identifier,
  and one signature's symbol column no longer leaks into the next.

# SigRepo 1.0.0

- Documentation: <https://montilab.github.io/SigRepo/>
- GitHub: <https://github.com/montilab/SigRepo/>
