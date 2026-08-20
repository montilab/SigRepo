# AhR/CYP1B1 MDA-MB-231 and SUM-149PT approved reference

This approved reference implements four complete limma differential-table
comparisons from an unpublished MontiLab Affymetrix Human Gene 2.0 ST project:

1. AHR knockdown versus CN in MDA-MB-231 cells.
2. CYP1B1 knockdown versus CN in MDA-MB-231 cells.
3. AHR knockdown versus CN in SUM-149PT cells.
4. CYP1B1 knockdown versus CN in SUM-149PT cells.

`limma.logFC` is the score. Positive values mean higher expression in the
knockdown condition; negative values mean higher expression in CN. The exact
signature rule is `adj.P.Val <= 0.01`, with no fold-change cutoff.

Each object retains a non-`NULL`, complete mapped parent `difexp`. The mapping
to unversioned human Ensembl gene IDs is reviewed and frozen at Ensembl release
114. Because the sources contain no genuine assay/probe IDs, deterministic
`feature_1`, `feature_2`, ... IDs are assigned once per standardized parent and
the child signature is filtered from that parent.

Platform: `transcriptomics by array`. Controlled sample types:
`MDA-MB-231 cell` and `SUM-149PT cell`.

## Objects and collection

- `BreastCancer_Hs_MDA_MB_231_AHR_KD_vs_CN_MontiLab2016`
- `BreastCancer_Hs_MDA_MB_231_CYP1B1_KD_vs_CN_MontiLab2016`
- `BreastCancer_Hs_SUM149PT_AHR_KD_vs_CN_MontiLab2016`
- `BreastCancer_Hs_SUM149PT_CYP1B1_KD_vs_CN_MontiLab2016`

Collection:
`BreastCancer_Hs_CellLine_AhR_CYP1B1_KD_MontiLab2016`.

## Frozen counts

| Key | Parent rows | Signature rows |
|---|---:|---:|
| `mda_ahr` | 22,841 | 217 |
| `mda_cyp1b1` | 22,841 | 6 |
| `sum149_ahr` | 22,841 | 2,552 |
| `sum149_cyp1b1` | 22,841 | 1,074 |

Mapping review resolved all labels: 147 source labels were explicitly excluded,
4 labels were valid one-to-many mappings, and 0 labels remain unresolved.

The clean development render and four-object validation passed. Save/upload
behavior remains disabled by default. Approved-location render and repository
contracts are the remaining post-promotion checks.

Exact approved render command to run later from the repository root:

```powershell
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' 'codex_reference\legacy_internal_not_gold\AhR_CYP1B1_MDA_SUM149_qualification\approved_location_qualification\render_and_validate_AhR_CYP1B1_approved.R'
```

After that render passes inspection, run the contracts with:

```text
codex_reference\tests\run_all_tests_windows_autodetect.cmd
```
