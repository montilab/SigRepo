# Scenario 1 manifest

This classification is based on the supplied scenario task files and existing
draft layout. It is not scientific approval of any draft.

| Folder | Main branch to test | Intended role | Current policy |
|---|---|---|---|
| `1_selman2009_s6k1` | Separate up/down array lists across three mouse tissues | Multi-object regression | Preserve as visible regression; old draft is not gold |
| `2_chakraborty2026_sp2509` | DESeq2-style transcriptomic differential-analysis tables | Stop-behavior regression | Final selection rule, exact contrast direction, and dose remain unsupported |
| `3_Liang2016_EMT_MultiSystem` | Multi-system EMT selected results with supplied Ensembl IDs | Identifier-validation regression | Compare with approved EMT rules; do not copy draft blindly |
| `6_Kanfi2012_SIRT6_KO_vs_WT` | Selected mouse array results and direction interpretation | Direction/threshold regression | Requires evidence review before execution |
| `7_MontiLab2023_HNSCC_Hs_OralMucosa_PML` | Oral-lesion transcriptomic comparisons | Stop-condition and contrast regression | Do not guess unresolved comparison direction |
| `8_AhR_CYP1B1_MDA_Sum149` | Four complete within-cell-line differential tables | Visible source regression retained | Original scenario remains visible; approved implementation is under `approved_examples/AhR_CYP1B1_MDA_SUM149` |
| `9_GHRH_KO_Sun2013` | Mouse liver selected or partially filtered results | Source-classification regression | Requires source-sheet review |
| `Hofmann_MycWT` | Multi-tissue array genotype signatures | Genuine-probe and collection regression | Preserve source probe IDs; draft is not gold |
| `Sebastiani2021_Centenarian_Proteomics` | Multi-table proteomic signatures | Proteomics mapping regression | Compare with Ding/LLFS patterns; no automatic promotion |

## Qualification order

1. Render and inspect AhR/CYP1B1 from its approved location.
2. Run API and repository contracts after that render.
3. Keep Chakraborty as a successful stop-behavior case because its final
   selection rule, exact contrast direction, and dose remain unsupported.
4. Use MontiLab and other ambiguous cases to test whether the Skill stops
   instead of guessing.
5. Keep at least one new paper outside all reference and regression materials
   for hidden evaluation.
