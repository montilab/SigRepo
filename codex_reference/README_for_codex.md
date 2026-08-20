# Codex reference materials for SigRepo OmicSignature ingestion

This directory is the executable reference foundation for the SigRepo
OmicSignature ingestion Skill.

## Authority order

Use evidence in this order:

1. manuscript, supplementary methods, source tables, and verified analysis code;
2. `references/README_SigRepo_core_conventions.md`;
3. the reviewed helper and behavioral tests;
4. approved examples under `approved_examples/`;
5. visible regression scenarios under the separate
   `codex_tests/scenario1_diff_table/` directory.

Folder names, filenames, worksheet names, old drafts, and prior generated
objects must never override study evidence.

## Approved examples

The following workflows are approved implementation patterns:

- `approved_examples/LLFS_Sebastiani2024/`
- `approved_examples/EMT_Youssef2024/`
- `approved_examples/Ding2025/`
- `approved_examples/AhR_CYP1B1_MDA_SUM149/`

They demonstrate different workflow branches. They are not study-specific
answer keys for unrelated papers.

## Development examples

`development_examples/` is reserved for a study that is actively being
qualified. It currently contains no active study workflow.

A study remains in development until all biological decisions are supported,
mapping review is complete, expected counts are frozen, the Rmd renders from a
clean session, and the approval contract passes.

## Regression scenarios

The user's visible regression corpus is stored separately at:

```text
codex_tests/scenario1_diff_table/
```

These scenarios contain source papers, source tables, task prompts, and earlier
Codex drafts. They are useful for regression testing and failure-mode review,
but their draft scripts are not approved code.

Use `regression/scenario1_manifest.md` to determine the intended role of each
scenario.

## Legacy files

Files under `legacy_internal_not_gold/` are historical only. Do not copy their
thresholds, mapping behavior, metadata, object structure, or package usage.

## Package compatibility

Generated workflows must source:

```text
helpers/omicsignature_compat.R
```

Compatibility is determined by the behavioral tests under `tests/`, not by
package version alone.

AhR/CYP1B1 supplies the transcriptomic differential-table branch: four
complete limma parents, retained non-`NULL` `difexp`, `adj.P.Val <= 0.01`,
frozen reviewed Ensembl v114 mapping, deterministic `feature_n` IDs assigned
once per parent, four objects, and one collection.

## Current objective

1. render and inspect the AhR/CYP1B1 workflow from its approved location;
2. rerun the API and repository contracts after that render;
3. retain the visible scenarios as regression material, including the blocked
   Chakraborty stop-behavior case; and
4. use at least one hidden study for final generalization testing.
