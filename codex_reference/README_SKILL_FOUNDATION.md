# SigRepo OmicSignature Skill — reconciled foundation

## Purpose

This directory is the validated technical foundation for the final reusable
Skill. It separates four concerns:

- study interpretation and repository conventions;
- package-specific compatibility behavior;
- approved executable examples; and
- visible regression scenarios.

## Current approved examples

| Example | Main workflow branch |
|---|---|
| LLFS/Sebastiani 2024 | Metabolomics, genuine source IDs, broader `difexp` |
| EMT/Youssef 2024 | Membership-only bulk RNA-seq, `difexp = NULL`, generated `feature_n` IDs |
| Ding 2025 | Proteomics, frozen reviewed mappings, multiple tissue collections |
| AhR/CYP1B1 MontiLab 2016 | Four complete limma parents, non-`NULL` `difexp`, Ensembl v114 mapping, four objects and one collection |

All four have passed local clean-session development rendering and
study-specific validation. AhR/CYP1B1 still requires its approved-location
render and subsequent contract run.

## Repository-generated probe IDs

When no genuine assay or probe identifier exists, the repository helper
assigns deterministic technical IDs:

```text
feature_1
feature_2
feature_3
...
```

These are generated once before package standardization. They are technical row
identifiers, not biological features and not claims about the source assay.

When a broader `difexp` table exists, IDs are assigned to that parent table
once, and the signature is filtered from the standardized parent so the same
IDs are retained.

## Visible regression corpus

The separate directory:

```text
codex_tests/scenario1_diff_table/
```

contains earlier Codex qualification scenarios. These are not gold-standard
implementations. They are retained to test whether the final Skill can:

- reproduce supported decisions;
- detect ambiguous comparisons;
- handle different identifier and table patterns; and
- stop instead of guessing.

## Remaining qualification work

Before packaging the final Skill:

1. render and inspect AhR/CYP1B1 from `approved_examples`;
2. rerun the API contract v4 and repository contract afterward;
3. retain the visible scenarios as regression cases, including Chakraborty as
   a stop-behavior case; and
4. use at least one new hidden study for final generalization testing.

## Final Skill contents

The final Skill will contain concise instructions, decision rules, templates,
and validators. It will reference this repository but will not duplicate all
large HTML, RDS, PDF, and workbook files.
