# Visible regression scenarios

The visible corpus is expected at:

```text
codex_tests/scenario1_diff_table/
```

It is a regression set, not a directory of approved implementations.

## Current roles

| Scenario | Primary regression purpose |
|---|---|
| Selman S6K1 | selected up/down lists across tissues; multiple objects |
| Chakraborty SP2509 | Stop behavior: final selection rule, exact contrast direction, and dose are unsupported |
| Liang EMT | multi-system selected signatures and identifier validation |
| Kanfi SIRT6 | direction and threshold evidence review |
| MontiLab HNSCC oral mucosa | unresolved-comparison stop behavior |
| AhR/CYP1B1 | Visible regression source retained; approved implementation lives under `approved_examples/AhR_CYP1B1_MDA_SUM149` |
| GHRH KO | source-table classification |
| Hofmann Myc | genuine array-probe and multi-tissue collection behavior |
| Sebastiani proteomics | proteomic source classification and mapping |

## Why these are not automatically approved

Older drafts predate the final helper, frozen-mapping policy, probe-ID rules,
and approval contract.

Two cases remain especially useful regression evidence:

- Chakraborty is a published DESeq2-style case, but the exact repository
  signature selection rule, exact contrast direction, and dose remain
  unsupported; it is a successful stop-behavior case.
- The original AhR/CYP1B1 scenario remains visible regression material, while
  its reviewed complete-limma implementation is an approved example.

The Skill uses four approved examples plus the API contract, templates, and
visible regression cases. At least one hidden study remains required for final
generalization testing.
