---
name: sigrepo-omicsignature-ingestion
description: Create, repair, review, validate, and qualify SigRepo OmicSignature R Markdown workflows and objects from manuscripts, supplementary tables, differential-analysis outputs, selected result tables, or membership lists. Use for SigRepo identifier mapping, difexp policy, probe IDs, metadata, collections, regression review, and approved-example promotion. Do not use for general omics analysis unrelated to SigRepo ingestion.
---

# SigRepo OmicSignature ingestion

Use this skill for one focused goal: produce an evidence-grounded, reviewable
SigRepo ingestion workflow that creates valid `OmicSignature` objects and
collections without inventing biological or technical decisions.

## Required repository foundation

Work inside the SigRepo repository. Locate the repository root by finding:

```text
codex_reference/
codex_tests/
```

Before editing a candidate workflow, read:

```text
codex_reference/README_for_codex.md
codex_reference/references/README_SigRepo_core_conventions.md
codex_reference/references/PROBE_ID_POLICY.md
codex_reference/references/DIFFEXP_POLICY.md
codex_reference/references/APPROVED_EXAMPLES_MANIFEST.md
codex_reference/helpers/omicsignature_compat.R
```

Use the packaged `references/` files only as a portable summary. The live
repository files are authoritative when they are present.

Never use `legacy_internal_not_gold/` as an implementation authority.

## Evidence hierarchy

Use evidence in this order:

1. manuscript, supplementary methods, source tables, and verified analysis code;
2. explicit project context supplied by the study team;
3. SigRepo core conventions and validated helper behavior;
4. approved examples;
5. visible regression scenarios and historical drafts.

Do not let a filename, folder name, worksheet title, old draft, or generated RDS
override stronger study evidence.

## Phase 1: preflight before editing

Inspect all supplied sources and produce a concise preflight report using
`assets/preflight_report_template.md`.

The preflight must establish:

- the biological comparison for each proposed object;
- what positive and negative scores mean;
- the exact signature source and selection rule;
- whether the source is a complete differential table, a selected result, or a
  membership-only list;
- whether `difexp` is retained or `NULL`;
- organism, assay type, platform, and controlled-vocabulary sample type;
- source identifier types and target repository identifier;
- genuine assay/probe-ID availability;
- mapping strategy, release, audit requirements, and unresolved mappings;
- expected signature objects and collection structure;
- expected source, mapping, signature, and collection counts when derivable;
- every unresolved question that blocks implementation or approval.

Do not create or revise files until the preflight is complete.

## Phase 2: classify the source pattern

Read `references/workflow_decision_tree.md`.

Use one of these primary branches:

### A. Complete or broader differential-analysis table

Retain a standardized mapped parent as `difexp`.

Assign or validate probe IDs once on the parent, standardize it once, then
derive the signature by filtering the standardized parent. Never independently
standardize the child signature.

Use `assets/differential_table_template.Rmd`.

### B. Selected result table

Use `difexp = NULL` unless a broader parent analysis is actually supplied.

Do not duplicate the selected signature into `difexp`.

Use `assets/membership_selected_result_template.Rmd`.

### C. Membership-only feature list

Use `difexp = NULL`. Use a documented constant score only when required by the
object schema, and state clearly that the score encodes membership rather than
an effect size.

Use `assets/membership_selected_result_template.Rmd`.

### D. Multiple related signatures

Create one object per justified biological comparison or signature family.
Create a collection only when the objects form a coherent study-defined group.
Do not create a collection merely because one object exists.

## Phase 3: decision gates

Stop and ask for review when any of the following remains unresolved:

- comparison or coefficient direction;
- exact threshold or comparison operator;
- whether a table is complete, partially filtered, or already selected;
- organism, tissue, cell type, sample type, or platform;
- identifier namespace or mapping release;
- one-to-many mapping interpretation;
- source-specific alias or composite identifier resolution;
- whether a publication or dataset identifier exists;
- expected object or collection membership.

Report the evidence inspected and the smallest question needed to proceed.
Never guess to keep the workflow moving.

## Phase 4: implementation rules

Place active qualification work under:

```text
codex_reference/development_examples/<StudyKey>/
```

Use `scripts/create_candidate_scaffold.py` only after the preflight is accepted.
The script refuses to overwrite an existing candidate.

Every candidate Rmd must:

1. use portable paths and environment-variable input overrides;
2. source `codex_reference/helpers/omicsignature_compat.R`;
3. validate source files, sheets, columns, types, and row counts;
4. preserve source-file, source-sheet, source-row, and original-identifier
   provenance during development;
5. use exact controlled-vocabulary values through the shared helper;
6. preserve genuine assay IDs;
7. use deterministic `feature_1`, `feature_2`, ... technical IDs only when no
   genuine assay ID exists;
8. retain valid repeated `feature_name` values and justified mapping expansion;
9. use biological `group_label` values, never generic `UP` or `DOWN`;
10. construct metadata, objects, and collections through the validated package
    API;
11. validate object counts, row counts, identifiers, factor values, thresholds,
    parent-child synchronization, and collection members;
12. keep save/upload chunks disabled with `eval=FALSE`;
13. contain a development-status section until approval.

## Mapping policy

Read `references/mapping_policy.md`.

Approved renders must not depend on a mutable live mapping service.

A live service may be used only in a one-time development builder that produces:

- an explicitly `UNREVIEWED` candidate cache;
- complete mapping results;
- unresolved and one-to-many audit tables;
- source context for manual review.

After review, create a frozen final cache and explicit decision log. The
approved Rmd reads only the reviewed frozen cache.

Do not silently discard unmatched or ambiguous source identifiers. Exclude
unresolved rows from final object inputs only after documenting them.

## Probe-ID policy

Read `references/probe_id_policy.md`.

- Preserve genuine assay/probe identifiers.
- Never use a gene symbol, Entrez ID, Ensembl ID, UniProt accession, or
  `feature_name` as a fabricated assay ID.
- When no genuine ID exists, call `sigrepo_standardize_signature()` once on the
  parent `difexp` or once on the final signature when `difexp = NULL`.
- Validate the full set of IDs and probe-to-feature relationships; do not
  require standardization to preserve row order.

## Validation and approval

Read `references/validation_contract.md`.

Before approval:

1. restart R or use a clean render environment;
2. render from `development_examples/<StudyKey>`;
3. inspect all created objects and collections;
4. verify expected counts and zero unresolved mappings;
5. replace the development section with the approved-status statement;
6. archive one-time mapping builders, candidate caches, and development-only
   audit sources under `legacy_internal_not_gold/`;
7. move the complete study folder to `approved_examples/`;
8. render once more from the approved location;
9. rerun the repository and API contracts.

Do not promote a study merely because the Rmd renders.

## Required response format

For each task, report:

1. **Preflight interpretation**
2. **Supported decisions**
3. **Unresolved decisions**
4. **Files to create or modify**
5. **Exact local commands**
6. **Expected validation outputs**
7. **Approval status**

When editing a repository, show the changed-file list and summarize the diff.
Do not claim a local render passed unless it was actually run and its output was
inspected.

## Approved examples and regression scenarios

Read:

```text
references/approved_examples.md
references/regression_scenarios.md
```

Approved examples demonstrate patterns, not answers for a new paper.

Regression scenarios test generalization and stop behavior. Their old draft Rmd
files are not gold-standard code and must not be copied blindly.
