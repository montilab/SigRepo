# Workflow decision tree

## 1. Is the source a broader differential-analysis table?

Examples:

- DESeq2 output with effect size, p-value, and adjusted p-value;
- limma output with coefficient, p-value, and adjusted p-value;
- a partially filtered differential table that still contains rows outside the
  final signature rule.

**Yes:** retain the mapped standardized parent as `difexp`.

**No:** continue.

## 2. Is the source already the selected final result?

Examples:

- a paper supplement labeled significant genes;
- a tissue-specific selected protein list;
- a table that contains only rows meeting the study criterion.

**Yes:** use `difexp = NULL` unless a separate broader parent is supplied.

**No:** continue.

## 3. Is the source a membership-only list?

Examples:

- curated epithelial and mesenchymal components;
- a signature gene set without signed statistics;
- an ordered or unordered feature list.

**Yes:** use `difexp = NULL`. Use a documented membership score only when the
schema requires one.

## 4. Are score direction and group labels supported?

Use the actual model coefficient or source statistic. Establish which condition
is the numerator/positive coefficient and which is the reference.

- Positive score: biological condition explicitly supported by the contrast.
- Negative score: the other biological condition.
- Never label rows merely `UP` or `DOWN`.

If direction is not supported, stop.

## 5. Does the source contain a genuine assay ID?

**Yes:** preserve it as character `probe_id`.

**No:** assign deterministic `feature_n` IDs through the shared helper once.

## 6. Does mapping expand one source row?

Retain justified one-to-many mappings when the source identifier genuinely maps
to several target features and repository policy permits expansion.

Do not expand when the source already identifies a specific measured accession
or when the apparent multiplicity is an alias artifact.

Document all source-specific decisions.

## 7. Is a collection justified?

Create a collection when several objects represent related tissues, contrasts,
components, or signature families from the same study.

A single-member collection is not required unless the repository or study
design explicitly needs it.
