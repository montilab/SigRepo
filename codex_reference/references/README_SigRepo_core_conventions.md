# SigRepo OmicSignature Core Conventions

Use these conventions for all Codex-generated SigRepo ingestion workflows.

The manuscript, supplementary methods, source tables, and analysis code are authoritative. Folder names, filenames, worksheet names, prior scripts, and reference examples may be incomplete or inaccurate and must not override the study evidence.

## 1. Preflight interpretation

Before writing an Rmd, provide a concise preflight summary that identifies:

- the biologically justified signature or signatures;
- the source table, worksheet, or analysis used for each signature;
- the biological comparison and any relevant subgroup restrictions;
- whether the source is:
  - a complete differential-analysis table;
  - a partially filtered differential-analysis table;
  - an already selected or curated signature;
  - a membership-only gene set;
  - a validation or replication analysis;
  - an exploratory analysis; or
  - supporting annotation only;
- the score definition, scale, and direction;
- the study-supported thresholds and exact comparison operators;
- the available identifier columns and mapping strategy;
- whether `difexp` should be populated or set to `NULL`;
- whether an `OmicSignatureCollection` is justified; and
- any unresolved ambiguity requiring review.

Do not create an OmicSignature from every supplementary worksheet.

Validation, replication, pathway, enrichment, exploratory, and supporting tables should not automatically become separate OmicSignature objects. Use the manuscript’s biological conclusions, named signatures, and primary analyses to determine which objects are justified.

## 2. Study-specific thresholds

- Use only thresholds supported by the manuscript, supplementary methods, source-table notes, or verified analysis code.
- Do not copy thresholds from another study or reference example.
- Do not add fold-change, raw-p-value, adjusted-p-value, q-value, or FDR cutoffs without study-specific support.
- Distinguish raw `p_value` from adjusted `p_value`, q-value, or FDR; do not substitute one for another.
- Preserve the study-reported comparison operator exactly:
  - use `<=` when the study specifies `≤`;
  - use `<` when the study specifies `<`;
  - use `>=` when the study specifies `≥`;
  - use `>` when the study specifies `>`.
- Metadata cutoff fields must exactly match:
  - the study criteria;
  - the scale of the score; and
  - the filters implemented in the workflow.
- Apply every study-defined signature criterion.
- Validate that the final signature rows satisfy all implemented criteria.
- Do not replace a reported threshold with a stricter or more permissive threshold without explicit justification.
- When the correct threshold is genuinely ambiguous, report the ambiguity in the preflight summary rather than guessing.

### Already filtered and partially filtered source tables

- Do not assume that a source table is the final signature merely because it has already been filtered by one criterion.
- Inspect whether the table contains more rows than the final study-defined signature.
- If additional study-defined criteria can still be applied, retain the broader mapped table as `difexp` and derive `signature` by applying all remaining criteria.
- If the source rows already represent the final signature membership, record and validate the reported selection criteria but do not re-filter unless the source contains rows outside those criteria.
- Document any prefiltering already applied to the source table or `difexp`.

## 3. Signature membership and `difexp`

- Use `difexp` when a sufficiently complete or partially filtered differential-analysis table is available and supports construction of the final signature.
- Set `difexp = NULL` when the source contains only:
  - an already selected final signature;
  - a curated list;
  - a membership-only gene set; or
  - another result that does not represent a broader differential-analysis table.
- If the source table contains statistics that allow the final signature criteria to be applied, retain the mapped source table as `difexp` and construct `signature` using all study-defined thresholds.
- A curated table may define signature membership without containing the appropriate score.
- When membership and score come from different tables:
  - use the curated table to define members;
  - join to the relevant parent differential-analysis table for score, direction, probe ID, and statistics; and
  - document the membership source and score source separately.
- Do not populate `difexp` with the same final selected rows merely to avoid `NULL` when no broader analysis table is available.

## 4. Score direction and fold-change handling

First determine whether the reported effect is:

- an already signed log2 fold change;
- an unsigned positive linear fold-change ratio;
- an inverse fold-change ratio;
- a standardized effect;
- a test statistic; or
- another explicitly defined score.

### Signed log2 fold changes

- If the source provides a signed `log2FoldChange`, retain it without transformation.
- Do not log-transform values that are already log fold changes.
- Verify the contrast numerator, denominator, reference level, and coefficient direction before interpreting the sign.
- Do not infer direction solely from worksheet names such as `A.vs.B`.

For DESeq2 or similar outputs, determine which condition is represented by a positive `log2FoldChange` using, in order of preference:

1. the analysis code or contrast definition;
2. the design and reference levels;
3. workbook notes;
4. supplementary methods; or
5. the manuscript.

If contrast direction cannot be verified, report it as unresolved and do not silently assign `group_label`.

### Positive linear fold-change ratios

Linear fold-change ratios are positive quantities and are not inherently signed.

If the source reports:

```r
treatment / control
```

and positive scores should indicate higher abundance in treatment, use:

```r
score <- log2(treatment_over_control)
```

If the source reports:

```r
control / treatment
```

but positive scores should indicate higher abundance in treatment, use:

```r
score <- -log2(control_over_treatment)
```

### Separate upregulated and downregulated blocks

When separate upregulated and downregulated blocks both contain positive linear ratios, derive the score sign from the biological direction.

If the downregulated block reports an inverse fold change, use:

```r
score_up   <-  log2(FC)
score_down <- -log2(inverse_FC)
```

### Score-scale consistency

- Express metadata cutoffs on the same scale as `score`.
- Examples:
  - a linear 1.5-fold threshold with log2 scores corresponds to `log2(1.5)`;
  - a reported threshold of `|log2FC| >= 1.5` remains `1.5`.
- Confirm that score sign, `group_label`, factor levels, phenotype, description, and signature name are mutually consistent.

### Contrast-direction evidence requirement

- For every differential comparison, record the evidence used to determine which condition corresponds to positive and negative scores.
- Acceptable evidence includes:
  - the explicit DESeq2 contrast or coefficient;
  - documented reference levels;
  - analysis code;
  - a source-table note defining the fold-change direction; or
  - an explicit manuscript statement.
- Worksheet names such as `A.vs.B` are not sufficient evidence because naming conventions vary.
- The numerical values in a differential-expression table alone do not establish the contrast direction.
- If the supplied materials do not explicitly establish direction:
  - mark the contrast direction as unresolved;
  - do not assign biological `group_label` values;
  - do not construct or save the affected OmicSignature object; and
  - report exactly what additional information is required.

### Evidence labels versus repository labels

- Record contrast evidence using the exact condition labels and syntax present in the authoritative source, such as the original `cond`, `control`, contrast, coefficient, or reference-level values.
- Do not rewrite source evidence using normalized repository labels.
- Standardized repository labels may be used in `group_label`, metadata, and signature names, but the workflow must explicitly document how each original source label maps to the standardized label.

## 5. Group labels

- `group_label` must be a factor, not a character vector.
- Use actual biological condition names.
- Do not use generic labels such as `UP`, `DOWN`, `POSITIVE`, or `NEGATIVE`.
- Define factor levels explicitly.
- Ensure the labels agree with the verified score direction.
- Use the same biological labels consistently in:
  - `group_label`;
  - phenotype;
  - description;
  - signature name; and
  - collection metadata.

Example:

```r
group_label <- factor(
  ifelse(score > 0, "Treatment", "Vehicle"),
  levels = c("Treatment", "Vehicle")
)

stopifnot(is.factor(group_label))
```

For scores equal to zero, define and document the handling explicitly. Do not assign zero-valued rows to a condition by default unless the study or workflow justifies it.

## 6. Signature naming and source labels

- Signature names must describe the actual:
  - biological perturbation or comparison;
  - organism;
  - tissue or sample type;
  - sex;
  - age group; and
  - other defining subgroup information when biologically relevant.
- Do not copy inaccurate labels from folder names, filenames, worksheet names, or legacy scripts.
- For example, do not use `KO` when the manuscript describes transgenic overexpression, knockdown, heterozygosity, or another distinct condition.
- Treat the manuscript, source data, and verified analysis code as authoritative.
- Use one naming pattern consistently across related signatures.

## 7. Platform

`platform` must exactly match one of the following approved values:

```text
DNA assay by ChIP-seq
DNA assay by ATAC-seq
genotyping by array
genotyping by whole-genome-sequencing
transcriptomics by array
transcriptomics by bulk RNA-seq
transcriptomics by long-read sequencing
transcriptomics by single-cell RNA-seq
ribosome transcriptomics
spatial transcriptomics
single-cell spatial transcriptomics
proteomics by array
proteomics by mass spectrometry
proteomics by NMR
proteomics by antibody or aptamer
proteomics by fluorescence
single-cell proteomics by mass spectrometry
metabolomics by mass spectrometry
metabolomics by gas chromatography
metabolomics by liquid chromatography HPLC
metabolomics by NMR
metabolomics by fluorescence
methylation by array
methylation by bisulfite sequencing
methylation by immunoprecipitation
single-cell CITE-seq
cell flow cytometry
```

Do not create alternative spellings, abbreviations, or near-synonyms.

## 8. Sample type

- `sample_type` must use an exact approved SigRepo controlled-vocabulary term.
- Do not invent free-text sample types.
- Inspect the available terms before assigning a value:

```r
approved_sample_types <- OmicSignature::OmicS_searchSampleType("")
```

- Select the most biologically appropriate exact term.
- Do not automatically use the first partial match.
- Preserve the exact spelling, capitalization, spacing, and formatting returned by `OmicS_searchSampleType()`.
- Confirm that the selected value is present in the approved vocabulary before object creation.
- If the function returns a table or another structured object, extract the field containing the approved terms before validation.
- If no clearly appropriate approved term is available, report the ambiguity rather than inventing a value.

Example validation:

```r
approved_sample_types <- OmicSignature::OmicS_searchSampleType("")

stopifnot(
  is.data.frame(approved_sample_types),
  all(c("ID", "Name") %in% names(approved_sample_types)),
  sample_type %in% approved_sample_types$Name
)
```

### Sample-type specificity across related objects

- Select the most specific approved sample-type term supported by the source.
- Do not replace a specific cell line, tissue, compartment, or specimen type with a broader generic term merely because multiple objects belong to one study.
- Related OmicSignature objects may require different `sample_type` values.
- Validate the selected sample type separately for each object.
- A collection-level description may summarize the broader study context, but it must not override object-specific sample types.

## 9. Publication identifiers

- For SigRepo OmicSignature metadata, use `PMID` as the primary publication identifier unless the task explicitly requires another identifier.
- Include `PMID` only when it can be verified from the manuscript or a reliable publication record.
- Never guess or invent publication identifiers.
- When an identifier is unavailable or cannot be verified, omit the field instead of using an unverified value or a placeholder such as `NA_character_`.
- `PMID` must be stored as a character string, not as a numeric value. Example:

```r
PMID = "37542347"
```

## 10. Feature and probe identifiers

### General feature mapping

Before mapping, identify:

- the organism;
- the platform;
- all identifier columns supplied by the source;
- the required repository feature identifier system; and
- the required reference release.

Use the most stable and specific supplied identifier as the primary mapping key.

General rules:

- Map source identifiers to the required repository feature system using the specified reference release.
- Preserve the original identifiers and mapping source in an audit output.
- Retain valid one-to-many mappings as expanded rows unless the repository explicitly requires another rule.
- Do not silently retain only the first mapping.
- Do not deduplicate solely by `feature_name` when multiple source probes or source rows map to the same feature.
- Exclude unmatched rows only from the final OmicSignature input.
- Retain unmatched rows in the mapping results and audit.
- Use symbols only as a documented rescue strategy when a more stable identifier fails or is unavailable.
- If supplied identifiers disagree, report the conflict rather than silently choosing one.

### Canonical identifier validation versus annotation retrieval

- When validating whether a supplied identifier exists in the required reference release, query only the canonical identifier needed for validation.
- Do not include annotation fields such as symbols or Entrez IDs in the direct validation query when those fields can create multiple rows for one valid identifier.
- Retrieve annotations separately only when required for auditing or rescue mapping.
- Mapping multiplicity should reflect genuine identifier mapping, not expansion caused solely by additional annotation attributes.

### Rescue before exclusion

- Do not exclude a source row solely because its primary identifier is missing, invalid, obsolete, or absent from the required reference release before rescue mapping is attempted.
- Retain rows with valid analytical values while attempting all supported rescue identifiers in the stated precedence order.
- Exclude a row from the final OmicSignature input only after all available primary and rescue mapping strategies have failed.
- Retain unsuccessfully mapped rows in the mapping audit.

### Identifier mapping precedence

Apply the following precedence rules.

#### Ensembl gene IDs supplied

- Remove version suffixes when present.
- Validate IDs against the required Ensembl release.
- Retain valid IDs directly.
- Rescue invalid or obsolete IDs using another supplied stable identifier or the official species-specific symbol.

#### Mouse MGI accessions supplied

- Map MGI accessions to Ensembl using the required Ensembl release.
- Use the MGI symbol only to rescue rows that fail accession-based mapping.

#### Entrez Gene IDs supplied

- Map Entrez Gene IDs to Ensembl using the required Ensembl release.
- Use the official species-specific symbol only as a rescue.

#### Gene symbols only

- When gene symbols are the only identifiers available, map official species-specific symbols to the required Ensembl release.
- Use HGNC symbols for human data and MGI symbols for mouse data.
- Preserve one-to-many mappings and unmatched symbols in the audit.

#### Array studies

- Preserve a genuine array probe ID as `probe_id`.
- Map the associated gene identifier or symbol separately to `feature_name`.
- Do not treat Entrez, MGI, Ensembl, or gene symbols as array probe IDs.

### Mapping audit

The mapping audit should retain, when available:

- source file;
- source sheet;
- source row;
- original probe ID;
- original accession or gene ID;
- original symbol;
- primary mapping key;
- rescue mapping key;
- mapping source;
- mapping status;
- mapping multiplicity; and
- final `feature_name`.

Record the organism, identifier system, and reference release used for mapping in the workflow or metadata.


### Repository-generated probe IDs when the source has no genuine assay ID

This rule applies only when the source does not contain a genuine assay or
probe identifier.

- Preserve genuine source assay or probe identifiers whenever available.
- Do not use a gene symbol, Entrez ID, Ensembl ID, UniProt accession, or
  `feature_name` as a fabricated assay identifier.
- Do not create study-specific pseudo-identifiers by adding prefixes or
  suffixes to biological identifiers.
- Use the shared helper:

```r
sigrepo_standardize_signature()
```

  which assigns deterministic technical row identifiers:

```text
feature_1
feature_2
feature_3
...
```

  only when `probe_id` is absent.
- These `feature_n` values are repository-generated technical identifiers. They
  do not represent a measured probe, molecule, or biological feature.
- When a mapped `difexp` table is available:
  - standardize the complete mapped parent table exactly once;
  - derive the final `signature` by filtering that standardized parent;
  - retain the same `probe_id`, `feature_name`, score, and relevant statistics
    in the signature; and
  - never regenerate IDs independently for `difexp` and `signature`.
- When `difexp = NULL`, standardize the final signature table exactly once.
- Do not deduplicate solely by `feature_name`. Multiple source or mapping rows
  may legitimately share one `feature_name`.
- Before object construction, verify that:
  - `probe_id` is a non-missing character value;
  - `probe_id` is unique within the standardized parent table;
  - automatically assigned IDs exactly equal
    `feature_1`, `feature_2`, ..., `feature_n` in row order;
  - every signature `probe_id` occurs in its corresponding `difexp`, when
    `difexp` is present; and
  - each signature row retains the same probe-to-feature relationship as its
    parent row.
- If assignment or synchronization fails, stop and report the problem.

### Array transcriptomics

- Preserve a genuine source array probe identifier in `probe_id`.
- Keep a genuine `probe_id` as a character column.
- Do not replace a genuine probe ID with a gene symbol, Entrez ID, Ensembl ID, or another gene identifier.
- Do not treat Entrez, HGNC, MGI, Ensembl, or other gene identifiers as array probe IDs.
- When the source does not contain a genuine array probe identifier, follow the repository-generated probe-ID rules above.
- Do not create pseudo-array identifiers by adding prefixes or suffixes to Entrez IDs, gene symbols, Ensembl IDs, or row numbers.

### Non-array transcriptomics

This includes bulk RNA-seq, single-cell RNA-seq, long-read RNA-seq, ribosome transcriptomics, spatial transcriptomics, and single-cell spatial transcriptomics.

- Do not set `probe_id = feature_name` or use a biological identifier as a pseudo-assay ID.
- When no genuine assay identifier is available, use the shared helper to assign deterministic `feature_n` technical IDs once, following the repository-generated probe-ID rules above.

### Proteomics

- Preserve genuine assay identifiers, such as SOMAmer or SomaScan IDs, as `probe_id`.
- When a source cell contains multiple UniProt accessions:
  - split it into separate rows;
  - preserve the same probe ID, score, statistics, and source-row information for every expanded row; and
  - preserve the original unsplit UniProt cell in the mapping audit.
- Clean hidden whitespace, delimiters, capitalization, and empty values before expansion.
- Do not require external resolver or feature-set files unless they are explicitly provided by the task or repository workflow.
- Do not claim that UniProt accessions were checked for deprecated, merged, secondary, or current status unless an authoritative validation step was actually performed.
- When no authoritative validation source is available, use the cleaned source accessions as provided and document that current-status validation was not performed.

## 11. Covariates metadata

- `covariates` should contain variables included as adjustment terms in the statistical model.
- Extract covariates from the manuscript methods, supplementary methods, or verified analysis code.
- Record all variables included in the differential-analysis model.
- Do not list tissue, assay platform, phenotype comparison, or general study description as covariates.
- Keep model covariates distinct from sample or subgroup restrictions.
- Record restrictions such as `sex fixed to male`, `age restricted to old animals`, or a specific tissue in the description or `others`, not in `covariates`.
- When no adjustment variables were used, record `none`.
- Do not use `none` when the model adjusted for one or more variables.

## 12. OmicSignatureCollection creation

- Do not create an `OmicSignatureCollection` when a study produces only one OmicSignature object.
- Create a collection only when at least two biologically related OmicSignature objects should be grouped, unless the task explicitly requests otherwise.
- When no collection is warranted, do not create:
  - collection metadata;
  - collection validation code;
  - collection output checks;
  - collection filenames; or
  - collection RDS files.
- Validate that collection membership contains exactly the intended objects and no supporting or exploratory signatures.

## 13. Validation and output safety

Before saving, validate at minimum:

- expected number of signatures;
- expected source tables and worksheets;
- required signature columns;
- non-empty `feature_name`;
- numeric, finite scores;
- factor-valued, non-missing `group_label`;
- score-to-label agreement;
- verified contrast direction;
- all study-supported thresholds and exact comparison operators;
- platform and sample-type vocabulary;
- probe-ID handling;
- feature mapping and mapping multiplicity;
- signature membership relative to `difexp`, when present;
- metadata consistency;
- publication identifiers;
- expected signature sizes or source-row counts when known; and
- expected collection membership.
- metadata field types required by the OmicSignature API;
- character-valued publication identifiers such as `PMID`, when present; and
- metadata field types required by the validated package contract; in the current approved workflows, multiple keyword terms are normalized to one scalar comma-separated metadata string through `sigrepo_keywords()`.


Additional requirements:

- Do not overwrite existing files.
- Write generated workflows and outputs only to the requested draft location.
- Do not execute an Rmd unless explicitly instructed.
- Show the Git diff after editing.
- Report unresolved assumptions, contradictions, or limitations rather than silently choosing a value.
