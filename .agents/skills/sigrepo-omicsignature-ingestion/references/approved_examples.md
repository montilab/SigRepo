# Approved examples manifest

## LLFS/Sebastiani 2024

**Pattern:** metabolomics with genuine source identifiers and broader retained
`difexp` tables.

Key behaviors:

- source `Compound.Name` retained as genuine `probe_id`;
- reviewed frozen RefMet rescue cache;
- three signature families derived from one mapped parent;
- expected source and signature counts frozen;
- output writes disabled by default.

## EMT/Youssef 2024

**Pattern:** membership-only bulk RNA-seq signatures.

Key behaviors:

- `difexp = NULL`;
- human symbols mapped to Ensembl release 114;
- valid mapping expansion retained;
- no genuine assay probes;
- deterministic `feature_1`, `feature_2`, ... IDs;
- two objects and one collection.

## Ding 2025

**Pattern:** tissue-specific proteomics with several signature families.

Key behaviors:

- study-provided exact protein-label crosswalk preferred;
- frozen reviewed UniProt mapping cache;
- source-specific resolution of apparent HGNC one-to-many cases;
- 13 age-associated, 6 decoupling, and 13 clock signatures;
- 32 objects and three collections;
- output writes disabled by default.

## AhR/CYP1B1 MontiLab 2016

**Pattern:** transcriptomic array differential-analysis tables with complete
parents.

Key behaviors:

- four complete limma parent tables;
- non-`NULL` mapped parent `difexp` retained for every object;
- exact signature rule `adj.P.Val <= 0.01` with no fold-change cutoff;
- frozen reviewed Ensembl release 114 mapping;
- deterministic `feature_1`, `feature_2`, ... IDs assigned once per parent;
- four objects and one four-member collection;
- output writes disabled by default.
