# Metadata and naming policy

## Metadata

Use only supported study evidence.

Verify exact controlled-vocabulary values for:

- platform;
- sample type;
- organism and assay context where applicable.

Do not invent PMID, DOI, GEO, ArrayExpress, dbGaP, or other identifiers.

Use `sigrepo_keywords()` for keyword normalization required by the validated
package contract.

Record:

- comparison;
- score interpretation;
- exact cutoff and operator;
- source table;
- mapping release;
- covariates when supported;
- publication or project provenance.

## Naming

Names should identify, in a stable readable order:

- biological domain or phenotype;
- species;
- tissue, cell line, or sample context;
- comparison or signature family;
- first author or project identifier;
- year when available.

Avoid unsupported mechanistic claims in names.

## Group labels

Use biological factor values such as:

- `SP2509` and `Vehicle`;
- `AHR_KD` and `CN`;
- `Epithelial` and `Mesenchymal`;
- tissue or age-group labels defined by the study.

Do not use generic `UP` and `DOWN`.
