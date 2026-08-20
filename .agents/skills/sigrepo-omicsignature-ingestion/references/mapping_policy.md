# Mapping policy

## Target identifiers

Choose the repository target identifier according to the study modality and
SigRepo conventions. Record the species and mapping release explicitly.

Typical patterns include:

- human or mouse gene symbols to species-specific Ensembl gene IDs;
- older Ensembl IDs to the required Ensembl release;
- protein labels to specific UniProt accessions;
- metabolite labels to the reviewed repository nomenclature.

## Development mapping

A one-time builder may query a versioned source. It must save:

- source identifier;
- cleaned identifier;
- candidate target identifier;
- mapping source and release;
- match status;
- multiplicity;
- source row context;
- unresolved and ambiguous cases.

Candidate outputs must be clearly marked `UNREVIEWED`.

## Review

Review every unresolved, alias, composite, previous-symbol, and one-to-many
case. Store explicit source-specific decisions.

Do not choose the first returned mapping merely because it exists.

## Approved mapping

Freeze the reviewed mapping in a stable local cache. The approved Rmd reads the
cache and must not call BioMart, UniProt, HGNC, MGI, RefMet, or another live
service.

Validate:

- expected source labels;
- zero unresolved labels required for approval;
- intended expansion;
- exact target-ID format;
- source and mapped row counts;
- no unreviewed cache references.
