# One-time mapping builder template

This file is intentionally a template, not an approved live mapper.

Required behavior:

1. Read the exact source labels and provenance.
2. Query a versioned mapping resource only during development.
3. Save an `UNREVIEWED` candidate cache.
4. Save complete mapping results and audit tables.
5. Separate:
   - unique exact matches;
   - aliases/previous symbols;
   - one-to-many cases;
   - unmatched labels;
   - composite identifiers;
   - source-specific accession evidence.
6. Stop before selecting ambiguous targets.
7. After manual review, create:
   - a reviewed frozen cache;
   - a decision log;
   - a final mapping summary.
8. Archive this builder before promotion.

The approved Rmd must read only the reviewed frozen cache.
