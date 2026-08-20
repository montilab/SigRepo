# Approved examples

This folder contains reviewed implementation patterns.

Current approved workflows:

- `LLFS_Sebastiani2024`
- `EMT_Youssef2024`
- `Ding2025`
- `AhR_CYP1B1_MDA_SUM149`

AhR/CYP1B1 is the transcriptomic differential-table pattern: four complete
limma parent tables, retained non-`NULL` `difexp`, `adj.P.Val <= 0.01`, Ensembl
v114 frozen reviewed mapping, deterministic `feature_n` IDs assigned once per
parent, four objects, and one collection.

An approved workflow must have:

- evidence-supported study interpretation;
- no unresolved mapping or metadata decisions;
- portable input discovery;
- reviewed frozen mapping resources when needed;
- expected source, mapping, signature, and collection counts;
- clean-session rendering;
- deterministic probe-ID handling;
- save/upload code disabled by default; and
- an explicit approval-status section.

Approved examples teach reusable implementation patterns. They never override
the manuscript, supplement, source tables, verified analysis code, or core
conventions.
