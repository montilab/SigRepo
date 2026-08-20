# SigRepo probe-ID policy

## 1. Genuine identifiers

Preserve a genuine assay or probe identifier exactly as a character value,
subject to cleaning and uniqueness validation.

Examples include:

- array probe IDs;
- SOMAmer or SomaScan IDs;
- platform-specific metabolite or protein assay IDs.

A biological feature identifier is not automatically a probe ID.

## 2. No genuine identifier

When the source has no genuine assay or probe identifier, use
`sigrepo_standardize_signature()` from:

```text
helpers/omicsignature_compat.R
```

The helper assigns:

```text
feature_1, feature_2, ..., feature_n
```

before package standardization.

These values are deterministic technical row IDs. They are not biological
identifiers and should not be described as source-provided probes.

## 3. Parent `difexp` synchronization

When `difexp` is retained:

1. construct the complete mapped parent table;
2. assign or validate probe IDs once on that parent;
3. standardize the parent once;
4. filter the final signature from the standardized parent; and
5. verify exact `probe_id`–`feature_name` synchronization.

Never assign IDs separately to the parent and signature.

## 4. Prohibited substitutions

Do not use the following as fabricated probe IDs:

- `feature_name`;
- gene symbols;
- Entrez IDs;
- Ensembl IDs;
- UniProt accessions;
- row numbers with study-specific prefixes.

The only approved no-source-ID convention is the shared deterministic
`feature_n` mechanism.
