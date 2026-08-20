# Use the Skill

## Start a new study

```text
$sigrepo-omicsignature-ingestion

Inspect this manuscript, supplement, and source table. Produce the complete
preflight interpretation first. Do not edit files until comparison direction,
signature rule, difexp policy, identifiers, mapping, probe IDs, metadata, and
objects are supported.
```

## Review an existing Rmd

```text
$sigrepo-omicsignature-ingestion

Review this candidate Rmd against the live SigRepo conventions, helper behavior,
approved examples, and validation contract. List unsupported assumptions before
making changes.
```

## Review mapping

```text
$sigrepo-omicsignature-ingestion

Inspect the candidate mapping cache and audit. Separate exact mappings,
one-to-many cases, aliases, composite identifiers, and unresolved rows. Do not
finalize ambiguous mappings without explicit evidence.
```

## Qualify for approval

```text
$sigrepo-omicsignature-ingestion

Run the approval checklist for this development example. Do not move it until a
clean development render, object inspection, frozen mapping, expected counts,
and zero approval-blocking unresolved items are confirmed.
```

## Expected interaction

The Skill should first return a preflight and either:

- state that the workflow is ready to scaffold; or
- stop with a small set of evidence-based questions.

A generated Rmd is not the first deliverable when the scientific interpretation
is unresolved.
