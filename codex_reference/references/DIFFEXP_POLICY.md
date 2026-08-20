# SigRepo `difexp` policy

## Retain `difexp`

Use a standardized parent `difexp` table when the source provides a sufficiently
complete or partially filtered differential-analysis result from which the
final signature can be derived using supported criteria.

Typical fields may include:

- signed effect size;
- raw p-value;
- adjusted p-value or q-value;
- test statistic;
- source identifiers; and
- provenance fields.

## Use `difexp = NULL`

Use `NULL` when the source contains only:

- an already selected final result;
- a curated or membership-only list;
- a named signature table without a broader parent analysis; or
- another result that is not a broader differential-analysis table.

Do not duplicate the final signature into `difexp` merely to avoid `NULL`.

## Synchronization contract

When `difexp` is present:

1. map and standardize the parent table once;
2. derive the signature from that standardized parent;
3. preserve exact `probe_id`, `feature_name`, score, and applicable statistics;
4. validate every implemented threshold and comparison operator; and
5. verify that every signature row has an exact parent row.

When membership and score come from different sources, document both sources
and join them through a justified stable key.
