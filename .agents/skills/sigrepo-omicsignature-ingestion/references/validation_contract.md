# Validation contract

## Source validation

- required files exist;
- sheet and column names are explicit;
- source row counts are recorded;
- numeric columns parse correctly;
- missing and duplicated source identifiers are reported;
- threshold operators match the source exactly.

## Mapping validation

- mapping release is explicit;
- every source row has a documented mapping status;
- mapped and expanded row counts are frozen;
- unresolved cases are reported;
- approved workflows reference no `UNREVIEWED` files;
- target identifiers match the expected format.

## Probe validation

- genuine IDs are preserved;
- generated IDs form the exact set `feature_1` through `feature_n`;
- IDs are nonmissing and unique within the standardized parent;
- repeated `feature_name` values are not removed merely because they repeat;
- probe-to-feature relationships are preserved without requiring row-order
  preservation.

## Parent-child validation

When `difexp` is present:

- standardize the parent exactly once;
- derive the signature from the standardized parent;
- every signature `probe_id` occurs in `difexp`;
- `probe_id`, `feature_name`, score, and applicable statistics agree exactly;
- no independent child probe-ID generation occurs.

## Object validation

- metadata uses supported controlled-vocabulary values;
- signature and `difexp` row counts match expectations;
- factor values are approved biological labels;
- all object and collection members are accessible;
- save chunks are disabled during qualification.

## Render and approval validation

- render from a clean R environment;
- inspect generated objects, not only the HTML;
- verify the approval-status sentence;
- verify the obsolete development-status heading is absent;
- rerun API and repository contracts after promotion.
