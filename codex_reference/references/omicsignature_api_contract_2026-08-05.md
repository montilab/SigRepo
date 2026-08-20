# OmicSignature API contract policy — 2026-08-05

## Current repository policy

Compatibility is behavioral rather than version-only.

The shared helper is responsible for:

- API discovery;
- exact controlled-vocabulary validation;
- metadata keyword normalization;
- preservation of genuine probe IDs;
- deterministic `feature_n` assignment when no genuine ID exists;
- row-count and probe-to-feature preservation during standardization; and
- collection-member access.

## Important package behaviors

The repository does not rely on the package constructor to safely create
missing probe IDs. The helper resolves this before construction.

Collection member list names are not part of the required contract. The
contract requires preservation of the intended member objects and member count.

Factor-level order is enforced on prepared input tables. Post-construction
validation requires preservation of the approved biological labels because the
constructor may reconstruct factor levels.

## Contract v4

Contract v4 tests:

- API discovery and vocabularies;
- explicit genuine probe-ID preservation;
- deterministic missing-ID assignment;
- repeated-feature preservation;
- parent `difexp` and child signature synchronization;
- object construction through the approved helper path; and
- collection access without requiring list-name preservation.
