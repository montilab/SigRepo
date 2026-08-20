# Evidence and stop rules

## Evidence that can support a decision

- explicit methods or supplementary methods;
- source-table headers and footnotes;
- verified analysis code defining the contrast and thresholds;
- project-context documentation from the study team;
- consistent values in the actual source table;
- official identifier-release resources used in a reviewed mapping step.

## Evidence that cannot support a decision by itself

- filenames;
- folder names;
- an old Codex draft;
- an RDS object created by an unreviewed script;
- a biological interpretation inferred only from the sign of a column whose
  contrast is unknown;
- a threshold guessed from the observed maximum value;
- a publication identifier copied from an unrelated source.

## Mandatory stop conditions

Stop when a decision could change signature membership, score direction,
feature identity, metadata interpretation, or collection membership and the
available evidence does not resolve it.

A correct stop is a successful regression behavior. The objective is not to
produce an Rmd at any cost.
