# Reconciled tests

## Easiest Windows method

Double-click:

```text
run_all_tests_windows_autodetect.cmd
```

The launcher finds `Rscript.exe`, runs both active contracts, and writes results
to:

```text
results_v4/omicsignature_api_contract_v4_results.txt
results_v4/repository_contract_results.txt
results_v4/all_tests_summary.txt
```

## What the tests cover

### API contract v4

- required OmicSignature API behavior;
- exact vocabulary searches;
- genuine probe-ID preservation;
- deterministic `feature_n` assignment;
- repeated `feature_name` preservation;
- parent `difexp` and signature synchronization;
- object construction; and
- collection access.

### Repository contract

- four expected approved examples, including the AhR/CYP1B1 complete-limma
  differential-table branch;
- one Rmd and one HTML per approved example;
- required reviewed reference files;
- no `UNREVIEWED` material in approved examples;
- approval-status text;
- disabled save chunks;
- reconciled foundation documentation; and
- presence of the external visible scenario corpus.

A nonzero exit code means the result files must be reviewed before building the
final Skill.
