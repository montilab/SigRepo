# Visible regression corpus

The actual source corpus remains outside this directory at:

```text
codex_tests\scenario1_diff_table
```

It must not be copied into the final Skill or treated as approved code.

The corpus is visible to Codex and therefore cannot serve as a hidden
generalization test. It is useful for:

- regression testing;
- comparing new Skill output with earlier drafts;
- identifying unsupported assumptions;
- testing stop conditions;
- retaining the original AhR/CYP1B1 scenario as visible regression material
  while its reviewed implementation lives under `approved_examples`; and
- preserving Chakraborty as a stop-behavior case because its final selection
  rule, exact contrast direction, and dose remain unsupported.

At least one hidden study remains required for final generalization testing.

See `scenario1_manifest.md` for case-specific roles.
