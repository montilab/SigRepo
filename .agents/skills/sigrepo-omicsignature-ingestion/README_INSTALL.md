# Install the SigRepo OmicSignature ingestion Skill

## Codex: repository-scoped installation

The recommended location is:

```text
<SigRepo repository root>/.agents/skills/sigrepo-omicsignature-ingestion/
```

The folder must contain:

```text
SKILL.md
references/
assets/
scripts/
agents/
```

Use the Codex drop-in ZIP and extract it into the SigRepo repository root.

Then restart Codex if the skill does not appear automatically. In Codex CLI or
the IDE extension:

```text
/skills
```

or invoke it explicitly:

```text
$sigrepo-omicsignature-ingestion
```

## Verify the repository foundation

From the SigRepo repository:

```bash
python .agents/skills/sigrepo-omicsignature-ingestion/scripts/verify_repository_foundation.py
```

Then run the existing foundation contracts under:

```text
codex_reference/tests/
```

## ChatGPT Skill upload

Use the standalone Skill ZIP in a ChatGPT surface that supports Skills:

```text
Skills → Create → Upload from your computer
```

Availability and sharing controls depend on the user's plan and workspace.

## Colleague setup

A colleague needs:

1. this Skill;
2. the SigRepo repository with the reconciled `codex_reference`;
3. an installed compatible OmicSignature R package;
4. their own authorized study source files;
5. both foundation contracts passing locally.

Do not redistribute manuscript supplements or unpublished study materials
unless their sharing permissions allow it.
