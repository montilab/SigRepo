# SigRepo Local Validation Harness

This directory contains a modular local validation harness for testing the
SigRepo R client against a local SigRepo server stack.

## Quick Start

From the `SigRepo` repository root:

```bash
cp local_validation/.env.local-validation.example local_validation/.env.local-validation
# edit local_validation/.env.local-validation
./local_validation/run_local_validation.sh
```

This:

1. runs shell-based API smoke checks with `curl`
2. runs shell-based MCP server smoke checks with `curl` (skipped if
   `SIGREPO_LOCAL_MCP_HOST`/`SIGREPO_LOCAL_MCP_PORT` aren't set)
3. runs R-based validation modules for schema, reference data, client CRUD,
   and the MCP protocol

The harness loads variables from `local_validation/.env.local-validation` by
default. You can point to a different file with:

```bash
SIGREPO_LOCAL_ENV_FILE=/path/to/custom.env ./local_validation/run_local_validation.sh
```

## Environment Variables

Recommended workflow:

- copy `local_validation/.env.local-validation.example` to `local_validation/.env.local-validation`
- fill in your local DB/API credentials and any optional fixture paths
- keep `.env.local-validation` untracked

Shared local stack settings:

- `SIGREPO_LOCAL_DB_NAME` default: `sigrepo`
- `SIGREPO_LOCAL_DB_HOST` default: `127.0.0.1`
- `SIGREPO_LOCAL_DB_PORT` default: `3306`
- `SIGREPO_LOCAL_API_HOST` default: `http://127.0.0.1`
- `SIGREPO_LOCAL_API_PORT` default: `8020`

Optional, for validating SigRepo_Server's MCP server
(`mcp/run_sigrepo_mcp.R`) -- both the shell smoke check and the R module
skip (not fail) when these are unset:

- `SIGREPO_LOCAL_MCP_HOST` e.g. `http://127.0.0.1`
- `SIGREPO_LOCAL_MCP_PORT` e.g. `8021`

Optional raw MySQL/bootstrap credentials:

- `SIGREPO_LOCAL_DB_ADMIN_USER`
- `SIGREPO_LOCAL_DB_ADMIN_PASSWORD`

Use these for schema checks, reference-table checks, and any local setup where
you need a privileged MySQL account. This is the right place to use `root`.

Read-only client credentials:

- `SIGREPO_LOCAL_READ_USER` default: `guest`
- `SIGREPO_LOCAL_READ_PASSWORD` default: `guest`

Write-capable client credentials:

- `SIGREPO_LOCAL_WRITE_USER`
- `SIGREPO_LOCAL_WRITE_PASSWORD`

These are different from bootstrap/root credentials. They must correspond to a
user that:

- can log into MySQL
- exists in the SigRepo `users` table
- is active
- has `editor` or `admin` role in SigRepo

Using MySQL `root` here will usually fail SigRepo CRUD validation unless you
have explicitly registered `root` as an active SigRepo admin user.

Optional expectations:

- `SIGREPO_LOCAL_EXPECT_GENESETS=1`
- `SIGREPO_LOCAL_EXPECT_METABOLITE_REFERENCE=1`

Optional omics fixtures for CRUD tests:

- `SIGREPO_LOCAL_PROTEOMICS_FIXTURE`
- `SIGREPO_LOCAL_METABOLOMICS_FIXTURE`
- `SIGREPO_LOCAL_METABOLOMICS_NOMENCLATURE`
- `SIGREPO_LOCAL_GENETIC_VARIANTS_FIXTURE`
- `SIGREPO_LOCAL_METHYLOMICS_FIXTURE`

Each fixture should be an `.rds` file containing an `OmicSignature` object.

## Included Modules

- `01_db_schema.R`
- `02_reference_data.R`
- `03_r_client_read.R`
- `04_signature_crud.R`
- `05_collection_crud.R`
- `06_mcp_protocol.R` -- validates the SigRepo_Server MCP server: `tools/list`
  advertises the expected tools with no admin/write-capable tools exposed,
  `list_vocabulary`/`search_signatures` succeed, `get_signature_context`/
  `compare_signatures` succeed against real signatures discovered via
  `search_signatures`, and an invalid `api_key` correctly returns an MCP
  tool error. Requires `SIGREPO_LOCAL_MCP_HOST`/`SIGREPO_LOCAL_MCP_PORT`;
  skips cleanly if unset.

## Extending the Harness

1. Add a new `NN_name.R` file under `local_validation/modules/`
2. Define a `run_<name>(ctx)` function
3. Register it in `build_local_validation_context()` in `modules/helpers.R`
