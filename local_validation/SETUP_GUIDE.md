# Local Validation Setup Guide

This is a step-by-step walkthrough for getting the local validation harness
(`./local_validation/run_local_validation.sh`) running against a real stack
from nothing -- no existing database, no running server. For a quick
reference of environment variables and modules once you already have a
stack running, see [README.md](README.md).

## What you need before you start

The harness validates the `SigRepo` R client and, optionally, the
`SigRepo_Server` MCP server against a **real, running** SigRepo stack. It
does not stand one up for you. You need, at minimum:

- A MySQL instance the harness can reach
- SigRepo_Server's Plumber API running against that MySQL instance
- The `SigRepo_Server` repo checked out somewhere (for its `mysql/schema/`
  and `mysql/data/` files, and `api/run_sigrepo_api.R`)
- Optionally, SigRepo_Server's MCP server running too
  (`mcp/run_sigrepo_mcp.R`), if you want the `06_mcp_protocol.R` module to
  run instead of skip

## 1. Start MySQL

Any local MySQL 8 works. A throwaway Docker container is the fastest path
and needs no host state:

```bash
docker run -d --name sigrepo-mysql \
  -e MYSQL_ROOT_PASSWORD=<pick-a-password> \
  -e MYSQL_DATABASE=sigrepo \
  -p 3306:3306 \
  mysql:8.0 --default-authentication-plugin=mysql_native_password

# wait for it to accept connections
until docker exec sigrepo-mysql mysqladmin ping -uroot -p<pick-a-password> --silent; do sleep 2; done
```

## 2. Start the SigRepo_Server API

From the `SigRepo_Server` repo root, pointed at the MySQL instance above:

```bash
DB_NAME=sigrepo DB_LOCAL_HOST=127.0.0.1 DB_PORT=3306 \
DB_USER=root DB_PASSWORD=<pick-a-password> \
ADMIN_KEY=<pick-an-admin-key> \
DIFEXP_DIR=/tmp/sigrepo-difexp \
SIGREPO_SERVER_DIR=$(pwd) \
SIGREPO_DIR=/path/to/SigRepo \
Rscript api/run_sigrepo_api.R
```

It logs `Running plumber API at http://0.0.0.0:3838` when ready. Leave it
running in this terminal (or run it in the background).

## 3. Bootstrap the database

A fresh MySQL instance has no schema and no reference data yet. Call
`/init_db` once to create both (this loads real reference CSVs -- expect
it to take a minute or two, it's populating ~135k transcriptomics and
~205k proteomics feature rows):

```bash
curl -X POST "http://127.0.0.1:3838/init_db?admin_key=<pick-an-admin-key>"
```

You should get back `{"MESSAGES":"Finish initialized the database."}`.

**Worth knowing**: `/init_db` auto-provisions a `root` user in the SigRepo
`users` table with an `admin` role and an active API key, alongside the
`guest`/`montilab` accounts from `mysql/data/users.csv`. If your MySQL
login is also named `root` (as in the Docker example above), that single
account satisfies both the raw-MySQL-login requirement *and* the
SigRepo-application-level admin requirement that `SIGREPO_LOCAL_WRITE_USER`
needs (see below) -- you don't need to manually insert a user row to get
the CRUD-validation modules running against a fresh stack.

## 4. (Optional) Start the MCP server

If you also want `06_mcp_protocol.R` to run (instead of skip), start
`mcp/run_sigrepo_mcp.R` from `SigRepo_Server` too, pointed at the same
database:

```bash
DB_NAME=sigrepo DB_LOCAL_HOST=127.0.0.1 DB_PORT=3306 \
DB_USER=root DB_PASSWORD=<pick-a-password> \
SIGREPO_SERVER_DIR=$(pwd) \
SIGREPO_DIR=/path/to/SigRepo \
MCP_PORT=8021 \
Rscript mcp/run_sigrepo_mcp.R
```

It logs `MCP server listening on http://0.0.0.0:8021` when ready.

## 5. Pre-load a signature for the read-path checks

`03_r_client_read.R` looks for a signature literally named
`LLFS_Aging_Gene_2023` and expects to find it -- it doesn't add one itself
(that's what `04_signature_crud.R` tests, separately, by adding and
removing its own temporary signature). Add SigRepo's builtin example once:

```bash
DB_NAME=sigrepo DB_LOCAL_HOST=127.0.0.1 DB_PORT=3306 DB_USER=root DB_PASSWORD=<pick-a-password> \
Rscript -e '
  pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE)
  conn_handler <- SigRepo::newConnHandler(
    dbname = Sys.getenv("DB_NAME"), host = Sys.getenv("DB_LOCAL_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD")
  )
  data(LLFS_Aging_Gene_2023, package = "SigRepo")
  SigRepo::addSignature(conn_handler = conn_handler, omic_signature = LLFS_Aging_Gene_2023, visibility = TRUE, verbose = FALSE)
'
```

If you skip this step, `03_r_client_read.R` will report `[fail]
searchSignature did not find LLFS_Aging_Gene_2023` -- that's expected, not
a bug, and this step is how you fix it.

## 6. Configure `.env.local-validation`

From the `SigRepo` repo root:

```bash
cp local_validation/.env.local-validation.example local_validation/.env.local-validation
```

Edit it to match what you started above. For the Docker/fresh-bootstrap
path in this guide, using `root` for admin/read/write (per the note in
step 3) looks like:

```bash
SIGREPO_LOCAL_DB_NAME=sigrepo
SIGREPO_LOCAL_DB_HOST=127.0.0.1
SIGREPO_LOCAL_DB_PORT=3306

SIGREPO_LOCAL_API_HOST=http://127.0.0.1
SIGREPO_LOCAL_API_PORT=3838

SIGREPO_LOCAL_MCP_HOST=http://127.0.0.1
SIGREPO_LOCAL_MCP_PORT=8021

SIGREPO_LOCAL_DB_ADMIN_USER=root
SIGREPO_LOCAL_DB_ADMIN_PASSWORD=<pick-a-password>
SIGREPO_LOCAL_READ_USER=root
SIGREPO_LOCAL_READ_PASSWORD=<pick-a-password>
SIGREPO_LOCAL_WRITE_USER=root
SIGREPO_LOCAL_WRITE_PASSWORD=<pick-a-password>

SIGREPO_LOCAL_EXPECT_GENESETS=0
SIGREPO_LOCAL_EXPECT_METABOLITE_REFERENCE=0
```

`EXPECT_GENESETS`/`EXPECT_METABOLITE_REFERENCE` are `0` here because a
stock `/init_db` creates those tables but doesn't populate them -- there's
no reference CSV/step for geneset or metabolite data in
`generate_db_tables()`. Only set these to `1` if you've separately loaded
that data.

See [README.md](README.md#environment-variables) for the full variable
reference, including the omics-fixture variables for the CRUD-validation
modules (`04_signature_crud.R`/`05_collection_crud.R`).

## 7. Run it

```bash
./local_validation/run_local_validation.sh
```

Output is one `[pass]`/`[fail]`/`[skip]` line per check, grouped under
`[module] <name>` headers, ending in a `[summary] pass=N fail=N skip=N`
line. A non-zero `fail` count exits the script with status 1 (useful in
scripts/CI; the harness itself is not currently wired into any CI
workflow, it's a manual/local tool).

## Troubleshooting

**`searchSignature did not find LLFS_Aging_Gene_2023`** -- see step 5, you
need to pre-load that signature once per fresh database.

**`write user 'X' is a valid MySQL login but is not registered in the
SigRepo 'users' table`** -- your `SIGREPO_LOCAL_WRITE_USER` must satisfy
*two* separate requirements: it needs to be a real MySQL account (for
`conn_init()` to open a DB connection at all) *and* a row in the SigRepo
`users` table with an active `admin`/`editor` role (checked separately by
the harness before running CRUD checks). These are two different
credential systems that happen to share a username in the simple
single-account setup this guide describes -- see step 3's note about the
auto-provisioned `root` user.

**`getSignature`/`addSignature` errors mentioning `group_label` or
`probe_id`** -- these point at real bugs in `OmicSignature`
reconstruction, not harness misconfiguration. As of this writing:
- A `group_label`-related crash on retrieval was fixed in
  `R/createOmicSignature.R` (uni-directional signatures' `difexp` tables
  don't always have a `group_label` column; the reconstruction code now
  guards for that, matching the same guard `sanitizeRetrievedSignature()`
  already used for the `signature` table).
- A remaining `probe_id` mismatch after that fix (signature and difexp
  probe_ids no longer match after the MySQL round-trip, despite matching
  in the original object) is a known, still-open issue.

**`OmicSignature Error` mentioning `keywords` and "condition has length >
1"** -- a bug in the external `montilab/OmicSignature` package's
`checkMetadata()`, triggered by any signature whose `keywords` metadata
has more than one value (several of SigRepo's builtin example datasets do)
combined with newer R versions treating `if()` on a length>1 condition as
a hard error. Out of scope to fix from the SigRepo repo; tracked
separately.

## Tearing down

```bash
docker stop sigrepo-mysql && docker rm sigrepo-mysql
```

Kill the `api/run_sigrepo_api.R` / `mcp/run_sigrepo_mcp.R` processes
however you started them (`Ctrl-C` in the foreground, or `kill <pid>` if
backgrounded).
