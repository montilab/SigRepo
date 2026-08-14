<br>

# <img src="man/figures/logo.png" align="left" width="190" /> SigRepo: An R package for storing, sharing, and comparing omic signatures

![build](https://github.com/montilab/SigRepo/workflows/rcmdcheck/badge.svg)
![validation](https://github.com/montilab/SigRepo/workflows/validation/badge.svg)
![lint](https://github.com/montilab/SigRepo/workflows/lint/badge.svg)
![pkgdown](https://github.com/montilab/SigRepo/workflows/gh-pages-pkgdown/badge.svg)
![Docker pulls](https://img.shields.io/docker/pulls/montilab/sigrepo)
![Docker image
size](https://img.shields.io/docker/image-size/montilab/sigrepo)
![GitHub last
commit](https://img.shields.io/github/last-commit/montilab/SigRepo)

## What is SigRepo?

High-throughput studies produce a growing volume of **omic signatures**.
Most of them end up in supplementary tables, with inconsistent metadata
and no shared representation, so they are rarely reused and comparing a
new result against prior work stays a manual job.

**SigRepo is a platform that treats signatures as first-class, reusable
objects**: stored in a common representation with controlled metadata,
searchable, shareable, and analyzable in place rather than only
downloadable.

This repository is the **R client**. It talks to a running
[**SigRepo\_Server**](https://github.com/montilab/SigRepo_Server)
instance, which provides the MySQL database, the REST API, the web
interface, and an MCP server for AI agents. You can use the client
against
<a href="https://sigrepo.org" target="_blank"><strong>our deployed
server</strong></a> or against your own.

Signatures are represented as R6 objects defined by
<a href="https://github.com/montilab/OmicSignature"
target="_blank"><strong>OmicSignature</strong></a>, our in-house package
(GPL-3), which pairs a curated feature set with its underlying
differential-expression table and a controlled metadata vocabulary.

- <a
  href="https://montilab.github.io/OmicSignature/articles/ObjectStructure.html"
  target="_blank">Overview of the object structure</a>
- <a
  href="https://montilab.github.io/OmicSignature/articles/CreateOmS.html"
  target="_blank">Create an OmicSignature (OmS)</a>
- <a
  href="https://montilab.github.io/OmicSignature/articles/CreateOmSC.html"
  target="_blank">Create an OmicSignatureCollection (OmSC)</a>

## What you can do with the client

**Store and organize**

- Upload signatures (`addSignature()`) and collections
  (`addCollection()`), update (`updateSignature()`) and remove
  (`deleteSignature()`) them.
- Group related signatures into collections
  (`addSignatureToCollection()`), and share them with specific users
  (`addUserToSignature()`, `addUserToCollection()`).

**Search and retrieve**

- Search metadata without pulling whole objects (`searchSignature()`,
  `searchCollection()`).
- Retrieve full signatures and collections, including their difexp
  tables (`getSignature()`, `getCollection()`,
  `getSignatureFeatureSet()`).
- Browse controlled vocabularies (`searchOrganism()`,
  `searchPhenotype()`, `searchPlatform()`, `searchSampleType()`,
  `searchAssayType()`).

**Analyze**

- Compare any set of signatures (`compareSignatures()`) by feature
  overlap, rank-based Kolmogorov–Smirnov statistics, or GSEA.
- Run gene set enrichment against MSigDB with
  <a href="https://github.com/montilab/hypeR" target="_blank">hypeR</a>
  (`runHypeR()`, `prepareHypeRSignatures()`).

## Installation

    # Load devtools package
    library(devtools)

    # Install SigRepo
    devtools::install_github(repo = 'montilab/SigRepo')

    # Install OmicSignature
    devtools::install_github(repo = 'montilab/OmicSignature')

    # Load packages
    library(tidyverse)
    library(SigRepo)
    library(OmicSignature)

## Before you begin

Navigate to our
<a href="https://sigrepo.org" target="_blank">sigrepo.org</a> portal to
create an account. On the login page, click `"Register here!"` and fill
out the registration form. You will receive an email when your account
has been activated.

Each person should use their own account. Due to SQL constraints,
multiple users sharing one account (for example, several people running
this tutorial on a shared test login) will fail to connect.

## Connect to the database

Once you have an account, create a connection handler with
`newConnHandler()`:

    # Create a connection handler
    conn_handler <- SigRepo::newConnHandler(
      dbname = "sigrepo",
      host = "sigrepo.org",
      port = 3306,
      user = <your_username>,
      password = <your_password>
    )

SigRepo stores that handler internally for the current R session, so
most functions can be called either way:

    # Option 1: explicit connection handler
    SigRepo::searchSignature(
      conn_handler = conn_handler,
      signature_name = "example_signature"
    )

    # Option 2: use stored handler from newConnHandler()
    SigRepo::searchSignature(
      signature_name = "example_signature"
    )

### Accounts and visibility

There are three types of user accounts:

- `admin` has **READ** and **WRITE** access to all signatures in the
  database.
- `editor` has **READ** and **WRITE** access to only their own uploaded
  signatures.
- `viewer` has **READ-ONLY** access to publicly available signatures.

SigRepo holds unpublished and sensitive signatures, so each signature
carries a visibility flag:

- `visibility = 1` (**public**, the default) — available to every
  account.
- `visibility = 0` (**private**) — retrievable only by the owner and
  users they have granted access to.

Note the difference between the two access paths: `searchSignature()`
returns **metadata only** and is visible to everyone, while
`getSignature()` returns the signature itself and **does** enforce
visibility.

## Comparing signatures

`compareSignatures()` wraps `OmicSignature::compare_omic_signatures()`
so a set of signatures can be compared directly:

    # Compare several signatures by feature overlap
    result <- SigRepo::compareSignatures(
      signature_names = c("signature_a", "signature_b", "signature_c"),
      method = "overlap"
    )

    result$comparisons$level1_vs_level1$jaccard

Supported methods:

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Method</th>
<th>What it measures</th>
<th>Needs difexp</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>overlap</code></td>
<td>Jaccard index + Fisher exact test on the retained feature sets</td>
<td>no</td>
</tr>
<tr>
<td><code>ks_rank</code></td>
<td>where one signature’s features fall in another’s ranking</td>
<td>yes</td>
</tr>
<tr>
<td><code>ks_score</code></td>
<td>the ranking scores of those features vs. the rest</td>
<td>yes</td>
</tr>
<tr>
<td><code>gsea</code></td>
<td>GSEA enrichment (via <code>fgsea</code>), with leading edge</td>
<td>yes</td>
</tr>
</tbody>
</table>

For bi-directional signatures, comparisons are performed per matched
group label, so “up vs. up” and “down vs. down” are reported separately.

## AI and agent access

SigRepo runs a **Model Context Protocol (MCP)** server at
<https://sigrepo.org/mcp/>, so AI assistants can query the repository
directly — searching signatures, retrieving signature context, comparing
signatures, browsing gene sets, and running enrichment, all grounded in
the stored data rather than the model’s recollection.

With
<a href="https://claude.com/claude-code" target="_blank">Claude Code</a>:

    claude mcp add --transport http sigrepo https://sigrepo.org/mcp/

Every tool call takes your SigRepo `api_key` as an argument (the same
credential the REST API uses); retrieve it with `SigRepo::getAPIKey()`.
This repository also ships a `connect-sigrepo-mcp` skill under
`.claude/skills/` that handles the setup.

## In development

The following are active work and **not yet available** in the released
package:

- **AI-assisted signature authoring.** The main barrier to contributing
  is curation, not storage: a depositor has to reshape a
  differential-expression result, assign controlled metadata, and
  satisfy the schema. We are building an agent that reads a study’s
  differential-expression output and description, proposes metadata from
  SigRepo’s controlled vocabularies, and emits a validated OmicSignature
  for the depositor to review — turning contribution from a curation
  task into a review step.
- **A modernized web interface** for browsing, inspecting, and comparing
  signatures.
- **Additional external gene-set resources.** Because analyses are
  exposed as discrete API endpoints over a common signature
  representation, new resources can be added without schema changes.

## Guides

- [Uploading Signature
  Tutorial](https://montilab.github.io/SigRepo/articles/signature-tutorials.html)
- [Uploading Signature Collection
  Tutorial](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
- <a
  href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html"
  target="_blank">Setting up your own SigRepo_Server</a>

Questions, or want an account? <a href="mailto:sigrepo@bu.edu">Contact
us</a>.
