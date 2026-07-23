---
name: connect-sigrepo-mcp
description: Registers SigRepo's MCP server (https://sigrepo.org/mcp/) with Claude Code if it isn't already added, asks the user for their SigRepo api_key, and carries it forward for the rest of the session so every MCP tool call (search_signatures, run_enrichment, etc.) includes it automatically. Use this whenever the user asks to "connect to SigRepo's MCP," "set up SigRepo MCP," "use the SigRepo MCP tools," or wants to start querying/searching SigRepo signatures, collections, gene sets, or run enrichment via MCP but hasn't already supplied an api_key this session.
---

# Connect to SigRepo's MCP server

SigRepo's MCP tools have no login of their own to configure -- the proxy in
front of them (`mcp/proxy/nginx.conf`) does rate limiting only, no auth.
Every tool call instead takes `api_key` as a plain argument, the same
SigRepo account credential already used for the REST API and the Shiny
app login. So "connecting" is really just two things: point Claude Code at
the URL, and get the person's api_key into context for this session.

## Step 1: Register the MCP server, if it isn't already

Check first -- don't add it blindly if a previous session already did:

```bash
claude mcp list
```

If `sigrepo` isn't in the list, add it:

```bash
claude mcp add --transport http sigrepo https://sigrepo.org/mcp/
```

No `--header` flag, no token, no username/password -- there's nothing else
to configure at this layer.

## Step 2: Ask the user for their api_key

Ask directly, in chat (don't use a multiple-choice prompt for this --
it's a free-text secret):

> "What's your SigRepo api_key? (The same one you use to log into the
> Shiny app or call the REST API.)"

Wait for their reply before continuing. Ask fresh every time this skill
runs -- sessions don't share state, so a key from an earlier conversation
isn't available here even if the person mentioned it before.

## Step 3: Carry it forward, don't persist it

Once you have it, include it as the `api_key` argument on every
`mcp__sigrepo__*` tool call for the rest of *this* conversation -- don't
ask again mid-session. Keep it in conversation context only; don't write
it to a file, and don't echo it back after this point.

## Step 4: Orient them

The registered server exposes 9 tools: `list_vocabulary`,
`search_signatures`, `get_signature_context`, `compare_signatures`,
`search_collections`, `search_geneset_resources`, `search_geneset_entries`,
`search_features`, and `run_enrichment`. A reasonable starting point if the
person hasn't said what they want yet: call `list_vocabulary` to show what
organisms/phenotypes/assay types actually have data, then `search_signatures`
from there.

Rate limiting is ~1 request/2s per IP with short bursts allowed (enforced
at the proxy, unrelated to the api_key) -- a `429` response means to slow
down, not that the key is wrong.
