# Utopia knowledge subsystem plan

**Status:** Proposal. Nothing in this document is implemented.

## Purpose

Utopia can provide a durable, evidence-backed knowledge base for Trashtalk
agent and workflow features. Its MCP server can search documents, retrieve
entity facts, explain graph changes, and record a proposed memory. Utopia
keeps source evidence and distinguishes world time from record time.

This integration must be an optional subsystem. Trashtalk stays usable without
Utopia, Docker, Postgres, a model endpoint, or network access.

## Product boundary

Trashtalk owns local objects, workflows, agent sessions, settings, and user
interaction. Utopia owns document ingestion, semantic retrieval, graph
construction, review, and its audit ledger.

The first integration will be a narrow MCP client. It will not embed Utopia,
query its database, call undocumented HTTP routes, or copy its graph into the
Trashtalk SQLite store. The documented MCP tools and RDF export are the only
upstream contracts this subsystem can depend on.

A configured connection is a named `Utopia::KnowledgeBase` object. It contains
an endpoint, a knowledge-base ID, and a reference to credentials. It is not a
new global agent backend or an automatic source of agent memory.

## Opt-in model

The user enables the subsystem explicitly in a preferences class. Configuration
must be absent by default. Loading Trashtalk must not make an HTTP request.

The subsystem needs these settings:

- `utopia.enabled`, default `false`
- `utopia.endpoint`, with no default
- `utopia.knowledgeBaseId`, with no default
- `utopia.tokenEnv`, default `TRASHTALK_UTOPIA_TOKEN`
- `utopia.timeoutSeconds`, with a conservative default

The token remains only in the named environment variable. Preferences store no
secrets. `@ Trash doctor` reports an enabled but incomplete configuration,
missing `curl` or `jq`, an unreachable endpoint, and an invalid MCP response.
It must not print a token.

The initial subsystem exposes an explicit connection, such as
`@ Utopia::KnowledgeBase default`. No existing agent session, inbox, or
workflow reads from it until a later feature opts in by name.

## Initial read surface

Implement only read operations first. Each operation takes an explicit
knowledge-base receiver and returns a Trashtalk JSON result envelope. Preserve
the upstream structured result in `data`; do not parse prose meant for people.

| Trashtalk message | Utopia MCP tool | Intended use |
| --- | --- | --- |
| `search: query` | `search_chunks` | Find evidence-backed passages. |
| `document: id` | `get_document` | Read the source behind a search hit. |
| `entitiesMatching: text` | `find_entities` | Resolve a named entity. |
| `factsFor: entity at: date asOf: instant` | `entity_facts` | Ask what was true, and what was known, at distinct times. |
| `changesSince: start until: end` | `changes` | Review new or corrected knowledge. |

The wrapper must preserve `document_id`, entity IDs, time values, truncation,
and citations. It must never claim a complete search result when Utopia marks a
result as limited or notes incomplete historical recall.

Use the repository's existing Tool-style result envelope where it applies:
`schema_version`, `tool`, `command`, `outcome`, `exit_code`, `stdout`, `stderr`,
and `data`. Add subsystem-specific error outcomes only after comparing the
existing adapter conventions. Candidate outcomes are `disabled`,
`misconfigured`, `unreachable`, `unauthorized`, `protocol_error`, and
`upstream_error`.

## Deferred write surface

Utopia exposes MCP `remember`, but it stores a sentence and sends extracted
facts to Utopia's human review queue. This is safer than an automatic graph
write, but it remains a durable external effect.

Do not implement `remember` in the first phase. Before adding it, decide:

1. Which Trashtalk action can request a memory proposal.
2. How the UI makes the external review requirement visible.
3. Whether Trashtalk records the returned upstream document ID in a local
   workflow event.
4. Whether write tokens use a separate opt-in setting from read tokens.
5. How users revoke a connection and audit prior proposals.

No agent receives a write-capable Utopia token automatically. A later write
feature must require a visible, user-initiated action and report “proposal
recorded, review pending”, never “fact added”.

## Architecture sketch

```text
Preferences / environment
          |
          v
Utopia::UtopiaSettings ----> Utopia::KnowledgeBase
                                      |
                                      v
                          narrow JSON-RPC-over-HTTP primitive
                                      |
                                      v
                        Utopia MCP endpoint for one knowledge base
```

Use a small raw Bash boundary for `curl` and JSON-RPC. Keep request validation,
message names, response shaping, and user-facing behavior in `.trash` classes
where the current DSL can express them. Do not add a resident service or a new
runtime dependency.

The client sends stateless JSON-RPC 2.0 requests to:

```text
POST {endpoint}/api/v1/kbs/{knowledgeBaseId}/mcp
Authorization: Bearer {token}
```

It first calls `initialize` and `tools/list` during an explicit health check.
Normal methods call only the specific tool they need. The implementation must
set connection and total timeouts, pass the token by request header, avoid
shell interpolation, and redact credentials from diagnostics.

## User experience questions

These questions shape the subsystem before implementation:

- Is one default connection enough, or do users need several named knowledge
  bases for different projects and privacy boundaries?
- Which existing workflows benefit first: agent research, inbox triage, typed
  decisions, or a dedicated knowledge browser?
- Is a search result a temporary answer, or should a user be able to save a
  cited reference in a Trashtalk object?
- Where does connection setup live: preferences source only, a guided command,
  or both?
- What does disabled look like to a caller: a distinct JSON outcome, a typed
  error, or a capability query before each use?

The first implementation should answer these with one small workflow. A useful
candidate is explicit agent research: a user asks an agent to search one named
knowledge base and receives passages with document IDs and quotes. It does not
change general agent memory.

## Delivery phases

### Phase 0: contract and configuration

1. Inspect the latest Utopia MCP documentation and pin the supported protocol
   version and structured fields in tests.
2. Define `Utopia::UtopiaSettings` and the disabled-by-default behavior.
3. Decide the named-connection model and exact result envelope.
4. Add `Trash doctor` checks that never contact Utopia unless enabled.

### Phase 1: read-only vertical slice

1. Implement one connection and a test double MCP server.
2. Add `search:` and `document:` with request, response, timeout, and redaction
   tests.
3. Return structured Utopia data unchanged inside the established envelope.
4. Add an explicit command or small UI entry point for the selected workflow.

### Phase 2: graph and time-aware reads

1. Add entity lookup, facts, and change-history messages.
2. Make world time (`at`) and record time (`asOf`) separate, required concepts.
3. Define presentation for evidence, limits, and partial historical search.
4. Add an opt-in live integration test against a disposable Utopia deployment.

### Phase 3: reviewed writes, if still wanted

1. Design the user confirmation and Utopia review handoff.
2. Require a separately configured write token.
3. Implement `remember` with durable local evidence and clear pending status.
4. Test revocation, permission refusal, and prompt-injection-resistant defaults.

## Acceptance rules

The subsystem is ready for each read feature when it meets all of these rules:

- A default Trashtalk install behaves exactly as it did before the subsystem.
- An enabled but broken setup returns a useful structured outcome.
- Tests cover authentication refusal, timeout, malformed JSON, MCP error, and
  a valid structured response.
- Logs, errors, result envelopes, and process listings contain no bearer token.
- Calls use only documented Utopia MCP tools or the documented RDF export.
- A live test remains opt-in and never uses personal documents or credentials.

## Upstream constraints and risks

Utopia is Apache-2.0 and has an actively maintained MCP surface. It is also a
pre-1.0 application. Its documentation states that database migrations only
move forward and have no rollback. Treat it as an external service that users
operate and upgrade independently.

MCP supports single JSON-RPC replies, not streaming. The supported structured
read contract is limited to selected tools. Historical full-text search can be
incomplete. Utopia's SQL query feature is not available over MCP. These limits
make it suitable for evidence-backed retrieval, not a replacement for
Trashtalk's local persistence or process tools.

## Next decision

Choose the first user-facing workflow and the connection model: one configured
default knowledge base, or several named knowledge bases. That decision fixes
the initial class and settings design.
