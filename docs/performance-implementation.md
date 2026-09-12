# Performance implementation, September 2026

Status: in progress. Baseline: `e23500b`.

The survey measured public operations with disposable databases and fake UI
and Jcode endpoints. It identified repeated JSON/process work in browsers and
message delivery, per-token serialization, redundant read transactions, and
whole-history transcript refreshes. Implement in this order:

- [x] Batch tokenizer serialization; retain exact token values and positions.
- [x] Use indexed session snapshot predicates, then batch session projections.
- [x] Add reusable object projection support and batch object browsers.
- [x] Batch inbox presentation, participant/date lookup, and preview projection.
- [ ] Consolidate conversation admission and optimize fresh read-only validation.
- [ ] Cache transcript projections per view and consume appended log records.
- [ ] Reduce repeated message field initialization and delivery serialization.
- [ ] Run full compiler/runtime verification and measure public interactions.

Keep domain behavior in the DSL and use small shared primitives for JSON, SQL,
filesystem and transport boundaries. Public message results, live object-cache
overlays, immediate initial persistence, declared property order, Unicode
presentation, and read-after-display semantics remain contracts.

Access decisions must use current owner/enabled/session membership state.
Consolidation retains the authoritative check under the worker lock; a cached
transcript never grants authority. Store conflict guards, atomic message/outbox
publication, exact input receipts, and no replay after ambiguous failure remain
required. Transcript caches are derived, scoped to a temporary view, and must
recover from file replacement/truncation, partial records, and window changes.

Broader result ABI changes and default-on value sends are deferred. The survey
did not qualify them for these workloads. Class preloading was neutral.

Each slice records meaningful regression coverage here or in the performance
documentation. Timing runs follow correctness checks, use fresh runtimes and
isolated state, and avoid concurrent builds/tests. Fixture timings exclude model
latency and terminal rendering.

Tokenizer validation: 76 focused checks and all 47 compiler test files passed.
The batch serializer propagates failures and preserves opaque values (including
newlines, Unicode and control characters) and numeric source locations.

Browser projection validation: 28 object browser checks, 30 agent browser checks,
and dedicated batch tests passed. Session snapshots share one indexed query and
retain request order. `Json projectEach:with:` builds JSONL from data templates
(`_at`, `_concat`, `_literal`); `Json writeDocuments:in:` validates preview
basenames and decodes the whole batch before writing. Templates contain no
executable expressions. Object projections retain the existing Runtime live
cache overlay and share scalar formatting functions. Tests cover typed and
opaque values, Unicode, declaration order, empty batches, failure propagation,
preview parity, lifecycle controls, and all six indexed subqueries.

Inbox validation: all 47 compiler test files, 52 inbox interaction checks, 21
presentation checks and a 200-message regression passed. `jsonRows:into:`
decodes selected fields across an array once and preserves inline block scope,
returns, failure propagation, typed text and opaque strings. Inbox loading now
uses one ordered reload, one query for distinct participants, and shared calendar
values from Bash strftime. The 200-row test asserts bounded serializer counts,
full preview text, fresh cache replacement, and unchanged unread state. Calendar
values match the existing Time API in UTC and New York across DST transitions.
