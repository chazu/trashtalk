# Workstation event, attention, and delegation layer

**Status:** Proposed design, revised 2026-09-14.

## Decision

Trashtalk should use **Honker Stream as the durable eventing layer** for local
workstation automation. A stream is the append-only event log and Honker's named
consumer offset is the durable subscription cursor. `EventBus`/pub-sub remains
an optional lossy wake hint only.

The first release adds only two persisted domain records:

1. **`EventSubscription`**, the user's typed policy for consuming one Honker
   stream and routing meaningful records.
2. **`Attention`**, a grouped, user-visible projection of matching stream
   records, linked to an existing Inbox `Message` and its ordinary outbox /
   `Agent::Delivery` records.

This deliberately does **not** add `EventCursor`, `WorkstationEvent`,
`ActionRequest`, `Approval`, `EffectRecord`, or `ExecutionAttempt` yet. Honker
already persists stream messages and consumer offsets. Inbox, Message,
Agent::Delivery, linked Questions, Agent::Run, and the worker already model
communication and recoverable delegated work. A separately designed, restricted
executor is a prerequisite for adding approved external effects.

## Goals

- Make a local event durable, replayable, inspectable, and attributable.
- Turn repeated events into one useful attention item, not notification spam.
- Route configured attention to an existing durable agent session without
  creating, resuming, or detaching one implicitly.
- Retain the current Bash-only, SQLite-backed, single-host architecture.
- Keep the initial object model small by reusing Honker and existing messaging.

## Non-goals

- A second event log, a custom cursor table, a generic plugin host, or a
  distributed event bus.
- Exactly-once arbitrary shell effects, an always-running model, or a claim that
  current harness permissions are a security sandbox.
- Replacing Inbox, Agent::Delivery, Questions, Agent::Worker, Whisker, or Innards.

## Why Honker is the right layer

`Stream` is already a cross-process durable log over Honker: producers publish
records, named consumers read them from an offset, and acknowledged offsets
survive a crash. That is the subscription and replay primitive this design
needs. `EventBus`, by contrast, is ephemeral and is appropriate only to wake an
already-running worker or refresh a prompt view.

The resulting rule is simple:

> A stream record is authoritative. A named consumer offset records progress.
> A pub/sub notification may be lost because the worker can always replay the
> stream.

A worker reads a bounded Stream batch, creates or updates its Attention and
Message inside a Store transaction, then acknowledges the Honker offset. A
crash before acknowledgement produces a duplicate delivery attempt. That is
safe because the Store transaction uses a uniqueness key of
`(subscription, streamName, partition, offset)` on the Attention-event link.
A crash after acknowledgement has already committed the durable projection.
No transaction spans the external stream read or offset acknowledgement.

## Domain model

### `EventSubscription`

`EventSubscription` is `Persistable`. It is the only new durable policy object.
It owns configuration, target identity, grouping policy, and a stable Honker
consumer name, for example `workstation/subscription_<id>`. Honker owns the
actual offset.

```json
{
  "schema_version": 1,
  "id": "subscription_...",
  "owner": "local-user",
  "enabled": true,
  "dispatchState": "enabled",
  "streamName": "workstation.command-receipts.v1",
  "consumerName": "workstation/subscription_...",
  "adapterKind": "command-receipt",
  "filter": {"exitNot": 0},
  "debounceSeconds": 15,
  "targetIdentity": "agentidentity_...",
  "grouping": "workspace-kind-fingerprint",
  "revision": 1
}
```

Creating a subscription explicitly chooses `from-now` or `from-start`. The
adapter converts that choice to a Honker consumer offset once and records that
setup decision in the subscription. Retention/gap errors are visible
configuration attention and suspend automatic dispatch until reconciled. Editing
the stream, adapter kind, or filter creates a new revision and an explicit
consumer migration, never a silent offset reinterpretation.

### `Attention`

`Attention` is the only new durable projection. It groups related stream
records and gives them a state that Message read/archive state does not express.
It links to, rather than replaces, one root Inbox Message.

```json
{
  "schema_version": 1,
  "id": "attention_...",
  "subscription": "subscription_...",
  "groupKey": "canonical workspace + kind + fingerprint",
  "state": "open",
  "message": "message_...",
  "firstStreamOffset": 421,
  "lastStreamOffset": 429,
  "eventCount": 3,
  "lastUpdatedAt": "..."
}
```

The per-record uniqueness link may be an internal Store table/index rather than
a third domain object. It exists only to make replay idempotent and to retain the
stream coordinates behind an Attention. Detailed evidence remains in the Honker
stream and is fetched by offset when the user chooses **Inspect**.

States are `open`, `acknowledged`, `snoozed`, `resolved`, and `suppressed`.
An acknowledged or snoozed matching event appends to the same Attention without
a new alert. A resolved item creates a new group unless a stated reopen window
matches. `enabled` controls source consumption; `dispatchState` controls new
agent routing; snooze controls presentation only. Changing any of these does
not stop an active run. Existing run/session controls remain explicit.

## Event path

The first producer is a command wrapper. After a command has a known exit
status, it publishes a bounded, redacted `command-receipt.v1` JSON record to
`workstation.command-receipts.v1` through `Stream publish:`. The producer
preserves the command's exit status, signal, stdin/TTY behavior, and does not
turn an event-publication error into a false command result. Output capture is
opt-in, bounded, redacted before persistence, and rendered safely.

The worker's subscription stage is a normal `Agent::Worker tick` stage:

1. `EventSourceAdapter` opens the subscription's named Stream consumer and
   reads a fair, bounded batch outside the Store lock.
2. It validates, filters, normalizes, and CUE-vets each stream payload.
3. A short Store transaction idempotently updates Attention, persists the root
   Message and ordinary routing outbox row when policy requires it, and records
   the accepted stream coordinate.
4. After commit, it acknowledges the Honker offset. A Honker pub/sub topic may
   wake the worker immediately, but polling/replay is the recovery path.
5. Existing Agent::Queue and Agent::Worker routing handle the persisted outbox.
   Resolution includes identity session scope, canonical workspace, current
   membership, lifecycle, role policy, and budget admission. No eligible or
   ambiguous session leaves Attention visible and pending. It never creates or
   resumes a session.

Attention-to-delivery causality is stored as metadata on the Message/outbox/
delivery. Each delivery records its exact stream offset range and resolved
session ID. Thus session replacement cannot rewrite history, and linked
Questions continue to release only their specific blocked deliveries.

## Source adapter hierarchy

Use an abstract `EventSourceAdapter` superclass with a closed class-side
registry. A trait is not the primary abstraction because `adapterKind` needs one
authoritative mapping to a polymorphic source implementation. Adapters are
stateless code classes. Stream, Subscription, Attention, Message, and Delivery
hold state.

```smalltalk
EventSourceAdapter subclass: Object
  classMethod: kind [ ^ '' ]
  classMethod: validateSubscription: subscription [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]
  classMethod: consumerFor: subscription [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]
  classMethod: read: consumer limit: limit [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]
  classMethod: normalize: streamRecord for: subscription [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]
  classMethod: groupKeyFor: event [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]
  classMethod: displayFor: event [ @ EventSourceAdapterError signal: 'abstract adapter selector' ]

CommandReceiptSourceAdapter subclass: EventSourceAdapter
```

`EventSourceAdapter forKind:` is a DSL allowlist. It never dispatches a
subscription-provided class name, shell command, path, CUE import, or selector.
The base class owns registry lookup, bounded batch envelopes, stream-coordinate
validation, and diagnostics. Subclasses use `rawClassMethod:` only at an actual
OS boundary. The first adapter only decodes Honker stream records, so it should
need little raw code.

Future `IntervalSourceAdapter` or `GitSnapshotSourceAdapter` classes require a
new producer stream and explicit replay, privacy, portability, and retention
tests. Traits are allowed later only for proven shared implementation such as
safe bounded artifact projection. They never own registry dispatch,
authorization, cursor/offset semantics, or lifecycle.

## CUE contracts

CUE is the declarative structural contract, not a persistence engine or
permission system. A repository-owned `schemas/workstation/v1` package defines
closed variants for command receipts, subscriptions, attention projections,
message/outbox causal metadata, and public Innards records. The same fixtures
are used by producer, consumer, browser, and tests.

CUE validates shape, variants, ranges, schema versions, and explicit
constructor defaults. Store transactions, Agent::Access, `Require`, and the
worker still enforce identity ownership, role authorization, workspace
canonicalization, uniqueness, lifecycle transitions, budgets, and liveness.
CUE success never authorizes routing or execution.

Validation occurs before the short acceptance transaction. The CUE CLI is never
run while holding the Store lock. Schema source is trusted and checked in;
stream payloads are data, never CUE imports. Missing CUE, schema digest mismatch,
or invalid schema fails closed for enabled workstation subscriptions while
leaving existing Stream evidence and Attention records inspectable. Diagnostics
are bounded and redact payload details.

## Attention UX

Whisker shows compact counts such as `!2 ?1`; it does not print every event.
Inbox and Agent::Browser offer an Attention view with **Inspect**, **Focus**,
**Acknowledge**, **Snooze**, **Resolve**, and **Suppress**. Inspect reads the
referenced stream range and causal message/run records. Focus attaches to the
recorded session only and never starts, resumes, stops, or detaches work.

Notifications are hints. Inbox, Attention, and the durable Honker stream remain
the source of truth. The fallback without Innards retains structured inspection
and explicit actions.

## Delegated action boundary

This design stops at observation, attention, and ordinary agent delegation.
Current Jcode, Maki, and Codex harnesses have user-level workspace access, so
role checks are cooperative controls rather than isolation. We will not add
approval/effect objects or advertise safely enforced local mutation until a
separate design provides an observation-only harness plus a restricted executor
channel which alone holds the relevant OS capability.

An agent may summarize, investigate, propose a SourceProposal, or ask a linked
Question under existing contracts. Agent-originated command receipts carry
origin delivery/run metadata. Default subscription policy rejects routing an
event to an ancestor delivery and has a small lineage-depth limit, preventing a
failing agent-run test from recursively waking the same agent.

## Phased implementation

### Phase 0: prove the thin model

Add CUE schemas and fixtures, `EventSubscription`, `Attention`, Stream
coordinate idempotency storage, display columns, and migration tests. Reuse
Honker Stream consumer offsets. Add no watcher and no model dispatch.

### Phase 1: command receipts and attention

Implement the receipt producer, `CommandReceiptSourceAdapter`, worker stage,
attention grouping, Inbox root message, Whisker count, inspect/focus/snooze
views, and replay tests. Ship automatic agent routing disabled by default.

### Phase 2: guarded routing

Enable per-subscription routing only after causal metadata, session-scope
resolution, recipient/message/run budget admission, duplicate suppression, and
lineage limits are tested. A missing, ambiguous, paused, or unauthorized target
must remain visible as Attention without creating work.

### Phase 3: additional producers

Add one interval or Git/filesystem producer at a time. Each publishes a versioned
Honker Stream record and supplies explicit gap, debounce, privacy, and replay
behavior. Do not create a generic plugin mechanism.

### Phase 4: separately reviewed effect system

Only after a real restricted executor exists, design immutable requested action,
approval, logical effect, and attempt records. That work is intentionally out of
scope here.

## Acceptance criteria

- A command receipt survives worker restart and is processed at least once from
  the same named Honker consumer offset.
- A crash before ack may replay, but creates no duplicate Message, outbox row,
  Attention count, or agent delivery for the same stream coordinate.
- No custom cursor/event table duplicates Honker Stream's message/offset model.
- Repeated matching failures append to one Attention according to its state
  transition table. Distinct subscriptions/projects never merge groups.
- Disabled, snoozed, and routing-suspended states have distinct tested effects.
- No subscription path holds the worker lock while reading Stream, invoking CUE,
  or rendering diagnostics.
- Unknown adapter kinds, invalid closed CUE variants, stream gaps, and schema
  digest mismatch fail closed without losing inspectable evidence.
- Focus/preview/acknowledge do not settle a delivery, answer a Question, or
  change session/worker lifecycle.
- An agent-generated receipt cannot recursively re-dispatch to its ancestor.
- Tests use local Honker fixtures and shell commands only. They need no paid
  model, desktop daemon, or live filesystem watcher.

## Success measure

A person runs a failing test, continues working, notices one compact prompt
indicator, opens a grouped attention item, sees the exact durable receipt and
causal session, and can delegate or answer without manually pasting output into
a chat. Restarting the worker or replaying a stream offset does not create
extra work. That useful, recoverable journey matters more than supporting many
event sources.

## Implemented operations (Phases 0 to 2)

See [workstation operations](workstation-operations.md) for the thin durable model,
private coordinate index, command receipts, the worker stage, local attention,
and guarded routing. Consumer progress remains Honker Stream offsets, not an
EventCursor. Phase 2 routing sends work only to an existing eligible session
and records lineage for loop prevention; Phases 3 and 4 remain proposals. The
compact prompt count described under [Attention UX](#attention-ux) is published
to `run/attention` by the worker and read by a Whisker file segment; see the
[guide](workstation-guide.md#in-your-prompt).
