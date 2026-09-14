# Workstation event layer: phased implementation plan

**Status:** Proposed task plan, 2026-09-14.

This is the execution companion to
[the workstation event, attention, and delegation design](workstation-event-attention-delegation.md).
It deliberately starts with the smallest durable model built on existing Honker
`Stream`, Store, Inbox, AgentQueue, and AgentWorker contracts.

## Delivery rules

- A phase is not “mostly done”. Its acceptance gates must pass before beginning
  the next phase.
- Use `Persistable`, `Store transaction:`, `Require`, `AgentAccess`, and the
  existing Honker `Stream` wrapper. Do not add raw SQLite or a second event log.
- `EventSubscription` and `Attention` are the only new persisted domain classes
  in Phases 0 and 1. Honker owns stream messages and consumer offsets.
- Source adapters are stateless `EventSourceAdapter` subclasses. A raw method is
  permitted only at a real OS/Honker boundary.
- All fixtures use a disposable SQLite/Honker store. No paid model, desktop
  daemon, live watcher, or uncontrolled shell command is required.
- Commit each independently testable task. Keep migrations backward-compatible
  and document recovery for every new state transition.

## Dependency map

```mermaid
flowchart LR
  P0[Phase 0: contracts and thin durable model]
  P1[Phase 1: command receipts and attention]
  P2[Phase 2: guarded agent routing]
  P3[Phase 3: additional event producers]
  P4[Phase 4: separately reviewed executor]
  P0 --> P1 --> P2 --> P3
  P2 --> P4
```

## Phase 0: contracts and thin durable model

### Purpose

Establish the two persisted records, CUE contracts, state invariants, and
Honker-coordinate idempotency storage without producing, consuming, or routing
a real workstation event. At completion, developers can create, validate,
inspect, transition, and migrate subscriptions/attention records in a
isolated database. The worker, Inbox, and agents are unchanged.

### Explicitly excluded

- No command wrapper or `CommandReceiptSourceAdapter`.
- No `AgentWorker` subscription tick stage.
- No Stream read/ack in production logic.
- No Inbox message/outbox publication, Whisker count, notification, or agent
  dispatch.
- No automatic action, approval, executor, filesystem watcher, or interval
  source.

### Task 0.1: establish the schema package and validation harness

**Deliverables**

- Add `schemas/workstation/v1/` with closed CUE roots for:
  - `#EventSubscription`
  - `#Attention`
  - a common stream-coordinate envelope
  - the future `command-receipt.v1` payload and safe display projection
- Include valid and invalid JSON fixtures next to the CUE package.
- Add a small test helper which invokes existing `Tools::Cue vet:json:` and
  reports CUE field-path diagnostics without echoing sensitive fixture content.
- Add a documented schema digest command for the package. The digest becomes
  metadata on the eventual feature policy revision, not a global runtime switch.

**Constraints**

- Definitions are closed for policy/security records; unknown fields fail.
- CUE validates structure only. It does not replace Store uniqueness, ownership,
  canonical paths, lifecycle, or authorization checks.
- CUE must not run inside a Store transaction or worker lock.
- A missing CUE binary fails the workstation feature's create/validate command
  clearly. It must not affect unrelated Trashtalk classes.

**Tests**

- Every valid fixture vets and every invalid fixture fails with a bounded
  diagnostic.
- Wrong `schema_version`, unknown fields, invalid state, malformed IDs, invalid
  debounce range, and malformed stream coordinates fail.
- The same fixture can be read as JSON by the normal Trashtalk JSON helpers.

### Task 0.2: define the `EventSubscription` persisted class

**Deliverables**

- Add `EventSubscription subclass: Object include: Persistable` with typed
  fields for owner, enabled flag, dispatch state, stream name, consumer name,
  adapter kind, filter JSON, debounce, target identity, grouping policy,
  revision, schema version/digest, timestamps, and explicit initial-position
  choice.
- Add DSL-facing class messages for create, read, list by owner, pause/resume
  dispatch, disable/enable, and inspectable summary/display projection.
- Add one transaction-only create/update selector that CUE-vets before the
  transaction and performs native semantic checks inside it.
- Canonicalize the workspace/target policy at the existing AgentAccess boundary
  when those fields are introduced, but do not resolve an agent session yet.

**Invariants**

- Consumer name is deterministic from immutable subscription ID and cannot be
  edited after creation.
- Only allowlisted adapter kinds and grouping policies are accepted.
- `enabled` and `dispatchState` are distinct. Neither mutates a Honker offset in
  Phase 0.
- A revision-changing update preserves immutable creation/consumer identity and
  writes an audit note/reason.
- Unknown CUE data, invalid owner, or malformed policy never persists.

**Tests**

- Create/reload/list round trip, including display columns.
- Duplicate ID/consumer-name rejection and invalid state transition rejection.
- CUE-valid but semantically invalid records, such as a non-allowlisted adapter,
  are rejected by native validation.
- Existing database records/classes remain readable after installing the schema.

### Task 0.3: define the `Attention` persisted class and state machine

**Deliverables**

- Add `Attention subclass: Object include: Persistable` with subscription,
  group key, state, optional root Message reference, first/last Stream
  coordinate, event count, snooze time, resolution/suppression note, timestamps,
  schema version/digest, and last resolved session as presentation metadata.
- Implement the transition table in one transactional selector:
  `open`, `acknowledged`, `snoozed`, `resolved`, and `suppressed`.
- Add DSL messages for acknowledge, snooze-until, resolve-with-note, suppress,
  reopen, summary, and safe display projection.
- Keep Message optional in this phase. Phase 1 creates and links it atomically.

**Invariants**

- A valid state transition is explicit and records actor/time/note when needed.
- Snooze requires a future normalized timestamp. Resolve/suppress require a
  nonempty reason. Acknowledging does not resolve.
- Group key is opaque canonical text at this layer. Its computation belongs to
  an adapter in Phase 1.
- Lifecycle actions never settle deliveries, answer Questions, or change a
  session/run state.

**Tests**

- Table-driven legal and illegal transition tests.
- Reload after every transition preserves state and display columns.
- No lifecycle action creates a Message, AgentDelivery, AgentRun, or Honker
  offset side effect.

### Task 0.4: add coordinate idempotency as internal storage

**Deliverables**

- Add a narrow internal Store schema/index mapping
  `(subscription_id, stream_name, partition, offset)` to `attention_id`.
- Expose transaction-only operations: `claimedCoordinate:`, `recordCoordinate:
  forAttention:`, and a bounded coordinate-range lookup for inspection.
- Do not model this mapping as a third public Persistable object.

**Invariants**

- The composite key is unique at the database boundary.
- Recording a coordinate and updating Attention count/range occur in the same
  Store transaction.
- A duplicate coordinate returns the existing Attention link and makes no
  counter or state change.
- Coordinates from different subscriptions or streams are never equal for
  idempotency purposes.

**Tests**

- Replay the same coordinate concurrently and prove one stored link/count.
- Roll back a forced transaction failure and prove neither link nor Attention
  update remains.
- Adjacent offsets extend the range; an out-of-order offset retains correct
  minimum/maximum and count.

### Task 0.5: create the adapter base and closed registry

**Deliverables**

- Add abstract `EventSourceAdapter` and the class-side contract from the design:
  `kind`, `validateSubscription:`, `consumerFor:`, `read:limit:`,
  `normalize:for:`, `groupKeyFor:`, and `displayFor:`.
- Implement `forKind:` as an explicit DSL allowlist. Initially it returns no
  production adapter or a test-only fixture adapter behind test setup.
- Add a fixture adapter that returns supplied normalized records without reading
  Stream. It validates the worker-facing envelope but performs no persistence.
- Add `EventSourceAdapterError` diagnostics for abstract/unknown calls.

**Invariants and tests**

- A persisted adapter class/selector/shell string can never determine dispatch.
- Unknown/disabled/schema-incompatible kinds fail before any source read.
- Base/fixture adapter output is bounded and includes stream coordinates.
- No adapter class writes Store records, sends Message, calls AgentQueue, or
  acknowledges a Stream offset.

### Task 0.6: migration, browser, and doctor integration

**Deliverables**

- Add idempotent schema migration installation to the existing migration
  mechanism. New tables/indexes are created once per database.
- Add `@ Trash doctor` checks for Honker Stream availability and CUE availability
  scoped to this feature, with remediation text and no automatic install.
- Add browser record projection tests for Subscription and Attention property
  columns. The generic instance inspector must show declared values on Enter.
- Add a concise operations/design cross-link explaining that Stream offsets,
  not an `EventCursor` object, represent consumer progress.

**Tests**

- Fresh database, database with unrelated persisted classes, and repeated
  migration install all succeed.
- Missing Honker/CUE reports unavailable feature capability but does not break
  basic build, Inbox, or existing agent operation tests.
- Browser projection never includes raw event payload/output or secret artifact
  contents.

### Phase 0 acceptance gate

Phase 0 is complete only when:

1. `EventSubscription` and `Attention` can be created, validated, reloaded,
   inspected, and transitioned with CUE plus native validation.
2. The coordinate index proves idempotent and transactional under replay tests.
3. No runtime code consumes/acks a production Stream or routes an Inbox message.
4. Existing full agent/browser/Store suites pass with and without optional CUE
   and Honker availability where their current contracts permit it.
5. The API, migration, CUE fixtures, and state table are documented before Phase
   1 introduces a single receipt producer.

## Phase 1: command receipts and local attention

### Purpose and boundary

Make one real, local event journey work without starting an agent: a wrapped
command publishes a bounded receipt to a durable Honker Stream; the worker
consumes it at least once; matching failures become one grouped Attention and
one ordinary user-Inbox Message; the user can inspect, acknowledge, snooze,
resolve, or suppress it. **No Phase 1 code may create AgentDelivery, route to
an identity/session, start a harness, or resume paused work.**

### Task 1.1: command receipt producer

Add a public exact-argv command wrapper, not an `eval` helper. It executes the
requested argv in a specified canonical working directory, preserves child exit
status/signal/stdin/TTY behavior, and publishes one closed
`command-receipt.v1` record through `@ Stream named: ... publish:` after the
child has a known outcome.

- Stream name is `workstation.command-receipts.v1`; the payload carries a
  schema version, stable receipt ID, canonical workspace, safe command label,
  exit status, timestamps, bounded/redacted summary, and optional origin
  metadata.
- Output capture is opt-in, bounded, redacted before persistence, and rendered
  as plain safe text. The default carries no raw stdout/stderr artifact.
- Publication failure is reported separately but never changes the child exit
  status into a false success/failure.
- A local fixture producer exercises the same public payload path without
  executing a command.

**Tests:** exact argv preservation, exit/signal preservation, success/failure
payloads, redaction/size limits, invalid payload rejection, publication failure,
and no shell interpretation of user-provided argv.

### Task 1.2: production `CommandReceiptSourceAdapter`

Promote only `command-receipt` into `EventSourceAdapter forKind:`. Implement
its class-side contract using a named `Stream` consumer:

- validate the subscription's closed typed filter and stream name;
- read a fair bounded batch of Honker records outside the Store lock;
- reject malformed/unknown-schema payloads with bounded diagnostics;
- normalize receipt records, apply `exitNot` filtering, create the canonical
  group key (`workspace + command label + normalized failure fingerprint`), and
  provide a safe display projection;
- return stream name, partition, and offset unchanged for idempotency.

It does not write Store records, publish Messages, acknowledge offsets, or
invoke AgentQueue. Fixture adapter availability remains test-only.

**Tests:** valid/filtering/invalid records, offset/partition preservation,
bounded batches, stable group keys, cross-workspace non-grouping, and no Store
or Inbox side effects from direct adapter calls.

### Task 1.3: worker subscription stage and replay-safe acceptance

Add a bounded, fair subscription stage to `AgentWorker tick`, after existing
recovery work and before any optional agent routing. It lists only enabled
`command-receipt` subscriptions, reads each named consumer outside the worker
lock, and processes a capped number of records per tick.

For each normalized matching record, a short Store transaction must:

1. claim its `(subscription, stream, partition, offset)` coordinate;
2. find/create the applicable Attention and update its count/range according to
   its state-transition policy;
3. create the root user-Inbox Message only when the group first requires one,
   or update its safe summary through a public Message/Inbox boundary;
4. persist causal stream coordinates on the Attention and Message metadata.

Only after commit does the worker acknowledge the named Honker consumer offset.
Duplicate/replayed coordinates are acknowledged without changing attention,
message, count, or state. A source error, CUE failure, or Store conflict leaves
the consumer offset where it was: the record is retried on the next tick, the
worker reports the subscription ID, and later records for that subscription
wait behind it. Nothing skips a record automatically; an operator replays or
acknowledges past a poison record explicitly after inspection.

The stage runs outside the agent OS lock (source reads and CUE never hold it),
reuses one durable `Stream` consumer handle per subscription, and installs the
feature schema once per worker process. Repeated CUE validation of identical
bytes within one runtime is a process-local memo keyed by schema digest, root,
and document hash; failures are never memoized.

**Tests:** the whole local journey through a supervised `AgentWorker tick`,
replay after commit-before-ack, acknowledgement failure after commit, rollback
of a failed projection, malformed records that block later records without
echoing payload, disabled subscriptions, batch bounds, and the absence of any
AgentDelivery, AgentRun, session, identity, or outbox side effect.

### Task 1.4: local Inbox projection and attention controls

The root Message is an ordinary `alert` in the owner's human Inbox with
`dispatchMode: manual`, created through a transaction-only Inbox boundary that
cannot reach the outbox, AgentQueue, or a foreground worker tick. Its subject
and body are the safe display projection plus the event count; causal first/last
stream coordinates and the count are Message metadata. Later events in the same
group update that one Message instead of sending another. A Honker wake hint is
emitted at most once per subscription debounce window, only after commit, and
only while the Attention is open (an expired snooze counts as open); read and
archive status on the Message never changes Attention state.

The Message exposes `inspectAttention`, `acknowledgeAttention`,
`snoozeAttentionUntil:`, `resolveAttentionWithNote:`, `suppressAttention:`, and
`reopenAttention`, each scoped to the current human owner. `Inbox attentionCount`
(and `Attention localOpenCount`) report open or due-snoozed Attention for the
owner's subscriptions as the compact count for status displays. Resolving a
group and receiving a later matching failure opens a new Attention and a new
root Message; suppressed groups keep counting silently.

**Tests:** count and message identity across acknowledge, snooze, expiry
inspection, suppress, reopen, and resolve; hint debounce; owner scoping.

### Phase 1 acceptance gate

Phase 1 is complete only when:

1. `bin/trash-command` publishes exact-argv receipts with preserved exit/signal
   behavior and safe bounded output policy, and `bin/trash-receipt` exercises
   the same payload path without executing anything.
2. `CommandReceiptSourceAdapter` is the only production adapter and has no
   Store, Inbox, offset, or AgentQueue side effects when called directly.
3. A supervised `AgentWorker tick` turns a failing receipt into exactly one
   Attention and one owner-Inbox Message, replays are idempotent, and every
   failure path leaves the consumer offset unacknowledged.
4. No Phase 1 code path creates AgentDelivery, AgentRun, AgentSession,
   AgentIdentity, or outbox rows.
5. Operations documentation covers subscription setup, the worker stage, the
   Message controls, the count, stuck-record recovery, and worker restarts.

## Phase 2: guarded agent routing

### Decision and boundary

Phase 2 adds delegation to the local Attention journey in small releases. It
does **not** add repository-scoped subscriptions. A subscription continues to
consume the local command-receipt stream. Each receipt's canonical `--cwd` is
the message/run context and is checked against the eventual role workspace
policy. This keeps the subscription model simple while still running delegated
work in the directory that actually produced the receipt.

The user must opt in. Existing subscriptions remain local-only unless they name
a target identity and enable delegation. Phase 2 sends existing durable work to
an existing eligible session. It never creates/resumes a session, broadens a
role, executes an unapproved OS effect, or treats a model harness as a sandbox.

### Phase 2A: target configuration and dry-run admission

**Deliverable:** an `EventSubscription` may name one target `AgentIdentity`,
but nothing is delivered yet. Add a public factory/configuration message and a
`routingStatusFor:` projection for an Attention.

The dry run resolves, without mutation: subscription delegation state, target
identity/session scope, receipt canonical `cwd`, role workspace authorization,
required receive capability, recipient/message/run budget admission, and an
eligible current session. Gusgus resolves its identity-scoped current session;
a specialist follows its own scope policy. Missing, ambiguous, paused, closed,
unauthorized, or budget-exhausted candidates return a structured reason and
leave Attention unchanged.

**Acceptance:** identity- and workspace-scoped fixture targets, canonical cwd
authorization, every ineligible reason, and a valid dry-run result create no
Message, outbox, AgentDelivery, AgentRun, or lifecycle change.

### Phase 2B: explicit one-attention delegation

**Deliverable:** the user selects one Attention and chooses **Delegate to
configured agent**. It creates one causal agent-facing Message and outbox row
through the existing transactional Inbox/AgentQueue publication boundary.

The transaction records Attention ID, exact Honker coordinates, receipt cwd,
target identity, resolved session, and lineage root/depth. Its uniqueness key
is `(attention, target identity, delegation revision)`. Duplicate clicks,
worker replay, and crash retry return the existing durable publication rather
than creating another delivery. Revalidate dry-run admission in this
transaction.

**Acceptance:** manual delegation reaches one eligible existing session with
its receipt cwd, survives worker restart replay, and proves repeated clicks
create no second outbox/delivery. Changed admission causes no publication.

### Phase 2C: opt-in automatic routing and loop controls

**Deliverable:** an explicitly confirmed subscription may automatically
delegate newly accepted Attention records. The default remains off.

Before publishing, atomically recheck target identity, workspace/recipient
policy, message/run budget, subscription enabled state, and delegation dispatch
state. Preserve origin metadata. Reject a receipt whose lineage contains the
target/root or exceeds the configured depth. It remains visible locally with a
loop-prevention reason. More receipts appended to an already assigned Attention
do not create a fresh prompt without an explicit re-delegate/retry policy.

**Acceptance:** default-off/opt-in, budget races, session replacement, grouped
failures, recursive origin, lineage depth, and restart replay yield at most one
eligible delegation.

### Phase 2D: attention-to-conversation operations and UAT

**Deliverable:** Attention details show routing status, target, stream
coordinates, message/delivery/run links, and the next action. **Focus delegated
conversation** attaches to the eligible existing session without resuming,
stopping, or otherwise changing it.

Document a disposable UAT: configure a target, run a failing command in a
chosen cwd, inspect its Attention, manually delegate, observe one delivery in

## Phase 3: additional producers

Add one producer at a time, beginning with interval or Git state. Each producer
publishes a closed, versioned record into its own Honker Stream and supplies its
own privacy, retention, debounce, replay, and portability tests. Do not add a
generic plugin ABI.

## Phase 4: restricted effects, separately designed

Do not reuse this plan to introduce mutation/approval. A future plan must first
provide an observation-only harness and a restricted executor that owns the OS
capability. Current harness role checks are cooperative controls, not a sandbox.

## Task rundown for Phase 1

| Task | Main result | Dependency |
| --- | --- | --- |
| 1.1 | `bin/trash-command`, `bin/trash-receipt`, `CommandReceipt` producer | Phase 0 |
| 1.2 | `CommandReceiptSourceAdapter` and `Stream consumerNamed:consumer:` | 1.1 |
| 1.3 | `WorkstationWorker tick` stage inside `AgentWorker tick` | 1.2 |
| 1.4 | Inbox/Message projection, controls, and count | 1.3 |

## Task rundown for Phase 0

| Task | Main result | Dependency | Estimated implementation shape |
| --- | --- | --- | --- |
| 0.1 | CUE package, fixtures, validation helper | existing `Tools::Cue` | docs/schema + test helper |
| 0.2 | `EventSubscription` class and native validation | 0.1 | DSL class + migration + tests |
| 0.3 | `Attention` class and transition table | 0.1 | DSL class + tests |
| 0.4 | private coordinate idempotency index | 0.2, 0.3 | narrow Store schema/transaction boundary |
| 0.5 | adapter superclass and closed registry | 0.2 | DSL base + fixture adapter tests |
| 0.6 | migrations, doctor, browser/docs | 0.1–0.5 | integration tests and projections |

The work should land in this order. Tasks 0.2 and 0.3 can be developed in
parallel after 0.1, but 0.4 must integrate both before its replay guarantees
are meaningful. Phase 0 should stay intentionally boring: it proves the data
contracts before any event can wake, notify, or delegate work.
