# Headless Agent Sessions Design

**Status:** Proposed
**Date:** 2026-09-07

## Problem

Trashtalk now has two useful pieces of agent infrastructure:

- durable named `Inbox` objects and persisted `Message` objects; and
- Honker-backed SQLite notifications, queues, durable streams, consumer
  offsets, leases, and scheduling.

The existing agent-session implementation does not compose those pieces into a
reliable ambient agent system. `AgentSession` currently models a tmux session,
`Agent` sends keystrokes to an interactive provider CLI, and session state is
mostly inferred from whether that terminal process still exists. That model
cannot reliably receive structured results, recover a missed wake, distinguish
the lifetime of a conversation from the lifetime of a process, or enforce an
agent role's authority.

Trashtalk needs a model for agents that are normally headless and asleep. Such
an agent should wake when durable work arrives, run one bounded turn, perform
only authorized actions, communicate through inboxes, and return to idle. A
human should be able to focus the session temporarily, interact with it, and
dismiss the view without stopping the agent.

## Goals

1. Give agents stable identities distinct from their current role, provider,
   process, and conversation session.
2. Make sessions durable logical objects whose state survives shell and worker
   process exit.
3. Wake sessions from durable inbox messages and event-stream positions rather
   than from ephemeral notification delivery.
4. Serialize turns per session and provide at-least-once wake processing with
   idempotent actions.
5. Enforce role capabilities outside the model prompt.
6. Let agents send attributable messages to other agents and to the user.
7. Use the public Trashtalk message-send surface behind narrow agent tools.
8. Provide a temporary Innards focus surface whose dismissal is independent of
   session interruption or termination.
9. Keep external agent harnesses behind narrow CLI adapters. Trashtalk should
   not become an LLM provider or tool-loop implementation.
10. Express orchestration and policy in the Trashtalk DSL. Keep raw Bash limited
    to process, file-descriptor, signal, SQLite-extension, and OS-supervision
    boundaries.

## Non-goals

- Reintroducing the retired `tt` daemon, native Procyon compiler, plugin mode,
  or native/Bash parity work.
- Treating a tmux session or provider process as the durable agent session.
- Reimplementing provider authentication, model selection, memory, retries, or
  tool loops in Trashtalk.
- Giving a model a generic `send(receiver, selector, args)` escape hatch.
- Letting Innards access the Trashtalk database or execute session actions.
- Guaranteeing exactly-once execution. The design uses at-least-once delivery
  plus idempotency and durable receipts.
- Keeping an Innards viewport permanently open alongside an active shell
  prompt.

## Design principles

### A session is not a process

An agent session is a durable conversation and work context. Most of the time
it has no running model process. A short-lived `AgentTurn` process is created
when the session wakes and exits when that turn reaches a terminal outcome.

Process IDs, leases, and heartbeats describe the current turn or worker. They
do not identify the session.

### Notifications are hints; persisted state is authority

Honker pub/sub may reduce wake latency, but a notification can be missed while
a listener is absent. Every wake must cause a fresh query of the session's
durable inbox receipts and stream cursors. The worker may safely receive the
same wake more than once.

### Role text is not authorization

An `AgentRole` describes intended behavior and granted capabilities. A
deterministic gateway enforces those grants. Prompt instructions alone never
authorize an action.

### The sender is derived, never claimed

An agent tool may supply a recipient and message body, but it may not supply its
own `from` identity. The session gateway derives the sender identity and session
from the claimed turn and worker lease.

### Human presentation is a projection

Focusing, searching, pausing display updates, or dismissing the Innards surface
does not mutate the session lifecycle. Session lifecycle actions require
separate explicit intents.

## Domain model

### `AgentIdentity`

A stable principal representing who acted.

Suggested fields:

```text
handle             stable human-facing name
displayName        optional presentation name
owner              user or system identity responsible for it
enabled            whether new work may be routed to it
defaultRole        role used when a caller does not specify one
createdAt
```

An identity can have more than one session. Identity is used for attribution
and new-work routing; a session address is used to continue a specific
conversation.

### `AgentRole`

A versioned policy and behavior definition.

Suggested fields:

```text
name
revision
instructionsRef
instructionsHash
backendProfile
capabilities
workspacePolicy
subscriptionTemplates
turnBudget
messageBudget
approvalPolicy
createdAt
supersedes
```

A session records the exact role revision and instruction hash it began with.
Changing a role creates a new revision; it does not silently expand the
authority of an existing session. An explicit `adoptRoleRevision:` operation
may update an idle session after policy validation.

### `AgentSession`

The durable logical conversation and unit of serialized work.

Suggested fields:

```text
identity
role
roleRevision
roleHash
workspace
lifecycleState
activityState
inbox
eventStream
parentSession
externalConversationRef
createdAt
lastActivityAt
lastTurn
```

`externalConversationRef` is optional and provider-specific. It is not the
session identity or source of truth.

### `AgentTurn`

One wake-to-idle execution attempt.

Suggested fields:

```text
session
attempt
state
wakeRequest
causeMessageIds
causeEventRanges
roleRevision
workerId
leaseTokenHash
backendRunId
startedAt
heartbeatAt
finishedAt
outcome
error
```

Every tool call and resulting action carries the turn ID, correlation ID, and
causation ID.

### `AgentSubscription`

A durable routing rule from a stream to a session.

Suggested fields:

```text
session
stream
eventTypes
filter
consumerName
cursor
enabled
batchLimit
coalesceWindow
```

Filters should initially support only a small declarative vocabulary. They
must not be arbitrary Bash or model-authored predicates.

### `InboxReceipt`

Processing state for one message consumer.

Suggested fields:

```text
message
consumer
state              pending | claimed | processed | failed
attempts
claimedBy
claimedAt
processedAt
lastError
```

This is deliberately separate from `Message.status`. `unread`, `read`, and
`archived` describe presentation; they do not prove that an agent successfully
processed the message.

### `AgentAction`

A durable record of an action requested by a model and decided by the gateway.

Suggested fields:

```text
turn
kind
arguments
idempotencyKey
policyDecision      allowed | approval_required | denied
approval
state               proposed | executing | succeeded | failed | cancelled
result
createdAt
finishedAt
```

## Addressing

There are two useful recipient forms:

- an identity address routes new work to an eligible session for that identity;
- a session address continues an exact conversation.

Initially, identity routing should succeed only when there is exactly one
eligible open session. If there are none, policy may create one. If several are
eligible, routing fails with an explicit ambiguity result rather than choosing
arbitrarily.

Canonical inbox names should be derived from object identities rather than
user-entered labels, for example:

```text
agent:<agent-identity-id>
session:<agent-session-id>
user:<user-id>
```

Aliases such as `reviewer` or `chazu` are resolver inputs, not authentication.

## Lifecycle

Session lifetime and current work activity are independent.

### Lifecycle state

```text
open | paused | closed | terminated
```

- `open`: eligible to wake and run turns.
- `paused`: retain messages and events but do not begin a new turn.
- `closed`: gracefully finished; retained and inspectable but not eligible for
  new work without an explicit reopen operation.
- `terminated`: force-stopped and permanently disabled.

### Activity state

```text
idle | queued | running | waiting_for_user | failed
```

- `idle`: no current turn and no known pending wake.
- `queued`: at least one durable wake request exists.
- `running`: a worker holds the turn lease.
- `waiting_for_user`: the agent emitted a question whose answer is required
  before useful progress can continue.
- `failed`: the last turn ended abnormally and retry policy has not yet settled
  it.

### Operations

- `pause` changes lifecycle state to `paused`; it does not discard delivery
  receipts or advance event cursors.
- `resume` changes lifecycle state to `open` and reconciles pending durable
  work.
- `interrupt` signals only the current turn process. The session remains open
  or paused.
- `close` records graceful logical completion. A running turn may finish or be
  interrupted according to the explicit close mode.
- `terminate` stops the active turn, invalidates its lease, marks the session
  terminated, and prevents further wake processing.
- `dismiss` is not a session operation. It exists only as an Innards focus
  outcome.

## Durable wake architecture

```text
Inbox delivery / durable domain event
                 |
                 v
        transactional WakeRequest
          queue: agent.wakes
                 |
                 v
      AgentWorker claims request
                 |
                 v
      acquire per-session turn lease
                 |
                 v
 query pending receipts and stream cursors
                 |
                 v
             AgentTurn
                 |
                 v
 persist actions, result, receipts, cursors
                 |
                 v
       acknowledge WakeRequest
```

### Wake requests

A wake request contains references rather than a copy of the agent context:

```json
{
  "schema_version": 1,
  "wake_id": "wake_...",
  "session_id": "agent_session_...",
  "reason": "inbox_message",
  "message_ids": ["message_..."],
  "event_ranges": [],
  "correlation_id": "...",
  "causation_id": "...",
  "created_at": "2026-09-07T12:00:00Z"
}
```

Multiple pending triggers for the same idle or running session may be
coalesced. Coalescing does not acknowledge their underlying messages or event
positions.

### Transaction boundary

The durable input and its wake request should be created in one SQLite
transaction. The existing Honker persist-and-enqueue primitive is the likely
raw boundary for inbox delivery. A periodic worker reconciliation remains
necessary to repair historical data or any producer that cannot use that
transaction.

Pub/sub notification can prompt the worker to poll immediately. It never
replaces the queue or reconciliation scan.

### Worker and supervision

SQLite cannot wake a model without a living process. `AgentWorker` is therefore
a narrow, explicitly activated service which does only the following:

1. claims `agent.wakes` jobs;
2. reconciles the referenced session's durable state;
3. acquires or renews one per-session turn lease;
4. launches the configured backend through an exact-argv adapter;
5. records lifecycle events and tool decisions;
6. commits receipts and cursors; and
7. acknowledges or retries the wake job.

During development the worker may run in the foreground. Persistent operation
should be owned by launchd, systemd, or another explicit OS supervisor. This is
not a general Trashtalk daemon and should not absorb unrelated runtime duties.

If Honker is unavailable, inboxes remain durable but automatic waking is
disabled. `@ session wake` or a reconciliation command can process pending
work manually.

## Turn processing

Only one turn may run for a session at a time. Additional wake requests remain
queued and are incorporated into the next turn.

The worker builds a bounded versioned context document containing:

- session and identity IDs;
- role name, revision, instruction hash, and declared capabilities;
- workspace and project context;
- the triggering messages and event ranges;
- unresolved questions and replies;
- recent turn summaries, not an unbounded duplicate provider transcript; and
- the structured action/tool schema available for this turn.

The backend emits normalized JSONL lifecycle records to the session event
stream. Provider-specific stream formats are parsed only inside their backend
adapters.

A turn reaches success only after its result, action outcomes, inbox receipts,
and stream cursors have been durably committed. Process exit alone is not
semantic success.

## Backend contract

The existing one-shot `AxeAgent` and `CodexAgent` paths remain valid for `@@`.
Headless sessions add a provider-independent turn-driver interface rather than
changing those adapters in place.

Suggested messages:

```smalltalk
@ backend beginTurn: context eventsTo: sink
@ backend resumeTurn: externalRef context: context eventsTo: sink
@ backend interruptTurn: backendRunId
@ backend supportsResume
```

The initial implementation should use independent one-shot turns. This keeps
the current Codex backend ephemeral and read-only and does not require provider
conversation persistence.

`externalConversationRef` may later enable a backend-native resume path. It is
an optional continuity aid. Losing it must not lose the Trashtalk session,
pending messages, event positions, or audit history.

Provider auth, model configuration, prompt expansion, tool-loop behavior,
budgets, and retries remain owned by Axe, Codex, or another external harness.

## Capabilities and actions

Agent tools should be narrow adapters whose implementation invokes fixed public
Trashtalk message sends. The model may supply only the arguments allowed by the
tool schema.

Initial capabilities:

```text
events.read
inbox.read
message.send
question.ask
source.read
task.runNamed
proposal.submit
```

The model must not receive a generic receiver/selector tool.

Suggested default policy:

| Action | Default decision |
|---|---|
| Read session inbox or subscribed events | Automatic |
| Send a message to an allowed identity/session | Automatic, budgeted |
| Ask the owning user a question | Automatic |
| Read source through a scoped adapter | Role-granted |
| Run an allowlisted named project task | Role-granted or approval-required |
| Submit `SourceProposal`/`ObjectEditProposal` | Automatic submission; human review before apply |
| Mutate source or objects directly | Denied |
| Execute an arbitrary shell command | Denied |
| Send an arbitrary Trashtalk selector | Denied |
| Change identities, roles, grants, or worker policy | Denied |

The session gateway receives a short-lived turn capability or lease token via
the child environment. It derives the caller's session and identity from that
token, validates the role revision, and records every decision as an
`AgentAction`.

Agent-to-agent messages require causal-depth, rate, and turn-budget limits so a
pair of sessions cannot generate an unbounded reply loop. A message never
creates an obligation to reply unless its typed contract says so.

## Messaging protocol

Human-readable `subject`, `body`, `kind`, and thread behavior remain useful.
Agent-routed messages add attributable machine fields:

```json
{
  "schema_version": 1,
  "id": "message_...",
  "to": "session:agent_session_...",
  "from": "agent:agent_identity_...",
  "sender_identity": "agent_identity_...",
  "sender_session": "agent_session_...",
  "subject": "Tests failed",
  "body": "Two integration tests failed after the latest event.",
  "kind": "question",
  "thread": "message_...",
  "reply_to": "",
  "expects_reply": true,
  "correlation_id": "...",
  "causation_id": "event_...",
  "dedupe_key": "turn_...:action_...",
  "created_at": "2026-09-07T12:00:00Z"
}
```

Conventional kinds remain `note`, `question`, `alert`, and `result`. Kinds may
be extended through a versioned schema rather than remaining unchecked
behavioral instructions.

Questions place a session in `waiting_for_user` only when the agent declares
that the answer blocks useful progress. A reply is an ordinary durable message
in the same thread and creates a new wake.

## Session event protocol

Each session publishes a durable, ordered event stream for recovery, audit, and
human presentation. A normalized event envelope is independent of provider
output:

```json
{
  "schema_version": 1,
  "event_id": "event_...",
  "event_type": "agent.turn.started",
  "session_id": "agent_session_...",
  "turn_id": "agent_turn_...",
  "source": {"kind": "worker", "id": "agent_worker_..."},
  "occurred_at": "2026-09-07T12:00:00Z",
  "correlation_id": "...",
  "causation_id": "wake_...",
  "payload": {}
}
```

Initial event types:

```text
agent.session.opened
agent.session.paused
agent.session.resumed
agent.session.closed
agent.session.terminated
agent.wake.queued
agent.turn.started
agent.turn.output
agent.action.proposed
agent.action.decided
agent.action.finished
agent.message.sent
agent.turn.waiting_for_user
agent.turn.succeeded
agent.turn.failed
agent.turn.interrupted
```

The authoritative stream should be durable. A matching ephemeral notification
may be emitted to wake active human views.

## Innards focus surface

### Responsibility split

Add an `inagent` binary to Innards, built on its existing inline-terminal
viewport. It renders a session projection and emits user intents. It does not
open the Trashtalk database, call agent providers, deliver inbox messages, or
signal worker processes.

```text
Trashtalk ---- JSONL snapshots/events ----> inagent stdin
                         inagent UI -----> /dev/tty
Trashtalk <----- JSONL user intents ------- inagent stdout
                         diagnostics ----> stderr
```

The stream is duplex while focused. Trashtalk sends snapshots and live event
frames on the child's stdin and reads intents from stdout. All records use
`schema_version: 1`.

The current string/eval-based `Coproc` is not suitable for this trust boundary.
Add one reusable exact-argv duplex process primitive, such as
`DuplexProcess` or `Tool startArgvJson:`, and keep its raw Bash confined to
FIFO/file-descriptor/process lifecycle handling.

### Focus controls

Suggested controls:

```text
q / Esc    dismiss the view; leave session and current turn untouched
m          compose and send a message
p          request pause or resume
i          request interruption of the current turn
X          explicitly confirm and request session termination
Enter      inspect the selected event, message, or action
/          search
Space      pause display auto-scroll, not the agent
```

Example output intents:

```json
{"schema_version":1,"intent":"dismiss"}
{"schema_version":1,"intent":"send_message","body":"Continue after fixing the test."}
{"schema_version":1,"intent":"pause_session"}
{"schema_version":1,"intent":"interrupt_turn","turn_id":"agent_turn_..."}
{"schema_version":1,"intent":"terminate_session","confirmed":true}
```

Trashtalk validates and executes every intent. Dismissing terminates only the
Innards process and its temporary event bridge. Ctrl-C or terminal loss also
cancel the focus surface without being translated into an agent signal.

The final bounded viewport may be retained in scrollback. A prompt hook can
separately show a cheap one-line summary of running agents and unread results;
it is not part of the focus lifecycle.

### View-only first slice

The first `inagent` version may be a session-aware live viewer with only search,
inspection, and dismiss. Message composition and lifecycle intents can be added
after the JSONL follow path and terminal cleanup are proven. The wire protocol
should reserve those intents from the beginning.

## Public Trashtalk API sketch

The exact selectors should be refined against compiler capabilities, but the
domain-facing surface should resemble:

```smalltalk
identity := @ AgentIdentity named: 'build-watcher'.
role := @ AgentRole named: 'test-observer' revision: '1'.
session := @ AgentSession openFor: identity role: role workspace: project.

@ session subscribeTo: 'tests' events: #('test.completed' 'test.failed').
@ session wake.
@ session pause.
@ session resume.
@ session focus.
@ session interrupt.
@ session close.
@ session terminate.

@ session send: 'Please inspect the failure' to: reviewerSession.
@ session askUser: 'Should I retry the integration test?'.
```

`Agent` may remain the convenience facade and backend selector used by `@@`,
but it should not also represent a persistent identity. The existing
tmux-oriented session methods should be deprecated as the headless path becomes
usable.

## Failure and recovery

### Missed notification

The worker's reconciliation scan finds a pending `InboxReceipt`, unadvanced
subscription cursor, or unacknowledged wake request and queues the session.

### Worker crash during a turn

The turn lease expires. The queue job becomes claimable again. The next worker
loads the prior turn and either resumes a backend-supported run or creates a new
attempt. Already successful actions are identified by idempotency key and are
not repeated.

### Agent process exits without a final record

The turn becomes `failed` with an explicit incomplete-output outcome. Process
exit and a provider `agent_end` event are insufficient without the expected
normalized terminal result.

### Session is paused while running

Pause prevents another turn from starting. It does not implicitly interrupt the
current turn. An explicit pause-and-interrupt operation can compose those two
actions when desired.

### Focus process exits unexpectedly

The UI bridge is cleaned up and the terminal is restored. No session lifecycle
message is sent. The session and current turn continue normally.

### Provider conversation state is missing

The worker starts an independent turn from durable Trashtalk context and records
that provider continuity was unavailable. Pending work is not lost.

## Security and trust boundaries

1. Role revisions and capabilities are persisted and checked for every turn.
2. Agent-supplied sender identities are ignored.
3. Workspace paths are canonicalized and checked against the role's workspace
   policy.
4. Tools expose fixed selectors and typed arguments, not command templates.
5. Agent-authored shell strings are never passed to `Shell exec:`.
6. Source and object changes remain proposals until their established review
   and validation gates accept them.
7. Turn lease tokens are short-lived, scoped, and stored only as hashes in
   durable objects.
8. Event and message payloads are size-bounded and treated as untrusted input.
9. Logs redact credentials and never capture the expanded child environment.
10. Agent-to-agent causal depth, rates, and total turn budgets are bounded.
11. Termination requires a distinct explicit user action. UI dismissal never
    implies termination.

## Implementation plan

### Phase 0: freeze contracts

- Add versioned schemas for identity, role, session, turn, wake, message
  provenance, action, and normalized session events.
- Specify lifecycle transitions and rejected transitions in tests.
- Specify delivery, cursor, lease, and idempotency invariants.
- Decide the minimal declarative event-filter vocabulary.

### Phase 1: durable headless tracer bullet

- Add `AgentIdentity`, `AgentRole`, `AgentTurn`, `AgentSubscription`,
  `InboxReceipt`, and `AgentAction`.
- Refactor `AgentSession` away from tmux/process ownership.
- Add transactional message/event-to-wake enqueueing.
- Add a foreground `AgentWorker tick` that processes one wake.
- Use one independent, read-only Axe turn with no mutation tools.
- Publish normalized lifecycle events.

The acceptance journey is a `test-observer` identity and role subscribed to a
`test.completed` event. The event wakes one turn, which sends a `result` or
`question` to the user's inbox and returns the session to idle.

### Phase 2: continuous worker and recovery

- Add the explicit `AgentWorker run` loop and OS-supervision documentation.
- Add per-session leases, visibility-timeout recovery, reconciliation scans,
  retry limits, and idempotent action receipts.
- Prove that missed notifications, duplicate wakes, and worker crashes do not
  lose or duplicate semantic actions.

### Phase 3: messaging and narrow actions

- Add session-derived sender attribution and identity/session address
  resolution.
- Add `events.read`, `inbox.read`, `message.send`, and `question.ask` tools.
- Add one allowlisted named-task action and one proposal-submission action.
- Add agent-to-agent loop limits and waiting-for-user behavior.

### Phase 4: Innards focus

- Add the exact-argv duplex process primitive in Trashtalk.
- Add view-only `inagent` with snapshot, durable catch-up, live updates, search,
  inspection, dismiss, and terminal cleanup.
- Add message composition and explicit pause, resume, interrupt, and terminate
  intents.
- Add the prompt-adjacent summary only after the session projection is stable.

### Phase 5: optional provider continuity

- Add backend capability discovery for resume and interrupt.
- Store an optional external conversation reference.
- Add a separate persistent-session Codex or Axe driver without changing the
  existing ephemeral one-shot `@@` contract.
- Prove fallback to an independent turn when provider state is unavailable.

## Verification

### Automated

- Identity attribution cannot be spoofed through tool arguments.
- Role changes do not affect an existing session without explicit adoption.
- One session cannot hold two active turn leases.
- Persisted input and wake enqueue are atomic.
- A missed pub/sub notification is repaired by reconciliation.
- Duplicate wakes do not duplicate successful actions.
- Stream cursors advance only after successful durable turn settlement.
- Viewing or marking a message read does not mark its receipt processed.
- Paused sessions retain work and catch up on resume.
- Interrupt stops a turn but leaves its session open.
- Dismiss and focus-process failure leave session and turn state unchanged.
- Termination prevents subsequent wake claims.
- Agent-to-agent loops stop at configured causal, rate, or budget limits.
- Innards stdin, stdout, stderr, and `/dev/tty` remain separated.
- Innards restores the terminal after dismiss, Ctrl-C, SIGTERM, and malformed
  input.

### Human acceptance

1. Start the worker in a direct shell and under the selected OS supervisor.
2. Emit a real test event and observe a sleeping session wake, report, and
   return to idle.
3. Focus the session in Innards while a turn is active.
4. Send a message from the focus surface and observe a causally linked wake.
5. Press `q` and confirm the agent continues running or remains eligible to
   wake.
6. Interrupt a turn and confirm the session remains open.
7. Explicitly terminate a session and confirm future wakes are rejected.
8. Repeat focus/dismiss under tmux and SSH and verify scrollback and terminal
   restoration.

## Resolved decisions

- The durable session is logical and normally has no live provider process.
- Identity, role, session, and turn are separate objects.
- Role revisions are snapshotted by sessions.
- Inboxes carry addressed conversation; streams carry ordered domain and
  lifecycle events; queues carry leased wake work.
- Pub/sub is a latency hint only.
- Presentation read state is separate from processing receipts.
- Agent tools use narrow fixed public message sends.
- Sender attribution is derived from the turn lease.
- The initial backend execution model is independent one-shot turns.
- Innards owns presentation and emits intents; Trashtalk owns state and actions.
- Dismissal never interrupts or terminates an agent session.

## Deferred decisions

- The final serialized representation for capability sets and role revisions.
- Whether role definitions live primarily as persisted objects, project files,
  or generated objects backed by project files.
- The exact OS-supervision installation and activation commands.
- Whether a later provider-native conversation is resumed by default or only
  for explicitly configured roles.
- The retention and compaction policy for session event streams and turn
  summaries.
