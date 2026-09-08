# Headless Agent Sessions Design

**Status:** Proposed; revised after document review
**Date:** 2026-09-07

## Problem

Trashtalk has durable named `Inbox` objects and persisted `Message` objects,
plus Honker-backed SQLite notifications, queues, durable streams, consumer
offsets, locks, and scheduling.

The existing session path does not compose those pieces into a reliable
headless agent system. [AgentSession](../trash/AgentSession.trash) maps
conversations to tmux sessions, and [Agent](../trash/Agent.trash) can send
keystrokes to interactive CLIs. Terminal existence cannot establish whether
work was received, completed, or recovered.

Trashtalk needs agents that work headlessly, receive durable messages and
events, and remain available between assignments. A wake delivers work to an
external harness, which may pursue it across many model turns and tool calls.
The harness can continue until completion, a blocking question, interruption,
or a configured limit; it does not need another agent to relay every turn.
A human can focus the session temporarily and dismiss the view without
stopping its work.

This document proposes that composition. The new domain objects, backend
protocol, supervision integration, and Innards applets below are not yet
implemented. Existing source capabilities and gaps are identified explicitly.

## Goals

1. Separate agent identity, behavioral intent, authority, durable session,
   harness execution, and temporary human focus.
2. Keep sessions and pending work available across shell, worker, and harness
   process restarts.
3. Deliver inbox messages and stream events through durable state, with
   at-least-once recovery and idempotent actions.
4. Support long-running harness work and incremental input without imposing
   one wake, one model turn, or one process lifetime on every session.
5. Enforce permissions outside the prompt and attribute every action.
6. Make agent-to-agent and agent-to-user conversations inspectable as a
   durable message stream, with thread and causal navigation.
7. Put external harnesses and supporting tools behind substitutable Trashtalk
   DSL interfaces. Reuse useful tools instead of rebuilding their internals.
8. Provide temporary Innards focus with Emacs-style navigation and editing.
9. Use provider-side caching effectively and measure reuse where the harness
   exposes it; include conversation continuity in the initial usable release.
10. Consolidate process lifecycle handling and support explicit worker
    activation on macOS with launchd and Linux with systemd.
11. Prefer DSL methods and reusable traits. Keep raw Bash at narrow process,
    file-descriptor, signal, SQLite-extension, and OS-supervision boundaries.
12. **Stretch goal:** provide an Innards stream applet for search, narrowing,
    inspection, and selection, initially useful for agent messages. Prototype
    generic object-oriented stream behavior only with bounded resource use
    and measured performance.

## Non-goals

- Reintroducing the retired `tt` daemon, native Procyon compiler, plugin mode,
  or Bash/native parity requirements.
- Reimplementing provider authentication, model selection, tool loops, or
  provider conversation storage in Trashtalk.
- Treating an external transcript, search index, PID, or notification as the
  authority for pending work, permissions, or successful processing.
- Giving a model unrestricted receiver/selector dispatch or letting Innards
  execute agent actions.
- Guaranteeing exactly-once external effects or keeping a viewport permanently
  open alongside the shell prompt.

## Design principles

### Session, run, and model turn have different lifetimes

An `AgentSession` is a durable conversation and work context. An `AgentRun`
records execution managed by a harness. A model turn is an exchange inside
that execution; the harness owns its sequencing and tool loop.

For example, a build-watcher session can exist for weeks. A failed build can
start a run that inspects logs, invokes allowed tools, asks a question, accepts
the answer, and eventually reports a result. Those exchanges do not require
Trashtalk to launch a new agent for every turn. Later work can reuse the
harness conversation, whether its process stayed alive or was restarted.

An idle session may have no process, or a resident harness waiting for input.
Neither arrangement changes its identity or the durability of its inbox.
Process handles and liveness belong to the run and process layer.

### Durable accessibility and ownership solve different problems

Multiple agents, tools, and human views can query permitted session data.
Shared access alone does not stop two workers from dispatching the same wake,
advancing a cursor past unfinished work, or submitting the same action twice.

Use atomic claims and ownership generations for those mutations. Fence each
run and any mutable backend conversation that requires exclusive ownership.
A fence is a generation checked on writes: a worker whose ownership expired
cannot settle work after a replacement has taken over.

Do not impose a global one-turn-per-session rule. A harness may do parallel
work internally. Multiple independent runs can share a logical session when
its execution policy and adapter support that arrangement; each has distinct
delivery receipts and action identities. A backend that cannot safely accept
concurrent input to one conversation must serialize input to that conversation.
This is a backend contract, not a definition of an agent session.

### Notifications are hints; persisted state is authority

Honker pub/sub reduces latency. Reconciliation reads durable receipts, run
deliveries, and stream cursors even when no notification arrives. Duplicate
wakes are expected and must not imply duplicate launches or actions.

### Behavioral intent and permission roles are separate

An `AgentArchetype` describes what an agent is for: reviewer, planner, or
test observer. An `AgentRole` is an authorization policy: what it may read,
which recipients it may contact, and which actions need approval.

Two reviewers can have different permission roles; a reviewer and a planner
can share a read-only role. Archetype instructions never grant authority.
A deterministic gateway enforces the role and records its decisions.

### Compose tools and traits behind public messages

Reuse external harnesses for execution and other tools for their strengths.
For example, a future `ConversationHistory search:` interface could use
`cass` or another implementation. Results must retain source references
and freshness information; search indexes supplement durable session state.

Prefer traits for shared behavior rather than an inheritance hierarchy added
only for code reuse. Document what a trait guarantees: an ephemeral observer
and a durable stream consumer are different contracts.

### Human presentation is a projection

Reading, searching, narrowing, pausing display updates, and dismissing a view
do not acknowledge work or change session lifecycle. Actions require distinct,
validated intents.

## Domain model

All fields and selectors below are proposed contracts.

### `AgentIdentity`

A stable principal identifying who acted.

```text
handle
displayName
owner
enabled
defaultArchetype
defaultRole
createdAt
```

One identity may own several sessions. Identity addressing routes new work;
session addressing continues an exact conversation. Labels are not credentials.

### `AgentArchetype` and `AgentRole`

An archetype is a versioned definition of purpose and operating guidance:

```text
name
revision
instructionsRef
instructionsHash
suggestedBackendProfile
subscriptionTemplates
createdAt
supersedes
```

A role is a separately versioned authority definition:

```text
name
revision
capabilities
workspacePolicy
recipientPolicy
approvalPolicy
runBudget
messageBudget
createdAt
supersedes
```

Subscriptions and backend suggestions still undergo role checks. A session
snapshots both revisions. Updating archetype text does not expand grants;
updating a role does not silently alter existing sessions. Explicit revision
adoption occurs at a safe execution boundary. Urgent grant revocation is
checked by the gateway immediately and may require interrupting active runs.

### `AgentSession`

The durable conversation, routing destination, and shared work context.

```text
identity
archetype
archetypeRevision
instructionsHash
role
roleRevision
roleHash
backendProfile
executionPolicy
workspace
lifecycleState
activitySummary
inbox
eventStream
parentSession
createdAt
lastActivityAt
```

`activitySummary` is a projection of runs, pending deliveries, and questions,
not a second source of execution authority. Backend conversation references
are associated with runs and may be reused by subsequent runs. A session can
therefore retain several explicitly identified conversation branches.

### `AgentRun`

A harness-managed execution, potentially containing many model turns and
incremental deliveries.

```text
session
state
archetypeRevision
roleRevision
backendProfile
externalConversationRef
backendRunId
ownerGeneration
workerId
capabilityTokenHash
startedAt
heartbeatAt
finishedAt
outcome
error
```

A run ID is stable across reconnection to the same execution. A fresh retry
attempt gets a new run ID linked to the prior attempt and the original work.
Backend conversation references are opaque, scoped to backend, workspace, and
account context, and never used as session identity or authorization.

Model turn IDs can appear in events when available, but Trashtalk does not
need a persistent `AgentTurn` object to drive the harness. Every gateway tool
call carries a run ID plus correlation and causation information.

### `AgentSubscription`

A durable routing rule from a stream to a session:

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

Implement shared filter, cursor, and delivery behavior in a reusable
`DurableSubscription` trait. `AgentSubscription` composes it with agent
routing. A generic subscription object can compose the same trait when a
non-agent consumer needs one; a base class is not required merely for reuse.

The existing [Observable trait](../trash/traits/Observable.trash) supplies
instance-scoped ephemeral emit/listen operations and persist-and-notify/enqueue
helpers. It does not retain a subscription, replay events, or settle consumer
receipts. Reuse it for notification hints, and use
[Stream](../trash/Stream.trash) and Honker for durable reads and offsets.
The current `Stream consumeDo:` and `consumeWith:` loops save offsets after
callback invocation without requiring durable successful processing. The new
consumer path must explicitly settle receipts before advancing offsets.

Filters initially use a small declarative vocabulary, not arbitrary Bash
or model-authored predicates.

### `AgentDelivery` and `InboxReceipt`

An `AgentDelivery` records one durable input batch assigned to a run:

```text
session
run
wakeRequestIds
messageIds
eventRanges
deliveryKey
state              pending | offered | accepted | processed | uncertain | failed
attempts
lastError
```

`offered` means dispatch was attempted; `accepted` means the harness
acknowledged input, not that the requested work succeeded. Delivery retries
reuse the same key. A claim or successful write to stdin is not an acceptance
receipt.

An `InboxReceipt` records processing for one message consumer:

```text
message
consumer
delivery
state              pending | claimed | processed | failed
attempts
claimedBy
claimedAt
processedAt
lastError
```

Stream ranges need equivalent durable processing receipts. Advance a consumer
cursor only through a contiguous successfully settled range: parallel
completion must not skip earlier unfinished input.

These receipts are separate from `Message.status`. Reading or archiving a
message is a presentation action, not proof of agent processing.

### `AgentAction`

A durable request and decision for an action:

```text
run
delivery
kind
arguments
idempotencyKey
policyDecision      allowed | approval_required | denied
approval
state               proposed | executing | succeeded | uncertain | failed | cancelled
result
createdAt
finishedAt
```

Action keys identify the logical operation and survive retries across run
attempts. A new run ID alone must not generate new identities for old effects.
For an external effect with an uncertain outcome, reconcile using its durable
key or surface uncertainty; do not blindly repeat a non-idempotent operation.

## Addressing and lifecycle

Identity routing initially succeeds only when exactly one eligible open
session exists. Policy may create a session if none exists; multiple eligible
sessions produce an ambiguity result. Session addresses route directly.

```text
agent:<agent-identity-id>
session:<agent-session-id>
user:<user-id>
```

Session lifecycle answers whether the conversation may accept new work:

- `open`: may start runs or deliver new input to existing runs.
- `paused`: retains input but does not dispatch new work.
- `closed`: gracefully completed and inspectable; explicit reopening is
  needed before further work.
- `terminated`: permanently rejects work and revokes run authority.

Run activity answers what its current execution is doing:

```text
starting | running | waiting_for_user | recovering | succeeded | failed | interrupted
```

A session can be open and idle overnight, then open with a running harness
after a test failure. Pausing it during that run prevents new deliveries but
lets already assigned work continue. Dismissing its view changes neither fact.
With several runs, show running counts, pending delivery counts, and blocking
questions rather than compressing mixed activity into one misleading status.

Operations:

- `wake` reconciles pending input and starts, resumes, or feeds eligible work.
- `pause` stops new dispatch without discarding input or advancing cursors.
- `resume` opens dispatch and reconciles pending work.
- `interruptRun:` interrupts the named execution; the session stays open or
  paused. A convenience `interrupt` is valid only with one unambiguous target.
- `close` finishes the logical session using an explicit drain-or-interrupt
  mode. Undelivered inputs receive a visible disposition.
- `terminate` revokes capabilities, rejects future dispatch, and requests
  all active runs to stop. Report pending or failed process termination until
  the adapter confirms it; lifecycle termination alone is not proof of exit.
- `dismiss` belongs only to a human view.

A blocking question suspends the affected work, not necessarily every run in
the session. A reply is a durable message that can feed a resident harness or
resume a conversation through the ordinary delivery path.

## Durable wake and execution architecture

```text
Inbox message / domain event
              |
              v
durable input + WakeRequest / outbox
              |
              v
AgentWorker claims and reconciles
              |
              v
persist run choice + AgentDelivery
              |
              v
start / resume / feed external harness
              |
              v
harness works across model turns and tools
              |
              v
durable checkpoints, actions, results
              |
              v
settle input receipts and contiguous cursors
```

### Wake requests and transaction boundaries

A wake contains references rather than a duplicate transcript:

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

Coalescing combines scheduling hints without dropping the underlying inputs.
Create input and its wake/outbox record atomically. Existing
[Inbox deliver:](../trash/Inbox.trash) saves a message and then emits a
notification; it does not yet implement this transaction. The Honker
persist-and-enqueue helper is a starting point, not proof of a transaction
covering message, receipts, and audit records.

Where one transaction cannot span publication and downstream routing, persist
an outbox record with the input and reconcile it idempotently. A periodic
scan repairs missed notifications and incomplete dispatch.

A wake queue job can be acknowledged once its durable dispatch obligation has
been transferred to a recoverable run/delivery record. The reconciler must
scan those records even after queue acknowledgement. Input processing receipts
and stream cursors settle separately, after a successful checkpoint or result.
A long run therefore need not hold one queue claim for its entire lifetime.

### Execution and ownership

The worker atomically selects an existing compatible run or registers a new
one, then persists a delivery before attempting backend I/O. Repeating a wake
finds that association instead of launching duplicate work.

If a harness supports input while active, send the delivery and record its
acknowledgement. Otherwise retain it for the next supported input boundary;
do not interrupt productive work merely to process a notification. Multiple
independent runs require explicit routing and adapter support. An input is
assigned once unless a routing rule deliberately creates separate consumers.

Use renewable ownership generations for run control and durable settlement.
The current [Honker wrapper](../trash/Honker.trash) exposes locks and claims
but not the complete generation-fencing and renewal contract required here.
Add a narrow primitive for any missing atomic operation before relying on it.

A harness that survives a worker crash may continue computing. A replacement
worker must inspect/reconnect to that execution, or confirm it stopped before
relaunching the same work. Lease expiry alone is not evidence of process death.
A stale owner's gateway requests and settlement writes must be rejected.

### Worker and process management

Here, `AgentWorker` means an explicitly started Bash process that runs a
Trashtalk dispatch/reconciliation loop. Sourcing the runtime or opening a
session must not silently start it. It can run in the foreground for
development, or under an OS supervisor for unattended operation.

The worker handles queue claims, durable assignments, backend connection
management, gateway decisions, and settlement. It does not prescribe every
model turn. It must multiplex active runs without blocking dispatch on a
single long-running harness.

Current process responsibilities are spread across these implementations:

| Existing surface | Relevant behavior | Proposed treatment |
|---|---|---|
| [Tool](../trash/Tool.trash) | Exact-argv synchronous execution and separate captured output | Reuse its validated request handling in the common process core |
| [Process](../trash/Process.trash), [Shell](../trash/Shell.trash) | Command-string execution and PID control | Make Process the lifecycle facade; add exact-argv and duplex support |
| [Coproc](../trash/Coproc.trash) | FIFO duplex transport using eval and merged stderr | Migrate callers to the common core before retiring duplicated machinery |
| [Actor](../trash/Actor.trash), [Scheduler](../trash/Scheduler.trash), [Stream](../trash/Stream.trash) | Separate background loops for queues, due tasks, and consumption | Retain domain behavior; consolidate process ownership, stop, wait, and cleanup |
| [Future](../trash/Future.trash) | Async Trashtalk computation and result retrieval | Keep its distinct result contract; reuse lifecycle mechanics where compatible |

Extend `Process` with one reusable exact-argv implementation, extracted from
and shared with `Tool`, rather than adding another parallel process manager.
It must retain real exit status, separate stderr from protocol stdout, manage
duplex I/O and process groups, and clean up on EOF, signals, and launch failure.
For example, current `Process wait` records zero after liveness polling; that
is insufficient evidence of successful harness execution.

Reuse common lifecycle behavior through traits where it helps the existing
facades. Inventory callers and preserve tested public behavior during migration;
remove duplicated machinery and obsolete entry points only after replacement
coverage exists. The agent worker uses typed handlers, not Actor's unrestricted
receiver/selector payload dispatch.

Provide a launchd definition for macOS and a systemd user unit for Linux,
both starting the same foreground worker entrypoint. Document installation,
explicit enable/start, stop/disable, logs, workspace/database paths, and restart
behavior. Do not add an internal self-daemonization layer. Platform acceptance
must cover Honker extension loading, Bash requirements, child reaping, signal
propagation, and terminal cleanup; systemd documentation alone does not prove
Linux support.

If Honker is unavailable, preserve inbox data and report automatic execution
as unavailable. Do not fall back to an unfenced shell loop. Manual dispatch
still requires the durable claim and settlement primitives.

## Harness contract, context, and caching

The existing one-shot `AxeAgent` and `CodexAgent` paths remain valid for
`@@`. Add separate session drivers behind `Agent`; preserve the Codex
one-shot path's ChatGPT authentication, stripped API-key environment, and
ephemeral read-only boundary.

Suggested session-driver messages:

```smalltalk
@ backend capabilities
@ backend startRun: context delivery: delivery eventsTo: sink
@ backend resumeConversation: externalRef delivery: delivery eventsTo: sink
@ backend deliver: delivery toRun: backendRunId
@ backend inspectRun: backendRunId
@ backend reconnectRun: backendRunId eventsTo: sink
@ backend interruptRun: backendRunId
```

Capabilities describe headless execution, conversation resume, active input,
input deduplication/acknowledgement, reconnect, interruption, concurrent
conversations, permission enforcement, and cache controls/usage reporting.
Adapters must reject unsupported operations explicitly. Maki and Jcode are
candidate harnesses, not claimed implementations of this contract.

Provider authentication, model configuration, prompt expansion, tool loops,
and provider-request retries remain the harness's responsibility. Trashtalk
owns durable delivery retries and policy budgets around those executions.

Distinguish resuming a stored conversation from reconnecting to a still-live
run: one does not imply support for the other. Persist external references
as soon as they are known. The initial usable release includes a tested
conversation-resume path on at least one suitable harness. A one-shot
read-only adapter is a useful first development slice, not the final session
execution model.

### Context and checkpoints

Supply versioned identity, archetype and role revisions, workspace context,
input references, unresolved questions, and available tool schemas. On resume,
deliver new context and relevant policy changes without duplicating a full
transcript the harness already owns. On a fresh start, reconstruct a bounded
context from durable work records, summaries, and referenced source material.

Normalize backend JSONL events inside the adapter. Separate text output,
input acceptance, progress, blocking questions, checkpoints, and final results.
A checkpoint can settle an explicitly named input batch while the run keeps
working. Successful completion requires durable results, action outcomes,
and settlement of the inputs it claims to have processed; process exit is
insufficient.

Read-only history/search tools may retrieve additional conversation context
through a substitutable DSL abstraction. Their absence or stale indexes must
not hide pending work or alter receipt authority.

### Provider-side caching

Cache reuse is an explicit adapter and acceptance concern. Stable prefixes and
appropriate cache boundaries matter in both
[OpenAI's caching model](https://developers.openai.com/api/docs/guides/prompt-caching)
and [Anthropic's caching model](https://platform.claude.com/docs/en/build-with-claude/prompt-caching);
the exact controls depend on the provider, model, and harness.

Design context assembly to preserve unchanged instructions, schemas, and shared
references, with changing deliveries appended after them. Avoid injecting
timestamps or reordered material into a reusable prefix. Retain conversation
continuity and avoid gratuitous transcript rewriting; deliberate compaction
still needs its own context and cache tradeoff.

The harness owns provider requests. Use supported cache controls, including
breakpoints or retention choices where exposed; Trashtalk must not bypass
the harness or introduce API credentials merely to configure caching.
Record normalized cache-read/write token counts, uncached input, latency,
and usage availability when reported. Missing measurements mean unknown,
not zero reuse. Resume alone is not proof of a cache hit.

For each qualified adapter, compare cold and repeated equivalent workloads
under documented settings, then repeat after a meaningful context change.
Use observations to select context layout and supported cache configuration.
Keep backend-specific controls and invalidation behavior inside the adapter.
Cache loss must affect efficiency only, never permissions or durable recovery.

## Capabilities and attributable actions

Tools invoke fixed public Trashtalk message sends with typed arguments.
Initial capabilities are:

```text
events.read
inbox.read
message.send
question.ask
source.read
task.runNamed
proposal.submit
```

| Action | Default decision |
|---|---|
| Read own inbox or permitted events | Automatic within role scope |
| Message an allowed identity/session | Automatic, budgeted |
| Ask the owning user a question | Automatic |
| Read scoped source | Role-granted |
| Run an allowlisted named task | Role-granted or approval-required |
| Submit SourceProposal/ObjectEditProposal | Submit automatically; established human review before apply |
| Direct source/object mutation or arbitrary shell/selector dispatch | Denied |
| Change identities, grants, or worker policy | Denied |

The gateway derives identity, session, and run from a short-lived capability
bound to current ownership and role revision. Never trust a model-supplied
sender field. Tokens are redacted and only hashes are persisted.

Gateway checks cover only actions routed through it. A driver must also
enforce the selected restrictions on the harness's built-in tools and process
environment. If it cannot, it is ineligible for that role; a restrictive prompt
or a narrow message tool does not contain an otherwise unrestricted harness.

Bound agent-to-agent causal depth, message rate, and total work budgets.
A received message does not itself create an obligation to reply.

## Messaging and observability

Preserve human-readable subjects, bodies, and threads, and add attributable
machine fields:

```json
{
  "schema_version": 1,
  "id": "message_...",
  "to": "session:agent_session_...",
  "from": "agent:agent_identity_...",
  "sender_identity": "agent_identity_...",
  "sender_session": "agent_session_...",
  "sender_run": "agent_run_...",
  "subject": "Tests failed",
  "body": "Two integration tests failed after the latest event.",
  "kind": "question",
  "thread": "message_...",
  "reply_to": "",
  "expects_reply": true,
  "correlation_id": "...",
  "causation_id": "event_...",
  "dedupe_key": "delivery_...:action_...",
  "created_at": "2026-09-07T12:00:00Z"
}
```

Kinds begin with `note`, `question`, `alert`, and `result` under a
versioned schema. Questions identify the work they block.

Publish attributable send, delivery, and processing events into a durable
agent-message stream. Users must be able to follow exchanges across sessions,
inspect bodies they are allowed to read, and navigate threads, recipients,
correlation, and causation. Per-session events link to the same message IDs;
an aggregate projection must not duplicate logical sends.

Persist messages and their audit/outbox records together. Rebuild projections
idempotently after failure. A viewer's cursor and read state are separate from
agent processing cursors. Apply access scope and redaction before delivering
frames to a viewer or an external search index.

### Session event envelope

```json
{
  "schema_version": 1,
  "event_id": "event_...",
  "event_type": "agent.run.started",
  "session_id": "agent_session_...",
  "run_id": "agent_run_...",
  "delivery_id": "delivery_...",
  "source": {"kind": "worker", "id": "agent_worker_..."},
  "occurred_at": "2026-09-07T12:00:00Z",
  "correlation_id": "...",
  "causation_id": "wake_...",
  "payload": {}
}
```

Initial events cover session open/pause/resume/close/termination, wake queued,
run start/output/checkpoint/wait/recovery/success/failure/interruption,
delivery offered/accepted/processed, action proposed/decided/finished, and
message sent/delivered/processed. Optional model-turn metadata is subordinate
to the run ID.

Persist an ordered stream per session; cross-session projections use their
own durable positions and causal links rather than timestamp ordering alone.
Ephemeral notifications only prompt readers to catch up.

## Innards applets

### Session focus

Add an `inagent` applet using Innards' inline terminal viewport. It renders
a session projection and emits user intents. Trashtalk remains responsible
for data access, authorization, dispatch, and process signals.

```text
Trashtalk ---- JSONL snapshots/events ----> inagent stdin
                         inagent UI -----> /dev/tty
Trashtalk <----- JSONL user intents ------- inagent stdout
                         diagnostics ----> stderr
```

Use the common exact-argv Process transport for the temporary duplex bridge.
Keep stderr separate from JSONL. Frames carry `schema_version: 1`, stable
IDs, and replay positions. A reconnect requests a snapshot and bounded durable
catch-up. Slow views must not block agent processing.

### Emacs-style controls

Build on Innards' existing inmacs/inpage movement and search conventions.
Bindings are context-sensitive:

| Key | Navigation/inspection | Message or search input |
|---|---|---|
| C-n / C-p | Next/previous item or line | Next/previous line |
| C-f / C-b | Forward/backward within inspected text | Forward/backward character |
| C-a / C-e | Beginning/end of line | Beginning/end of line |
| M-< / M-> | Beginning/end of loaded view | Beginning/end of input |
| C-v / M-v | Page forward/backward | Page within multiline input |
| C-s / C-r | Incremental forward/backward search | Search within input |
| C-k / C-y | No destructive action on event data | Kill to end of line / yank |
| C-g | Cancel search/subview; at root dismiss | Cancel current operation without sending |
| C-x C-c | Dismiss focus | Dismiss focus, handling unsent draft explicitly |
| Return | Inspect selected item | Insert newline in multiline composition |
| C-c C-c | No lifecycle action | Explicitly send the composed message |
| M-x | Choose a named command | Choose a named command |

Named commands include compose-message, pause-session, resume-session,
interrupt-run, terminate-session, and toggle-follow. Interruption names its
run; termination requires separate confirmation. Bare letters remain text in
input fields. Implement control-key decoding without terminal flow-control
conflicts, and restore the original terminal settings on exit.

Example intents:

```json
{"schema_version":1,"intent":"dismiss"}
{"schema_version":1,"intent":"send_message","body":"Continue after fixing the test."}
{"schema_version":1,"intent":"pause_session"}
{"schema_version":1,"intent":"interrupt_run","run_id":"agent_run_..."}
{"schema_version":1,"intent":"terminate_session","confirmed":true}
```

Dismiss, Ctrl-C cancellation, or terminal loss close only the applet and its
bridge. Do not forward focus-process signals to harness process groups.
A final bounded viewport may remain in scrollback. A cheap prompt summary
can show running work and unread results independently.

The first applet can provide read-only viewing, search, inspection, and
dismissal. Add composition and lifecycle commands once catch-up, stream
separation, and terminal restoration pass.

### Stretch goal: stream search, narrowing, and selection

Provide a reusable stream applet, potentially by extending `inpick`, for
agent-message history and other typed event streams. Reuse Innards' existing
JSONL selection surface before introducing a new binary. Its input is a
projection, and its output is a selected stable record ID or a query intent.

Distinguish incremental search within loaded records from querying historical
storage. Support narrowing by sender, recipient, kind, thread, and text, plus
explicit live-follow versus frozen-view state. Fetch older pages through a
Trashtalk query adapter, which may delegate indexed search to a tool such as
`cass` if that tool fits the data contract. Mark index lag and result scope.

For an object-oriented stream interface, prefer a stream object with batched
record values and composable query/consumer traits. Do not allocate and
persist one Trashtalk object for every displayed token or event. Keep paging,
filtering, retention, and bounded queues explicit; skip or coalesce display
updates only with a visible gap and a durable replay path.

Before choosing the generic abstraction, measure event ingestion, history
query latency, memory at a fixed viewport size, and Bash/jq process counts
across increasing histories. Set acceptance thresholds in the prototype
before comparing implementations. This experiment is a stretch goal, not a
dependency of basic message visibility.

## Public Trashtalk API sketch

```smalltalk
identity := @ AgentIdentity named: 'build-watcher'.
archetype := @ AgentArchetype named: 'test-observer' revision: '1'.
role := @ AgentRole named: 'workspace-reader' revision: '1'.
session := @ AgentSession openFor: identity archetype: archetype role: role workspace: project.

@ session subscribeTo: 'tests' events: #('test.completed' 'test.failed').
@ session wake.
@ session pause.
@ session resume.
@ session focus.
@ session interruptRun: runId.
@ session closeWith: 'drain'.
@ session terminate.

@ session send: 'Please inspect the failure' to: reviewerSession.
@ session askUser: 'Should I retry the integration test?'.
```

Refine selectors against compiler capabilities before implementation.
`Agent` remains the convenience facade/backend selector, not a persistent
identity. Deprecate tmux-oriented session methods as replacement coverage
and migration paths become available.

## Failure and recovery

- **Missed notification:** reconciliation finds pending receipts, unadvanced
  ranges, or incomplete deliveries, even if their wake jobs were acknowledged.
- **Duplicate wake:** the existing delivery key and run assignment prevent
  duplicate semantic dispatch.
- **Crash after sending input but before recording acceptance:** inspect the
  backend delivery acknowledgement or retry with a supported deduplication
  key. If neither is available, mark delivery uncertain and reconcile or seek
  operator resolution before replaying work that could have effects.
- **Worker crash during live execution:** reclaim ownership with a new
  generation; reconnect or prove the old execution stopped before replacement.
  Reject stale capability use and stale settlement.
- **Harness exits without an expected final result:** record incomplete
  execution and preserve unsettled work. A generic provider end event alone
  does not settle it.
- **Pause during execution:** finish assigned work if possible, retain new
  input, and require explicit interruption to stop the current run.
- **Focus crashes:** clean up the bridge and terminal; preserve all session
  and harness activity.
- **Provider conversation is missing:** record lost continuity and reconstruct
  from durable context, receipts, and action records. Reconcile any live or
  uncertain execution before starting fresh. Do not treat an empty search
  index or cache miss as loss of the authoritative work record.

## Security and trust boundaries

1. Snapshot archetype and role independently; validate grants at action time.
2. Derive sender identity from a current run capability and ownership generation.
3. Canonicalize workspaces and scope tools, history access, and message recipients.
4. Expose typed fixed-selector tools; never eval agent-authored shell strings.
5. Keep source/object mutation behind existing proposal and acceptance gates.
6. Qualify restrictions on harness built-in tools as well as gateway tools.
7. Bound and redact payloads, tokens, and logs; do not record expanded environments.
8. Preserve idempotency keys across retries and surface uncertain external effects.
9. Bound causal depth, rates, and aggregate work across interacting agents.
10. Treat termination as explicit authority revocation plus a separately
    observed stop outcome. UI dismissal never grants termination authority.

## Implementation plan

### Phase 0: contracts and consolidation design

- Define identity, archetype, role, session, run, delivery, receipt, action,
  wake, and event schemas, including rejected lifecycle transitions.
- Specify ownership generation, renewal, idempotency, dispatch acknowledgement,
  checkpoint settlement, and contiguous cursor invariants.
- Inventory Process/Tool/Shell/Coproc and background-loop callers; identify
  the shared exact-argv lifecycle core and compatibility tests.
- Qualify candidate harness capabilities, permission containment, resume,
  active input, reconnect, and cache observability before selecting one.
- Specify the minimal durable-subscription trait and declarative filters.

### Phase 1a: durable headless development slice

- Add the domain records and refactor AgentSession away from tmux ownership.
- Implement transactional input/outbox creation and idempotent delivery
  assignment with fencing before enabling dispatch.
- Build the common Process core and a foreground `AgentWorker tick` that
  reconciles a bounded batch; do not add another detached loop.
- Use one read-only one-shot adapter to prove the path, with minimal
  gateway-validated message.send and question.ask.
- Persist normalized run/message events and settlement.

Acceptance: a test.completed event wakes a test-observer archetype under a
workspace-reader role. It sends a durable result or question to the user.
A replayed wake does not create another logical send. This is an intermediate
development milestone.

### Phase 1b: initial usable session release

- Add at least one qualified session driver with conversation resume and
  reference persistence; retain the one-shot adapter as an explicit fallback.
- Exercise multi-turn work, checkpoints, and later input in one conversation.
  Feed active input where supported; otherwise queue it for a safe boundary.
- Implement inspect/reconnect or a proven stop-before-restart recovery path.
- Add stable context assembly, supported cache configuration, and available
  usage measurements. Compare cold and warm journeys.
- Verify that neither work nor authority depends on a provider transcript,
  cache entry, or search index.

Phase 1b is required for the initial usable release; continuity is not a
distant optional follow-up.

### Phase 2: continuous operation and platform qualification

- Run the same worker entrypoint continuously with bounded polling, run
  multiplexing, lease renewal, retry limits, and reconciliation.
- Provide launchd and systemd user-service installation and activation paths.
- Test real worker and harness failures, duplicate input, lease takeover,
  uncertain acceptance, and idempotent effect recovery.
- Migrate relevant background callers onto the common lifecycle core and
  remove superseded internals after compatibility checks.
- Qualify on macOS and Linux, including Honker loading and process cleanup.

### Phase 3: richer actions and message visibility

- Add scoped event/inbox reads, one named task, and proposal submission.
- Add cross-session message views with thread/cause links and access checks.
- Verify blocking questions, incremental replies, and agent-loop limits.
- Add a substitutable history-query adapter where it improves retrieval.

### Phase 4: Innards focus and optional stream prototype

- Add read-only inagent snapshots, catch-up, live updates, search, and dismiss.
- Implement the Emacs-style controls, then composition and lifecycle intents.
- Prove terminal restoration and agent survival after focus teardown.
- Prototype the reusable stream applet and batched stream traits against
  explicit performance targets before committing to a generic abstraction.

## Verification

These are implementation acceptance requirements, not results of this design
revision.

### Automated

- Identity spoofing and stale ownership tokens cannot authorize actions.
- Archetype changes do not expand grants; roles require explicit adoption.
- Duplicate dispatch does not create duplicate runs or accepted deliveries.
- A backend conversation is not concurrently mutated beyond its declared
  capabilities; independent runs can share permitted session data.
- A harness run spans multiple model turns without another agent relaying them.
- Input during active work is accepted or visibly queued according to capability.
- Input persistence and wake/outbox creation survive every crash boundary.
- Reconciliation finds incomplete deliveries after wake acknowledgement.
- Checkpoints settle named inputs; parallel completion cannot skip cursor gaps.
- External action retries retain logical keys and reconcile uncertain effects.
- Reads, archiving, search, and display cursors never acknowledge agent work.
- Pause retains work; interruption leaves the session open or paused.
- Termination rejects new actions and accurately reports unconfirmed process stops.
- Resume, lost conversation state, and cache loss preserve durable work.
- Context fixtures preserve stable prefixes; usage normalization distinguishes
  unavailable metrics from zero and records observed cache reuse when exposed.
- Message projections preserve attribution, causal links, redaction, and replay.
- Process transport preserves argv, exit status, stderr separation, and cleanup.
- Innards handles EOF, malformed frames, slow readers, and terminal signals
  without altering agent activity.
- Emacs controls navigate, edit, kill/yank, search, and cancel without accidental
  send, pause, or termination.

### Human acceptance

1. Start and stop the worker in a direct shell, under launchd on macOS, and
   under a systemd user unit on Linux.
2. Emit a real test event and watch a headless harness pursue it across model
   turns, checkpoint, and report.
3. Deliver another message while it is working; inspect acceptance or queuing.
4. Resume the same conversation for later work, and repeat with its provider
   state unavailable to demonstrate recovery.
5. Inspect cache usage and latency for repeated equivalent workloads under
   documented backend settings; record unavailable telemetry explicitly.
6. Follow an agent-to-agent exchange across session boundaries and inspect
   the thread without acknowledging processing.
7. Focus during active work, exercise Emacs navigation/search/composition,
   and dismiss with C-x C-c; confirm the harness keeps working.
8. Interrupt a selected run, then explicitly terminate a session; inspect
   both lifecycle and actual process-stop outcomes.
9. Repeat focus/dismiss under tmux and SSH, including terminal loss.
10. For the stretch prototype, narrow and select from a growing message
    history while measuring bounded memory and query latency.

## Resolved decisions and remaining choices

The design commits to durable sessions, harness-managed multi-turn runs,
separate archetypes and permission roles, trait-based subscription reuse,
early conversation continuity, measured cache behavior, a shared Process
lifecycle implementation, macOS/Linux supervision, and Emacs-style Innards
controls. Message visibility is core; a generic stream applet is a stretch goal.

Implementation must still select the first qualified session harness, exact
capability serialization, storage form for versioned definitions, supervision
commands, event retention/compaction policy, and performance thresholds for
the stream experiment. Those selections must satisfy the contracts above;
they must not silently revert to one model turn per wake.
