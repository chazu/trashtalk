# Headless Agent Sessions Design

**Status:** Design with partial implementation. The durable worker/recovery and
snapshot session browser are described in [agent operations](agent-operations.md).
The broader subscription, role, live-focus, and multi-host fencing contracts
below are not all implemented.
**Date:** 2026-09-08

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

It also commits to a deliberately thin first slice: one harness process per
delivery, continuity through conversation resume, and an agent that reports
back to Trashtalk through Bash. Richer transports are later capabilities that
must not change the durable contracts. The first persistent session is
Gusgus, the assistant behind `@@`.

## Goals

1. Separate agent identity, behavioral intent, authority, durable session,
   harness execution, and temporary human focus.
2. Keep sessions and pending work available across shell, worker, and harness
   process restarts.
3. Deliver inbox messages and stream events through durable state, with
   at-least-once recovery and idempotent actions.
4. Support long-running harness work without imposing one model turn per
   wake. Incremental input to an active process is a later capability; the
   first slice queues input for the next process boundary.
5. Attribute every action to a run through a token the model cannot forge.
   Enforce permissions outside the prompt where the harness or OS can, and
   never claim containment the harness does not provide.
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
- Treating priming instructions as containment. In the first slice the model
  has a shell and can send any selector; only the harness sandbox and the OS
  user boundary limit it. Innards never executes agent actions.
- Guaranteeing exactly-once external effects or keeping a viewport permanently
  open alongside the shell prompt.

## Design principles

### Session, run, and model turn have different lifetimes

An `AgentSession` is a durable conversation and work context. An `AgentRun`
records execution managed by a harness. A model turn is an exchange inside
that execution; the harness owns its sequencing and tool loop.

For example, a build-watcher session can exist for weeks. A failed build can
start a run that inspects logs, invokes allowed tools across many turns, and
asks a question. The reply starts a second run that resumes the same
conversation and eventually reports a result. Neither run required Trashtalk
to relay a model turn.

An idle session normally has no process; a resident harness waiting for input
is a later adapter capability. Neither arrangement changes its identity or the
durability of its inbox. Process handles and liveness belong to the run and
process layer.

### One harness process per delivery; continuity through resume

The first slice launches one harness process per delivery. The process runs
as many model turns and tool calls as the work needs, then exits. A later
delivery resumes the same backend conversation by reference in a fresh
process. This satisfies the rule against one turn per wake while avoiding
live input, pipe ownership, worker multiplexing, and pipe-level reconnection.

Input that arrives while a process is active waits for the next process
boundary, which the delivery rules below already permit. Feeding an active
process is a later, per-adapter capability declared through `capabilities`.

### Attribution comes from the run token; containment comes from the sandbox

The harness process receives a run-scoped token in its environment. Every
agent-facing selector derives sender identity, session, and run from that
token and ignores any model-supplied sender. In the first slice the model
reaches those selectors through its shell tool and
[bin/trash-send](../bin/trash-send); a later MCP or typed-tool gateway can
wrap the same selectors without changing their contract.

Containment is a separate concern. The harness sandbox and the OS user decide
what the shell may touch. A role's capability list is enforced only at the
boundaries Trashtalk owns (recipients, budgets, settlement, proposals) until
a gateway that intercepts built-in tools exists.

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
Trashtalk enforces the role deterministically at the selectors it owns and
records its decisions.

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
inbox
createdAt
```

One identity may own several sessions and owns one inbox, where mail
addressed to the identity lands before routing. Identity addressing routes
new work; session addressing continues an exact conversation. Labels are not
credentials.

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

`runBudget` carries a turn cap, a USD cap, and `retryLimit`, the number of
attempts a delivery gets before it stalls. `messageBudget` carries a count
and a USD cap. A driver applies whichever caps the harness supports (Codex
and maki expose both turn and dollar limits); the worker enforces the rest by
counting attempts, runs, and messages.
Archetypes and roles are stored as Persistable instances, and a new revision
is a new instance linked through `supersedes`.

Subscriptions and backend suggestions still undergo role checks. A session
snapshots both revisions. Updating archetype text does not expand grants;
updating a role does not silently alter existing sessions. Explicit revision
adoption occurs at a safe execution boundary. Urgent grant revocation takes
effect at the owned selectors immediately and at the worker before the next
launch; it may also require interrupting active runs.

### Workspace

A workspace is the directory scope a session is bound to: the git repository
root when the starting directory is inside one, otherwise the real path of
the directory itself. Symlinks are resolved, so one directory is one
workspace however it is spelled, and two checkouts of the same repository
are two workspaces.

The workspace is fixed when a session opens. Every run launches the harness
with it as the working directory and as the sandbox's writable root. Source
reads, proposals, and writes are scoped to it. The Trashtalk store under
`~/.trashtalk` is not a workspace; it is always added as a second writable
root so `trash-send` works, and nothing else outside the workspace is.
Changing workspace means a different session.

For Gusgus the workspace is derived from where `@@` runs and is the identity
routing key. For other agents it is passed explicitly at `openFor:`. The
role's `workspacePolicy` is the set of paths an identity may open sessions
in.

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

A harness-managed execution containing many model turns and, with a later
adapter that accepts live input, incremental deliveries.

```text
session
state
archetypeRevision
roleRevision
backendProfile
externalConversationRef
ownerGeneration
workerId
capabilityTokenHash
processPid
outputLog
priorRun
startedAt
heartbeatAt
finishedAt
outcome
error
```

A run corresponds to one harness process in the first slice. `outputLog` is
a durable per-run file that receives the harness's stdout; with `processPid`
it lets a replacement worker observe a process it did not launch. The worker
writes `heartbeatAt` on each tick from process liveness; it is separate from
the worker's own lease renewal, since a harness turn can be silent for
minutes while the process is healthy. A retry
gets a new run ID whose `priorRun` links the attempt to the original work.
Backend conversation references are opaque, scoped to backend, workspace, and
account context, and never used as session identity or authorization.

Model turn IDs can appear in events when available, but Trashtalk does not
need a persistent `AgentTurn` object to drive the harness. Every agent-facing
call carries the run token, from which the run ID, correlation, and causation
information are derived.

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

`cursor` on the subscription record is the authoritative stream position,
because settlement lives there. The Honker consumer offset is derived from it
and may be rebuilt from the record at any time.

### `AgentDelivery` and `InboxReceipt`

An `AgentDelivery` records one durable input batch assigned to a run:

```text
session
run
wakeRequestIds
messageIds
eventRanges
deliveryKey
state              pending | offered | blocked | processed | uncertain | failed | skipped
attempts
lastError
```

`skipped` is set only by a human through the session's `skip:note:`
operation. It requires a note, emits an event, and is the only way the
contiguous cursor advances past a permanently failing input.

`offered` means a harness process was launched with this delivery's input
references in its prompt. `blocked` means the agent asked a blocking question
about it and the process may exit without settling it; the reply returns it
to `pending` so the next process receives both. `processed` means the agent
explicitly settled the delivery through `trash-send`. `uncertain` means the
process exited with a result but left the delivery `offered`; the worker
never infers success from exit. `failed` means nonzero exit, no result, or a
launch failure.

There is no `accepted` state. No harness provides semantic acceptance of
input: an echoed input record, a started-turn event, or a successful write to
stdin says nothing about whether the model understood the work. A claim or a
launched process is not a receipt either. Delivery retries reuse the same key.

An `InboxReceipt` records processing for one message consumer and mirrors the
delivery that carries its message:

```text
message
consumer
delivery
state              pending | claimed | processed | uncertain | failed
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

An `approval_required` decision reuses the question path. The worker sends a
`question` to the owning user carrying the action's key, leaves the action
`proposed`, and marks the delivery `blocked`. The reply records the decision
on the action before the next launch, so the resumed run finds it `allowed`
or `denied`. There is no second approval mechanism and no timeout: the
question sitting in the owner's inbox is the alert, and the delivery waits
for the reply.

## Addressing and lifecycle

Mail addressed to an identity lands in that identity's inbox, and the worker
routes it to a session. Routing initially succeeds only when exactly one
eligible session exists; `open` and `paused` sessions are both eligible, so
routing never creates a duplicate beside a paused session. Policy may create
a session if none exists; multiple eligible sessions produce an ambiguity
result. Session addresses route directly. Mail arriving at a `closed` session
is held there and an alert goes to the owner. Existing named inboxes such as
`maki:abc123` map onto identity handles; migrating those names is part of
the tmux deprecation.

```text
agent:<agent-identity-id>
session:<agent-session-id>
user:<user-id>
```

Session lifecycle answers whether the conversation may accept new work:

- `open`: may launch runs. New input waits for the next process boundary
  unless the adapter declares live input.
- `paused`: retains input but does not dispatch new work.
- `closed`: gracefully completed and inspectable; explicit reopening is
  needed before further work.
- `terminated`: permanently rejects work and revokes run authority.

Run activity answers what its current execution is doing:

```text
starting | running | recovering | succeeded | waiting_for_user | unsettled | failed | interrupted
```

The last five are terminal process outcomes; settlement truth lives on the
deliveries. `waiting_for_user` means the process exited with every delivery
settled or `blocked` on a question, and the reply starts a new run that
resumes the conversation. `unsettled` means it exited with a result but left
a delivery `offered`, which the worker then marks `uncertain`.

A session can be open and idle overnight, then open with a running harness
after a test failure. Pausing it during that run prevents new deliveries but
lets already assigned work continue. Dismissing its view changes neither fact.
With several runs, show running counts, pending delivery counts, and blocking
questions rather than compressing mixed activity into one misleading status.

Operations:

- `wake` reconciles pending input and launches a fresh or resumed harness
  process for eligible work.
- `pause` stops new dispatch without discarding input or advancing cursors.
- `resume` opens dispatch and reconciles pending work.
- `interruptRun:` interrupts the named execution; the session stays open or
  paused. A convenience `interrupt` is valid only with one unambiguous target.
- `close` finishes the logical session using an explicit drain-or-interrupt
  mode. Undelivered inputs receive a visible disposition.
- `terminate` revokes capabilities, rejects future dispatch, and requests
  all active runs to stop. Report pending or failed process termination until
  the adapter confirms it; lifecycle termination alone is not proof of exit.
- `skip:note:` marks a failed delivery `skipped` so the cursor can advance.
  Human-only; the note is required and recorded in the event.
- `requeue:` returns a failed or uncertain delivery to `pending` with its
  attempt count reset, for use after fixing whatever made it fail. Human-only.
- `dismiss` belongs only to a human view.

A blocking question marks the affected delivery `blocked`, not every delivery
in the run. A reply is a durable message that returns the blocked delivery to
`pending` and resumes the conversation through the ordinary delivery path.

### Lifecycle transitions

Any transition not listed below is rejected and recorded as an event.

Session:

| From | To | Trigger |
|---|---|---|
| (new) | open | `openFor:` |
| open | paused | `pause` |
| paused | open | `resume` |
| open, paused | closed | `close` after drain or interrupt |
| closed | open | explicit reopen |
| open, paused, closed | terminated | `terminate` |

`dismiss` and every other view action leave session state unchanged.
`terminated` is permanent.

Run:

| From | To | Trigger |
|---|---|---|
| starting | running | PID recorded |
| starting | failed | launch failure |
| running | recovering | ownership reclaimed while the process is alive |
| recovering | running | replacement worker observes the process |
| running | succeeded | exit with a result; no delivery left `offered` or `blocked` |
| running | waiting_for_user | exit with a result; at least one `blocked`, none `offered` |
| running | unsettled | exit with a result; at least one delivery left `offered` |
| running | failed | nonzero exit or no result |
| running | interrupted | `interruptRun:` and exit observed |

Terminal run states never change. A retry is a new run linked by `priorRun`.

Delivery:

| From | To | Trigger |
|---|---|---|
| pending | offered | worker launches a run carrying it |
| offered | processed | agent `settle:` |
| offered | blocked | agent `askUser:` or an `approval_required` action |
| offered | uncertain | run ends `unsettled`; interruption after any output |
| offered | pending | interruption before any output; crash with no live process found |
| offered | failed | run fails |
| blocked | processed | agent `settle:` in the same run |
| blocked | pending | reply arrives |
| failed | pending | automatic retry while attempts are below `retryLimit`; human requeue |
| failed | skipped | human `skip:note:` |
| uncertain | pending | human requeue |
| uncertain | processed | a later run of the same session settles it |
| uncertain | skipped | human `skip:note:` |

`processed` and `skipped` are terminal. A `settle:` for a delivery the run
was never offered is rejected. Receipts mirror their delivery.

Action:

| From | To | Trigger |
|---|---|---|
| proposed | executing | policy `allowed`, or an approval reply grants it |
| proposed | cancelled | policy `denied`, an approval reply refuses, or the run is terminated |
| executing | succeeded, failed, uncertain | outcome observed |
| uncertain | succeeded, failed | reconciliation by idempotency key |

`succeeded`, `failed`, and `cancelled` are terminal.

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
launch detached harness process (fresh or resumed conversation)
              |
              v
harness works across model turns and tools;
agent settles inputs and sends messages via trash-send
              |
              v
process exits; worker records outcome and applies fallbacks
              |
              v
advance contiguous cursors from settled receipts
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
notification; it does not yet implement this transaction. The Observable
trait's `saveAndEmit:payload:` and `saveAndEnqueue:payload:` helpers are
atomic, but they persist the receiver (the Inbox) rather than the Message, so
Inbox cannot adopt them as-is. They are a starting point, not proof of a
transaction covering message, receipts, and audit records.

Where one transaction cannot span publication and downstream routing, persist
an outbox record with the input and reconcile it idempotently. A periodic
scan repairs missed notifications and incomplete dispatch.

A wake queue job can be acknowledged once its durable dispatch obligation has
been transferred to a recoverable run/delivery record. The reconciler must
scan those records even after queue acknowledgement. Input processing receipts
and stream cursors settle separately, after a successful checkpoint or result.
A long run therefore need not hold one queue claim for its entire lifetime.

### Execution and ownership

The worker atomically claims the pending deliveries for a session, registers
a run, and persists the delivery as `offered` before launching anything.
Repeating a wake finds that run instead of launching duplicate work. One
session has at most one active run in the first slice; a later execution
policy may permit more where an adapter supports it. An input is assigned
once unless a routing rule deliberately creates separate consumers.

The worker launches the harness in its own session (`setsid`) with stdout
redirected to the run's `outputLog`, stderr to a separate log, and the run
token in its environment. The worker holds no pipe to the process. It records
the PID, then observes the log and PID; a replacement worker after a crash
observes the same files. That is what reconnect means in the first slice:
re-attaching to a still-running process's durable output, not re-acquiring
its stdin.

Input that arrives while a process is active stays `pending` and is delivered
by the next process, which resumes the same conversation. Do not interrupt
productive work merely to process a notification.

Use ownership generations for run control and settlement. A generation is a
compare-and-set in plain SQLite (`UPDATE ... WHERE owner_generation = ?`) and
must not depend on Honker. The current
[Honker wrapper](../trash/Honker.trash) exposes locks and claims but no
fencing or renewal. The vendored extension exports `honker_heartbeat`,
`honker_retry`, and `honker_fail`, which the Bash layer does not wrap; those
are the right primitives for queue visibility timeouts and should be wrapped
rather than reinvented.

A harness that survives a worker crash keeps computing because it holds no
pipe to the worker. Lease expiry alone is not evidence of process death; check
the PID and log before relaunching the same work. A stale owner's settlement
writes must be rejected.

### Worker and process management

Here, `AgentWorker` means an explicitly started Bash process that runs a
Trashtalk dispatch/reconciliation loop. Sourcing the runtime or opening a
session must not silently start it. It can run in the foreground for
development, or under an OS supervisor for unattended operation.

The worker handles queue claims, durable assignments, process launch, outcome
recording, fallback settlement, and cursor advancement. It does not prescribe
any model turn. Because it holds no pipes, waiting on many runs means polling
PIDs and logs, not multiplexing streams, so dispatch never blocks on a single
long-running harness.

Current process responsibilities are spread across these implementations:

| Existing surface | Relevant behavior | Proposed treatment |
|---|---|---|
| [Tool](../trash/Tool.trash) | Exact-argv synchronous execution and separate captured output | Use its capture path unchanged in the first slice; add detached launch beside it; extract the shared core in Phase 2 |
| [Process](../trash/Process.trash), [Shell](../trash/Shell.trash) | Command-string execution via eval and PID control | Phase 2: make Process the lifecycle facade with exact-argv and duplex support |
| [Coproc](../trash/Coproc.trash) | FIFO duplex transport using eval and merged stderr | Migrate callers to the common core before retiring duplicated machinery |
| [Actor](../trash/Actor.trash), [Scheduler](../trash/Scheduler.trash), [Stream](../trash/Stream.trash) | Separate background loops for queues, due tasks, and consumption | Retain domain behavior; consolidate process ownership, stop, wait, and cleanup |
| [Future](../trash/Future.trash) | Async Trashtalk computation and result retrieval | Keep its distinct result contract; reuse lifecycle mechanics where compatible |

The first slice needs only detached launch with exact argv, redirected output,
and a recorded PID, which is a narrow addition next to `Tool`'s existing
`captureArgvJson:` path. The consolidation below is Phase 2 work.

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

If Honker is unavailable, preserve inbox data and report notification-driven
execution as unavailable. Claims, generations, and settlement are plain SQLite
and keep working, so a manual `wake` remains fenced. Today only `Inbox` guards
on Honker availability; Stream, Actor, Scheduler, and EventBus fail on a
missing function and must gain the same guard before the worker depends on
them. Each `_db_sql` call spawns a fresh `sqlite3` and reloads the extension,
so a polling worker must batch its queries per tick.

## Harness contract, context, and caching

The existing one-shot `AxeAgent` and `CodexAgent` paths remain valid for
`@@`. [ClaudeAgent](../trash/ClaudeAgent.trash) is the legacy tmux path with
no headless or resume support; deprecate it with the tmux session methods and
stop defaulting `Agent primary` to it. Add separate session drivers behind
`Agent`; preserve the Codex one-shot path's ChatGPT authentication and
stripped API-key environment. The session driver relaxes the one-shot path's
read-only sandbox only as far as the workspace and the Trashtalk store.

Suggested session-driver messages:

```smalltalk
@ backend capabilities
@ backend launch: context delivery: delivery run: run
@ backend resume: externalRef context: context delivery: delivery run: run
@ backend outcomeOf: run
@ backend interrupt: run
```

`launch:` and `resume:` return once the detached process exists, with its PID
and log paths recorded on the run. `outcomeOf:` parses the run's output log
into a normalized outcome. Live input to an active process, stdin reconnect,
and concurrent conversations are later capabilities exposed through
`capabilities`; adapters must reject unsupported operations explicitly.

### Harness selection

The first slice requires headless execution, resume by conversation id in a
new process, machine-readable output or a last-message file, cheap startup,
and a shell tool through which the agent can run `trash-send`.

| Harness | Fit | Notes |
|---|---|---|
| Codex (`codex exec`) | First driver | Already integrated and ChatGPT-authenticated. `--json` JSONL, `-o` last-message file, `exec resume <id> <prompt>`. The only candidate with a real sandbox. Verified on this machine (codex-cli 0.153.4): `trash-send`, SQLite persistence, and the Honker dylib all work under the workspace-write seatbelt, and writes to `~/.trashtalk` succeed only with it declared as a writable root (`--add-dir` on `exec`, or `-c 'sandbox_workspace_write.writable_roots=[...]'`). The thread id is on line 1 as `thread.started.thread_id`; resume in a fresh process carried context and reported cached input tokens. Gotchas: `exec` blocks reading stdin unless launched with `< /dev/null`; `exec resume` does not accept `--sandbox`, so set `-c sandbox_mode` instead; SIGINT exits promptly with status 1 and writes no terminal JSONL event, so interruption is detected by exit, not by output. Cheapest catalog model was `gpt-5.4-mini`, a candidate for the low-power profile. |
| maki | Second driver | Claude Code protocol clone: `-p`, `--output-format stream-json`, caller-chosen `--session-id` and `--session` resume, `--input-format stream-json` for later live input, cache token counts in usage. Verified (maki 0.5.2): print mode strips the built-in `question` tool from the tool list, and `--permission-prompt-tool stdio` emitted no control request, so the only blocking-question path is `askUser:` over `trash-send`, which is the design anyway. SIGINT is ignored in print mode and the turn runs to completion, so interruption needs SIGTERM. No sandbox; containment needs `--disallowed-tools` for `task`, `memory`, `code_execution`, and `webfetch`, plus `--no-plugins` and `--strict-mcp-config`. Two processes on one session id have no lock, so the serialization rule is a hard requirement. Direct OpenAI models rate-limited on this host; OpenRouter worked. |
| jcode | Not first | Headless `run` lives in an auto-started daemon with swarm, auto-poke, and memory side features that must be disabled by environment. Its daemon attach story is a reference for later reconnect work. |
| Axe | One-shot only | No session or resume concept; stays behind `@@`. |

Provider authentication, model configuration, prompt expansion, tool loops,
and provider-request retries remain the harness's responsibility. Trashtalk
owns durable delivery retries and policy budgets around those executions.

Distinguish resuming a stored conversation from reconnecting to a still-live
process: the first slice needs only the former plus observation of a detached
process. Persist external references as soon as they are known; where the
harness accepts a caller-chosen conversation id (maki), choose it before
launch and persist it before any I/O. Where the harness assigns the id
(Codex reports it in its first output event), the durable output log is the
record: a replacement worker recovers the id from the log even if the
original worker died before persisting it. The initial usable release
includes a tested conversation-resume path on the first driver.

Output normalization is jq inside each adapter. The Rust adapters in
`rk-harness` are not a dependency.

### Context and checkpoints

Supply versioned identity, archetype and role revisions, workspace context,
input references, unresolved questions, and available tool schemas. On resume,
deliver new context and relevant policy changes without duplicating a full
transcript the harness already owns. On a fresh start, reconstruct a bounded
context from durable work records, summaries, and referenced source material.

Normalize backend output inside the adapter. Separate text output, progress,
blocking questions, and final results.

Settlement is authored by the agent. The prompt lists the delivery ids
verbatim, and the priming instructions require the agent to settle each
delivery through `trash-send` as its final action. That call is explicit,
attributable through the run token, idempotent, and written into the
authoritative store, which is stronger than any input acknowledgement a
harness protocol offers. An agent may settle a named delivery early while it
keeps working on others.

The worker supplies the fallback. A process that exits with a result but
leaves deliveries unsettled marks them `uncertain` and surfaces that to the
user; the worker never replays them automatically and never infers success
from exit. Successful completion requires durable results, action outcomes,
and settlement of the inputs the agent claims to have processed.

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

In the first slice the agent reaches Trashtalk through its shell tool and
`bin/trash-send`. The agent-facing surface is one small class, addressed
through the run token in `TRASHTALK_RUN_TOKEN`:

```text
trash-send AgentRun settle: <deliveryId>          # mark a delivery processed
trash-send AgentRun settle: <deliveryId> note: <text>
trash-send AgentRun result: <body>                # reply in-thread to the delivery's sender
trash-send AgentRun send: <body> to: <address>    # message a session, identity, or user
trash-send AgentRun send: <body> to: <address> key: <idempotencyKey>
trash-send AgentRun askUser: <question>           # blocking question; marks the delivery blocked
trash-send AgentRun inbox                         # unread messages for this session
trash-send AgentRun events: <rangeSpec>           # permitted stream events
```

Every selector derives identity, session, and run from the token and ignores
any model-supplied sender. A send with a `key:` is deduplicated on run plus
key, so a model that repeats itself produces one logical message. Tokens are
redacted from logs and only hashes are persisted. These selectors are what a later MCP or typed-tool gateway would
wrap; the contract does not change when the transport does.

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
| Direct source/object mutation | Denied at the proposal gate |
| Change identities, grants, or worker policy | Denied at the owning selectors |
| Arbitrary shell or selector dispatch | Not containable in the first slice; bounded by the harness sandbox and OS user |

Trashtalk enforces recipients, budgets, settlement, and proposal gates at the
selectors it owns. Everything else the shell can do is bounded by the harness
sandbox and the OS user, not by the role. A role that needs stronger
containment than the selected harness provides is ineligible for that harness;
a restrictive prompt does not contain an unrestricted shell. State this
limitation in the archetype instructions and the session summary so it is
not mistaken for enforcement. A later gateway that intercepts built-in tools
(for example maki's `can_use_tool` control request) can close the gap without
changing the selectors above.

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
  "blocks_delivery": "delivery_...",
  "correlation_id": "...",
  "causation_id": "event_...",
  "dedupe_key": "delivery_...:action_...",
  "created_at": "2026-09-07T12:00:00Z"
}
```

Kinds begin with `note`, `question`, `alert`, and `result` under a
versioned schema; today `Message.kind` is free-form and unvalidated, so
validation is new work. Questions name the delivery they block in
`blocks_delivery`; the reply returns that delivery to `pending`.

Both `from` and the `sender_*` fields are derived from the run token. A
message sent from an interactive shell without a token carries the OS user as
sender and is marked human-sent; that attribution is trusted, not proven.

Publish attributable send, delivery, and processing events into
`AgentStream`, a durable agent-message stream distinct from the ephemeral
`EventBus` wrapper. Users must be able to follow exchanges across sessions,
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
delivery offered/blocked/processed/uncertain/failed/skipped, action
proposed/decided/finished, and
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

Use `Tool`'s exact-argv path, extended with duplex I/O, for the temporary
bridge.
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

## Gusgus: the `@@` assistant session

`@@` currently sends one read-only request through Axe or Codex and forgets
it. It becomes the entry point to a persistent assistant named Gusgus.

- Identity `gusgus`, archetype `assistant`, role `assistant`: workspace read,
  message and ask the user, submit proposals; no named tasks at first.
- Backend profile `assistant-low-power`: a Sonnet-class model or the "luna"
  tier at the very most, never a frontier model. The profile maps to a model
  id per driver (Codex: its lowest tier that supports resume; maki:
  `anthropic/claude-sonnet-*` once Anthropic auth is available). Model ids
  live in the profile record, not in code.
- One session per workspace, where the workspace is the repository root
  containing the current directory, or the directory itself outside a
  repository. Identity routing on that workspace finds the session or creates
  it on first use. Sessions stay `open` indefinitely; `@@ --fresh` closes the
  current one and opens another.

`@@ 'text'` persists a message from the user to that session, runs a
foreground `AgentWorker tick` so no worker daemon is required, prints the
message id, and returns. It does not wait. Gusgus's answer arrives as an
ordinary message in the user's inbox, in the same thread as the question,
sent by the agent through `AgentRun result:` before it settles the delivery.
The user reads and answers it with the existing inbox messages; Innards
views come later.

```bash
@@ 'why did the last build fail?'        # sends, launches Gusgus, returns
inbox=$(@ Inbox named: 'chazu')
@ $inbox list                             # Gusgus's reply appears here
@ $inbox show: $msg
@ $msg reply: 'try it with verbose on'    # lands in Gusgus's inbox, same thread
@ $inbox thread: $msg                     # the whole exchange, oldest first
```

A reply addressed to an agent identity or session is itself a delivery:
`Inbox deliver:` runs a foreground tick for the target session after saving
the message, so answering Gusgus relaunches the conversation without a
worker. A second message while Gusgus is working queues for the next
process, which resumes the same conversation. Because settlement and the
result message are both written by the agent, the answer lands in the inbox
even if no tick runs afterwards; the worker's uncertain-on-exit fallback
applies on the next tick, whenever that is.

`@@ --dry-run` still shows the assembled context, `@@ --fresh` starts a new
session, and `@@ --one-shot` keeps the current stateless path. The existing
per-call context (working directory, last status, last result) is appended
after the stable prefix as delivery context, so caching still applies across
calls.

Gusgus is the Phase 1a dogfood target alongside the test observer. It needs
no event subscription, exercises resume on every call, and makes the
settlement fallback visible immediately.

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
identity. The tmux-backed class is renamed `TmuxSession` before the durable
`AgentSession` is added; the rename is mechanical and covered by existing
tests. Deprecate tmux-oriented session methods as migration paths become
available.

## Failure and recovery

- **Missed notification:** reconciliation finds pending receipts, unadvanced
  ranges, or incomplete deliveries, even if their wake jobs were acknowledged.
- **Duplicate wake:** the existing delivery key and run assignment prevent
  duplicate semantic dispatch.
- **Crash between launch and recording the PID:** the launch wrapper writes
  its own PID and the run id to the run's pid file before exec'ing the
  harness, so the process is discoverable without the worker. Reconciliation
  adopts a live PID from that file; if the file is absent or the PID is dead
  with no result, the delivery returns to `pending` and relaunches under the
  same key.
- **Worker crash during live execution:** the harness keeps running because
  it holds no pipe to the worker. A replacement worker reclaims ownership
  with a new generation, observes the PID and output log, and waits for exit.
  It relaunches only after proving the process is gone. Reject stale
  capability use and stale settlement.
- **Process exits without settling:** mark its deliveries `uncertain`, record
  the outcome, and surface it to the user. Never replay automatically; a
  provider end event alone settles nothing.
- **Process exits without a final result:** mark the run `failed` and its
  deliveries `failed`; retry under the same delivery key up to
  `runBudget.retryLimit`, linking each attempt through `priorRun`.
- **Delivery exhausts its retries:** the worker marks it `failed` with
  attempts exhausted, sends an `alert` to the owner's inbox naming the
  session and delivery, and the session's activity summary shows a stalled
  count. It stays there until a human runs `skip:note:` or fixes the input
  and requeues it. Nothing behind it advances in the meantime.
- **Interrupted run:** deliveries that were `offered` become `uncertain` if
  the process produced any output, and return to `pending` otherwise. If
  that distinction proves hard to make reliably, treat all of them as
  `uncertain`. Codex exits on SIGINT without a terminal event; maki needs
  SIGTERM. The adapter's `interrupt:` owns that difference.
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
4. Trashtalk never evals model-authored strings. The model's shell runs inside
   the harness sandbox, and the first slice does not claim to contain it.
5. Keep source/object mutation behind existing proposal and acceptance gates.
6. Qualify each harness's built-in tool restrictions and record which roles
   it is eligible for.
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
- Inventory Process/Tool/Shell/Coproc and background-loop callers so Phase 2
  consolidation has a baseline; do not start the consolidation.
- Done: `trash-send` works inside the Codex `workspace-write` sandbox with
  `~/.trashtalk` as a writable root; maki's containment flags are recorded
  in the harness table. `exec resume` with `-c sandbox_mode` resumed a real
  Gusgus conversation and the agent settled through `trash-send` from inside
  it. Note that `exec resume` rejects exec-only flags such as `--color`.
- Specify the minimal durable-subscription trait and declarative filters.

### Phase 1a: durable headless development slice

- Rename the tmux-backed `AgentSession` to `TmuxSession`, then add the
  domain records under `AgentSession`.
- Implement transactional input/outbox creation and idempotent delivery
  assignment with plain-SQLite fencing before enabling dispatch.
- Add detached launch with exact argv, redirected output, and PID recording
  next to `Tool`'s existing capture path; do not build the common Process
  core here.
- Add the Codex session driver: `codex exec --json` with
  `--sandbox workspace-write --add-dir ~/.trashtalk`, `exec resume` for later
  deliveries, and the run token in the environment.
- Add the `trash-send`-reachable `AgentRun settle:`, `result:`, `send:to:`,
  and `askUser:` selectors with token-derived attribution.
- Persist normalized run/message events and settlement, including the
  uncertain-on-exit fallback.
- Add a foreground `AgentWorker tick` that reconciles a bounded batch; do
  not add another detached loop.
- Add Gusgus: the identity, archetype, role, and profile records, the `@@`
  send-and-tick path, and the tick-on-deliver hook so inbox replies relaunch
  the session.

Acceptance: a test.completed event wakes a test-observer archetype under a
workspace-reader role. The agent settles the delivery and sends a durable
result or question to the user through `trash-send` from inside the sandbox.
A replayed wake does not create another logical send. A second delivery
resumes the same Codex conversation in a new process. A `@@` question is
answered by a message in the user's inbox in the same thread, an inbox
reply to that message relaunches Gusgus with the conversation intact, and a
second `@@` issued while it is working is answered by the next process.

### Phase 1b: initial usable session release

- Add maki as the second driver with caller-chosen session ids and resume;
  keep Codex as the default.
- Exercise multi-delivery work in one conversation; queue input arriving
  mid-process and confirm the next process receives it.
- Prove the recovery path: kill the worker mid-run, confirm the harness keeps
  working, and confirm the replacement observes rather than relaunches.
- Add stable context assembly, supported cache configuration, and available
  usage measurements. Compare cold and warm journeys.
- Verify that neither work nor authority depends on a provider transcript,
  cache entry, or search index.

Phase 1b is required for the initial usable release; continuity is not a
distant optional follow-up.

### Phase 2: continuous operation and platform qualification

- Run the same worker entrypoint continuously with bounded polling, batched
  queries, lease renewal, retry limits, and reconciliation.
- Provide launchd and systemd user-service installation and activation paths.
- Test real worker and harness failures, duplicate input, lease takeover,
  unsettled exits, and idempotent effect recovery.
- Build the common Process/Tool lifecycle core, migrate background callers
  onto it, and remove superseded internals after compatibility checks.
- Add live input as a declared capability where the adapter supports it
  (maki stdin stream-json), and the `can_use_tool` permission bridge.
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
- Duplicate dispatch does not create duplicate runs or duplicate offered
  deliveries.
- A backend conversation is not concurrently mutated beyond its declared
  capabilities; independent runs can share permitted session data.
- A harness run spans multiple model turns without another agent relaying them.
- Input during active work is visibly queued for the next process boundary,
  or delivered live only where the adapter declares it.
- `trash-send` settle from inside the Codex sandbox succeeds and is
  attributed to the run; a settle with a stale or missing token is rejected.
- A process that exits without settling leaves its deliveries `uncertain`,
  never `processed`.
- A worker crash mid-run leaves the harness running, and the replacement
  worker observes it rather than relaunching.
- A delivery that exhausts its retries alerts the owner and stalls the
  cursor; only a human `skip:note:` advances past it.
- A repeated `send:to:key:` from one run produces one logical message.
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
3. Deliver another message while it is working; inspect its queuing and its
   delivery by the next resumed process.
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

The design commits to durable sessions; one harness process per delivery
with continuity through resume; agent-authored settlement over `trash-send`
with token-derived attribution; sandbox-based containment in the first slice;
Codex as the first driver and maki as the second; separate archetypes and
permission roles; trait-based subscription reuse; early conversation
continuity; measured cache behavior; plain-SQLite fencing; macOS/Linux
supervision; and Emacs-style Innards controls. Message visibility is core; a
generic stream applet is a stretch goal. The shared Process lifecycle
implementation is Phase 2 work, not a prerequisite.

Also decided: rename the tmux class to `TmuxSession` before the durable
`AgentSession` lands; approvals reuse the question path; a human-only
`skipped` state with owner alerts handles head-of-line blocking; the
subscription record owns the cursor; interruption yields `uncertain` when
output exists and `pending` otherwise; budgets carry turn and USD caps;
identities own inboxes and paused sessions stay routable; questions carry
`blocks_delivery`; sends accept an idempotency key; the worker writes
heartbeats from process liveness; the durable message stream is
`AgentStream`; `rk-harness` is not reused; versioned definitions are
Persistable instances linked by `supersedes`.

Deferred, none of which block Phase 1a:

- `executionPolicy` values and the definition of a compatible run, once
  more than one run per session is allowed.
- Exact capability serialization, supervision commands, event
  retention/compaction policy, and performance thresholds for the stream
  experiment.
- The exact model id behind the `assistant-low-power` profile per driver.
  On Codex, `gpt-5.4-mini` was the cheapest catalog entry and resumed
  correctly.

Those selections must satisfy the contracts above; they must not silently
revert to one model turn per wake.
