# Workstation event, attention, and delegated-action layer

**Status:** Proposed design, 2026-09-13.

## Decision

Trashtalk should become a local, durable coordination layer in three deliberate
steps:

1. **Observe:** persist local events and subscription cursors.
2. **Orient:** turn meaningful events into grouped, actionable Inbox attention.
3. **Act safely:** let agents prepare or perform narrowly authorized actions,
   with explicit approvals and durable effect identity.

This builds on the implemented single-host AgentIdentity, AgentSession, Inbox,
AgentWorker, role capability checks, question-to-delivery links, and Innards /
Whisker surfaces. It does **not** introduce a daemon RPC API, a generic plugin
host, a distributed scheduler, or a mandatory new terminal applet.

## Goals

- Let ordinary workstation events produce durable, inspectable work without a
  user manually copying terminal output into a chat.
- Make interruptions useful and bounded: an event should become one grouped
  attention item with a clear next action, not a stream of notifications.
- Allow a user to delegate preparation and low-risk work while retaining clear
  attribution, approval, cancellation, and recovery semantics.
- Preserve the current Bash-only, SQLite-backed, single-host architecture.
- Keep all decisions and external-effect claims auditable through public
  messages and persisted objects.

## Non-goals

- Exactly-once execution of arbitrary shell commands or external effects.
- A general distributed event bus, cloud service, or multi-machine leader
  election.
- An always-running model or an agent that can silently act beyond its role.
- Replacing the shell, terminal multiplexer, desktop notification system, or
  existing Innards components.
- A broad plugin ABI. New sources are explicit adapters until repetition proves
  a stable primitive is needed.

## Why these are the highest-leverage pieces

Durable agents already receive messages and can recover from worker failure.
What they lack is a reliable reason to receive work when the user is not
actively composing a request. Durable subscriptions provide that reason.

Raw event streams are noisy. An attention model turns repeated failures,
modified files, and requests for input into a manageable queue visible at the
prompt and in the existing Inbox/session browser.

Finally, a response that merely describes a repair has limited value. Delegated
action becomes useful only when the system can answer: which event caused it,
which role was allowed to do it, whether the user approved it, and whether a
retry might repeat it.

## Representative applications

| Event source | Subscription | Attention item | Delegated outcome |
|---|---|---|---|
| test command exits nonzero | watched command receipt | one failure group per command/workspace/fingerprint | summarize regression, propose patch, ask before edits |
| Git working tree changes | debounced repository snapshot | review-needed group | summarize changes, run configured checks |
| a file changes | path snapshot plus debounce | changed-config group | validate format, propose a correction |
| scheduled local check | interval cursor | overdue or failed-check group | investigate and report |
| agent asks a question | existing Message/Question | blocking attention item | focus exact conversation and answer it |

The first shipped source must be **a watched command receipt**, especially a
failed test. It has a bounded payload, clear deduplication key, no filesystem
watcher portability problem, and immediately exercises observation, attention,
and delegation.

## Domain model

### `EventSubscription`

A persisted, versioned declaration owned by a local user:

```json
{
  "id":"subscription_...",
  "kind":"command-receipt",
  "workspace":"/canonical/project",
  "enabled":true,
  "source":{"commandKey":"test"},
  "filter":{"exitNot":0},
  "debounceSeconds":15,
  "targetIdentity":"agentidentity_...",
  "attentionPolicy":"group",
  "createdAt":"...",
  "revision":1
}
```

`kind` selects a narrow built-in adapter. `source` and `filter` are validated
JSON for that adapter, never shell text. A subscription has an owner and a
canonical workspace. It may target only an identity whose current role permits
that workspace.

### `EventCursor`

A cursor is owned by one subscription and records the last *durably accepted*
source position, not merely the last observed value:

```json
{"subscription":"...","sourceVersion":"...","acceptedAt":"..."}
```

The initial source needs only opaque command-receipt IDs. Later file and timer
adapters may use a snapshot digest or an interval sequence. The adapter must be
able to replay its source from that cursor or explicitly report a gap.

### `WorkstationEvent`

An immutable normalized fact:

```json
{
  "id":"event_...",
  "subscription":"...",
  "sourceVersion":"...",
  "occurredAt":"...",
  "workspace":"/canonical/project",
  "kind":"command.failed",
  "fingerprint":"sha256:...",
  "payload":{"commandKey":"test","exitStatus":1,"summary":"..."}
}
```

The unique key `(subscription, sourceVersion)` makes source replay idempotent.
A fingerprint is for grouping, not for deduplication of source facts.

### `AttentionItem`

A durable projection that represents what the person needs to notice:

```json
{
  "id":"attention_...",
  "groupKey":"workspace + kind + fingerprint",
  "state":"open",
  "urgency":"normal",
  "eventIds":["event_..."],
  "message":"message_...",
  "session":"agentsession_...",
  "snoozedUntil":"",
  "lastUpdatedAt":"..."
}
```

The group key is policy-controlled and includes the subscription identity to
prevent unrelated projects merging. A transaction appends a new event to an
open group, or creates a group and its Inbox message. A group can be
`open`, `acknowledged`, `snoozed`, `resolved`, or `suppressed`. It is not the
same as read/archive state on its message.

### `ActionRequest` and `EffectRecord`

An agent never treats a chat sentence as authorization to make an effect.
`ActionRequest` names the causal event/attention item, requested capability,
workspace, normalized parameters, risk class, and required approval. An
approved request yields an `EffectRecord` with a deterministic idempotency key:

```json
{
  "request":"actionrequest_...",
  "key":"sha256(role revision, capability, workspace, parameters, cause)",
  "state":"prepared|started|succeeded|uncertain|failed",
  "receipt":"..."
}
```

The record prevents a second local attempt for the same approved request. It
cannot make an arbitrary OS effect exactly once. A crash after launch is
`uncertain`, matching current AgentWorker recovery policy, and requires review
rather than automatic retry.

## Architecture and transaction boundaries

1. A small source adapter reads a local source and proposes normalized event
   candidates. It performs no agent work and does not advance a cursor.
2. `EventSubscription pollWithin:` runs in the existing Store transaction.
   It validates the subscription and role workspace permission, inserts unseen
   events, updates/creates the attention group, writes an Inbox message/outbox
   notification if required, and advances the cursor in the same transaction.
3. Existing Inbox routing and AgentWorker assign the resulting message to the
   target identity's current session. The event, attention item, message,
   delivery, session, and run IDs remain linked as causal metadata.
4. The agent may report, ask a linked Question, or create an ActionRequest.
   Existing question-to-delivery linkage remains the sole mechanism for
   resuming blocked delivery work.
5. Approval and effect execution occur through public, role-checked messages.
   The executor records `started` before crossing the OS boundary and writes a
   receipt/result afterwards. Ambiguous launches become `uncertain`.

SQLite remains the only persistence implementation. The new classes use
`Persistable`, `Store transaction:...`, schema migration helpers, `Require`, and
AgentAccess validation. Only adapter observation and an approved executor may
be raw Bash, because they cross OS boundaries.

## Attention UX

Whisker displays a compact count by state, for example `!2 ?1`, rather than a
new line per event. The Inbox/browser adds an **Attention** view with actions:

- **Inspect**: show grouped events, source receipt, causal messages, and runs.
- **Focus**: attach to the exact existing session without resuming/starting it.
- **Acknowledge** and **Snooze**: change only AttentionItem state.
- **Resolve**: requires a note or a linked completed action/effect.
- **Review uncertain action**: opens its evidence and offers explicit retry or
  abandonment under the existing uncertainty rules.

Quiet hours and desktop notifications are presentation policy, not event
routing. A notification is a lossy wake hint. The durable attention item is the
source of truth.

## Authorization and privacy

- Subscription creation/editing requires the local user owner. Agents can
  propose subscriptions but cannot enable one without an explicit user action.
- Source adapters use allowlisted kinds and typed configuration. No event
  payload is evaluated as Bash.
- Workspace canonicalization and role authorization happen before polling,
  routing, action request creation, and execution.
- Recipient policy, capability, run/message budgets, and approval policy are
  enforced at the corresponding public boundary, not merely stored on
  `AgentRole`.
- Event payloads are size-limited, redact configured secrets before persistence,
  and store file paths/command output only when policy allows. Large output is a
  bounded artifact with digest and access path, not an Inbox body.
- An approval identifies the exact immutable normalized parameters and expires
  when they change. “Approve fixing tests” is never an approval for an arbitrary
  shell command.

## Phased implementation

### Phase 0: contracts and migrations

Define schemas, invariants, display columns, Store transaction selectors, and
fixture adapters. Add no OS watcher or model dispatch. Test migrations, invalid
source config, authorization, replay, and uniqueness.

### Phase 1: command-receipt subscription

Add a public command wrapper that writes a durable receipt after a user-run
command, plus `command-receipt` subscription polling. Implement event/cursor
atomicity and attention grouping. Route an inbox notification to an existing
workspace-scoped specialist or Gusgus only when configured. Demonstrate a
failed test becoming one inspectable attention group after repeated runs.

### Phase 2: attention surface

Expose AttentionItem in Whisker, Inbox, and AgentBrowser using the existing
inpick/inpage composition. Add acknowledge, snooze, resolve, and focus. Ensure
that detaching a conversation changes no session/worker lifecycle state.

### Phase 3: causal links and question handoff

Add explicit event/attention IDs to routed messages, AgentDelivery, and agent
result metadata. Provide causal timeline inspection. Reuse existing linked
questions, never reintroduce broad “any message unblocks work” behavior.

### Phase 4: approved local actions

Implement ActionRequest, review UI, immutable approval, EffectRecord, and one
low-risk executor such as running a named test command. Enforce recipient,
message, run, and action budgets at public send/execution boundaries. Add
uncertain-launch review and idempotency tests before any file-mutating action.

### Phase 5: additional narrow adapters

Only after the command-receipt path is reliable, add an interval source and one
filesystem/git source. Each adapter must state its replay, gap, debounce,
privacy, and test strategy. Do not add a generic plugin mechanism prematurely.

## Required invariants and tests

- Replaying a source version creates at most one WorkstationEvent.
- A crash cannot persist a cursor advance without its accepted event and
  attention projection, nor persist an event without the cursor decision.
- Ten identical failed test receipts produce one open group with ten linked
  events, subject to a bounded history policy.
- A disabled/snoozed subscription never dispatches agent work; a snoozed item
  may still accumulate events without waking the user.
- Two workspaces or subscriptions never share a group merely because their
  output fingerprint matches.
- A stale role revision, wrong workspace, expired approval, or modified action
  parameters rejects execution before the OS boundary.
- A process crash after `started` produces `uncertain`, never automatic retry.
- Focus, preview, and acknowledgement do not settle deliveries, answer
  questions, resume paused sessions, or start a harness.
- All UI actions work without Innards through structured fallback behavior.
- Tests use fixture sources and shell executors. No test requires a paid model,
  desktop notification daemon, or live filesystem watcher.

## Measures of success

A person can run a failing test, continue working, see one unobtrusive prompt
indicator, inspect exactly what failed and which agent was asked, answer a
question in the retained conversation, and approve a bounded next action. A
worker restart or duplicate receipt does not create duplicate work. An ambiguous
external action is visibly uncertain, not silently retried.

The first release is successful when this journey is more reliable and less
interrupting than manually pasting test output into a chat. It is not successful
merely because it can observe many event sources.
