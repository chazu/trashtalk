# Durable agent delegation

An agent entrusting background work creates an `Assignment` before the worker
can launch it. Responsibility belongs to an `Agent::Identity`; a session and
its runs participate in that assignment. The existing manual `draft:` /
`assignTo:` / `workIn:` walkthrough remains manual.

## Agent interface

Use the authenticated run launcher shown in the agent prompt:

```bash
assignment=$("$launcher" Agent::Run delegate: 'Explain the failing fixture' \
  criteria: 'Record the reproduction and evidence identifying its cause' \
  key: 'fixture-investigation-1')
"$launcher" "$assignment" show
```

The default selects the `specialist` identity's current session in the run's
execution workspace, or creates it with the assignment specialist archetype
and role. A new specialist session uses the requesting run's backend profile;
an existing session retains its profile and provider conversation. To select a
configured specialist explicitly, use
`Agent::Run delegate: OBJECTIVE to: SESSION criteria: CRITERIA key: KEY`.

The request key is stable within the requesting conversation. Retrying the same
key returns the same Assignment, including after completion. Reusing it with a
different objective, criteria, workspace or explicit target is rejected. Choose
a new key for genuinely new work.

This slice permits one open child per requesting conversation and one open
assignment per specialist session. The caller needs a live authenticated run
with `message.send`, the specialist must have `assignment.work`, and both
identities must belong to the same human. Assignment workers cannot delegate
recursively. Ambiguous batches of originating requests are rejected.

The returned handle means work is **queued**. The worker records actual running
activity separately. The prompt instructs the coordinator to acknowledge the
handle and use Assignment inspection when reporting its status.

## Specialist lifecycle

The worker claims the existing delivery and includes its Assignment in the
prompt. The specialist reads the objective, criteria and history through the
public interface:

```bash
assignment=$("$launcher" Trash currentAssignment)
"$launcher" "$assignment" show
"$launcher" "$assignment" progress: 'Reproduced the failure; evidence is ...'
question=$("$launcher" "$assignment" ask: 'Which branch should I investigate?')
# Wait for the human's ordinary Inbox reply and a subsequent worker notification.
"$launcher" "$assignment" complete: 'Cause, evidence, tests, and remaining risks ...'
```

Progress is durable. Questions retain their exact assignment/delivery links;
reading or archiving a question does not answer it. An ordinary reply resumes
the assignment. If the human continues it in another session, the answer follows
the current participation while retaining the original question and history.

Completion records the outcome, settles the selected work delivery and publishes
the result to the original requesting session in one Store transaction. The
committed outcome also obliges the worker to update the human's status item.
Generic `Agent::Run result:`, `settle:` and
`askUser:` cannot bypass this Assignment protocol. A process exiting zero with
unsettled work leaves the Assignment open and visible as needing review.

The human can use the existing explicit recovery, continuation and cancellation
operations. Cancellation is separate from stopping an execution; reconcile its
effects before cancelling or selecting a replacement session.

## Human visibility and recovery

Each automatic Assignment owns one durable status Message in the human inbox.
Queued, running, meaningful progress, waiting, completion and review states
update that item instead of adding a message per transition. Questions remain
separate actionable messages. The status includes the assignment, identity,
session and run handles; the workboard derives activity from these same records.

The status Message also appears in the requesting conversation. An updated
visible message is acknowledged again by `inagent`. Worker reconciliation
repairs run-status projections after restart, including terminal outcomes whose
status update has not yet been published, even if Gusgus is busy, paused or
unavailable. Progress and completion write domain records; only the worker
updates the existing status Message, avoiding competing writes from the agent.
Outcome notifications tell the coordinator that the human already has the
durable result, preventing an unnecessary reply back to the specialist.

This is Assignment reporting. General native-chat unseen cursors and coalesced
conversation notifications are a separate integration.

## State machines and advice

`StateMachine` is a small trait: the including class supplies `transitionRules`,
and `requireTransitionFrom:to:` validates an edge. Run, Delivery and Assignment
retain separate state spaces. Each class owns its guards, persistence and
effects; the trait does not infer assignment completion from run success.
Persistence is optional for the trait. Its validator neither changes nor saves
state; these three hosts persist changes in their own transition operations.
See the [StateMachine reference](state-machines.md) for the full contract,
including cached mutations, explicit saves and concurrency guards.

Delegation uses explicit domain methods and Store transactions. Method advice
does not provide a durable transaction or a worker recovery boundary. Before /
after advice may be useful for optional diagnostics, but it does not create
work, launch agents, settle deliveries or publish required outcomes here.

External issue references remain opaque metadata. Assignment completion does
not imply acceptance, merge, deployment or a remote tracker state change.

## New implementation types

| Type | Responsibility and entry points | Persistence |
| --- | --- | --- |
| [Agent::Delegation](../trash/Agent/Delegation.trash), class | Implements the `Agent::Run delegate:criteria:key:` and explicit-target entry points through `create:session:criteria:key:`. Validates the coordinator, deduplicates the request, selects a specialist and publishes work. `reconcile` repairs status reporting from committed records. | Uses class methods, without creating Delegation instances. Durable responsibility belongs to Assignment and its linked session, delivery and message records. |
| [Assignment::Notifications](../trash/Assignment/Notifications.trash), trait | Adds `updateStatusWithin:` to Assignment. Builds and maintains its single human inbox status Message from current activity and evidence. | Requires a Store transaction. Creates the initial status with delegation; the worker updates it thereafter. Unchanged content leaves read status alone. |
| [StateMachine](../trash/traits/StateMachine.trash), trait | Shares `requireTransitionFrom:to:` validation using host-defined `transitionRules`. | No storage dependency or automatic writes; the host owns persistence. |

Agents use the public Run and Assignment operations shown above. The `Within:`
methods are transaction implementation hooks, not standalone agent commands.
`Agent::Worker` invokes reconciliation; agents do not need to poll for it.

Automatic Assignments retain `requestKey`, `requesterSession`, `originRun`,
`originMessage` when present, `delegationTarget` and `statusMessage` alongside
their existing ownership and participation history. The status Message's
`assignmentState` records which lifecycle state has been projected; this lets
the worker find terminal outcomes still needing a status update after restart.

## Agent prompt contract

Gusgus's `assistant` archetype revision 2 describes both inbox and direct input,
durable delegation, prompt acknowledgement and evidence-based outcome reporting.
The common `Agent::Worker writePrompt:run:session:deliveries:` context supplies
the exact run launcher and commands for ordinary deliveries, Assignment work
and outcome notifications. `Agent::Conversation writeContextFor:` supplies the
direct-input protocol and delegation instructions without inventing a delivery
to settle.

The generated instructions require the coordinator to acknowledge a queued
Assignment promptly, distinguish queued from running, and remain available
instead of polling. The specialist records progress, ends its turn after a
blocking question, and explicitly completes with evidence. Outcome instructions
require a report in the conversation and settlement of that notification,
without an extra result message back to the specialist or human inbox.

Existing sessions keep their immutable archetype revision and provider
conversation. They receive the current operational instructions in each newly
generated run context; an already-running turn is not retroactively rewritten.
The current protocol explicitly governs routing and delegation when older
archetype text describes an earlier workflow. A fresh Gusgus session uses
revision 2. Prompt guidance supplements the API's durable and authorization
checks; it is not itself an enforcement boundary.

The top-level `Agent` one-shot facade is separate. Its Codex request
contexts identify `one-shot-read-only` mode and explicitly prohibit creating
Assignments, delegating, sending inbox messages or promising background work.
Such work belongs in a persistent session with a current authenticated launcher.

## Validation

`tests/test_agent_delegation.bash` exercises the actual detached ShellDriver,
worker, authenticated launcher and Assignment API in an isolated checkout.
The manual Assignment and transaction tests retain their existing contracts.
The final `make verify` run passed all 97 runtime files and 50 compiler files,
with no failures or timeouts. Focused coverage passed 35 delegation checks and
202 transaction checks. Innards passed 14 agent unit tests and two PTY tests;
the resulting `inagent` binary was installed locally.

On 2026-09-16, an isolated live Jcode (`v0.84.0`, `gpt-5.6-terra`) journey
completed: Gusgus delegated a read-only fixture task, the specialist recorded
progress and returned the exact value from `evidence.txt`, the existing human
status item showed completion, and Gusgus read and settled the outcome
notification. All runs became idle and all deliveries drained. This qualifies
one complete provider journey; question/replacement and fault cases are covered
by the deterministic tests.

The live check also exposed missing cwd metadata on a new empty Jcode
conversation. The adapter now bootstraps a fresh native session with a verified
`pwd -P` result before model input when that metadata is absent. Existing
conversations retain their native reference. `tests/test_jcode_workspace.bash`
covers that fallback and refuses wrong directories or busy targets.

The subsequent prompt clarification passed the 35-check delegation journey,
31 direct-conversation checks (including an unchanged revision-1 archetype
receiving current run instructions), and 43 checks for each one-shot adapter.
A second isolated live Jcode journey used only a plain-language request for a
specialist to inspect `evidence.txt`, without API names or a request key. Gusgus
created and acknowledged the Assignment, the specialist recorded progress and
completed it, and Gusgus reported the exact value and evidence after receiving
the outcome. All three runs succeeded and all deliveries drained. This is
evidence for that journey, not a guarantee that every model response follows
the prompt.
