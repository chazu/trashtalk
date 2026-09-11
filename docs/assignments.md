# Assignments: manual walkthrough

An `Assignment` records work entrusted to an `AgentIdentity`. It retains its
objective, completion criteria, requester, progress, questions, outcome, and
session/run participation history when execution moves to another session.
Its lifecycle is `open`, `completed`, or `cancelled`. Completion reports an
outcome; acceptance, merging, deployment, and external issue state are separate.

This is the first manual slice of the [delegation plan](agent-delegation-implementation.md).
`workIn:` publishes a Message and a held AgentDelivery in the chosen session's
inbox. The worker does not dispatch these deliveries. Assignment messages and
replies remain available through Inbox, including when a worker service is
running. Automatic specialist dispatch and generated assignment prompts are
the next slice. No model login is needed for this walkthrough.

## Try it from Bash

Use a real workspace directory. The following creates an explicitly configured
identity and session; the `shell` profile will not be launched by these calls.

```bash
source "${TRASHTALK_DIR:-$HOME/.trashtalk}/lib/trash.bash"
workspace=$(@ AgentSession workspaceFor: "$PWD")
agent=$(@ AgentIdentity named: assignment-specialist)
@ "$agent" owner: "${TRASHTALK_USER:-$USER}"
@ "$agent" save
archetype=$(@ AgentArchetype define: assignment-specialist revision: 1 \
  instructions: 'Inspect the assignment and read messages through Inbox.' profile: shell)
role=$(@ AgentRole define: assignment-specialist revision: 1 \
  capabilities: '["inbox.read","message.send","assignment.work"]' \
  workspacePolicy: '[]' runBudget: '{}')
session=$(@ AgentSession openFor: "$agent" archetype: "$archetype" role: "$role" \
  workspace: "$workspace" profile: shell)

assignment=$(@ Assignment draft: 'Explain the failing integration test' in: "$workspace")
@ "$assignment" criteria: 'Record the cause, reproduction, and supporting evidence.'
# Optional, opaque metadata; this never contacts a tracker:
@ "$assignment" issueReference: 'TKT-123'
# When continuing an existing request, attach its Message before workIn:
# @ "$assignment" origin: "$requestMessage"
@ "$assignment" assignTo: "$agent"
@ "$assignment" workIn: "$session"
@ "$assignment" show

@ "$assignment" progress: 'Reproduced the failure; checking the fixture expiry.'
question=$(@ "$assignment" ask: 'Should the investigation include the integration suite?')
@ "$question" readText
```

The human is the trusted local operator. The progress and question above are
attributed to that human. An authenticated, authorized run uses the same
operations with its session attribution. A question is a Message to the
assignment's human owner; reading or archiving it does not answer it.

Continue the same assignment in another session, retaining the question:

```bash
@ "$session" close
nextSession=$(@ AgentSession openFor: "$agent" archetype: "$archetype" role: "$role" \
  workspace: "$workspace" profile: shell)
@ "$assignment" workIn: "$nextSession"
@ "$question" reply: 'Yes, include it.'
@ "$assignment" show
@ "$assignment" complete: 'The fixture credential has expired; the integration reproduction confirms it.'
outcome=$(@ "$assignment" resultMessage)
@ "$outcome" readText
```

Completion atomically records the outcome, settles only the selected work
delivery, and publishes the result Message. The result replies to the origin
Message's sender and thread, or goes to the human owner when there is no origin.
When that requester is an agent, its identity and originating session are
recorded separately. Notifications to sessions are held for manual inspection
in this slice, just like assignment work and question replies.

Inspect from another shell by passing the saved assignment handle:

```bash
source "${TRASHTALK_DIR:-$HOME/.trashtalk}/lib/trash.bash"
@ assignment_REPLACE_WITH_YOUR_HANDLE show
```

For an **open** assignment, a human can explicitly select the convenience
context with `export TRASHTALK_ASSIGNMENT_ID="$assignment"`, then use
`@ Trash currentAssignment`. An agent's context is resolved from the exact
delivery its authenticated run holds. Zero matches or multiple matches produce
an error; with multiple assignments, supply the handle explicitly or set
`TRASHTALK_ASSIGNMENT_ID`. `snapshot` returns a fresh JSON view for scripting.

## Rules and recovery

- `assignTo:` requires an enabled `AgentIdentity`, owned by the local operator
  or with no owner set. Repeating it with that identity is harmless; changing
  identities is outside this slice.
- `workIn:` requires an open session of that identity in the same canonical
  workspace, allowed by its role. Repeating the selection does not publish
  another message. Another draft intentionally creates another assignment.
- Stop or reconcile a live execution before replacing its session or completing
  as the operator. An offered, failed, or uncertain delivery prevents session
  replacement. Inspect its run/effects, then use the existing session recovery
  operations to settle or explicitly skip it before continuing. A closed
  session alone does not resolve execution or change assignment lifecycle.
- Questions and answers survive replacement. Only a direct reply with matching
  sender and recipient answers a question. All questions must be answered before
  completion. The queue stores answer links separately from Message read state.
- Agent mutations require a live run token, the `assignment.work` capability,
  and ownership of the current generation's exact delivery. A supplied invalid
  token fails even in a human shell. Superseded runs cannot complete the work.
- Repeating `complete:` with the same outcome is harmless. A different outcome
  after completion fails. Use another assignment for follow-up work.
- `cancel:` records a cancellation reason and skips the selected delivery. It
  is operator-only and requires live execution and uncertain effects to be
  resolved first; it does not stop unrelated work.
- Objective criteria, origin, and optional issue reference are configured before
  initial publication. Questions, evidence in progress entries, and the outcome
  remain on the assignment. No ticket status is mirrored or changed.

These are public API consistency checks under Trashtalk's trusted local-user
model. They are not an OS security boundary. Unrestricted Bash can still write
directly to the database or use low-level object setters.

## Validation

`tests/test_assignment.bash` performs this journey in an isolated store and
uses controlled run fixtures for authorization. It covers atomic publication
and completion rollback, concurrent replay, preserved session history and
questions, exact result routing, manual dispatch exclusion, and stale-token
rejection. Real harness delegation is deliberately deferred to slice 2.

Validation on 2026-09-10: `make` succeeded. Assignment (103), agent records
(207), questions (32), worker (68), recovery (17), and Inbox (73) passed all
500 checks in isolated stores. The walkthrough and run fixtures exercise the
public Bash API; no live model was dispatched for this slice.

Persistence uses ordinary `Persistable` saves inside a shared `Store` transaction;
Assignment changes, delivery settlement, and message/queue publication commit
together. A conflicting concurrent mutation requires an explicit retry. Identical
selection or completion can acknowledge a result already committed by another
caller. See [the persistence design](assignment-persistence-implementation.md).
