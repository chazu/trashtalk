# Agent delegation: implementation in small steps

**Status:** Slices 0 and 1 are implemented. Jcode is the default harness for new
Gusgus sessions; the common contract supports queued inbox notifications and
explicit stop. The Assignment API supports the manual human walkthrough and
controlled run fixtures, with work held from automatic dispatch. See the
[Assignment walkthrough](assignments.md) for executable commands. Automatic
specialist delegation and later slices below remain proposals.

The [Assignment persistence refactor](assignment-persistence-implementation.md)
is implemented: domain behavior uses DSL methods and traits over shared Store
transactions. Automatic specialist delegation remains the next slice.

**Original planning baseline:** `8be14c8` plus the then-uncommitted worker,
question, termination, and Maki changes. See [agent operations](agent-operations.md),
[Maki support](maki-session-driver.md), the
[headless design](headless-agent-sessions-design.md), and the
[direction review](ambient-agent-direction-review.md).

## Goal and approach

Gusgus should be able to hand someone a specific piece of work, remain
available, and bring the outcome back to the right conversation. A human
should be able to perform and inspect the same operations in ordinary Bash.

Keep the public language about things people recognize: agents, assignments,
messages, questions, repositories, worktrees, issues, and remembered facts.
Runs, tokens, outboxes, and processing acknowledgements remain implementation
concepts that are available for diagnosis but unnecessary for ordinary use.

Build one useful journey at a time. Do not build all the objects in this
document before demonstrating delegation. A proposed future interface is
a place to discuss a responsibility, not a commitment to add a class now.

## What exists and what needs changing

The runtime already has identities, archetypes, roles, durable sessions,
detached harness runs, durable message routing, questions linked to specific
deliveries, and Innards inspection. Maki and Codex can resume conversations.
The worker can run independent sessions concurrently while serializing its
dispatch decisions under the store lock.

`AgentRun send:to:` can already address another session, whose `result:` can
reply to Gusgus. However:

- Assignment now provides a durable responsibility and completion relationship
  for the manual walkthrough; model-driven delegation remains to be connected.
- `result:forDelivery:` and Assignment completion target a specific request.
  The older broad `result:` remains available and should not be used to report
  an assignment outcome across unrelated inputs.
- Identity routing requires exactly one eligible session. It does not select
  a specialist session for a particular workspace or assignment.
- Keyed sends use lookup followed by creation; delegation needs transactional
  uniqueness across concurrent calls and restarted runs.
- Prompts currently teach low-level `AgentRun result:` and `settle:` commands.
- The worker supplies inbox references in notification prompts and supports
  authorized exact-run stop. Assignment-specific context and dispatch remain
  part of the one-specialist slice.
- Roles have policy fields, but enforcement is incomplete. Maki has normal
  user OS permissions and no OS sandbox.
- Workspace is a path; repository, worktree, external issue, and long-term
  memory objects have not been added.

These are implementation gaps, not reasons to replace the worker or harness.

## Ongoing sessions and a common harness contract

A session is an ongoing conversation. A run is a period of autonomous work
within it and can contain many model/tool turns. The harness process or
connection may remain alive between runs. Completing work must not require
that a resident harness process exit. An idle resident agent need not spend
model turns polling for work.

Humans and agents use the same Trashtalk operations regardless of harness.
The minimum contract is:

| Operation | Required behavior |
|---|---|
| Send a message | Persist it in the addressed inbox and schedule a notification for its selected recipient session. Return a durable message reference, without implying that the recipient has acted. |
| Notify an idle session | Start or resume processing so the agent can read pending inbox messages. |
| Notify a busy session | Schedule a subsequent prompt to read pending messages after the current prompt finishes. A harness may also support presenting the notification at a safe point during current work. |
| Stop current work | Allow an authorized human or agent to request that the selected execution stop; record the request and report confirmed stop or an unresolved failure. |
| Inspect and reconnect | Retain message, execution, and outcome references across controller disconnection; reconcile pending notification and stop requests. |

Use **steer** for supplying input during current work and **stop** for ending
that work. Steering is an optional capability. Scheduling a later prompt and
stopping work are required. Ordinary senders do not choose ACP methods, SDK
requests, or a harness-specific messaging path. A caller that explicitly
requires steering must receive an unsupported result when the harness can
only queue input; an ordinary message remains deliverable through the common
queue behavior.

Stopping is a control operation, independent of the model reading a message
that asks it to stop. Authenticate the calling agent, resolve the target
execution, and check its authority to stop that work. Record the target's
execution generation so a delayed request cannot stop replacement work.
Stopping a run does not terminate its identity or close its session, and it
does not prove that external effects were rolled back. Queued input remains
inspectable and must not immediately restart the stopped work; continuing
that work requires an explicit action. A shared harness daemon or other
sessions must survive a targeted stop.

Add Jcode next behind this common contract. Evaluate its native Harness API
for resident sessions and optional steering; using a native protocol inside
the adapter does not change the public Trashtalk interface. Jcode is the
leading candidate for the default, with a default change following the
walkthrough below. Maki remains suitable if queued prompts and confirmed stop
work through the same interface. If it cannot meet that minimum, use Jcode
alone for this workflow rather than exposing separate public APIs.

The current launch-time run token and process-exit reconciliation assume one
process per run. The Jcode slice must establish how a resident harness gives
each execution the correct acting context and how terminal run events are
recorded independently of daemon lifetime. Keep Trashtalk responsible for
assignment, delivery, and stop authority; do not introduce a second owner of
those decisions through harness-native delegation or autonomous wake features.

### Notifications point to inbox messages

Message bodies, sender attribution, thread relationships, and replies belong
to durable `Message` objects in the messaging plane. A harness notification
carries only the references needed to discover pending messages, such as an
inbox handle and message handles or a processing cursor. It instructs the
agent to read those messages through the public Inbox/Message operations.
Do not substitute a copied body or a generated summary in the harness prompt
for that inbox read. This applies to inbox input, inbox questions and answers,
and assignment outcomes. Human input in the conversation view is direct native
session interaction and creates no Message or AgentDelivery. `@@` remains inbox
mail, with every argument treated literally.

The sequence is: persist message and notification obligation; present a wake
prompt when the harness can accept it; let the agent read the identified
messages from the inbox; record the reply or assignment outcome through the
same messaging plane. On a busy harness without steering, the queued prompt
says that messages are available and tells the agent where to read them.

Select pending work from durable delivery records, independently of unread
or archived presentation state. Identity-addressed and session-addressed
messages must both remain discoverable from the selected execution, even
when their original inboxes differ. Reading or archiving a message does not
settle its delivery or complete an assignment.

Notifications can be coalesced, but messages retain their individual identity
and thread. A repeated notification must not create another logical delivery.
A message arriving during an inbox read must remain pending for that read or
a subsequent notification. Record the difference between a stored message,
a harness accepting its notification, and the agent handling its delivery.
After a disconnect, reconcile that state; a successful transport write alone
does not establish consumption or make an uncertain replay safe.

## Agents own assignments; sessions work on them

An agent is an **`AgentIdentity` instance**. Assignments are assigned to that
object, not a bare name or an execution session. An **`AgentSession` instance**
is a particular conversation with its own workspace, archetype/role snapshots,
and harness configuration. Several sessions can work on one assignment over
time without changing its assignee.

Gusgus illustrates the distinction. The `Gusgus` class is a convenience wrapper
that finds/configures the identity whose handle is `gusgus` and finds or opens
its one identity-scoped current conversation. Workspace-scoped specialists still
use one current session per canonical workspace. These calls return different objects:

```bash
gusgus=$(@ Gusgus identity)                # AgentIdentity instance
session=$(@ Gusgus sessionFor: "$PWD")     # AgentSession instance
```

There is no implicit agent called “someone.” For the first specialist, explicitly
find or create an identity, configure its human owner, default archetype and
role, and select an execution profile. Then explicitly open a session for that
identity with the intended workspace and configuration. `AgentIdentity named:`
currently finds or creates an identity; it does not by itself configure a
ready-to-run specialist. The first slice must make this setup inspectable and
document the actual messages used, reusing existing objects.

Separate assignment from execution selection:

- `assignTo:` takes an `AgentIdentity` instance and records responsibility.
- `workIn:` takes an `AgentSession` instance, validates that it belongs to the
  assignee and is suitable for the work, and records its execution participation.
  It is the explicit dispatch boundary when harness execution is enabled.

These selectors are proposed. Automatic selection or creation of a suitable
session is later convenience; it must preserve this distinction.

## Ordinary Bash and one public object interface

Yes: use regular Bash and `@` message sends. An agent's shell tool should load
the same runtime a person loads. The launcher supplies execution context;
object methods derive the acting session from that context. Humans select
objects explicitly; agents may also ask for the assignment currently being
worked on.

For example, a specialist's shell invocation could be:

```bash
# Implemented API; agent use currently requires a controlled run fixture.
source "$TRASHTALK_DIR/lib/trash.bash"
assignment=$(@ Trash currentAssignment)
@ "$assignment" show
@ "$assignment" ask: 'Should this include the integration suite?'
# On a later invocation after the question is answered:
@ "$assignment" complete: 'The failure comes from an expired test credential.'
```

An interactive human uses that same `ask:` or `complete:` method on a selected
assignment. `currentAssignment` reports when none is selected, or when the
execution has several and explicit selection is required. Never guess the
first delivery. Shell variables need not survive between harness tool calls.

Creation and assignment should also read naturally:

```bash
# Manual Assignment API; these calls do not launch a harness.
agent=$(@ Gusgus identity)
session=$(@ Gusgus sessionFor: "$PWD")
assignment=$(@ Assignment draft: 'Explain the failing integration test' in: "$PWD")
@ "$assignment" assignTo: "$agent"
@ "$assignment" workIn: "$session"
@ "$assignment" show
```

Here Gusgus is the assignee; delegating to a specialist uses that specialist's
configured identity and session instead. Assignment alone does not launch a
harness. Repeating `assignTo:` with the same agent is a no-op, and repeating
the same execution selection must not publish duplicate work. Creating
another draft deliberately creates another assignment. For agent delegation,
resolve the draft from its originating request and logical delegation step
before assigning it; a restarted run must not manufacture a new draft each time.

`trash-send "$assignment" complete: '...'` remains an equivalent convenience
for a fresh process. It is an executable entry point to the same messages,
not a second agent protocol. Normal operations return object handles or
ordinary values; `show`, `help`, and `browse` explain them. Structured output
can serve composition without requiring people or models to construct JSON.

Behind `complete:`, Trashtalk validates the actor, records the result, settles
the relevant delivery, and queues the right notification together. An agent
should not have to remember a separate acknowledgement after every outcome.
An existing but invalid run token must fail; it must not fall back to human
authority. Human attribution follows the existing trusted local-user model.

Using the same language does not mean equal permissions. Assignment methods
can validate ownership and allowed transitions. An unrestricted shell can
still bypass the public methods or alter files directly. This plan does not
claim that these checks contain Maki or make the current runtime a security
gateway. Stronger containment is a separate, explicit capability.

## How to prompt agents

Keep prompt assembly in Trashtalk, with ordinary editable archetype text and
a small generated context section. Keep harness-specific JSON wrapping inside
the driver. Do not implement coordination in a Maki plugin.

Each run should receive:

1. **Purpose:** the specialist's archetype and expected working style.
2. **Environment:** workspace, applicable repository instructions, selected
   harness/profile, and actual capabilities. Read the applicable `AGENTS.md`
   and referenced instructions; avoid duplicating their contents indefinitely.
3. **Work:** assignment handle, assignee identity, current execution session,
   references for inspecting its objective, completion criteria, requester,
   and progress, plus inbox/message references and the source revision when
   applicable. Message and answer contents are read from the inbox.
4. **A short Trashtalk guide:** how to load the runtime, inspect the assignment,
   read notified inbox messages, ask a question, record an outcome, and
   discover additional methods.
5. **Optional evidence:** a bounded set of relevant remembered observations,
   labeled with their sources and dates.

The guide should use the same examples as human documentation. A possible
instruction is: “Use the assignment object to report your outcome or ask for
missing information. Completion means you have supplied the requested
evidence; it does not merge code or close an external issue.”

The generated context is refreshed on every run, including resumed runs;
notifications during a run identify newly available inbox messages.
It should distinguish instructions from quoted requests, retrieved text,
and evidence. Memory and old transcripts do not override current instructions
or the stored state of work. Preserve the assembled prompt for inspection;
do not include credentials or expanded run tokens.

First implement this guide for one specialist and test it by manually doing
the same job through Bash. Introduce no prompt framework or automatic prompt
optimization. Version the guide alongside the methods it documents.

## Assignment, task, and external issue

Prefer **`Assignment`** over the earlier proposed `AgentTask`. It means “this
work was given to this person or agent.” It can exist without a ticket, and
several assignments can contribute to the same external issue.

An assignment needs an objective and completion criteria, requester,
assignee identity, origin message, workspace, state, and result references.
Record the originating session separately when the requester is an agent;
it supplies conversation context, not a lifetime limit on the assignment.
Execution history links the sessions and runs that participated, with an
explicit current execution selection. Do not represent that history as one
permanent child-session field. The first implementation adds only fields used
by this journey; optional repository, issue, and memory associations arrive
when exercised.

Closing or replacing a session does not complete, cancel, or reassign its
assignment. A later session of the same agent can continue using the recorded
progress, questions, evidence, and outcomes; it need not recover the old
harness transcript. The first slice supports sequential participation only:
reconcile or stop existing execution before selecting another session, preserve
prior participation, and reject completion from a superseded execution.
An uncertain external effect still needs review before continuing. Supporting
several sessions over time does not require concurrent execution or automatic
reassignment to another identity.

Begin with lifecycle `open`, `completed`, or `cancelled`. Being unassigned,
running, waiting on a question, or needing review is activity derived from
the assignment's relationships and execution records. A failed run leaves the
assignment open with an actionable failure; it does not establish completion.
Avoid copying every run/delivery state into another state machine.

Separate reported work from accepted work: a specialist completing an
assignment has supplied an outcome. That does not assert that code is merged,
a deployment succeeded, or the external issue should be closed. Gusgus or the
human can inspect the evidence and request a follow-up assignment.

An external **issue** belongs to its tracker. Beads Rust, GitHub, or the user's
chosen `git issue` implementation owns that record's native status and
dependencies. Trashtalk owns dispatch, execution, questions, and local
outcomes. Do not mirror an entire backlog or invent a universal status enum.

Start by attaching an opaque provider/key or URL reference. After choosing
one actual tracker, add its concrete Tool adapter and an issue object that
can read the current record and perform an explicit update. A later journey
may support `@ "$issue" assignTo: "$specialist"`. Preserve native fields and
show failures; assignment completion must not silently close the issue.

Tracker-specific claims, offline sync, cross-worktree storage, and update
idempotency must be checked against that particular tool before implementation.
The phrase `git issue` alone is insufficient to select an implementation.
Do not build adapters for all three in advance or start two-way background
synchronization. A local reference to an issue is enough for the first slice.

## Delegation and continuation

Initially configure one specialist identity and one explicit profile, then
select its initial session explicitly. Assign work to the identity and select
the execution session with `workIn:`. An assignment may continue in a later
session of that identity; the model must not require one session per assignment
or tie completion to session lifetime. Resolve the exact session for each
dispatch; do not depend on ambiguous identity-inbox routing.

Persist the execution selection and its outgoing delivery obligation in one
transaction, referencing the durable assignment. A draft or assignee selection
alone must not acknowledge a successful handoff. The assignment's logical key
is tied to the originating request and a stable
delegation step, not the current process/run ID. Repeating the operation after
a crash returns the same assignment; reusing the key with different contents
reports a conflict. Deliberately starting another assignment remains possible.
Moving to a later session records a new execution participation under the same
assignment, with its own deduplicated delivery obligation.

Gusgus's handling of the input can finish once the handoff is durable. The
assignment remains outstanding. A child outcome persists a message linked to
the assignment and original human thread, together with an obligation to
notify Gusgus. Gusgus reads that message from its inbox and can summarize it
in the human thread. Reading the outcome does not complete unrelated work or
obligate Gusgus to reply to the child again. A busy Gusgus can receive a queued
notification, or a live notification when its harness supports steering.

Waiting for a specialist is distinct from waiting for a human. Neither should
keep a model process alive polling. Initially child questions go to the human
owner and remain linked to the assignment; Gusgus can inspect them. Letting
Gusgus answer selected questions can follow after the basic route works.

Completion, outcome message persistence, and creation of the notification
obligation must be atomic and replayable. Failed
or uncertain execution remains visible; silence never proves that retrying
external effects is safe. Cancelling an assignment records intent separately
from confirmed process stop. Pausing or terminating Gusgus does not silently
cancel independent child work. In particular, terminating a Gusgus session
does not terminate the Gusgus identity or its outstanding assignments. Preserve
the originating conversation link even if a later Gusgus session handles the
continuation. Until an appropriate session is explicitly selected, hold the
continuation durably and keep it visible to the human. Cancelling an assignment
must not stop unrelated work merely because it shares a participating session.

Start with one delegated assignment active at a time and no child delegation,
enforced at the public dispatch boundary. Other conversation can continue.
Batching must preserve assignment boundaries and target-specific results.

## Repository and worktree responsibilities

Repository domain objects, local-copy domain objects, and managed worktrees
are required follow-up work. Their implementation is deferred while the first
specialist execution journey is completed. The names below describe the intended
boundaries; they are not implemented APIs.

Keep these concepts distinct:

| Concept | Meaning |
|---|---|
| Repository | A durable logical Git repository identity, independent of a particular machine, clone, or checkout path. |
| LocalRepository | One local copy/clone of a Repository, with its own Git common directory, configuration, and linked worktrees. Independent clones remain distinct local copies. |
| Workspace | The directory where this assignment runs; it may not be Git-backed. |
| Worktree | A checkout belonging to a LocalRepository, with its own path, branch, and working files. |
| Issue | A record in an external task tracker. |
| AgentIdentity | The agent responsible for assigned work, such as Gusgus. |
| AgentSession | A conversation that can participate in the agent's assignments. |
| Assignment | Work entrusted to an agent identity, potentially across sessions, optionally concerning an issue and repository. |

First retain the explicit workspace path. Before enabling coding assignments,
discover a `LocalRepository` and its `Worktree` from a directory, and associate
that local copy with a `Repository`. Use Git's common directory to recognize
linked worktrees belonging to one local copy; do not assume `.git` is a
directory. Durable IDs must survive path updates. Remote URLs are metadata,
not unique identity: forks, multiple remotes, and independent clones require
explicit treatment. Association of clones with the same logical Repository
must be inspectable and correctable.

A `Worktree` belongs to a local repository and records its path, branch, base commit,
and owning assignment. Put Git operations behind a concrete Tool wrapper;
keep decisions about allocation and retention in the domain objects. Do not
add a separate `Workspace` class until path-plus-associations is insufficient.

For the first investigator, inspect the specified checkout and report whether
the observation includes uncommitted changes. A dirty checkout is not evidence
about a clean commit, and it can change during inspection.

Before allowing edits, use one managed worktree per writing assignment:

- Record an explicit base commit and create a branch/path owned by that
  assignment. A new worktree does not automatically include the user's dirty
  files. Decide whether the assignment targets committed or uncommitted work;
  never silently stash, copy, or discard those changes.
- Record allocation intent before the Git operation. On restart reconcile
  Git's actual worktree inventory and ownership before retrying creation.
- Run project setup and validation in that checkout. Missing dependencies,
  secrets, or unavailable services should yield an actionable question/failure.
- Return a branch/diff, base and result commits, and validation evidence.
  Existing `SourceProposal` handles a single `.trash` file; it is not a general
  Git change-review or landing implementation.
- Keep review and landing explicit. Do not auto-merge, push, close the issue,
  delete the branch, or remove the worktree on assignment completion.
- Retain work on failure/cancellation. Later cleanup must check ownership,
  active use, and uncommitted/untracked work; no forced removal of user paths.

A worktree separates edits, not OS permissions. Avoid multiple writers to the
same checkout; keep repository-wide mutations serialized where necessary.

## Persistent memory requirements and provider evaluation

Persistent shared memory, with facts scoped to logical repositories, is required
follow-up work. Implementation is deferred; provider selection and a small
evaluation come first. The [code-intelligence and memory research](code-intelligence-and-memory-research.md)
compares concrete tools to wrap in Tool classes. An API-only service can still
have a narrow adapter; a CLI is convenient but not mandatory.

The first [code and session Tool adapters](code-and-session-tools.md) are
implemented: Roam for checkout-local graph queries, ast-grep for structural
search, cass for existing session history, and Chad for explicit headless tasks.
These use the common Tool process boundary. Repository associations, learned
memory, and a persistent Chad AgentDriver remain follow-up work.

Repository memory must be reusable by authorized agents across independent
local copies and worktrees of the same Repository. Scope by durable repository
identity, not a checkout basename, absolute path, harness name, or session ID.
Attach source assignment, author identity, evidence, observed revision, and
relevant branch/worktree or dirty-state information to facts. A fact observed
on one branch must not silently become true of every checkout.

Individual agent memory and collective project memory are separate scopes.
Personal memory follows `AgentIdentity`; repository facts belong to the shared
repository store. Memory should outlive harness conversations and remain
inspectable, correctable, and removable by humans and authorized agents.

Separate the provider executable/service from the selected **memory store**.
For example, a future repository association could return a `MemoryStore`
instance, just as `Trash userInbox` returns an Inbox instance. Avoid making
the `Memory` class implicitly mean one particular person's collection.

Illustrative messages are:

```bash
# Future experiment, not required for delegation.
memory=$(@ "$repository" memory)
@ "$memory" search: 'integration test credentials'
@ "$memory" remember: 'The suite reads TEST_TOKEN from the environment.' \
  source: "$assignment"
```

Start with repository-scoped search and explicit writes of useful observations.
Each stored observation should carry provenance, date, scope, and relevant
revision. Users need a way to inspect, correct, and forget entries. Identity
preferences and repository knowledge are different scopes; shared access must
be chosen rather than inferred from co-location on a machine.

Do not automatically ingest entire transcripts, credentials, or every passing
thought. A plausible model statement is not established fact. Prefer observations
with evidence, and record uncertainty. Retrieval helps the agent find what to
check; it does not authorize actions or decide assignment/tracker state.

Use a small result limit and explicit unavailable/error outcomes. Missing
memory must not prevent delegation or erase execution history. Keep provider
credentials out of prompts and logs. Start with direct messages to the store;
only add automatic prompt retrieval after the tool proves useful.

Evaluate one real question across two fresh sessions: does a saved observation
reduce repeated investigation, remain attributable, and get corrected when
the code changes? Compare with a checked-in note and plain search. If the
external system adds no value, keep a simpler durable note store and remove
that adapter without changing delegation. The shared-memory requirement remains.

## Delivery sequence and acceptance

Slice 0 implementation is now present: the `jcode` profile, common inbox
notification prompts and run launchers, delivery-scoped results, exact-run stop,
and conservative resident-session recovery. See [Jcode session driver](jcode-session-driver.md)
for the implementation and validation record. Jcode is now the default for new
Gusgus sessions; native steering remains optional and unexposed. Slice 1 is also
implemented: identity assignment, explicit sequential session participation,
durable progress/questions, and atomic completion. Its work and session replies
are held for the [manual walkthrough](assignments.md). Slice 2 and later remain
planned; automatic specialist dispatch is not enabled.

Each row is a bounded change followed by a pause to use and review it. Later
rows are options in dependency order, not authorization for one large patch.

| Slice | Concrete change | Evidence required before moving on |
|---|---|---|
| 0. Common messaging and Jcode | Add the Jcode adapter, notification-driven inbox reads, queued input, explicit authorized stop, and resident-session reconciliation behind the common Trashtalk interface. Test Maki against the same minimum contract. | During substantive work, send another message and observe the agent read it from the inbox after notification. Queued delivery works when steering is unavailable; the message body is absent from the notification prompt. An authorized agent can stop another execution without stopping unrelated sessions; stale or unauthorized stop requests fail. Reconnect and reconcile pending input without silently losing or duplicating work. Qualify optional Jcode steering separately and choose the default from this walkthrough. |
| 1. Human walkthrough | Explicitly configure one agent identity and its initial session using existing objects. Add minimal Assignment draft/assignTo:/workIn:/show/ask:/complete: operations and session participation history. Exercise them manually from Bash before enabling a model; implement atomic publication/settlement. | Assign to the identity instance, select its session, and inspect the assignment from another shell. Continue the same open assignment in a second session of that identity with progress/questions retained. A foreign identity's session is rejected; repeated selection/completion creates no duplicate work/outcome; superseded or stale execution cannot complete it. |
| 2. One specialist | Add assignment-specific context and dispatch through the selected specialist session, reusing the worker and common harness interface established in slice 0. Keep new dispatch limited to one child and no recursion. | Do the same walkthrough through a real harness; it reads notified inbox messages through documented Bash operations. A later session can continue the same identity's assignment without its predecessor's transcript. Questions resume the right work; its result notifies Gusgus with the original conversation link. An unrelated user message receives no specialist result. |
| 3. Recovery and visibility | Add assignment inspection from session/inbox views, cancellation reporting, and continuation reconciliation. | Restart at assignment creation and completion boundaries; no lost or duplicated handoffs. An unrelated user message receives no specialist result. Human visibility survives paused/terminated Gusgus. |
| 4. Persistent memory | Establish stable repository scope, evaluate one provider, and implement its small Tool/store interface with explicit storage/retrieval first. | Fresh-session and cross-agent recall within the same repository; isolation across repositories; revision-aware provenance, correction/deletion, and provider-unavailable behavior. |
| 5. One coding assignment | Add Repository/LocalRepository/Worktree objects and run an edit in an explicitly selected managed checkout. Repository identity may be introduced earlier for memory scoping. | Independent clones and linked worktrees are distinguished; dirty user checkout stays intact; worktree creation recovers after interruption; changes and validation are reviewable; failure preserves work. |
| 6. One tracker | Attach a real issue and implement just the chosen provider's read and explicit update operations. | Native tracker state remains authoritative; local completion causes no implicit remote closure; retries do not duplicate remote updates. |

Slice 1 is deliberately a small human exercise, not a production task manager.
The human is exercising the shared API as the trusted local operator, not
impersonating the specialist or making a human name a new assignee type.
Use controlled run fixtures for assignment-authorization checks; real model
delegation begins in slice 2, after slice 0 qualifies harness messaging and
control. Explicit session replacement in slice 1 proves the object
model without requiring an automatic session-selection policy.
Do not enable unattended delegation before slices 2 and 3 pass. Automated
tests should target meaningful transaction boundaries and routing mistakes;
an authenticated harness test proves the prompt/interface journey. There is
no need to retest the whole compiler for a documentation or adapter-only change.

Keep orchestration in the DSL. Raw methods are appropriate for SQL
transactions, filesystem/Git operations, and external JSON protocols. Reuse
the existing queue, process lifecycle, and exact-argv Tool methods rather than
introducing another scheduler or duplicating shell command construction.

## Decisions to revisit after the first walkthrough

- Is `Assignment` the term we naturally use, or does another name describe
  the human experience better?
- Which first specialist job demonstrates value without source edits?
- Which memory system should we actually try, and who may write shared entries?
- Which tracker and exact `git issue` implementation, if any, matters first?
- Should coding target a clean committed base or the user's current dirty work?
- Which specialist model/profile is appropriate once more than one is needed?

General event subscriptions, recursive delegation, parallel writers, generic
tracker synchronization, and automatic landing remain deferred. Optional
harness steering can be qualified in slice 0; it is not required for the
common queued-message and stop contract.
