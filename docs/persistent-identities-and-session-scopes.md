# Persistent identities and session scopes

**Status:** stabilized design, awaiting implementation

### Preparatory implementation

`AgentAccess` now supplies fresh, typed identity/session ownership checks for
human conversation views. Focus validates ownership on open, snapshot refresh,
and each non-dismiss UI request. Open and paused sessions may open a live view,
and validation does not resume paused work. Closed sessions cannot open a new
live view. These checks use the public Store and Require APIs rather than a
second SQLite adapter. `tests/test_agent_access.bash` covers stale ownership,
foreign identities, missing records, run-token rejection, and read-only checks.

This is not current-session membership enforcement. Durable membership,
scope-policy revisions, transactional resolution/admission/claim, migration,
fencing, and per-delivery execution workspaces remain unimplemented. The
resolution APIs below and the optional Option-U binding are not yet provided.

## Problem

Trashtalk currently resolves Gusgus sessions by identity and workspace. A
persistent assistant therefore gets a new long-lived conversation in each
directory, leaves many plausible sessions, and makes focus shortcuts surprising.

An `AgentIdentity` is the durable persona and authority boundary. An
`AgentSession` is a mutable conversation and execution context. Session scope
must be explicit because a persistent assistant and a project specialist have
different isolation needs.

## Goals and non-goals

- A persistent identity has at most one current session.
- A task identity can retain one current session per canonical workspace.
- Historical sessions, messages, runs, and provider references stay attributed
  and inspectable.
- Focus is read-only and never creates or guesses a session.
- No migration transfers a provider conversation across identities, rewrites
  history, or silently moves outstanding delivery work.

## Scope policy

`AgentIdentity` revisions carry a `sessionScope` policy. The stable identity ID,
not a display name or revision ID, owns the scope key.

| Scope | Scope key | Intended use |
| --- | --- | --- |
| `identity` | `(identityId)` | Gusgus and other persistent assistants |
| `workspace` | `(identityId, canonicalWorkspace)` | Project and task specialists |

`AgentSession.workspace` is the creation workspace. Under `identity` scope it
is provenance only, never the uniqueness key. Every `AgentDelivery` additionally
records its canonical originating workspace, authorization snapshot, and role
revision. A worker reauthorizes that workspace before dispatch and uses it as
the run's execution directory. Missing or unauthorized workspaces fail rather
than falling back to the session creation directory. Workers do not batch
incompatible-workspace deliveries.

## Current-session membership

A durable, unique membership record is the source of truth. It contains:
`identityId`, scope key, `sessionId`, `policyRevision`, fencing generation, and
state (`current`, `migration_pending`, or `superseded`). Only an open or paused
session can be current.

A **lifecycle close** or termination atomically releases membership. Detaching or
closing a conversation *view* is not a lifecycle transition: it only stops that
viewer and leaves the current session, its worker, and any background run
unchanged. Reopen, resume, direct
creation, direct persistence, and lifecycle changes, delivery admission, worker
claim, and recovery must all check membership transactionally. Reopening a
historical session conflicts if another eligible session owns its scope key.
Cached objects, retained session IDs, display handles, and workspace paths
never authorize routing.

## Resolution APIs

```smalltalk
@ identity currentSessionFor: workspace       # read-only
@ identity ensureCurrentSessionFor: workspace # transactional creation
@ Gusgus focusCurrent                         # read-only focus
```

`currentSessionFor:` returns no session, pending migration, or ambiguity as an
explicit result. It never writes. `ensureCurrentSessionFor:` performs bounded
retry on a uniqueness conflict and rechecks active policy revision before
publication. `@@` may use ensure because sending is explicit work creation.
Focus and browse use current only. A stale browser selection revalidates
membership and ownership before attaching.

Delivery admission and worker claim atomically re-read membership, lifecycle,
identity owner, policy revision, and the delivery workspace. Workers are fenced
by generation after a migration cutover. Old client or worker schema versions
are rejected as upgrade-required rather than allowed to use legacy
workspace-keyed creation.

`focusCurrent` resolves the current user’s authorized Gusgus identity without
creating a session. A paused session remains visibly paused and queues admitted
work. Focus never resumes it implicitly. Superseded sessions have a separate
read-only historical inspection action.

## Explicit migration

Changing scope requires identity-owner authority and informed consent. In
particular, conversion to identity scope warns that one provider conversation
can carry context between workspaces. No ordinary send, focus, or read-only
resolution can start migration.

1. Add membership storage and `sessionScope`. Legacy identities remain
   `workspace` scoped until explicitly changed. Backfill validates identity
   links, lifecycle fields, canonical paths, and duplicates before publication.
2. Create `migration_pending` while prior routing remains authoritative. Zero
   candidates creates nothing. One candidate needs confirmation. Multiple
   candidates require user selection. Concurrent selections conflict.
3. Quiesce new claims and drain active runs or explicitly stop them. Pending,
   blocked, failed, and uncertain deliveries stay on their originating session.
   They are never silently moved and provider actions are never replayed.
4. Atomically publish selected membership, supersession state, policy revision,
   and indexes. A crash before commit retains old routing. A crash after commit
   resumes from durable membership state.
5. Fence old writers before cutover. Superseded sessions remain inspectable but
   cannot receive live focus, dispatch, requeue, resume, or reopen while their
   scope key is occupied.

The reverse `identity → workspace` conversion is equally explicit. It cannot
silently choose a destination for a conversation that contains multiple
workspaces. Sessions and runs retain their original identity and policy revision
for auditability.

## Authority and privacy

Identity ownership is checked for resolution, focus, inspection, historical
browsing, migration choice, delivery admission, and worker claim. An identity
handle, session ID, or caller-supplied path is not authority. Scope conversion
preserves the existing provider conversation only for the same identity and
requires the cross-workspace disclosure described above.

## Acceptance and adversarial test matrix

1. Gusgus messages from two directories use one identity-scoped current session
   while preserving both delivery workspaces.
2. A workspace-scoped specialist has independent current sessions per canonical
   workspace.
3. Synchronized tests cover ensure versus reopen, migration selection versus
   legacy creation, enqueue or claim versus cutover, and stale-save conflicts.
4. Crash injection covers every migration commit boundary and proves provider
   actions are not replayed after transaction retry.
5. Tests reject unauthorized cross-workspace work, canonical-path aliases,
   missing legacy paths, stale focus, and old-version writers.
6. Closed, terminated, and superseded sessions remain browsable with their
   original messages, runs, identity links, and provider references intact.

## Adversarial review

Two independent Jcode reviewers reviewed this design. Their required changes
were incorporated: durable membership and lifecycle enforcement, atomic
admission and claim checks, a staged and quiescent migration protocol, explicit
supersession, revision and fencing semantics, per-delivery workspace
authorization, privacy consent for cross-workspace reuse, mixed-version gating,
and crash/race acceptance tests.
