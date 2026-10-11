# Permanent agent configuration

**Status:** Partially implemented
**Date:** 2026-10-10
**Scope:** Configured, durable agent identities. Ephemeral, assignment-owned agents are explicitly deferred.

## Decision

Add a user-owned **agent roster**: a declarative list of named permanent agents
that Trashtalk reconciles into durable `Agent::Identity` records. A roster entry
states the agent's intended identity, operational defaults, and authority
limits. The durable identity remains the source of truth for conversations,
runs, assignments, and history.

The roster is not a replacement for `Agent::Identity`, `Agent::Session`, roles,
or archetypes:

- An **agent roster entry** is desired configuration, kept in the user's
  versioned `trash/user/` source.
- An **identity** is the durable person-owned authority boundary.
- A **session** is a mutable conversation and execution context.
- An **archetype** supplies instructions and a suggested backend profile.
- A **role** supplies capabilities, workspace policy, and run budget.

`Gusgus` remains a built-in permanent identity. The existing `specialist`
identity remains the default reusable delegation target for compatibility. It
is not redefined as an ephemeral worker in this work.

## Goals

- Let a user declare several named, durable agents without hand-editing database
  records.
- Let each agent have a stable handle, display name, profile, instructions,
  role, session scope, and enabled state.
- Let the configuration of durable agents be expressed in trashtalk classes
- Make configured agents visible, selectable, focusable, and explicit delegation
  targets.
- Preserve the existing ownership, session-membership, workspace authorization,
  and Assignment checks.
- Make reconciliation safe to repeat and safe after a restart.
- Keep configuration free of credentials and executable code.

## Non-goals

- Ephemeral, per-assignment, or automatically spawned agents.
- Automatic task routing, model selection, or delegation fan-out.
- Per-workspace agent configuration. A roster entry is user-wide.
- Changing an active session's archetype, role, provider conversation, or
  execution workspace.
- Sharing an identity between human owners.
- Replacing `Preferences` for scalar settings such as Jcode's model.

## Configuration model

`Preferences` works for a fixed set of typed scalar settings. It cannot express
an arbitrary, repeatable set of agents, so the roster uses a second restricted
user-source form: one `AgentProfile` class per configured agent. These classes
live in `trash/user/`, are ignored by this repository, and can be symlinked from
a dotfiles repository like `Preferences` classes.

An `AgentProfile` class is declarative only. It can contain its header and the
following literal declarations, in this order:

```smalltalk
Researcher subclass: Agent::AgentProfile
  handle: 'researcher'
  displayName: 'Researcher'
  profile: 'jcode'
  archetype: 'researcher' revision: 1
  role: 'researcher' revision: 1
  sessionScope: 'workspace'
  enabled: true
```

The parser rejects methods, instance variables, traits, expressions, duplicate
fields, unknown fields, and non-literal values. A profile name must be a valid
capitalized class name. `handle:` follows the existing identity-handle rules.

The required fields are `handle:`, `displayName:`, `profile:`, `archetype:`, and
`role:`. `sessionScope:` defaults to `workspace`; it accepts only `workspace`
or `identity`. `enabled:` defaults to `true`.

The profile does not hold a model or provider secret. `profile:` selects an
existing harness profile such as `jcode`, `pi`, `maki`, `codex`, `chad`, or
`shell`. Harness-wide settings still come from Settings and Preferences.

An archetype and role declaration must already exist. Their revision is pinned
in the profile. This makes an agent's instructions and authority reviewable in
the same source change as its roster declaration. A later revision requires an
explicit profile edit and reconcile.

The compiler writes all validated declarations to
`trash/.compiled/.agent-profile-manifest.json`. It reports duplicate handles,
invalid scopes, unknown profile kinds, missing archetype or role revisions, and
invalid class bodies during the build. Two enabled or disabled profiles cannot
claim the same handle.

To view configured agents, create at least one `AgentProfile` file under
`trash/user/`, then rebuild the local Trashtalk artifacts:

```sh
cd ~/.trashtalk
make bash
source lib/trash.bash
@ Agent::Roster reconcile
@ Agent::Session browse
```

The build must succeed before the browser can read a profile. Inspect the
compiled input with `cat trash/.compiled/.agent-profile-manifest.json`. An empty
`profiles` array means that the browser has no configured agents to show.

## Reconciliation and lifecycle

`@ Agent::Roster reconcile` is the only operation that applies declared
configuration to durable identities. It runs after a successful build, at worker
startup, and from a user-visible roster action. It is idempotent.

For each profile, reconciliation runs one transaction that:

1. Finds the identity by handle or creates it with the current human as owner.
2. Rejects an existing identity owned by another human.
3. Records the profile name and a content digest in new identity configuration
   provenance fields.
4. Updates display name, enabled state, default archetype, and default role.
5. Applies a session-scope change only through the existing explicit scope
   migration flow. A normal reconcile reports this change as pending and makes
   no routing change.

The profile is not allowed to overwrite manually owned identities by accident.
An existing identity without matching profile provenance becomes managed only
when its handle and owner match and the user confirms adoption. Built-ins such
as `gusgus` remain code-managed and cannot be claimed by a profile.

Removing a profile does not delete or disable its identity. Reconcile reports it
as **unmanaged**. This protects historical conversations and assignments and
makes removal from version control non-destructive. The user can later adopt it
again, disable it with a profile, or explicitly retire it.

Setting `enabled: false` disables new session creation, direct input, and new
delegation targeting after normal authorization checks. Existing history stays
browsable. Existing queued or running work is not silently cancelled. Re-enable
restores eligibility using the retained identity and current-session policy.

Changing an archetype, role, or profile changes defaults for sessions created
later. Existing sessions retain their immutable archetype revision, role
revision, backend profile, provider conversation, and execution directory.
Users can choose **Start fresh conversation** after active work is quiescent.

## Session scopes and workspace safety

A permanent agent defaults to `workspace` scope. This produces a separate
current conversation per canonical workspace, which suits project specialists.

An `identity` scope agent has one current conversation across workspaces. It is
intended for assistants such as Gusgus. Each delivery still records and is
re-authorized against its originating execution workspace. This avoids treating
a global conversation as permission to execute in every directory.

Changing scope is never an ordinary configuration update. Reconcile detects the
difference, creates a visible pending change, and directs the owner to the
existing explicit migration UI. The UI presents the cross-workspace disclosure,
requires a candidate session selection where needed, drains or stops active
runs, and publishes the membership change atomically.

## Public interface and UI

The following public operations are implemented:

```smalltalk
@ Agent::Roster list
@ Agent::Roster reconcile
@ Agent::Roster inspect: 'researcher'
@ Agent::Roster disable: 'researcher'
@ Agent::Roster retire: 'researcher' reason: 'No longer needed'
@ Agent::Roster focus: 'researcher'
@ Agent::Run delegate: objective toAgent: 'researcher' criteria: criteria key: key
```

`list` returns the compiled manifest. `inspect:` returns an owner-authorized
projection of one durable identity and its manifest declaration. It reports
managed, unmanaged, or pending scope migration status. `focus:` resolves only
an owned current session. It rejects when no session exists, so it never creates
work. Sending direct input or creating a delegation can call
`ensureCurrentSessionFor:` because that is explicit work creation.

The session browser now places manifest-backed configured agents before
historical sessions. Its configured-agent menu offers Focus existing
conversation, Inspect configuration, and Disable agent. Records have an
`agent:` namespace and cannot be treated as session IDs. Disabled agents retain
their history. Start conversation, Delegate, Pause, and assignment-workboard
presentation remain deferred controls.

`delegate:toAgent:criteria:key:` resolves a roster-managed, enabled identity for
the current owner, then uses that identity's current session for the run's
execution workspace. It delegates through the existing Assignment transaction,
with the same request-key deduplication, capability checks, owner checks, and
one-run-per-session behavior as explicit-session delegation. It rejects a
profile whose role lacks `assignment.work`, the requesting identity itself, an
ambiguous identity, or a pending scope migration.

The existing `delegate:criteria:key:` continues to use `specialist` unchanged.
This avoids a behavior change for existing prompts and scripts.

## Authorization and failure behavior

A roster handle is a lookup key, not authority. Every operation rechecks the
identity owner and current role policy inside its transaction. A stale UI row,
removed profile, cached session ID, or caller-supplied workspace cannot route
work.

Reconcile fails an individual invalid or conflicting profile visibly and leaves
that identity unchanged. It continues with unrelated valid profiles and reports
a complete result. Build-time errors prevent a new manifest, so worker startup
continues using the last successful manifest and reports that the source needs a
successful build.

Retirement is an explicit, owner-authorized lifecycle operation. The delivered
operation disables the identity and clears roster provenance while retaining all
sessions and history. Active-run checking and membership closure remain deferred
before retirement can become a full session-lifecycle control.

## Delivered implementation

Commits `ea9fc115` and `002f2cd` deliver the restricted `AgentProfile` form,
compiler manifest generation, reconciliation, provenance, adoption, configured
delegation, inspection, disable, retirement, focus, and browser presentation.
Reconciliation processes every profile independently. The focused regression
suite covers multiple profiles, idempotence, provenance, disable, retirement,
and unmanaged identity protection.

## Deferred controls

- Add Start conversation, Delegate, and Pause to the configured-agent menu.
- Add configured-agent display names to Inbox and Assignment workboards.
- Add an explicit scope-migration UI.
- Require an idle identity and clear current-session memberships during retire.
- Add the remaining acceptance coverage for cross-owner access, scope routing,
  profile upgrades, source deletion, and configured-agent delegation.

## Validation evidence

The delivered commits passed these commands in the repository checkout:

```sh
make single CLASS=Agent/Roster
make single CLASS=Agent/Delegation
make single CLASS=Agent/Browser
bash tests/test_agent_roster.bash
bash tests/test_agent_delegation.bash
bash tests/test_agent_browser.bash
```

`tests/test_agent_roster.bash` completed 18 checks after the lifecycle work.

## Acceptance tests

- A user can declare `researcher` and `reviewer`, build, reconcile, and see two
  owned permanent identities with their declared defaults.
- Reconciliation is idempotent and never creates a duplicate identity or inbox.
- A second owner cannot adopt, configure, focus, or delegate to another owner's
  configured identity.
- A workspace-scoped configured agent gets independent current sessions in two
  canonical workspaces. An identity-scoped agent retains one current session,
  while each delivery retains its execution workspace.
- Updating profile, role, or archetype affects only newly created sessions.
  Existing sessions and provider conversations remain unchanged.
- A scope change becomes pending and cannot route work until the owner completes
  explicit migration.
- Disabled agents reject new sessions and delegation but retain inspectable
  history and do not cancel work already in progress.
- Deleting a profile source file leaves its identity and history intact as
  unmanaged.
- Explicit configured-agent delegation produces the same durable Assignment,
  status updates, recovery behavior, and request-key deduplication as the
  current specialist flow.

## Deferred: ephemeral agents

Ephemeral agent creation is deferred. It will need a distinct design because it
changes identity lifetime, concurrency limits, cleanup, worker isolation,
assignment ownership, and audit behavior. It must not be implemented by making
the permanent roster create temporary profiles or by changing the existing
shared `specialist` identity.
