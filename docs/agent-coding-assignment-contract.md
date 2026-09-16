# Coding assignment contract

**Status:** public operating contract for the existing `Assignment`, Inbox,
Agent::Session, and Agent::Worker surfaces. It adds no persisted object or parallel
protocol.

## Purpose

A coding agent should receive enough context to make one bounded contribution,
while the coordinator can later decide whether to accept it. The contract makes
handoffs comparable and keeps a run from silently becoming an open-ended
request for autonomous changes.

Use it for research, implementation, review, and validation assignments. It is
a prompt and outcome convention, not an authorization system. Existing roles
and OS permissions still define what an agent can actually do.

## The brief

Create a normal Assignment and put the following sections in its objective and
completion criteria. Keep quoted issue text, logs, and agent output clearly
separate from these instructions.

```markdown
# <kind>: <short outcome>

## Objective
<one concrete result, not a list of loosely related work>

## Workspace and boundary
- Workspace: <canonical path>
- Allowed scope: <paths/components that may be inspected or changed>
- Excluded scope: <paths/effects that must not be touched>
- Concurrency: <the only writer, or "read-only">

## Starting evidence
- <failure, benchmark, issue, commit, or reproduction command>

## Completion criteria
- [ ] <observable required result>
- [ ] <test or benchmark and expected result>
- [ ] <documentation/review requirement, if any>

## Decision authority
<what the agent may decide, and what needs a question or human approval>
```

The requester records this on the existing object before `assignTo:` and
`workIn:`:

```bash
assignment=$(@ Assignment draft: "$objective" in: "$workspace")
@ "$assignment" criteria: "$criteria"
@ "$assignment" assignTo: "$identity"
@ "$assignment" workIn: "$session"
@ "$assignment" show
```

`workIn:` creates the durable message and delivery. It is deliberately manual
in the current Assignment journey. Do not bypass it with a copied prompt or a
second queue.

## Required outcome

Before `complete:`, the assignee reports an outcome in this form. A failed,
blocked, or uncertain result remains useful evidence and must not be described
as completed work.

```markdown
## Outcome
Status: completed | blocked | needs-review | cancelled

### Summary
<what was learned or changed>

### Evidence
- Commands: `<exact command>`
- Results: <concise result, numbers where available>
- Files inspected/changed: <paths, or "none">

### Risks and limits
<remaining uncertainty, regressions not checked, or why no change was made>

### Follow-up
<next smallest useful action, or "none">
```

Use `ask:` as soon as missing direction blocks a correct decision. Do not keep
a run alive polling for an answer. The answer remains linked to the Assignment
and the workboard exposes it as `waiting on question`.

## Kinds and default limits

| Kind | Default authority | Completion evidence |
| --- | --- | --- |
| Research | Read-only inspection and bounded diagnostics | ranked findings, citations, profiling plan |
| Implementation | One agreed slice, one writer for the workspace | changed paths, tests, diff/risk summary |
| Review | Read-only review of a known diff/branch | prioritized findings or explicit approval |
| Validation | Reproduce and measure only | exact commands, environment, before/after data |

A research agent does not implement its recommendations. An implementer does
not silently expand scope. A reviewer does not modify the checkout it reviews.
Use separate Assignments when those roles must run concurrently.

## Parallelism rule

Many read-only assignments may inspect the same workspace. At most one active
writer should modify a checkout. If two implementation streams are necessary,
use independently prepared workspaces and name the ownership boundary in both
briefs. Current roles are cooperative controls, not an OS sandbox.

## Coordinator loop

1. Create the bounded Assignment and choose the identity/session explicitly.
2. Inspect `@ Trash workStatus` while work runs. It is read-only and aggregates
   local Attention plus open Assignment activity.
3. Answer questions, inspect Inbox threads and durable delivery/run state, and
   review the outcome against the completion criteria.
4. Create a new follow-up Assignment for any approved next step. Completion
   reports evidence. They do not merge code, close tickets, or authorize a new
   effect.

This retains one public messaging plane and preserves replay-safe dispatch,
question routing, cancellation, and recovery behavior already provided by the
existing runtime.
