# Simplifying Assignment execution

**Status:** Design, executed on branch `simplify-agent-tasks` (2026-10-01).
Supersedes the coordinator-driven continuation in
[assignment-recovery.md](assignment-recovery.md), which now describes the
result.

## Why

On 2026-09-28 Gusgus delegated "implement protocol fortification" to the
specialist. Each specialist turn made one commit and then ended without
`complete:`. Gusgus continued it three times, reached the continuation limit,
and the Assignment stayed `open`. Three days later:

- The specialist rejected all new work: *Specialist session already has an
  open Assignment*.
- Gusgus could not cancel the Assignment, because cancel is owner-only.
- The owner's `cancel:` failed first because the delivery was `uncertain`.
  After an explicit `skip:`, it failed because the outcome Message targeted
  the closed requesting conversation (fixed in `d87c0e4`).

Every guard did what it was written to do. Together they produce a
responsibility that nobody can finish, and a protocol Gusgus could not explain
correctly. Closing one Assignment required reasoning about the Assignment,
Delivery (7 states), Run (9 states), Session lifecycle, membership triggers,
continuation keys, and a per-assignment allowance.

## Principles

1. **Closing always works.** An authorized `cancel:` never depends on delivery
   state, notification routing, or an unrelated run. If the Assignment's own
   run is active, cancel stops it.
2. **The worker handles routine continuation.** An unfinished turn is normal
   for long work. The worker resumes it without a coordinator round trip, up
   to a fixed number of attempts.
3. **People handle exceptions with one verb.** After the attempts are used up,
   or after an explicit stop, the Assignment needs review. The owner or the
   requesting coordinator chooses `retry` or `cancel:`. No delivery IDs, keys,
   or allowances.
4. **A stuck Assignment blocks only itself.** A specialist queues more work
   instead of rejecting it. An Assignment that needs review does not stall
   other deliveries in its session.

## Changes

### 1. Close always works

- `cancel:` accepts the current delivery in any state and marks it `skipped`.
- The requesting coordinator may cancel its own delegated Assignment, as well
  as the owner. Before, only the owner could.
- If the run holding the current delivery is active, `cancel:` stops it
  first. A coordinator may stop that run without `agent.stop`. If stopping
  paused an open session, cancel reopens it.
- Only the run working this Assignment can fence its completion. Another
  Assignment's run in the same session does not.
- The owner may `complete:` with a failed or uncertain delivery after
  inspecting effects. An agent still completes only from its held delivery.
- If the requesting conversation is closed, the outcome goes to the owner
  (`d87c0e4`).

### 2. The worker continues unfinished turns

When a run ends and still holds an Assignment delivery (`offered`), the
worker puts the delivery back to `pending` instead of `uncertain`, while its
attempt count is below `assignment.attempts`. The default is 4, which equals
the old 1 + 3 continuations. A launched run that failed gets the same
treatment. The next tick resumes the same harness conversation with the
Assignment snapshot. The prompt says which attempt this is, and that the
previous turn ended without `complete:`.

Uncertainty still applies after an explicit stop, a session termination, or
reaching the attempt limit. The status Message then reports *needs review*,
and the owner receives one alert.

The specialist prompt replaces "leave the Assignment open" with "if you
cannot finish, `ask:` the blocking question". A question blocks the delivery,
stops automatic continuation, and resets the turn count, so a real blocker is
not retried mechanically. The answer's ordinary delivery is settled with its
Assignment and never stalls the session.

### 3. One recovery verb: `retry`

`@ "$assignment" retry` is available to the owner and the requesting
coordinator. It moves a `failed` or `uncertain` current delivery back to
`pending` with a fresh attempt count, and journals a `retried` event. It is
refused while the delivery's run is active or a question is unanswered. It
reopens a session that a stop of this Assignment's run paused.

Removed:

- `continue:afterDelivery:key:`, `allowContinuations:reason:`, continuation
  receipts and keys, and `continuationAllowance` (the `Assignment::Recovery`
  trait)
- Recovery notifications to the coordinator (`notifyRecoveryWithin:`) and the
  `needs review` notification branch of the coordinator prompt
- The continuation paragraphs in the run and conversation prompts
- The session browser's continuation path, which now calls `retry`

Old records keep their `continued` events and `continuationAllowance` field.
Nothing reads them.

### 4. A stuck Assignment blocks only itself

- Delegation no longer rejects a busy specialist or a coordinator that
  already has an open child. The Assignment is queued.
- `Assignment::Participation select:` no longer requires the target session
  to be idle. The worker starts one run per session at a time anyway.
- Dispatch claims at most one Assignment delivery per run and does not batch
  it with ordinary deliveries. This keeps "the Assignment held by this run"
  unambiguous.
- Dispatch is gated only by stalled *ordinary* deliveries. A stalled
  Assignment delivery is visible in the Assignment's own status and does not
  stop the queue behind it.

## What this gives up

- **Coordinator judgment before each continuation.** Previously a
  coordinator read the evidence before continuing. Now continuation is
  mechanical, bounded by the attempt count. The specialist is told to inspect
  existing changes, and a question stops the loop.
- **Fresh provider context per continuation.** Resuming the same conversation
  keeps context and is cheaper. A poisoned conversation fails at most
  `assignment.attempts` times, then needs review.
- **Idempotent continuation receipts.** `retry` is a simple state move. A
  duplicate `retry` on an already pending delivery is refused harmlessly.
- **Serial specialist work as a forcing function.** Queued Assignments run
  one after another in the same workspace. They may see each other's
  uncommitted changes, just as successive human tasks would.

## Not changed (deliberately deferred)

The general Run and Delivery state machines, session membership triggers,
and the jcode resident-recovery path remain. They also govern ordinary
conversation, and this incident did not implicate them. Collapsing Run to
`running | exited` and folding Delivery into Run is the next candidate. Do
that only if the next incident points there.

## Validation

- `tests/test_assignment_close.bash`: cancel after the requesting
  conversation closes, coordinator cancel that stops the live run, uncertain
  cancel, and authority
- `tests/test_assignment_recovery.bash`, rewritten: automatic continuation up
  to the limit, then needs review, `retry`, authority, refusal while active
  or questioned, cancel that stops an active run, and cancel of an uncertain
  delivery
- `tests/test_agent_delegation.bash`, `tests/test_agent_browser.bash`,
  `tests/test_assignment.bash`: updated for queueing, `retry`, and the new
  activity rules
