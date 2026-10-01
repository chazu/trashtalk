# Assignment continuation

**Status:** Current. Replaces coordinator-driven continuation; see the
[simplification design](assignment-simplification.md) for the reasoning.

A provider turn can end before its Assignment is complete. That is routine for
long work, and neither exit zero nor token exhaustion counts as completion.

## Automatic turns

When a run ends still holding an Assignment delivery, the worker returns the
delivery to `pending` instead of marking it `uncertain`. The next tick resumes
the same harness conversation with the durable Assignment snapshot. From the
second turn on, the prompt says which turn it is and that the previous turn
ended without `complete:`. A launched run that failed is treated the same way.

The delivery's `attempts` counts turns. Once it reaches `assignment.attempts`
(default 4; env `TRASHTALK_ASSIGNMENT_ATTEMPTS`), the delivery becomes
`uncertain`. The Assignment's status item then reads *needs review*, and the
owner receives one worker alert.

These cases are not resumed automatically:

- **A question.** `ask:` blocks the delivery until it is answered and resets
  its turn count. The answer starts a fresh set of turns. The answer arrives
  as an ordinary delivery batched with the Assignment, and it is settled with
  the Assignment, so it never stalls the session. The specialist prompt tells
  it to ask instead of stopping when it is blocked.
- **An explicit stop or session termination.** The work becomes `uncertain`
  for review.
- **A failure before launch.** This follows the role's ordinary
  `retryLimit`.

The run itself still records `unsettled` with the stop reason `unfinished`.
Unknown stop causes stay unknown. The Jcode turn-completion receipt does not
identify budget exhaustion.

## Review

```bash
@ "$assignment" show      # state, attempts, runs, progress, questions
@ "$assignment" retry     # requeue with a fresh set of turns
@ "$assignment" cancel: 'Effects inspected; not worth finishing'
```

`retry` is open to the owner and the requesting coordinator. If stopping
this Assignment's run paused its session, `retry` reopens it. A session a
person paused or closed is refused with that reason. Coordinator
authority belongs to the requesting identity, so a later conversation of the
same coordinator may act. `retry` requires a `failed` or `uncertain` current
delivery whose run is no longer active, and no unanswered question. It resets
`attempts` to 0 and journals a `retried` event. It keeps the same delivery,
generation and conversation, and does not reset any external provider budget.

`cancel:` always closes once given a reason. It stops a run still working
the Assignment, retrying the lookup once if the worker has just resumed the
work in a new run. It skips the delivery in whatever state it is, and reopens a
session that a stop of this Assignment's run paused. The outcome goes to the requesting conversation, or to the owner when
that conversation is closed. The owner may also `complete:` uncertain work
whose effects meet the criteria.

A stalled Assignment does not hold back its session. The worker keeps
dispatching other queued work, and only stalled ordinary deliveries stop
dispatch. Ordinary deliveries keep the session-level `requeue:` and `skip:`
recovery. Assignment deliveries refuse `requeue:` and point to `retry`.

Records from the earlier protocol keep their `continued` events and
`continuationAllowance` field. Nothing reads them.

## Validation

`tests/test_assignment_recovery.bash` drives real worker turns through the
shell driver. It covers:

- automatic turns up to the limit, then review
- the turn notices in the prompt
- `retry` authority, and its refusal while a run is live or a question is
  open
- a question stopping the turns
- queued work running past a stalled Assignment
- stop without automatic resume

`tests/test_assignment_close.bash` covers cancel after the requester closes,
coordinator cancel that stops the live run, uncertain cancel, and authority.
