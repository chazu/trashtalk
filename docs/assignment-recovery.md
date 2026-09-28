# Assignment continuation

A provider turn can end before its Assignment is complete. Keep the responsibility
open and the unfinished delivery uncertain; neither exit zero nor token exhaustion
is completion. Recovery is an explicit domain command, separate from creation:

```bash
@ "$assignment" continue: 'Resume from the recorded evidence' afterDelivery: "$delivery" key: 'continue-request-1'
```

The owner or original coordinator may continue a failed/uncertain current delivery
only after its run is terminal, with no active run or unanswered question. Validate
current session membership, owner, role and workspace again. A guarded Store
transaction supersedes the old delivery, publishes a new generation, and journals
the reason, caller, key and both delivery IDs. Return the new delivery only after
commit. Repeating the same key/arguments returns that receipt; changed arguments
or a stale delivery fail without side effects. Retain old run and attempt records.

A continuation starts fresh provider context using the same specialist session.
Its prompt includes the durable Assignment snapshot (criteria, progress, questions,
prior runs and continuation reason), and instructs it to inspect existing changes
before resuming. This does not reset an external account or provider budget.

Assignments allow three continuations by default, including existing records.
The owning human can raise the total with `allowContinuations: total reason: text`;
coordinators cannot grant themselves more. The count is the number of committed
continuation events and is never reset. This bounds recovery, not tokens or money:
Trashtalk currently has no enforced cumulative token/cost budget. A provider hard
limit must be reported as blocked, not evaded by creating another Assignment.

On a stalled delivery, reconciliation publishes one durable recovery notification
per delivery/run to the requesting coordinator, alongside the existing human status.
It gives the exact continuation command and observed stop diagnostic. Unknown stop
causes remain unknown; the Jcode turn-completion receipt does not identify budget
exhaustion. Current direct and inbox prompts teach this recovery path, distinguish
queued from running, and forbid promising work after a rejected command. Ordinary
session requeue is owner-only and Assignment deliveries use continuation instead.
The session browser retry action uses the same continuation command for Assignments.

Validation covers incomplete exit through continuation to explicit completion,
rollback, duplicate/concurrent requests, stale/foreign callers, active/recovering
runs, unanswered questions, the continuation allowance, fresh context, notification
replay, and preservation of historical attempts. No live assignment is resumed by
the tests.
