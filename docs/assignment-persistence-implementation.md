# Assignment persistence refactor

**Status:** Implemented. `AssignmentStore` has been removed.

`Assignment` uses `Persistable` saves inside a shared `Store` transaction.
Lifecycle rules are DSL methods; authorization, reporting and presentation are traits.
`Message` and `AgentDelivery` construct their own objects from ordinary defaults.
`AgentQueue` owns question links, outbox publication, and post-commit hints.

## Assignment composition

`Assignment` remains the public class and persisted record. Its collaborators
live in the `Assignment` package:

| Component | Responsibility |
| --- | --- |
| `Assignment` | Drafting, ownership, description, completion/cancellation, current lookup |
| `Assignment::Participation` | Session selection, delivery claims and participation history |
| `Assignment::Authority` trait | Fresh operator/run authorization |
| `Assignment::Reporting` trait | Progress, inbox questions and event recording |
| `Assignment::Presentation` trait | Snapshots, derived activity and readable output |

Participation uses class methods over the existing Assignment record; it creates
no separate persistent objects. Public operations retain the whole-operation
Store transaction. The stored shape and existing handles are unchanged.

## Transaction boundary

```bash
@ Store transaction: "$object" sending: 'changeWithin:' with: "$argument"
```

The receiver executes against a private SQLite store and object cache shared
across Bash command substitutions. Reads import individual objects, recording
original values or absence; saves remain private. Indexed membership queries
record their original result. Under one live write lock, commit rechecks those
records and queries, applies object changes, and inserts queue rows. Constraints
or conflicts roll back everything. Only successful commit releases the result,
invalidates affected caller caches, and invokes notification hints.

A failed send poisons the transaction even if a later expression succeeds.
Nested transactions, deletion, and arbitrary SQL are rejected. Guarded query
membership reflects the recorded live snapshot; object reads see staged saves.
The API does not transact external effects. SIGKILL can leave scratch files.

Selection/completion allow one **read-only** conflict replay, so concurrent
identical requests can acknowledge an already committed result. A replay that
would write fails; callers must explicitly retry other conflicts.

## Validation

`tests/test_assignment.bash` preserves the public manual workflow.
`tests/test_assignment_transaction.bash` exercises production transactions:
rollback, swallowed failures, constructors, cache isolation, publication hints,
run authorization, query membership changes, collisions, and concurrent workers.
It also checks that unrelated history does not expand the completion read set.
`tests/test_namespaced_traits.bash` covers qualified trait dispatch, inspection,
and reload, including preserving package helpers when the root class reloads.

Validated: 106 workflow checks, 202 transaction checks, and 19 namespace checks.
Full validation passed 62 runtime and 43 compiler test files with
`TRASH_TEST_TIMEOUT=360 TRASH_TEST_JOBS=4 make verify`; the extended timeout
accommodates the concurrent and history-size journeys.

The [earlier experiment measurements](assignment-transaction-measurements.json)
remain historical evidence, not production benchmarks. Completion loaded three
objects at each tested history size. Production additionally guards absence of
the deterministic result key. No alerts, tracker synchronization, harness changes,
or automatic delegation are part of this refactor.
