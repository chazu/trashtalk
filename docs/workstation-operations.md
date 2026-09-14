# Workstation records: Phase 0 operations

Phase 0 is a thin durable model, not an event service. See the
[implementation plan](workstation-event-attention-implementation-plan.md) and
[design](workstation-event-attention-delegation.md) for later phases.
Only `EventSubscription` and `Attention` are new persisted domain classes.
There is no producer, worker stage, Inbox publication, agent routing, or effect.

## Installation and capability checks

```bash
make
source lib/trash.bash
@ WorkstationSchema ensureSchema
@ WorkstationSchema capabilities
@ Trash doctor
```

`ensureSchema` uses the existing feature-local `ensureSchema` DDL convention
used by `AgentSession`. One SQLite transaction installs the indexes and private
coordinate table in the current `SQLITE_JSON_DB`. `IF NOT EXISTS` makes repeated
installation safe on fresh or populated databases. Creation installs it lazily
before entering a Store transaction. It does not rewrite unrelated records or
require Honker/CUE. The runtime's existing lazy declared-field migration remains
unchanged. Do not install DDL inside a Store transaction.

If installation fails, SQLite rolls back the DDL. Correct the reported database
or constraint problem and rerun `ensureSchema`. Back up the database before any
manual repair. Never delete claims or reset consumer progress to repair a view.

Doctor reports **optional workstation capabilities**, not new global requirements.
CUE discovery and read-only introspection of Honker's four Stream functions never
bootstrap a stream, consume a record, acknowledge an offset, or install tools.
Missing CUE blocks workstation create/validate commands with remediation. It does
not block build, Store, Inbox, or existing agents. Missing Honker does not block
Phase 0 record operations. Install tools explicitly using the doctor's guidance.

## Validation and creation

The [closed CUE package](../schemas/workstation/v1/README.md) documents fixtures,
validation and the exact digest command. `@ WorkstationSchema digest` returns the
package digest. Set `schemaDigest` on input documents to that value. A digest is
revision metadata, not a global feature switch.

```bash
@ WorkstationSchema validate: "$subscriptionDocument" as: EventSubscription
sub=$(@ EventSubscription createFrom: "$subscriptionDocument")
a=$(@ Attention createFrom: "$attentionDocument")
@ EventSubscription read: "$sub"
@ EventSubscription listByOwner: "$TRASHTALK_USER"
@ Attention read: "$a"
@ "$sub" summary
@ "$a" summary
```

CUE runs **before** the Store transaction. Native checks inside it enforce the
current human owner, deterministic immutable consumer name (`workstation/<id>`),
allowlisted grouping/adapter, revision, identity access, and lifecycle invariants.
Direct `new`/`save` are not supported domain mutation APIs. Internal `*Within:`
selectors require the exact prevalidated document and a transaction. This is a
cooperative runtime contract, not a sandbox against arbitrary Bash/Store writes.

No production adapter is registered in Phase 0. Fixture creation requires both
`TRASHTALK_TEST_ISOLATED=1` and `TRASHTALK_WORKSTATION_FIXTURES=1` in a disposable
test store. Do not enable fixture mode on a real workstation database.

## Subscription changes and recovery

- `pauseDispatch: reason` / `resumeDispatch: reason` change dispatch state only.
- `disable: reason` / `enable: reason` change source enablement only.
- `EventSubscription updateFrom:` takes a complete validated next revision with
  an audit note. ID, owner, consumer, source/filter, creation time, digest and
  initial-position choice are immutable. Source changes await a reviewed migration.
- Repeating an already-applied state change is rejected. Reload and inspect the
  revision after a conflict or uncertain result, then issue a new explicit change.

Neither flag modifies Honker offsets. **Honker Stream consumer offsets represent
progress. There is no `EventCursor` object or second event log.**

## Attention lifecycle

| Current state | Allowed destinations |
| --- | --- |
| open | acknowledged, snoozed, resolved, suppressed |
| acknowledged | open, snoozed, resolved, suppressed |
| snoozed | open, acknowledged, resolved, suppressed |
| resolved | open |
| suppressed | open |

Use `acknowledge`, `snoozeUntil: '2099-01-01T00:00:00Z'`,
`resolveWithNote: reason`, `suppress: reason`, or `reopen`.
Self-transitions fail. Snooze requires a future normalized UTC timestamp.
Resolve and suppress require nonblank reasons. Actor, update time and note are
persisted. Acknowledgement is not resolution. Reopen is the explicit recovery
from resolved/suppressed state. On any conflict, reload and inspect before retrying.
These actions never settle deliveries, answer Questions, create Messages, or
change sessions/runs. Message and resolved-session references are optional
presentation metadata in this phase.

## Internal coordinate index

After installation, these Store selectors are **transaction-only**:

- `claimedCoordinate: coordinate` returns an existing Attention ID or empty text.
- `recordCoordinate: coordinate forAttention: id` returns the existing link on a
  duplicate, otherwise stages a claim and increments Attention count/range atomically.
- `coordinateRange: startCoordinate through: lastOffset limit: n` inspects a
  guarded **committed** range in one subscription/stream/partition, ordered by
  offset, with `1 <= n <= 1000`. It returns JSON `{offset,attention}` rows. Staged
  claims in the current transaction are visible to `claimedCoordinate:`, not this
  committed inspection query.

Coordinates are closed `{subscription,streamName,partition,offset}` values.
Offsets are exact nonnegative integers up to `9007199254740991`. The database
primary key includes all four components. Attention range endpoints must stay
within one stream partition. Adjacent/out-of-order offsets update min/max and
unique count without changing lifecycle state. New claims update `updatedAt`.
The private `stageCoordinate:` serializer is not a domain API.

A callback can invoke `recordCoordinate:forAttention:` through
`Store transaction:sending:with:replaying: true`. On an optimistic conflict,
Store's existing read-only replay may recognize a competitor's committed link,
never rebase a new mutation. Replays leave count, range, timestamp and state
unchanged. A failed transaction commits neither the link nor the Attention
update. Reload after failure, inspect `claimedCoordinate:`, then retry explicitly
if still unclaimed. No Stream read or acknowledgement belongs to these APIs.

## Fixture adapter and inspection

`EventSourceAdapter forKind:` is a closed DSL allowlist, not a class-name resolver.
Unknown, disabled, and incompatible input fails before any source read.
`FixtureEventSourceAdapter read:limit:` takes
`{subscription: <document>, records: [<normalized envelope>, ...]}`.
It returns at most 1000 supplied records, bounds input to 1 MiB/1000 records,
validates the selected envelopes, and never persists or accesses Stream.
`normalize:for:`, `consumerFor:`, `groupKeyFor:`, and `displayFor:` expose the base
contract. `displayFor:` returns only the closed title/summary projection.

The generic browser works through `@ Trash instanceRecordsFor: EventSubscription`
and `Attention`. Declared properties appear as columns. Enter opens the existing
instance inspector with actual declared values, including booleans, null
coordinates and lifecycle state. `inspectionRecordFor:` exposes the same safe
record data for noninteractive tests. No model stores raw event payload, command
output, or artifact contents. Future phases must not add those to generic views.

## Regression commands

```bash
for test in tests/test_workstation_*.bash; do bash "$test" || break; done
TRASH_TEST_JOBS=6 TRASH_TEST_TIMEOUT=300 make test
```

Tests use isolated databases and fixture tools. Workstation CUE-specific tests
report an explicit skip if CUE is absent. Migration and missing-capability doctor
tests still run. Replay tests synchronize competing transactions before commit,
check the database unique constraint, force rollback, and cover staged duplicates,
adjacent/out-of-order offsets, and subscription/stream separation.
