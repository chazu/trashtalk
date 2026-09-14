# Workstation records: operations

Phases 0 and 1 are a thin durable model plus one local event journey: a wrapped
command publishes a receipt, the worker turns matching failures into grouped
`Attention` and one owner-Inbox Message, and the owner inspects, acknowledges,
snoozes, resolves, or suppresses it. See the
[implementation plan](workstation-event-attention-implementation-plan.md) and
[design](workstation-event-attention-delegation.md) for later phases.
Only `EventSubscription` and `Attention` are new persisted domain classes.
There is no agent routing, executor, or automatic effect: Phase 1 never creates
AgentDelivery, AgentRun, AgentSession, AgentIdentity, or outbox rows.

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

## Phase 1 command producer

```bash
bin/trash-command --cwd "$PWD" --label 'unit tests' -- make test
bin/trash-command --cwd "$PWD" --label 'local check' --capture -- ./check 'exact argument'
bin/trash-receipt --publish "$receiptJson"  # fixture, no command execution
```

The wrapper never evaluates argv. Its narrow Perl/POSIX supervisor is necessary
at the OS boundary to distinguish `exit(143)` from SIGTERM, which Bash `$?` alone
cannot do. The child inherits stdin/stdout/stderr and the foreground terminal
without a new session or process group. Catchable INT/TERM/HUP/QUIT addressed to
the wrapper are forwarded to the child. After `waitpid`, the public DSL producer
publishes once through Stream. The wrapper then returns the exact exit code or
terminates with the child's signal. SIGKILL, machine loss, and publication failure
cannot guarantee a receipt. A failed publication prints a separate diagnostic
and never replaces the known child outcome. There is no hidden receipt spool.

Use an intentionally safe label, never argv containing secrets. Receipt metadata
contains a generated stable ID, canonical physical working directory (not Git
root), label, status, start/end timestamps, and an exit-class fingerprint. No
command arguments, environment, or raw output are persisted. Optional origin is
closed presentation-only `{producer: "safe-label"}` metadata, never routing.

`--capture` explicitly replaces stdout/stderr with pipes and tees bytes to their
original destinations. Its conservative redaction policy discards **all output
content** before publication and retains only a saturated `4096+` byte count in
the safe summary. This protects arbitrary secrets, not just recognized token
formats. No raw artifact is created. Default execution does not touch the child
output descriptors. Capture does not wait for detached descendants holding a
pipe open. Never use capture when the child requires a TTY. All persisted display
text is bounded to 256 characters and excludes terminal control characters.

## Production adapter and consumer registration

Only `command-receipt` is production-allowlisted. `consumerFor:` returns one
durable `Stream consumerNamed:consumer:` handle per subscription (found or
created by stream and consumer name), and `CommandReceipt publish:` reuses one
`Stream producerNamed:` handle per stream. Honker owns offsets and records, so
no handle is created and destroyed per publish or read. It requires stream
`workstation.command-receipts.v1`, grouping `opaque`, empty `targetIdentity`, and
closed filter `{exitNot: 0}` (an omitted `exitNot` also means zero). Disabled
policies, arbitrary consumer names, malformed coordinates, and unknown schemas
fail before acceptance. Display and grouping are stateless. Group keys hash the
canonical workspace, safe command label (legacy display title fallback), and
normalized exit-class fingerprint. Worker grouping also scopes the partition.

Adapter reads return at most eight records and 512 KiB per batch. Each envelope
is at most 64 KiB. Honker `topic`, `key`, and `offset` become stream name,
partition, and offset without reinterpretation. If a source supplies explicit
`partition`, that takes precedence over `key`. The adapter never acknowledges,
creates Attention/Message, or accesses AgentQueue.

`Stream initializeFrom:` atomically registers the **existing Honker consumer**
with INSERT OR IGNORE. `from-start` starts at zero. `from-now` starts at the high
water mark on first registration, not at subscription document creation time.
Registration of an empty stream still writes offset zero, so a later restart
cannot accidentally skip its first receipt. Repeated registration and changes
to enablement never reset that position. `acknowledgeThrough:` advances it
monotonically even with competing readers. These narrow Stream/Honker SQL
boundaries use Honker's own tables, not another event log or cursor table.

## Phase 1 worker stage and local attention

Enable the journey by creating one `command-receipt` subscription for the
current human owner. The digest, stream, consumer name, adapter, grouping, and
filter are closed; `debounceSeconds` bounds wake hints, not consumption.

```bash
digest=$(@ WorkstationSchema digest)
sub=$(jq -c --arg d "$digest" '.adapterKind="command-receipt"
  | .streamName="workstation.command-receipts.v1" | .schemaDigest=$d
  | .filter={exitNot:0} | .debounceSeconds=900 | .initialPosition="from-now"' \
  schemas/workstation/v1/fixtures/EventSubscription.valid.json)
@ EventSubscription createFrom: "$sub"
bin/trash-command --cwd "$PWD" --label 'unit tests' -- make test
bin/trash-worker --once          # or leave the supervised service running
inbox=$(@ Trash userInbox)
@ "$inbox" attentionCount        # compact count for status displays
@ "$inbox" list                  # the root alert Message per failing group
```

`AgentWorker tick` runs `WorkstationWorker tick` after agent reconciliation and
outside the agent OS lock. Each tick visits at most four enabled subscriptions
in a wrapping keyset order, reads at most four records per subscription through
its named Honker consumer, and processes each record in one short Store
transaction: claim the coordinate, find or create the open/acknowledged/snoozed/
suppressed Attention for the group, extend its count and range, then create or
update the owner's root Message. Only after commit does the worker acknowledge
the consumer offset. Replayed coordinates acknowledge without changing anything.

Records that match the filter (an exit code equal to `exitNot`) are validated
and acknowledged without any domain write. Grouping hashes the workspace, safe
label, exit-class fingerprint, and stream partition, so the same failing command
in two workspaces never shares an Attention.

### The root Message

The Message is an ordinary `alert` from `workstation` in the owner's Inbox with
`dispatchMode: manual`. Its subject and body carry the safe display projection
and the event count; `attention`, `attentionFirstCoordinate`,
`attentionLastCoordinate`, and `attentionEventCount` are causal metadata. Later
events in the same group update that one Message rather than sending another.
Reading or archiving the Message never acknowledges or resolves the Attention.

A Honker `message` wake hint is emitted at most once per subscription debounce
window, only after commit, and only while the Attention is open (an expired
snooze counts as open). Acknowledged and suppressed groups keep counting silently.

Attention controls live on the Message and require the current human owner:

```bash
@ "$msg" inspectAttention
@ "$msg" acknowledgeAttention
@ "$msg" snoozeAttentionUntil: '2099-01-01T00:00:00Z'
@ "$msg" resolveAttentionWithNote: 'fixed upstream'
@ "$msg" suppressAttention: 'known noise'
@ "$msg" reopenAttention
```

Resolving a group and receiving a later matching failure opens a new Attention
and a new root Message. `Attention localOpen` and `localOpenCount` list open or
due-snoozed Attention for the owner's subscriptions without mutating a snooze.

### Recovery

- **A subscription was not advanced past its failed record.** The worker
  prints this with the subscription ID and leaves the consumer offset where it
  was; later records for that subscription wait behind it. Inspect the record
  with `@ "$consumer" read: 1` on `@ CommandReceiptSourceAdapter consumerFor:`,
  fix the cause (missing CUE, schema digest drift, a rejected owner), and let
  the next tick retry. To skip a poison record deliberately, acknowledge past it
  with `@ "$consumer" ack: <offset>`; nothing skips automatically.
- **Acknowledgement failed after commit.** The transaction is durable; the next
  tick replays the coordinate, finds the existing claim, and acknowledges it.
- **Disabled subscription.** Nothing is read or advanced. Re-enable with
  `enable: reason`; consumption resumes from the stored Honker offset.
- **Stale worker after a build.** A long-running `bin/trash-worker` loads
  compiled classes lazily, so a rebuild can leave it calling helpers it never
  sourced (`_store_matching_lines: command not found` in `run/worker/stderr.log`
  is the symptom). Restart the service after `make`:
  `bin/trash-worker-service stop && bin/trash-worker-service start`.

### Cost per tick

An idle tick with no enabled subscriptions is one Store query. With
subscriptions, the schema is installed once per worker process, the consumer
handle is reused, and CUE validation of identical bytes within one runtime is a
process-local memo (keyed by schema digest, root, and document hash; failures
are never memoized). A new failing record still runs CUE about three times, so
budget roughly one second per accepted event on a laptop.
