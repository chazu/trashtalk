# Workstation records: operations

New here? Start with the [workstation guide](workstation-guide.md). This page is
the operator and implementation reference behind it.

Phases 0 and 1 are a thin durable model plus one local event journey: a wrapped
command publishes a receipt, the worker turns matching failures into grouped
`Attention` and one owner-Inbox Message, and the owner inspects, acknowledges,
snoozes, resolves, or suppresses it. Phase 2 adds
[guarded agent routing](#phase-2-guarded-agent-routing): an opt-in target
identity, a dry run that explains eligibility, explicit or automatic delegation
to an existing session, and loop controls. See the
[implementation plan](workstation-event-attention-implementation-plan.md) and
[design](workstation-event-attention-delegation.md).
Only `EventSubscription` and `Attention` are new persisted domain classes.
There is no executor or automatic effect. Phase 1 alone never creates
AgentDelivery, AgentRun, AgentSession, AgentIdentity, or outbox rows; Phase 2
creates one Message, one AgentDelivery, and one outbox row per delegation and
never creates, resumes, replaces, or stops a session.

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

The Inbox browser (`@ "$inbox" browse`) shows the same controls on a
workstation alert instead of Reply: acknowledge, snooze for a day, resolve and
suppress (each asks for a note on a terminal), and reopen. Replying to the
`workstation` sender is deliberately not offered; nothing reads that inbox.

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
  compiled classes lazily, so a rebuild could leave it calling helpers it never
  sourced (`_store_matching_lines: command not found` in `run/worker/stderr.log`
  was the symptom). The worker now notices files newer than its start under
  `trash/.compiled` or `lib` (the compiler cache directories are pruned from
  that scan) on its next beat, logs `Trashtalk runtime rebuilt`, and
  re-executes itself with the same pid. If a worker predates this behavior,
  restart it once: `bin/trash-worker-service stop && bin/trash-worker-service start`.

### Cost per tick

An idle tick with no enabled subscriptions is one Store query. With
subscriptions, the schema is installed once per worker process, the consumer
handle is reused, and CUE validation of identical bytes within one runtime is a
process-local memo (keyed by schema digest, root, and document hash; failures
are never memoized). A new failing record still runs CUE about three times, so
budget roughly one second per accepted event on a laptop.

### The prompt indicator file

`AgentWorkboard indicator` is the compact count: `!N` open or due-snoozed
Attention for the local human (the same query as `Attention localOpenCount`),
`?M` rows in `agent_questions` with no answer whose Message is addressed to the
human, separated by one space, or an empty line when both are zero.
`publishIndicator` writes that line to `AgentWorkboard indicatorPath`
(`$TRASHTALK_RUN_DIR/attention`, else `$TRASHTALK_DIR/run/attention`, else
`~/.trashtalk/run/attention`) through a temporary file and one rename, so a
reader sees the old line or the new one. `AgentWorker tick` publishes after its
workstation stage and `Attention transitionTo:until:note:` after its commit;
both run outside Store transactions and the worker lock. A write failure is
reported on stderr and never fails the tick or the transition. The file is
rewritten even when its text is unchanged, so its modification time tells a
reader whether the worker is alive; Whisker's `max_age` uses exactly that.
Answering a question refreshes the file on the next tick, at most
`TRASHTALK_WORKER_MAX_INTERVAL` seconds later. Cost per idle tick: two Store
queries and one `mv`. `tests/test_attention_indicator.bash` covers the text,
the path, the failure policy, and liveness; `test_workstation_worker.bash`
checks it across the lifecycle.

## Phase 2: guarded agent routing

Routing is off until a subscription names a target identity. Subscriptions
keep consuming the local receipt stream; each receipt's canonical `--cwd`
becomes the delegated work's execution directory (its Git top level when the
directory is inside a repository, as for every other delivery). Delegation
sends existing durable work to an existing eligible session. It never creates,
resumes, replaces, or stops a session, never broadens a role, and never
executes anything itself.

### 2A: target configuration and dry-run admission

```bash
identity=$(@ AgentIdentity named: gusgus)        # or any identity you own
@ "$sub" target: "$identity" reason: 'route failing test runs'
@ "$sub" clearTarget: 'stop routing'            # also resets delegation to manual
@ "$msg" routingStatus                          # dry run; changes nothing
@ "$attention" routingStatus
```

`target:reason:` is an audited revision; the identity must exist, be enabled,
and belong to the current owner. The dry run resolves, without mutation:

| Order | Check | Reason code when it fails |
| --- | --- | --- |
| 1 | subscription owner and `enabled` | `owner-mismatch`, `subscription-disabled` |
| 2 | target named, present, owned, enabled | `no-target`, `target-missing`, `target-unauthorized`, `target-disabled` |
| 3 | receipt workspace recorded and still present | `workspace-unknown`, `workspace-missing` |
| 4 | already delegated for the current revision | status `delegated` |
| 5 | receipt lineage (see loop controls) | `lineage-target`, `lineage-depth` |
| 6 | one current session for the target and execution workspace | `no-session`, `session-ambiguous` |
| 7 | that session is open | `session-paused`, `session-closed` |
| 8 | session role: workspace policy, `inbox.read`, recipient policy | `workspace-unauthorized`, `capability-missing`, `recipient-denied` |
| 9 | role `messageBudget.count` versus the session's pending, offered, and blocked deliveries (`0` means unlimited) | `budget-exhausted` |

Session resolution follows the identity's own scope policy through the same
membership rules as `AgentSession currentWithinFor:`: Gusgus resolves its
identity-scoped current session; a workspace-scoped specialist resolves the
session for the receipt's execution workspace. The result is JSON with
`status` (`eligible`, `ineligible`, `delegated`), `reason`, `detail`,
`session`, `sessionState`, `targetIdentity`, `targetHandle`,
`receiptWorkspace`, `executionWorkspace`, `lineageDepth`, `lineageLimit`,
`delegationRevision`, the stream coordinates, `message`/`delivery`/`run` links
with their states, `routingNote`, and `nextAction` (`configure-target`,
`delegate`, `focus`, or `inspect`). A dry run creates no Message, outbox row,
AgentDelivery, AgentRun, or lifecycle change.

Attention records now carry `workspace`, closed `origin`, `lineageDepth`,
`delegationRevision`, `delegatedMessage`, `delegatedSession`,
`delegatedIdentity`, and `routingNote`. All are optional in the CUE contract,
so records written before Phase 2 remain valid and read as unrouted groups.

### 2B: explicit one-attention delegation

```bash
msg=$(@ "$root" delegateAttention)      # from the owner's root alert Message
@ "$attention" delegate                 # same operation on the Attention
@ "$root" redelegateAttention: 'session replaced'
```

`delegate` runs the dry run, CUE-vets the intended Attention patch, then
revalidates admission inside one Store transaction that creates one
agent-facing Message (`message_<attention>_delegation_<revision>`, kind
`attention`, addressed to `session:<id>`, sent by the owner so the agent's
reply lands in the owner's inbox thread), one automatic `AgentDelivery`
(`agentdelivery_<attention>_delegation_<revision>`) with the receipt's
execution workspace and role snapshot, and one outbox row already assigned to
the session, through `AgentQueue publishManual:`. The Message carries the
receipt cwd, first/last stream coordinates, event count, and lineage depth.
The Attention records the message, session, identity, and revision.

The uniqueness key is `(attention, delegation revision)`: deterministic ids plus
the transaction's optimistic guard on the Attention row mean a duplicate click,
worker replay, or crash retry returns the existing message and writes nothing.
If admission changed between the dry run and the transaction (another session,
paused, closed, budget), nothing is published and the call fails with the
reason. Later events in the same group append to the Attention and refresh the
root alert; they never create a fresh prompt. A supervised worker tick claims
the delivery like any other automatic delivery; with `TRASHTALK_NO_AUTOTICK`
unset, `delegate` also ticks the session in the foreground.

`redelegateAttention:` requires a reason, skips the prior delivery only if it
never started (pending or blocked), clears the assignment, and records
`redelegated: <reason>` in `routingNote`. The next `delegate` publishes the
next revision, typically to a replacement session.

### 2C: opt-in automatic routing and loop controls

```bash
@ "$sub" enableAutomaticDelegation: 'I understand delegated work runs without review' reason: 'opt in'
@ "$sub" resumeDispatch: 'route automatically'   # dispatchState gates automatic routing only
@ "$sub" lineageLimit: 1 reason: 'default'
@ "$sub" disableAutomaticDelegation: 'opt out'
```

The default is off. Enabling requires a target and a nonblank confirmation
note; both are recorded in the subscription's closed `delegation` policy
(`mode`, `maxLineageDepth`, `confirmation`) as an audited revision. While the
subscription's `dispatchState` is `paused`, automatic routing is withheld and
the group shows `routingNote: automatic routing withheld: dispatch-paused`.

Inside the acceptance transaction, after the coordinate claim and before the
root alert, the worker rechecks the full admission for a group that is `open`
and not yet delegated (on creation and on every later event) and publishes at
most one delegation. An ineligible group stays local with
`routingNote: automatic routing withheld: <reason>`; the next event retries, so
a group withheld for `no-session` routes once a session exists. Acknowledged,
snoozed, suppressed, resolved, and already-delegated groups are never routed
automatically. Replay after commit-before-ack finds the claimed coordinate and
publishes nothing.

**Lineage.** Commands an agent run executes through `bin/trash-command` carry
a closed `origin: {producer: "agent-run", run: <id>}` when the run's
`TRASHTALK_RUN_TOKEN` validates; an invalid or missing token yields an ordinary
human receipt. The worker fixes each group's `lineageDepth` at creation: `0`
for human receipts, otherwise at least `1`, or one more than the deepest
delegated Message the origin run held. Routing rejects a receipt whose origin
run belongs to the target identity (`lineage-target`) or whose depth exceeds
`maxLineageDepth` (`lineage-depth`, default `1`). Origin is loop-prevention
metadata, not an authorization boundary.

### 2D: attention-to-conversation operations

The Inbox browser offers **Routing status**, **Delegate to configured agent**,
**Redelegate**, and **Focus delegated conversation** on workstation alerts next
to the Phase 1 controls. `focusDelegatedAttention` opens the recorded session
through `AgentFocus open:`, which checks ownership and liveness and never
resumes, stops, or replaces it. `routingStatus` shows target, session, reason,
stream coordinates, message/delivery/run links, and the next action.

Disposable UAT, in a throwaway store:

```bash
export SQLITE_JSON_DB=$(mktemp -d)/uat.db TRASHTALK_USER=$USER
source lib/trash.bash; honker_bootstrap
digest=$(@ WorkstationSchema digest)
sub=$(@ EventSubscription createFrom: "$(jq -c --arg d "$digest" '.adapterKind="command-receipt"|.streamName="workstation.command-receipts.v1"|.schemaDigest=$d|.filter={exitNot:0}|.debounceSeconds=900|.initialPosition="from-now"' schemas/workstation/v1/fixtures/EventSubscription.valid.json)")
identity=$(@ Gusgus identity); @ "$sub" target: "$identity" reason: uat
@ Gusgus sessionFor: "$PWD" >/dev/null           # the existing eligible session
bin/trash-command --cwd "$PWD" --label 'uat failure' -- false
bin/trash-worker --once
root=$(@ "$(@ Trash userInbox)" unread | head -1)
@ "$root" routingStatus | jq .                   # eligible, nextAction delegate
@ "$root" delegateAttention                      # one delivery to that session
@ "$root" routingStatus | jq '{delivery,deliveryState,run,runState}'
@ "$root" focusDelegatedAttention                # attach without resuming
```

Regression: `bash tests/test_workstation_routing.bash` covers every reason
code, replay, changed admission, grouped failures, session replacement,
recursive origin, lineage depth, redelegation, focus, and the absence of
created sessions or identities. The receipt adapter now accepts a target
identity in the subscription policy; everything else in its contract is
unchanged.
