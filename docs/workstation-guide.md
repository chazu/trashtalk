# Workstation guide: command receipts, attention, and delegation

**Status:** user guide for the implemented feature set (Phases 0 to 2), 2026-09-14.
Operator and recovery detail lives in [workstation operations](workstation-operations.md);
the design rationale is in [the design note](workstation-event-attention-delegation.md).

## What it does

You run commands all day. Some fail, and you keep working. The workstation layer
turns those failures into one compact, durable item per problem instead of a
flood of output or notifications:

1. You run a command through a thin wrapper, `bin/trash-command`.
2. When the command finishes, the wrapper publishes a small **receipt** (exit
   status, directory, label, timestamps) to a durable stream in your Trashtalk
   store. The command's own exit status, terminal, and signals are untouched.
3. The background worker reads new receipts, keeps the failures, and groups
   repeats of the same failure in the same directory into one **Attention**.
4. Each Attention is one **alert Message** in your inbox. Later repeats update
   that one message and its count rather than sending another.
5. You **acknowledge**, **snooze**, **resolve**, or **suppress** the item, and,
   if you want, **delegate** it to an agent you already have running, or let a
   subscription delegate new failures automatically.

Nothing in this layer executes commands on your behalf, changes files, or
starts agent sessions. Delegation only sends a message to a session that
already exists and is yours.

## Concepts

| Term | What it is |
| --- | --- |
| Receipt | A closed JSON record about one finished command: workspace, safe label, exit code, start and finish time, a redacted summary. Never the command's arguments, environment, or raw output. |
| Stream | The durable append-only log receipts are published to (`workstation.command-receipts.v1`). Backed by the Honker SQLite extension. |
| Subscription | Your policy for consuming that stream: which exit codes count, how often to nudge you, and optionally which agent identity may receive delegated work. An `EventSubscription` record. |
| Attention | One grouped, stateful item: the same failing label in the same directory with the same exit class. Carries an event count and the first and last stream positions. |
| Root alert | The single Message in your inbox that represents an Attention. Subject and body come from the receipt's safe title and summary plus the count. |
| Delegation | Sending one Attention as a message to an existing agent session so it can investigate and reply into your inbox thread. |
| Worker | `bin/trash-worker`, the same background process that already dispatches agent work. It reads receipts and maintains Attention on every tick. |

## Setup

### Prerequisites

- A working Trashtalk install (`make`, `source lib/trash.bash`).
- The Honker SQLite extension, for the durable stream: `bin/install-honker`.
- The CUE CLI, for validating every record before it is stored:
  `go install cuelang.org/go/cmd/cue@latest`.
- `TRASHTALK_USER` set to your inbox name if it differs from `$USER`.

Check all of it at once:

```bash
@ Trash doctor
@ WorkstationSchema capabilities     # {"cue":true,"stream":true}
```

Missing CUE or Honker only disables this feature. Builds, the inbox, and
existing agents are unaffected.

### Create a subscription

One subscription per person is the normal setup. It says: read the command
receipt stream from now on, treat any non-zero exit as a failure, and nudge me
at most once per fifteen minutes per group.

```bash
source lib/trash.bash
digest=$(@ WorkstationSchema digest)
sub=$(jq -c --arg d "$digest" '
  .adapterKind="command-receipt"
  | .streamName="workstation.command-receipts.v1"
  | .schemaDigest=$d
  | .filter={exitNot:0}
  | .debounceSeconds=900
  | .initialPosition="from-now"' \
  schemas/workstation/v1/fixtures/EventSubscription.valid.json)
sub=$(@ EventSubscription createFrom: "$sub")
@ "$sub" summary
```

Fields you may want to change before creating:

| Field | Meaning |
| --- | --- |
| `filter.exitNot` | The exit code that means "fine". Receipts with exactly this code are skipped. `0` is the usual choice. |
| `debounceSeconds` | Minimum gap between wake hints for the same group (0 to 86400). Consumption is never delayed; only the nudge is. |
| `initialPosition` | `from-now` ignores receipts published before the worker first registers the subscription; `from-start` replays the whole stream. |
| `id`, `consumerName` | Keep them paired: `consumerName` must be `workstation/<id>`. |
| `owner` | Must be your inbox name. |

The fixture starts with `enabled: true` and `dispatchState: paused`. Enabled
controls whether receipts are read at all. Dispatch state only matters for
automatic delegation (see below), so leave it paused until you opt in.

### Run the worker

The worker is the existing agent worker. Run one tick by hand, or leave the
supervised service running:

```bash
bin/trash-worker --once                  # one tick, then exit
bin/trash-worker-service install && bin/trash-worker-service start
bin/trash-worker-service status
```

After `make`, a running service notices the rebuilt runtime and restarts
itself on its next idle beat.

## Running commands

Wrap any command you want tracked:

```bash
bin/trash-command --cwd "$PWD" --label 'unit tests' -- make test
bin/trash-command --cwd ~/proj --label 'lint' -- npm run lint -- --fix
```

- Everything after `--` is the exact program and arguments. Nothing is passed
  through a shell, so quoting, globs, and `$variables` behave exactly as they
  would without the wrapper.
- The command keeps your terminal, stdin, and foreground status. Interactive
  programs work. `Ctrl-C` and other signals are forwarded.
- The wrapper exits with the command's exit code, or terminates with the same
  signal the command died from. Scripts and `&&` chains see no difference.
- `--label` is the name you will see in your inbox. Choose something safe to
  display; it is stored verbatim. Never put secrets or full argument lists in it.
- `--cwd` is the directory the command runs in and the directory the receipt
  records. Use the directory that produced the failure; that is where a
  delegated agent will work.

### Optional output capture

```bash
bin/trash-command --cwd "$PWD" --label 'flaky suite' --capture -- ./run-tests
```

`--capture` routes the command's stdout and stderr through the wrapper (still
printing them to your terminal) and records only a byte count, saturated at
`4096+`, in the summary. The content itself is discarded before anything is
stored. Do not use `--capture` with programs that need a TTY.

### Handy aliases

```bash
tc() { bin/trash-command --cwd "$PWD" --label "$1" -- "${@:2}"; }
tc 'unit tests' make test
```

### What is and is not recorded

Recorded: a generated receipt id, the canonical directory, your label, the exit
code, start and finish timestamps, a short summary such as
`Command exited with status 2`, and an exit-class fingerprint. When a command
runs inside an agent run, a closed origin `{producer: "agent-run", run: <id>}`
is added so delegation can avoid loops.

Never recorded: command arguments, environment variables, stdout or stderr
content, or any file contents. Display text is bounded to 256 characters and
cannot contain terminal control characters.

If publication fails (for example Honker is unavailable), the wrapper prints
`Command receipt publication failed; child outcome is unchanged` to stderr and
still returns the command's real status. There is no retry spool; that run is
simply not tracked.

## What you see

### The alert in your inbox

On the worker's next tick, a failing receipt becomes one alert:

```bash
inbox=$(@ Trash userInbox)
@ "$inbox" list
# * message_...  [alert] workstation -> chazu: unit tests
@ "$inbox" show: message_...
```

The body reads like `Attention attention_...: 3 matching events. Command
exited with status 2`. The count and the first and last stream positions are
kept on the message as metadata. Running the same failing command again
updates this message; it does not create a second one.

Grouping is by directory, label, and exit class. The same failing `make test`
in two repositories produces two alerts. A different label in the same
directory produces a separate alert.

### The count

`attentionCount` is the compact number meant for prompts and status bars: how
many groups are open or have a snooze that expired.

```bash
@ "$inbox" attentionCount          # e.g. 2
@ Attention localOpenCount         # same number, no inbox needed
```

A prompt snippet that shows `!2` only when something needs you:

```bash
_ws_attn() { local n; n=$(@ Attention localOpenCount 2>/dev/null); [[ ${n:-0} -gt 0 ]] && printf '!%s ' "$n"; }
PS1='$(_ws_attn)\w \$ '
```

### In your prompt

The worker also publishes the count as one short line in
`~/.trashtalk/run/attention` (under `TRASHTALK_RUN_DIR`, or `TRASHTALK_DIR/run`,
when those are set): `!2` for open or due-snoozed attention, `?1` for questions
still waiting on your answer, `!2 ?1` for both, and an empty line when nothing
waits. It is rewritten after every worker tick and after every attention
lifecycle change, so a prompt can show it without starting a Trashtalk runtime.
With Whisker, the two-line Bash prompt, its `file` segment reads it:

```toml
[view.dev]
segments = ["attention", "directory", "git"]

[segment.attention]
file = "~/.trashtalk/run/attention"
atomic = true
max_age = "1m"
```

`max_age` hides the segment once the worker has stopped refreshing the file, so
a stale count never sits on the prompt looking current. Any other prompt can
read the same file.

### Wake hints

If Honker is installed, the inbox emits a `message` event at most once per
subscription debounce window for each group, only after the alert is durably
stored, and only while the group is open (an expired snooze counts as open).
Acknowledged, snoozed, suppressed, and resolved groups keep counting silently.

Hints are optional. If a hint is lost, the alert and the count are still
correct; nothing depends on the hint being received. To react to hints:

```bash
handler=$(@ Block params: '["payload"]' code: 'osascript -e "display notification \"$(jq -r .subject <<<"$payload")\" with title \"Trashtalk\""' captured: '{}')
@ "$inbox" onMessage: "$handler"
@ "$inbox" stopListening
```

## Acting on an attention

### From the command line

All controls live on the alert message and are limited to the inbox owner:

```bash
@ "$msg" inspectAttention                       # the Attention record as JSON
@ "$msg" acknowledgeAttention                   # seen; keeps counting quietly
@ "$msg" snoozeAttentionUntil: '2026-09-15T09:00:00Z'
@ "$msg" resolveAttentionWithNote: 'fixed the fixture'
@ "$msg" suppressAttention: 'known flaky; ignore'
@ "$msg" reopenAttention
```

### From the inbox browser

```bash
@ "$inbox" browse
```

Pick the alert and press Enter. Workstation alerts show attention actions in
place of Reply: acknowledge, snooze for a day, resolve and suppress (each asks
for a note on a terminal), reopen, routing status, delegate, redelegate, focus,
view thread, details, and archive. The browser uses Innards `inpick` when
installed and falls back to `fzf` or `$EDITOR`.

### What each state means

| State | Counted? | New matching failures | Typical use |
| --- | --- | --- | --- |
| open | yes | append to this alert; may nudge again after the debounce window | default |
| acknowledged | no | append silently | "I saw it, I'm on it" |
| snoozed | no until the time passes, then yes | append silently | "not before tomorrow" |
| suppressed | no | append silently, forever | known noise |
| resolved | no | start a **new** group and a **new** alert | done |

Rules that catch people out:

- Reading or archiving the message never changes the Attention. Archive hides
  the message; the count still includes the group.
- Acknowledging is not resolving. An acknowledged group keeps counting events.
- Snooze needs a future time in normalized UTC form (`YYYY-MM-DDTHH:MM:SSZ`).
  The browser's snooze is always 24 hours.
- Resolve and suppress require a non-blank note. Reopen is the way back from
  either.
- Transitions are explicit: a state cannot transition to itself, and a
  concurrent change fails cleanly. Reload and look before retrying.

## Delegating to an agent

Delegation hands one Attention to an agent session you already have, as a
message in that session's inbox. The agent reads it, investigates in the
directory that produced the failure, and replies into your inbox in the same
thread as its delegation message. Nothing is delegated until you name a
target, and nothing is delegated automatically until you opt in.

### Name a target

Any identity you own works. Gusgus, the assistant behind `@@`, is the common
choice because it already has one current conversation:

```bash
identity=$(@ Gusgus identity)
@ "$sub" target: "$identity" reason: 'route failing test runs to Gusgus'
@ Gusgus sessionFor: "$PWD" >/dev/null       # make sure a session exists
```

A specialist works the same way; sessions for a workspace-scoped identity are
per directory:

```bash
identity=$(@ AgentIdentity named: test-fixer)
@ "$identity" owner: "$TRASHTALK_USER"; @ "$identity" save
session=$(@ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: ~/proj profile: jcode)
```

`clearTarget:` removes the target and turns automatic delegation off. Both are
audited revisions of the subscription.

### Check before you send: routing status

```bash
@ "$msg" routingStatus | jq .
```

This is a dry run. It changes nothing and tells you exactly what a delegation
would do, or why it cannot:

```json
{
  "status": "eligible",
  "reason": "",
  "nextAction": "delegate",
  "targetHandle": "gusgus",
  "session": "agentsession_...",
  "sessionState": "open",
  "receiptWorkspace": "/Users/you/proj/sub",
  "executionWorkspace": "/Users/you/proj",
  "lineageDepth": 0,
  "delegationRevision": 0,
  "message": "", "delivery": "", "run": ""
}
```

`status` is `eligible`, `ineligible`, or `delegated`. When ineligible, `reason`
is one of:

| Reason | Meaning | What to do |
| --- | --- | --- |
| `no-target` | the subscription names no identity | `target:reason:` |
| `subscription-disabled` | receipts are not being read | `enable:` |
| `target-missing`, `target-unauthorized`, `target-disabled` | the identity is gone, someone else's, or disabled | fix the identity or pick another |
| `workspace-unknown`, `workspace-missing` | the receipt's directory was not recorded or no longer exists | run the command again from a real directory |
| `no-session` | the identity has no current session for that workspace | start one (`@@`, `Gusgus sessionFor:`, `AgentSession openFor:`) |
| `session-ambiguous` | a legacy identity has several eligible sessions | `selectCurrentSession:` on the identity |
| `session-paused`, `session-closed` | the session is not open | resume or open a session |
| `workspace-unauthorized` | the session's role does not allow that directory | adjust the role or use another identity |
| `capability-missing` | the role lacks `inbox.read` | the agent could not read the message anyway |
| `recipient-denied` | the role's recipient policy excludes you | adjust the role |
| `budget-exhausted` | the session already holds as many unsettled deliveries as its `messageBudget.count` | let it finish, or raise the budget |
| `lineage-target`, `lineage-depth` | the failure was produced by the target itself, or by a chain of delegations deeper than allowed | see loop controls |

The receipt directory is used as the agent's working directory, normalized the
same way every delivery is: the Git top level when the directory is inside a
repository, otherwise the real path.

### Delegate one attention

```bash
@ "$msg" delegateAttention          # prints the delegation message id
```

This creates, in one transaction, one message addressed to the session
(`kind: attention`, sent by you), one delivery, and the queue row the worker
uses. The Attention records which message, session, and identity it went to.
The worker's next tick starts the agent; if you delegated from a foreground
shell, the session is ticked immediately.

The message the agent receives includes the attention id, your label, the
working directory, the event count, and the summary, with the instruction to
investigate and reply, not to change anything. Its reply lands in your inbox:

```bash
@ "$inbox" thread: message_<attention>_delegation_1
```

Repeating `delegateAttention` returns the same message id and creates nothing.
A crash or worker restart between steps does the same. If something changed
between the dry run and the send (the session paused, a budget filled), nothing
is sent and the call fails with the reason.

More failures in an already delegated group update the alert and the count but
never send the agent a second message.

### Follow, refocus, or send again

```bash
@ "$msg" routingStatus | jq '{delivery,deliveryState,run,runState}'
@ "$msg" focusDelegatedAttention          # attach to the session's live view
@ "$msg" redelegateAttention: 'session was replaced'
@ "$msg" delegateAttention                # publishes delegation 2
```

Focus opens the recorded session in the conversation view (needs the Innards
`inagent` applet). It never resumes a paused session, stops a run, or starts
anything; the session must still be open or paused and yours.

Redelegate clears the assignment and skips the previous delivery only if it
never started. Work that a run already picked up is left alone. The next
delegate sends a fresh message, usually to a replacement session.

### Automatic delegation (opt in)

With automatic delegation on, every newly accepted failure group is checked
against the same admission rules and, when eligible, delegated during the same
worker tick that creates it. The default is off, and enabling it requires a
target and a confirmation note that is stored with the policy:

```bash
@ "$sub" enableAutomaticDelegation: 'I understand delegated work runs without review' reason: 'opt in'
@ "$sub" resumeDispatch: 'route automatically'
```

`dispatchState` is the master switch for automatic routing. `pauseDispatch:`
stops new automatic delegations without touching the policy, and without
stopping the worker from reading receipts or maintaining alerts. Manual
delegation from the inbox still works while dispatch is paused.

When an automatic delegation is withheld, the Attention keeps a visible note,
for example `automatic routing withheld: no-session`, and the alert stays in
your inbox. The next matching failure retries, so a group withheld because no
session existed routes as soon as you open one. Groups you have acknowledged,
snoozed, suppressed, or resolved are never routed automatically.

Turn it off again with `disableAutomaticDelegation: 'reason'`.

### Loop controls

Agents run commands too. If Gusgus reruns the failing suite while investigating
your delegation, that receipt must not wake Gusgus again. Two rules apply to
both manual and automatic delegation:

- A receipt produced inside a run of the target identity is never routed to
  that identity (`lineage-target`).
- Each group has a lineage depth: `0` for your own commands, at least `1` for
  a command run by an agent, and one more than the deepest delegation that
  run was handling. Groups deeper than the subscription's limit are not
  routed (`lineage-depth`). The default limit is `1`; change it with
  `@ "$sub" lineageLimit: 2 reason: 'allow one hand-off'`.

Depth is fixed when a group is created. Receipts from agent runs carry the run
id as origin when the run's token is valid; otherwise they are treated as
yours.

### Budgets and roles

Delegation respects the target session's role: its workspace policy, its
recipient policy, and `messageBudget.count` (0 means unlimited) counted against
that session's pending, offered, and blocked deliveries. Roles are not modified
by this feature. Gusgus's default role allows any workspace and has no budget.

## Managing subscriptions

```bash
@ EventSubscription listByOwner: "$TRASHTALK_USER"
@ "$sub" summary                               # enabled, dispatch, target, mode, revision
@ "$sub" disable: 'vacation'                   # stop reading receipts
@ "$sub" enable: 'back'                        # resume from the stored position
@ "$sub" pauseDispatch: 'no automatic routing today'
@ "$sub" resumeDispatch: 'ok'
@ "$sub" target: "$identity" reason: '...' | clearTarget: '...'
@ "$sub" enableAutomaticDelegation: 'confirmation' reason: '...' | disableAutomaticDelegation: '...'
@ "$sub" lineageLimit: 1 reason: '...'
```

Every change is a new revision with a reason you can read back. The stream,
filter, consumer name, and initial position are fixed at creation; to change
them, create a new subscription. Disabling and re-enabling never loses or
replays receipts: the stream position is remembered by Honker, not by the
subscription.

The generic browser (`@ Trash browse`) shows `EventSubscription` and `Attention`
records with their fields as columns.

## Privacy and safety guarantees

- Receipts carry metadata only. Output is discarded even with `--capture`.
- Every stored record is validated against a closed schema first; unknown
  fields are rejected, and validation never runs while the store is locked.
- The worker never skips a record on its own. A record it cannot process stays
  put, is reported with its subscription id, and blocks later records for that
  subscription until you look at it (see recovery below).
- Nothing here runs a command, edits a file, or approves anything. Delegated
  agents operate under their existing role and the same cooperative controls as
  any other message you send them.
- Delegation never creates, resumes, replaces, or terminates a session.
- All controls are scoped to the inbox owner; another user's identities and
  sessions are never eligible targets.

## Troubleshooting

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| `@ Trash doctor` warns "Workstation CUE unavailable" | CUE not installed | `go install cuelang.org/go/cmd/cue@latest`; subscription creation is blocked until then |
| doctor warns "Honker Stream unavailable" | extension not built or `sqlite3` cannot load extensions | `bin/install-honker`, and use an extension-capable `TRASH_SQLITE3` |
| Commands run but no alert appears | worker not running, subscription disabled, or the exit code equals `exitNot` | `bin/trash-worker --once`; `@ "$sub" summary`; check the exit code |
| Alert appeared once, then nothing on repeats | that is grouping working | `@ "$msg" inspectAttention` shows the growing `eventCount` |
| Count is non-zero but the inbox looks empty | the alert was archived | archiving hides the message only; acknowledge, resolve, or suppress the group |
| Worker prints `subscription ... was not advanced past its failed record` | a record failed validation, CUE is missing, or the schema digest drifted | fix the cause and let the next tick retry; to skip a poison record deliberately: `@ "$consumer" ack: <offset>` where `consumer=$(@ CommandReceiptSourceAdapter consumerFor: "$subDocument")` |
| `Command receipt publication failed` on the wrapper | Honker or the store was unavailable at that moment | the command result is still correct; nothing to repair |
| `routingStatus` says `no-session` for Gusgus | no current conversation | `@@ 'hi'` or `@ Gusgus sessionFor: "$PWD"` |
| `routingStatus` says `session-ambiguous` | a legacy identity with several open sessions | `@ "$identity" selectCurrentSession: "$session"` |
| Delegated, but no run starts | worker not running, session paused, or a stalled delivery on that session | `bin/trash-worker --once`; `@ "$session" summary`; `@ AgentSession browse` |
| Worker logs `command not found` after `make` | a worker older than the auto-restart behavior | `bin/trash-worker-service stop && bin/trash-worker-service start` |

Everything durable can be inspected directly:

```bash
@ Attention localOpen                         # ids needing you
@ Store getInstance: "$attention" | jq .
@ "$msg" show                                 # full message with metadata
```

## Command reference

| Purpose | Command |
| --- | --- |
| Run and track a command | `bin/trash-command --cwd DIR --label 'text' [--capture] -- PROGRAM [ARGS...]` |
| Publish a receipt without running anything (testing) | `bin/trash-receipt --publish "$json"` |
| One worker tick / supervised service | `bin/trash-worker --once` / `bin/trash-worker-service install\|start\|stop\|status` |
| Create, list, inspect subscriptions | `EventSubscription createFrom:`, `listByOwner:`, `summary` |
| Subscription switches | `enable:` `disable:` `pauseDispatch:` `resumeDispatch:` `target:reason:` `clearTarget:` `enableAutomaticDelegation:reason:` `disableAutomaticDelegation:` `lineageLimit:reason:` |
| Count for prompts | `@ "$inbox" attentionCount`, `@ Attention localOpenCount` |
| Alert controls | `inspectAttention` `acknowledgeAttention` `snoozeAttentionUntil:` `resolveAttentionWithNote:` `suppressAttention:` `reopenAttention` |
| Delegation | `routingStatus` `delegateAttention` `redelegateAttention:` `focusDelegatedAttention` |
| Browser | `@ "$(@ Trash userInbox)" browse` |
| Prompt indicator | `AgentWorkboard indicator`, `publishIndicator`, `indicatorPath` |
| Capability check | `@ Trash doctor`, `@ WorkstationSchema capabilities` |
