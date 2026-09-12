# Jcode session driver

**Status:** Implemented driver contract. See agent-operations.md for use and recovery; provider protocol details describe the validated version, not all future Jcode releases.

## Problem

An AgentSession outlives individual AgentRuns. A resident harness must not retain
an old run's authority, treat a lost connection as completed work, or confuse direct human input with
inbox delivery. Every harness must support queued notification
delivery and an explicit stop of an exact run.

## Design

Context maintenance uses the same tracked-run lifecycle. `AgentSession compact`
starts native manual compaction only while the session is open and idle. The
adapter retains its attachment until changed compaction metadata is durable.
Jcode writes metadata to either its snapshot or an append-only journal; both
are checkpointed and inspected. Partial journal appends do not prove completion.
The logical session ID, native reference, and complete transcript are retained.
Compaction failures are visible in the run. Refusal, transport loss, or the
five-minute durability timeout fail maintenance without pausing ordinary work;
compaction owns no inbox deliveries or agent tool execution.

Stop discards a valid old birth receipt when its PID has been reused, without
signalling the unrelated process. An unidentifiable remaining process group
still prevents a false stop confirmation. This uses the POSIX rule that a PID
cannot be reused while its previous process group still exists.

`jcode` is a persistent-session backend profile behind AgentDriver. It uses the
[Jcode Harness API](https://jcode.sh/sdk) over `api-bridge --stdio` (API v1,
qualified initially against Jcode 0.84.0). Each Trashtalk session has a private
Jcode home and daemon socket. Each run gets a short-lived Bash protocol adapter;
the daemon and its native conversation survive between runs. Jcode's own wake
scheduler is set to external ownership and its tool list excludes native agent
delegation. Trashtalk remains the message and scheduling authority.

The worker persists messages and deliveries before dispatch. Busy sessions keep
new deliveries pending; the next worker tick after completion sends a notification
prompt containing message references. Agents read their contents with Inbox's
public `show:` message, then reply and settle with AgentRun. Notifications contain
no message bodies or sender-supplied instructions. No model polls its inbox.

Direct conversation input uses the same native API connection. `AgentSession
input:` starts an idle turn with literal user content and a separate system
reminder carrying fresh run authority. While a run is active, the adapter forwards
input as `soft_interrupt`, acknowledged for the next safe point. A private,
locked control channel prevents acceptance after the adapter closes admission.
Requests with uncertain acknowledgements are retained for inspection and never
replayed automatically. Composer input creates no Message or AgentDelivery.
The view projects acknowledged input and streamed assistant text from these runs.
It continues the native working directory and rechecks the role policy. Jcode
advertises `live_input:true`; this integration uses Harness API v1, not ACP.

Each delivery records its execution workspace and a role-policy snapshot. Runs
batch only one workspace and reauthorize it before claiming work. For an existing
native conversation, the adapter compares the persisted directory (including
journal metadata) with the run's directory. If needed, a native `subscribe`
control exchange checks idle state, sets the directory, and verifies `pwd -P`.
This happens before the normal API attachment; Jcode can reject a second control
attachment while that adapter owns the session. The normal attachment must then
report the expected directory before any model input is submitted. This native
boundary was exercised against Jcode 0.84.0; a changed protocol fails closed.

Every harness receives a private, immutable run-specific `trash-send` launcher.
It loads that run's token and database context and invokes the ordinary public
message dispatcher. This works when the daemon's inherited environment predates
the run. The prompt contains the launcher's path, never the token. Old launchers
lose authority when their run stops or finishes.

The adapter records the native session reference before sending the prompt, and
records send intent before writing to the wire. Completion requires an accepted
message followed by `turn_done` for the same session. An acknowledgment, process
exit, or daemon liveness alone cannot prove completion. A lost adapter after send
leaves the run recovering and the session paused; no automatic prompt replay.

`@ "$run" stop` targets an exact run under the worker lock. It pauses its session,
revokes the token, and asks its driver to stop. For Jcode, the adapter is stopped
first to fence late sends, then a separate client cancels the recorded native
session and confirms idle with a fresh attachment. Jcode 0.84.0 deliberately
preserves foreground Bash jobs across cancellation. A private Bash launcher
records each tool's process group and birth under an OS lock. Stop closes tool
admission, kills only verified groups, confirms their exit, and shuts down this
session's private daemon. The native conversation remains on disk for explicit
resume. An unidentifiable surviving group yields an unconfirmed stop rather than
signalling a potentially unrelated process. A failed confirmation leaves
the run active and retryable. Confirmed stops mark unsettled deliveries uncertain.
Queued messages remain durable and require explicit resume and delivery review.
An authenticated agent uses `@ AgentRun stop: "$targetRun"`; its role needs
`agent.stop` and both sessions must have the same owner. These controls are
cooperative API authority, not OS isolation from a harness with Bash access.

## Implementation boundaries

AgentWorker/AgentRun own lifecycle, authorization, queueing, and settlement in the
existing DSL. JcodeDriver adapts launch/outcome/stop. `lib/jcode-api.bash` only owns
the bidirectional JSON wire protocol and local event receipts. There is no new
daemon implementation or compiler dependency. Authentication uses existing OpenAI
subscription credentials; managed homes link only the relevant credential files.
Native steering is exposed through direct conversation input; inbox deliveries
retain their queued notification path.
`lib/jcode-processes.bash` is the narrow OS boundary for registered Bash process
groups; it uses the same existing Perl/flock/setsid facilities as Tool/Process.

Jcode is the default for new Gusgus sessions following live qualification.
Existing sessions retain their captured profile; `TRASHTALK_GUSGUS_PROFILE`
can explicitly select Maki or another supported profile for new sessions.

## Validation

The stateful API fixture exercises queued inbox reads, scoped replies, native
conversation reuse, false completion events, adapter loss, cancellation that is
acknowledged without stopping, same-owner stop authorization, stale tokens, and
preserved pending work. Existing ShellDriver and Maki tests cover the shared
worker and notification path.

On 2026-09-10, Jcode 0.84.0 with OpenAI `gpt-5.6-terra` and medium effort completed
two real queued notification/inbox/reply/settlement exchanges against an isolated
database. The stronger stop test waits for a foreground shell job to start before
requesting cancellation, verifies that the tool process exits without finishing
its remaining work, and resumes the same conversation after restarting the
private daemon. All 15 authenticated acceptance checks passed. The full runtime
suite (59 test files) and compiler suite (43 test files) passed; final receipt
changes also passed the 44-check Jcode driver fixture and termination regressions.
Direct input and live steering were subsequently verified against the installed
Jcode 0.84.0: a single native conversation returned both requested markers,
completed its tracked run, and created no inbox messages.
