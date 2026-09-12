# Reliable agent delivery and session inspection

Gusgus has one current conversation across directories. `@@` sends all arguments
as one inbox message; it has no flags. Option-U and
`@ Gusgus focusCurrent` attach to it without creating or resuming work. A paused
conversation stays paused. `@ Gusgus fresh: "$PWD"` explicitly replaces an idle current
conversation; the browser retains its history. Workspace-scoped specialist
identities retain one current conversation per canonical workspace.

To select among legacy Gusgus conversations, inspect their history, wait for
active runs to finish, then explicitly choose one:

```bash
identity=$(@ AgentIdentity findByHandle: gusgus)
@ "$identity" selectCurrentSession: "$chosenSession"
```

Selection preserves that conversation's provider reference, closes its other
open conversations, and leaves their deliveries attributed to them. Outstanding
historical work requires review; selection does not replay or move it. The
provider conversation may contain context from several workspaces. `@@` retains the caller's canonical directory on each message and
delivery. Direct composer input continues the native session's current directory. Runs execute there, after checking the current role policy; requests
from different directories use separate runs in the same conversation.
Neutral notifications use the recipient session's creation workspace. For an
explicit message context, use `@ Inbox send: 'text' to: "$address" from: "$sender"
in: "$workspace"`. Replies retain the original request's context.

For a large Jcode conversation, use **Compact context** in the session browser
or `@ "$session" compact`. The background run checkpoints the native snapshot
and journal, waits for durable compaction metadata, and keeps the same session
and full history. Acknowledgement alone is not completion. Inspect its run state
and logs as with ordinary work.

Implemented in September 2026. This document describes the current single-host
worker, snapshot browser, and live Innards attachment. The broader
headless-session design remains a partially implemented plan.

## Daily use

### Jcode sessions

Jcode is the default for newly opened Gusgus sessions. Install it from
[jcode.sh](https://jcode.sh), then authenticate:

```bash
@ Jcode version
@ Jcode login                            # OpenAI subscription login
@ Jcode authStatus
@ Trash doctor                          # checks the default Jcode executable
@ Gusgus fresh: "$PWD"                   # requires the current session to be idle
@@ 'Implement the feature described in TASKS.md'
```

Existing sessions keep their backend. Jcode defaults to `gpt-5.6-terra` at medium
effort; `TRASHTALK_JCODE_MODEL` selects another model. Each session uses its own
Jcode home and daemon socket, with existing OAuth credential files linked into
that home. The driver requires `api-bridge --stdio` and Harness API v1.

All harnesses use the same flow: Inbox persists the message, AgentWorker queues
its delivery, and a notification prompt gives the agent references to read with
Inbox `show:`. A private per-run `trash-send` launcher supplies current run
authority, including inside a reused daemon. Agents send attributed messages
with `AgentRun send:to:`, reply to a specific delivery with
`AgentRun result:forDelivery:`, and acknowledge it with `AgentRun settle:`.
Reading a message does not settle its delivery. Agents do not poll their inbox.

Stop an exact execution with `@ "$run" stop`, obtaining the run ID from
`@ "$session" activeRun`. This pauses the session and revokes the run's token
before asking its driver to stop. Jcode confirms native cancellation and idle
state, including when its adapter connection has disappeared. It also terminates
verified Bash tool process groups and shuts down this session's private daemon:
native cancellation alone preserves foreground shell jobs in Jcode 0.84.0. An unconfirmed
stop remains retryable on the same run; it does not claim successful cancellation.
Agents call `AgentRun stop: "$targetRun"`; the caller must have a valid run
token, `agent.stop` in its versioned role, and the same owner as the target.
Existing assistant roles are not silently granted this capability.

Stopped or disconnected work requires review: inspect the run logs, stop any
recovering run, then explicitly requeue or skip its uncertain deliveries and
resume the session. Queued messages remain in Inbox. A stale stop request cannot
stop a replacement run. The next explicit resume restarts the private daemon and
loads the recorded conversation. Process PID fields describe the per-run
adapter, not that daemon. See the
[Jcode driver design](jcode-session-driver.md) for protocol boundaries.

### Maki installation and login

```bash
export TRASHTALK_GUSGUS_PROFILE=maki     # optional backend for new sessions
@ Trash doctor                         # installs Maki if missing; checks its executable
@ Maki version
@ Maki loginToProvider: 'openai'         # interactive Maki provider login
@ Maki authStatus
```

`Maki` is a `Tool` subclass. Installation uses the [official Maki installer](https://maki.sh/)
and defaults to `~/.local/bin` (`MAKI_INSTALL_DIR` overrides the destination).
The wrapper finds that directory even if it is not on PATH; add it to PATH
to invoke `maki` directly. An existing working executable is kept as-is.
Installation failures make doctor fail; doctor never starts a provider login.
`@ Maki install` explicitly installs the latest release. Login inherits your
terminal and returns Maki's exit status.

The `maki` profile selects `MakiDriver`, using `openai/gpt-5.6-terra` at medium effort
with OpenAI OAuth. `TRASHTALK_MAKI_MODEL` selects another `openai/` model.
The driver strips API-key overrides and checks OAuth before each launch.
Stock Maki handles execution and conversation resume; Trashtalk owns delivery,
questions, settlement, and termination.

Maki runs with normal user OS permissions, without an OS sandbox. Each run
uses a standard Maki configuration containing only the medium-effort setting;
global custom plugins/MCP configuration are excluded. Project `.maki/init.lua`,
`.maki/mcp.toml`, `.maki/.env`, and legacy `~/.maki` are rejected with a run-log
diagnostic. Custom commands and Maki's native Task/Memory tools are disabled.
See [Maki session driver](maki-session-driver.md) for protocol details.

`TRASHTALK_GUSGUS_PROFILE` changes the profile for newly opened sessions.
`codex` and the legacy `assistant-low-power` profile still select Codex.
Existing sessions retain their recorded profile. `@ Gusgus fresh: "$PWD"`
closes an idle session and opens one with the current default. Harnesses have
incompatible conversation IDs: switching an existing session must clear its
old `lastConversationRef`. Trashtalk messages and run history remain stored,
but the new harness starts without the previous harness's internal context.

### Sessions and inboxes

```bash
@ AgentSession browse             # sessions that have not been terminated
session=$(@ Gusgus sessionFor: "$PWD")
@ "$session" browse              # one session's actions
@ "$session" details             # plain-text snapshot

inbox=$(@ Trash userInbox)
@ "$inbox" browse                # read and reply to the agent's messages
```

The session picker excludes terminated sessions and refreshes after termination.
A retained session can still be inspected by ID with `@ "$session" browse`.
The picker shows lifecycle, latest run state, queued deliveries,
blocked deliveries/questions, and work needing review. Its actions provide
conversations, run metadata and the last 100 lines of each log, pause,
resume, and confirmed retry of a selected failed or uncertain delivery.
Snapshot menus refresh when returning to an action menu. Choose **Attach to
conversation**, or send `@ "$session" focus`, for live backlog and harness output.
The `inagent` composer sends directly to the session with **C-c C-c**.
**Option-U** toggles the view; **C-x C-c** also detaches. Jcode uses its native
Harness API: `send_message` for idle input and `soft_interrupt` at the next safe
point during an active turn. These inputs create no Message or AgentDelivery.
Pause and stop are explicit actions. Detaching never stops the worker or harness.
See [the live session view](agent-session-view.md) for navigation and installation.

Reading displayed messages marks them read, without settling deliveries or
changing archive state. The Inbox message menu offers **Attach to sender session**
when the recorded origin resolves to a session. Session/run pickers need `inpick`
or `fzf`; ordinary paging falls back to terminal output without `inpage`.

Press **Ctrl-D** on a session in the session list, then choose **Terminate
session and stop active work** to terminate it. **Cancel** is selected by
default; Escape also cancels. Termination revokes run access, prevents further
dispatch, and sends INT then TERM if needed to the active harness. A stop that
cannot be confirmed is shown for inspection and can be retried with
`@ "$session" terminate`.
Messages and logs remain available; unsettled offered deliveries become
uncertain once the process has stopped. The same control is available as
`@ "$session" terminate`.

Legacy records created before the durable-session model may have no lifecycle
field. They can be explicitly terminated from the same picker, preserving their
stored context. Termination also updates their field metadata so a fresh shell
retains the terminal state; these incomplete records cannot be resumed.

Displaying an Inbox preview automatically marks that message read and clears
its unread dot. Opening a thread or session conversation also marks its messages
read. Archived messages stay archived; reading does not answer a question or
settle an agent delivery. Preview generation alone does not mark unseen messages.

Inbox previews and session transcripts share a readable message format: resolved
participant names, local timestamps, and the body below a short header. Inbox
rows show the subject (or first nonblank body line), `●` for unread, `?` for a
question, and `!` for an alert. A question marker describes its kind, not whether
it has been answered. Full addresses and body text remain searchable in inpick;
choose **Message details** in the inbox action menu for IDs and routing metadata.
The compact layout requires the updated Innards inpick; older versions retain
the generic record layout. `@ "$message" show` retains its full metadata format.

Pause prevents new dispatch but leaves current work running. Resume permits
queued work and requests a tick. An uncertain delivery may already have caused
effects: inspect its run before explicitly confirming retry. Failed/uncertain
work stalls subsequent dispatch in that session until requeued or skipped.
Skipping remains a public message requiring a reason:

```bash
@ "$session" skip: "$delivery" note: 'reviewed; no further work required'
```

## Blocking questions and replies

`AgentRun askUser:` publishes a question and blocks the run's held deliveries
in one transaction. An agent can name one delivery or a JSON array of deliveries
when different pieces of work need independent answers:

```bash
trash-send AgentRun askUser: 'Which branch?' forDelivery: "$delivery"
trash-send AgentRun askUser: 'Which environment?' forDeliveries: '["delivery_one","delivery_two"]'
```

Only deliveries currently held by the authenticated run can be linked. Reply
through `@ "$question" reply: 'main'` or the Inbox reply action. Routing matches
the reply's `replyTo` and the original sender/recipient addresses; it records
the first matching answer and resumes only linked deliveries. If several
questions block one delivery, all must be answered. A new message in the same
session or thread does not answer a question by itself.

Reading and archiving do not answer questions or release blocked work. An
archived question can still be answered by its message ID. Inspect links with
`@ "$question" blockingDeliveryIds` and the recorded answer with
`@ "$question" answerId`; these processing records are independent of message
read/archive state. Old answer replays cannot release blockers from a later run.

Questions created before this linkage was introduced have no inferred delivery
links. Any legacy blocked delivery needs explicit operator reconciliation;
unrelated messages no longer release it. The live store had no blocked
deliveries at the time of this migration.

## Continuous operation

The same entrypoint works in a foreground shell or under an OS user supervisor:

```bash
bin/trash-worker --once
bin/trash-worker                  # Ctrl-C stops the worker, not its harnesses
TRASHTALK_WORKER_INTERVAL=2 bin/trash-worker

bin/trash-worker-service install
bin/trash-worker-service start
bin/trash-worker-service status
bin/trash-worker-service stop
```

The default polling interval is one second after each completed tick. macOS
uses `~/Library/LaunchAgents/org.trashtalk.agent-worker.plist`; Linux uses
`$XDG_CONFIG_HOME/systemd/user/org.trashtalk.agent-worker.service` (default
`~/.config/systemd/user`). Installation captures the current executable path,
PATH, store path, and `TRASHTALK_USER`; run install again after changing those.
Restart an already-running service with stop/start after reinstalling it.
launchd logs go to `run/worker/`; systemd logs go to the user journal.
The Linux unit uses `KillMode=process`, and launchd abandons the worker process
group, so service restart leaves detached harnesses alive. Linux unit generation
is tested here; an actual Linux service needs host qualification.

`TRASHTALK_NO_AUTOTICK=1` suppresses the foreground launch on message delivery.
It does not pause a separately running worker. Pause the session or stop the
service to prevent its dispatch.

## Persistence and recovery

* Final message persistence and an agent-routing outbox row commit in one
  SQLite transaction. Notifications are wake hints; pending outbox rows can be
  routed after a crash, even if no notification was observed.
* Assignment is transactional and uses a deterministic delivery identity.
  Replaying routing preserves the original delivery and its attempt count.
  An identity inbox with no unique eligible session remains pending; unresolved
  rows rotate behind other routing work rather than consuming every batch.
* A portable `Process withLock:receiver:selector:argument:` primitive serializes
  workers and foreground ticks for one database. The OS releases the lock on
  owner exit; no timeout can admit a second live owner. Detached harnesses close
  the inherited lock descriptor. This is a local-filesystem, single-host
  contract, not a distributed lease or a replacement for multi-host fencing.
* Workers refresh their cached agent records between ticks. Harness commands
  use independent shell caches and authenticate against current durable state.
* A replacement worker observes the durable launcher PID file, even when the
  previous worker died before saving `processPid`. New launchers also record
  process start time to detect PID reuse. Exit files take precedence over PID
  liveness.
* Failures before launch may retry up to the role's retry limit. Once launch
  begins, an unsuccessful or vanished process with unsettled deliveries is
  uncertain and needs human review, even when it produced no output. A clean
  exit also requires explicit delivery settlement to count as processed.
* State transitions and their associated notes/outcomes are atomic. Provider
  errors from stderr or Codex error events survive into the run details.

Normal serialized operation is deliberately conservative at the spawn boundary:
if launch began but neither a trustworthy live PID nor an exit receipt exists,
work is stalled for review rather than blindly launched again. Explicit retries
can repeat external effects; arbitrary external actions do not become exactly
once because the queue is durable.

## Verification

`tests/test_jcode_driver.bash` exercises the real adapter against a stateful
native-API fixture: queued notifications, inbox reads, scoped replies, resume,
lost connections, token revocation, authorized stop, and cancellation that is
acknowledged without actually stopping. It requires no Jcode login.
`TRASHTALK_TEST_JCODE_LIVE=1 bash tests/test_jcode_live.bash` opts into real
subscription-backed execution in a disposable workspace, database, and private
daemon. It verifies two queued inbox exchanges, stops a running foreground Bash
job, and resumes the same conversation after daemon restart.
`tests/test_jcode_processes.bash` checks process group ownership, stale PID
receipts, and rejection of tool launches after stop.

`tests/test_agent_recovery.bash` drives the real foreground worker with a local
ShellDriver harness. It kills the worker mid-run, loses the stored PID, replays
routing concurrently with foreground ticks, restarts the worker, and checks
that two queued deliveries produce two replies without another user tick.
`tests/test_agent_termination.bash` stops a real local harness, checks token
revocation, retries a reported stop failure, and rejects a stale PID. Browser
tests cover Ctrl-D confirmation, cancellation, and visible failure diagnostics.
The record, worker, browser, service, and Tool detach suites cover transition
rejection, retry limits, read-only dismissal, explicit UI actions, generated
supervisor contracts, and independent child lifetime. No paid model calls are
required by these tests.

## One-shot requests

`@ Agent ask: 'question' workingDirectory: "$PWD" status: '0' lastResult: ''`
uses the one-shot `Agent` facade, selected by
`TRASHTALK_AGENT_BACKEND=axe` (default) or `codex`. `@ Agent dryRun:workingDirectory:status:lastResult:` previews that
request without running a model. These do not join the persistent Gusgus session.
Codex one-shot requests require ChatGPT CLI login, remove API-key overrides, and
use ephemeral read-only execution. Proposal application remains a separate
explicit SourceProposal operation.

The former tmux Agent API, ClaudeAgent, and TmuxSession have been retired. Use
AgentIdentity/AgentSession for durable identity and execution. Tools::Tmux remains
a general command adapter. See [cleanup and migration notes](cleanup-2026-09.md).
