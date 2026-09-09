# Reliable agent delivery and session inspection

Implemented in September 2026. This document describes the current single-host
worker and snapshot browser; the broader headless-session design remains the
longer-term plan.

## Daily use

### Maki installation and login

```bash
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

This wrapper prepares the CLI. Gusgus still uses its configured Codex driver;
Maki session execution needs a separate driver.

### Sessions and inboxes

```bash
@ AgentSession browse             # all sessions, grouped by their own identity/workspace
session=$(@ Gusgus sessionFor: "$PWD")
@ "$session" browse              # one session's actions
@ "$session" details             # plain-text snapshot

inbox=$(@ Trash userInbox)
@ "$inbox" browse                # read and reply to the agent's messages
```

The session picker shows lifecycle, latest run state, queued deliveries,
blocked deliveries/questions, and work needing review. Its actions provide
read-only messages, run metadata and the last 100 lines of each log, pause,
resume, and confirmed retry of a selected failed or uncertain delivery.
Snapshots refresh when returning to an action menu; this is not a live event
viewer. Closing the picker or pager does not stop the worker or harness and
does not change a message's unread or archived state. Reply through the Inbox
browser. Session/run pickers require `inpick` or `fzf`; paging falls back to
plain terminal output if `inpage` is absent.

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

`tests/test_agent_recovery.bash` drives the real foreground worker with a local
ShellDriver harness. It kills the worker mid-run, loses the stored PID, replays
routing concurrently with foreground ticks, restarts the worker, and checks
that two queued deliveries produce two replies without another user tick.
The record, worker, browser, service, and Tool detach suites cover transition
rejection, retry limits, read-only dismissal, explicit UI actions, generated
supervisor contracts, and independent child lifetime. No paid model calls are
required by these tests.
