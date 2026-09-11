# Live agent session view

Implemented in Trashtalk and the companion Innards checkout.

```bash
@ AgentSession browse             # choose a session, then Attach to conversation
@ "$session" focus               # attach an existing AgentSession directly
@ "$session" attach              # synonym
```

In the inbox, press Enter on a message and choose **Attach to sender session**.
The action is offered only when its recorded sender resolves to a session.
Multiple replacement sessions produce a chooser; closed historical sessions
open with their lifecycle visible and reject new messages.

The composer starts focused. Enter inserts a newline; **C-c C-c** sends through
the inbox. **Tab** switches to the transcript, **C-n/C-p** move by line,
**C-v/M-v** page, **C-s/C-r** search, **M->** follows latest output, and **M-<**
loads earlier history. **C-x C-c** detaches; an unsent draft requires an explicit
discard choice. Esc leaves the composer, then dismisses from the transcript.
**M-x** offers follow, earlier history, pause, resume, stop, and detach.
Stop confirms the displayed run ID and pauses new work through the existing
worker API. **C-x ^ / C-x -** and Alt-Down/Alt-Up resize by one row.

After building Trashtalk, install the applet from the Innards checkout with
`cargo install --path . --bin inagent --locked --force`. Existing Trashtalk
shells that loaded these classes can reload them, or start a fresh shell.

## Boundaries and implementation

The existing agent browser offers snapshots and log files. The Innards
`inagent` applet attaches to a logical AgentSession: scroll and search the
conversation, follow current harness output, compose messages, and detach.

Trashtalk owns session state, inbox delivery, and run control. The applet owns
only presentation and drafts. A temporary exact-argv duplex bridge sends JSONL
snapshots to stdin and accepts a fixed set of JSONL intents on stdout; terminal
output uses `/dev/tty`. Detach, terminal loss, and bridge failure never stop an
agent. Explicit stop targets the displayed run ID and uses the existing worker
authorization and pause behavior.

Backlog combines durable messages with run output and diagnostics. It normalizes
Jcode, Codex, and Maki records into text/tool/status entries and shows shell
logs as plain output. Snapshots carry stable entry IDs, a bounded recent
window, and an earlier-history indicator. Loading earlier history expands the
window explicitly. Reading the view does not acknowledge deliveries. Composer
submissions use the messaging plane and remain queued when a run is busy.
Only displayed message entries issue validated `mark_viewed` intents; these
preserve archive state and never settle work. Drafts clear only after a positive
send acknowledgement. Duplicate request IDs are replayed from the temporary
bridge's response cache; a new attachment never automatically resends a draft.

The initial window is 400 entries/physical log lines per run, doubling on an
earlier-history request up to 100,000. Limits and missing log files are visible.
Snapshots reconcile roughly once a second plus projection time; the view does
not start a worker just by attaching. It reads durable run logs, so a lost native
adapter cannot supply live output until execution is recovered. Source logs
remain available through the older session browser. There is no new durable
event store, harness login, model picker, or direct harness prompt channel here.

Jcode normalization follows its [Harness API event schema](https://github.com/1jehuang/jcode/blob/master/crates/jcode-harness-api/src/events.rs).
Codex and Maki use their existing driver JSONL logs. Native tool text is rendered
as text, with terminal control bytes removed from the displayed projection.

Public entry points: `@ session focus` (also `attach`), `@ AgentSession browse`,
and an attach action in the existing session menu. Message actions offer the
sender's current session: resolve the originating identity and workspace from
the recorded session, prefer that session while open/paused, otherwise choose
among current sessions in the same workspace. Do not invent a session or cross
workspaces. Historical originating sessions remain inspectable when no current
session exists. Ambiguity requires a picker.

Tests validate JSON contracts, backlog/live updates, literal message content, origin
resolution, stale stop intents, detached-process survival, and PTY restoration.
An installed-app smoke test exercises public focus, inbox sending, and detach
against a real detached shell process. The applet can be installed separately
from the other Innards binaries.
