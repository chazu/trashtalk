# Live agent session view

Implemented in Trashtalk and the companion Innards checkout.

Outstanding dismissal and detached-notification work is recorded in
[deferred improvements](agent-ui-future-improvements.md). Gusgus conversation
creation from the applet is implemented.

```bash
@ Agent::Session browse             # choose a session, then Attach to conversation
@ "$session" focus               # attach an existing Agent::Session directly
@ "$session" attach              # synonym
@ Gusgus focusCurrent             # current global conversation, or empty composer
# Option-U toggles this view from the configured shell.
```

In the inbox, press Enter on a message and choose **Attach to sender session**.
The action is offered only when its recorded sender resolves to a session.
An identity-scoped sender resolves to its current conversation across directories.
Workspace-scoped senders can produce a chooser. Closed historical sessions offer
messages, runs, and details in the browser; they cannot open a new live view.

The composer starts focused. Enter inserts a newline; **C-c C-c** sends directly
to the attached agent session. **Tab** switches to the transcript, **C-n/C-p** move by line,
**C-v/M-v** page, **C-s/C-r** search, **M->** follows latest output, and **M-<**
loads earlier history. **Option-U** toggles the view; **C-x C-c** also detaches; an unsent draft requires an explicit
discard choice. Esc leaves the composer, then dismisses from the transcript.
**M-x** offers follow, earlier history, pause, resume, stop, **Start fresh
conversation**, compact context, and detach. Starting fresh asks for
confirmation, atomically closes the attached idle session, and opens a new
session with that same agent identity, archetype, role, workspace, and profile.
It works for Gusgus and any other attached agent the current human owns. It
does not stop or move active work: stop the displayed run first, then start
fresh. The former conversation and its history remain inspectable.
Compaction requires an idle, open Jcode session. It runs in the background and
retains both the logical conversation and provider history.
Stop confirms the displayed run ID and pauses new work through the existing
worker API. **C-x ^ / C-x -** and Alt-Down/Alt-Up resize by one row.

When Gusgus has no current conversation, `focusCurrent` opens an empty `inagent`
composer instead of creating a session. The header shows Gusgus, its global
scope, configured profile, and canonical starting workspace. The first
**C-c C-c** creates or reuses the global conversation and sends the draft as
direct session input. Detaching before that send creates nothing; a failed
creation or send retains the draft and never falls back to inbox mail.

Scrolling upward pauses following so new output does not pull you away from
the text you are reading. Reaching the bottom with Down, **C-n**, PageDown or
**C-v** resumes following automatically; **M->** also follows immediately.

After building Trashtalk, install the applet from the Innards checkout with
`cargo install --path . --bin inagent --locked --force`. Existing Trashtalk
shells that loaded these classes can reload them, or start a fresh shell.

## Boundaries and implementation

The existing agent browser offers snapshots and log files. The Innards
`inagent` applet attaches to a logical Agent::Session: scroll and search the
conversation, follow current harness output, compose messages, and detach.

Trashtalk owns session state, inbox delivery, and run control. The applet owns
only presentation and drafts. A temporary exact-argv duplex bridge sends JSONL
snapshots to stdin and accepts a fixed set of JSONL intents on stdout; terminal
output uses `/dev/tty`. Detach, terminal loss, and bridge failure never stop an
agent. Explicit stop targets the displayed run ID and uses the existing worker
authorization and pause behavior.

Backlog combines existing inbox exchanges with direct session conversation text.
The adapter records acknowledged user input and streamed assistant text; raw tool
output and reasoning stay in run inspection. Snapshots carry stable entry IDs,
a bounded recent window, and an earlier-history indicator. Reading the view does
not acknowledge deliveries.

Composer input uses `Agent::Session input:`. Jcode sends the text through its native
Harness API, using `send_message` for an idle session and `soft_interrupt` for
input at the next safe point while working. It uses the active adapter connection,
so there is no second native controller. Idle input starts a tracked run with
fresh authority supplied as a separate system reminder; the user text stays literal.
There is no Message or Agent::Delivery for composer input. Native acknowledgements
clear the draft; rejection or a lost acknowledgement retains it. Paused sessions
must be resumed, and drivers without direct-input support reject the operation.
If the native connection was lost, the session keeps its recovering run for
explicit stop. Choose **M-x → Stop displayed run**, wait for confirmation, then
**M-x → Resume queued work** and send again. Resuming alone does not clear a
recovering run. A failed stop leaves the session paused and can be retried;
its control connection remains available while new tool launches are blocked.
If Jcode reports `refresh_token_reused`, run `jcode login openai` in a terminal
and complete sign-in before retrying input.
`@@` remains an inbox message shortcut and treats every argument as text.

Only displayed message entries issue validated `mark_viewed` intents; these
preserve archive state and never settle work. Drafts clear only after a positive
send acknowledgement. Duplicate request IDs are replayed from the temporary
bridge's response cache; a new attachment never automatically resends a draft.
If a message's displayed text, title or kind changes under the same ID, its new
visible content is acknowledged again. This supports the single updating
Assignment status Message without treating new progress as already viewed.

The initial window is 400 entries, doubling on an earlier-history request up to
100,000. Snapshots refresh roughly once a second plus projection time. Attaching
does not start a worker. Direct conversation text is retained in private per-run
`conversation.jsonl` projections; original provider history and operational logs
remain available through run inspection. Losing the native adapter stops live
output until execution is recovered.

Jcode normalization follows its [Harness API event schema](https://github.com/1jehuang/jcode/blob/master/crates/jcode-harness-api/src/events.rs).
Terminal control bytes are removed from displayed text. Other drivers retain
inbox history inspection and reject composer input until they expose a direct
session adapter.

Public entry points: `@ session focus` (also `attach`), `@ Agent::Session browse`,
and an attach action in the existing session menu. Message actions offer the
sender's current session: resolve the originating identity and its session
policy. Identity-scoped Gusgus resolves to the selected current conversation
across directories; workspace-scoped identities resolve within the workspace. Historical originating sessions remain inspectable when no current
session exists. Ambiguity requires a picker.

Tests validate JSON contracts, backlog/live updates, literal message content, origin
resolution, stale stop intents, detached-process survival, and PTY restoration.
An installed-app smoke test exercises the selected Gusgus conversation,
direct input, live steering, streamed replies, and Option-U detach. The applet can be installed separately
from the other Innards binaries.

## Option-U setup

The local Bash setup uses `bind -x` to preserve a partially edited shell command:

```bash
trashtalk_focus_gusgus() { @ Gusgus focusCurrent; }
bind -x '"\eu": trashtalk_focus_gusgus'
```

Ghostty maps `alt+u` to `text:\x1bu`. Inside `inagent`, the same key detaches
without stopping the agent; an unsent draft follows the existing discard prompt.
Reload the shell binding and terminal configuration after changing them.
