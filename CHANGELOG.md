# Changelog

## 2026-09-10

### Added

- Manual `Assignment` workflow: identity ownership, completion criteria, origin
  and optional opaque issue reference, sequential session/run history, progress,
  inbox questions, and atomic outcome publication/settlement. Work is held from
  automatic harness dispatch. Repeated selection/completion is idempotent, and
  current-delivery authority fences stale or superseded runs.
- `jcode` persistent-session profile, `JcodeDriver`, and `Jcode` CLI discovery,
  authentication status, and interactive login. The native Harness API adapter
  uses a private resident daemon per session and resumes its conversation.
- Common exact-run stop (`@ "$run" stop`; authenticated agents use
  `AgentRun stop:` with `agent.stop` and the same owner). Stops revoke run
  authority, pause dispatch, and preserve unsettled work. Jcode stop confirms
  native idle state, terminates verified Bash tool process groups, and closes
  the private daemon even after the run adapter disappears.
- Stateful fake-Jcode regression coverage and an opt-in authenticated test.

### Changed

- Assignment helpers now live in the `Assignment` package: Participation handles
  session selection and claims; Authority, Reporting and Presentation are traits.
  The public class and stored records are unchanged. Qualified traits support
  compilation, message dispatch, method inspection and reload.
- Assignment lifecycle and authorization now use DSL methods and traits with
  ordinary `Persistable` saves. Shared selective Store transactions retain
  atomic message/queue publication and conflict checks; `AssignmentStore` is removed.

- Innards marks displayed Inbox previews and opened conversations read, updates
  the unread indicator, and preserves archived state and first-read timestamps.
- Jcode resolves its helper paths when an interactive shell has not set
  `TRASHTALK_DIR`, matching the common launcher's default.
- New Gusgus sessions default to Jcode. Explicit profile overrides and existing
  session profiles are retained; `Trash doctor` checks the selected harness.
- All session harnesses receive inbox notifications containing references;
  agents read message contents through Inbox. Private per-run `trash-send`
  launchers carry current authority across resident harness environments.
- `AgentRun result:forDelivery:` replies to one held delivery's thread.
- Jcode connection loss after send pauses the session and retains a recovering
  run for explicit stop, without replaying its prompt.
- Worker reconciliation reloads delivery state after harness exit, preserving
  blocking questions committed after the worker's initial snapshot.
- Worker snapshots refresh published messages and final delivery states, while
  excluding message drafts that another process is still filling in.

## 2026-09-09

### Added

- `MakiDriver` for stock Maki execution and SDK conversation resume, including
  OpenAI OAuth preflight, API-key stripping, medium-effort configuration,
  persisted failure diagnostics, and fake/opt-in authenticated acceptance tests.
- `Maki` Tool class with version/authentication checks and interactive
  `loginToProvider:`. `Trash doctor` installs Maki when missing using the
  official installer and verifies the resulting executable.
- Ctrl-D in the session list opens a termination confirmation. Confirming
  revokes run access and stops active work, retaining messages and logs and
  reporting any unconfirmed process stop.
- Explicit blocking-question links, `AgentRun askUser:forDelivery:` and
  `askUser:forDeliveries:`, and message queries for linked deliveries and the
  first recorded answer. Question publication and delivery blocking are atomic.
- A durable agent-routing outbox, committed atomically with each delivered
  message. Routing replay preserves delivery identity and attempt history.
- A continuously running `bin/trash-worker`, with launchd and systemd user
  service configuration through `bin/trash-worker-service`.
- Single-host worker locking and recovery across worker restarts, including
  detached launcher PID files and process start-time checks for PID reuse.
- `@ AgentSession browse` to select a session, and `@ "$session" browse` to
  inspect that session directly. The browser shows activity, messages, runs,
  and logs, with pause, resume, and confirmed retry controls.
- Shared readable message formatting for inbox previews and session
  transcripts: participant names, local timestamps, compact unread/question/
  alert markers, and the body below a short header. Updated Innards inpick
  provides compact rows and a larger preview; full addresses and body text
  remain searchable. **Message details** exposes IDs and routing metadata.
- An [agent operations guide](docs/agent-operations.md) covering setup,
  controls, recovery, and current limitations.

### Changed

- New Gusgus sessions default to Maki with `openai/gpt-5.6-terra` at medium
  effort. Codex profiles remain available for existing sessions and explicit
  selection. Maki runs with normal OS permissions, without Codex's sandbox.
- The session picker hides terminated sessions, including immediately after
  confirmation. Their history remains accessible by session ID.
- Legacy sessions without a lifecycle field can be explicitly terminated from
  the session picker; their stored context is retained.
- Replies resume only the deliveries linked to the question they answer, once
  all questions blocking each delivery have been answered. Unrelated messages,
  reading, archiving, and replaying old answers do not release blocked work.
- Access the current user's inbox through `@ Trash userInbox`, then send
  `count`, `unreadCount`, or `browse` to that instance. `Inbox` class methods
  no longer stand in for one user's inbox; class-side `count` counts inboxes.
- Failed or vanished harnesses that may have performed work leave unsettled
  deliveries uncertain for human review. Only failures before launch retry
  automatically. Failed/uncertain deliveries stall later work in that session
  until explicitly retried or skipped.
- Run and delivery transitions persist their associated outcomes and notes
  atomically. Provider stderr and Codex error events are retained in run details.
- Dismissing the session browser leaves agent execution and message presentation
  state unchanged; pause prevents new dispatch while current work continues.
- Cue and Mise missing-tool tests retain access to Homebrew jq on macOS.

The worker recovery contract is single-host. Linux service configuration is
tested, but live Linux service operation has not yet been qualified.

## 2026-09-08

- Configured Gusgus's Codex driver to use Terra with medium reasoning effort.
- Added direct inbox archiving with Ctrl-D in Innards, preserving messages and
  their threads while hiding archived items from the inbox.
- Corrected inbox unread and visible-message counts, including archived items
  and counts beyond the message-list query limit.
