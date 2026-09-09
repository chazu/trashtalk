# Changelog

## 2026-09-09

### Added

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
