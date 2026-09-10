# Maki session driver

Implemented 2026-09-09 against stock Maki 0.5.2. Jcode became the default on
2026-09-10; Maki remains available through `TRASHTALK_GUSGUS_PROFILE=maki`.

## Problem

The Maki Tool wrapper installed the executable and exposed provider login,
but Gusgus still dispatched Codex. Making Maki the default required an actual
session adapter: launching a process alone does not establish resumable
conversation identity, durable replies, or successful delivery settlement.

## Design and implementation

`TRASHTALK_GUSGUS_PROFILE=maki` selects Maki; `AgentWorker driverFor:` resolves that
profile to `MakiDriver`. Existing `codex` and `assistant-low-power` mappings
remain Codex so historical runs retain their correct interpretation. Sessions
capture their profile on creation. The one-shot `Agent` backend is unchanged.

Maki runs through its SDK protocol: `--print --input-format stream-json
--output-format stream-json`. The driver wraps the worker's prompt in one
`user` JSONL record and closes stdin. Maki completes the queued turn before
exiting. Subsequent runs pass `--session` with the recorded Maki session ID.
Plain print mode is insufficient because it does not resume stored sessions.

The existing detached launcher owns PID/exit files, logs, environment run
tokens, and process lifecycle. The worker continues to own routing, claims,
question links, and settlement. Maki calls the same `trash-send AgentRun`
surface as other harnesses. No Maki plugin implements Trashtalk behavior.

The driver reads the session ID from `system/init` or `result` events. The
last result must be a successful, non-error result; exit zero alone is not
success. The worker additionally requires held deliveries to be settled.
Provider errors and preflight diagnostics remain available in run logs.

## Model, authentication, and configuration

The default model is `openai/gpt-5.6-terra`, overridable with
`TRASHTALK_MAKI_MODEL`. This version of the adapter requires an `openai/` model
and an OpenAI OAuth login. It strips `OPENAI_API_KEY`, `CODEX_API_KEY`, and
`OPENROUTER_API_KEY` both when checking auth and when launching.

Maki 0.5.2 accepts but ignores the compatibility `--effort` flag. Instead,
each run receives a standard `maki.setup({ always_thinking = "medium" })`
configuration through its own `XDG_CONFIG_HOME`. OAuth credentials stay in
Maki's normal state location; they are not copied into run directories.

The controlled global config excludes custom plugins and MCP servers. Project
Lua/MCP/env additions and the legacy `~/.maki` directory are rejected because
they would bypass that configuration choice. Project permission rules still
apply. Custom commands and native Task/Memory tools are disabled, keeping
background work within Trashtalk's session lifecycle.

Maki's SDK uses `bypassPermissions` for unattended tool execution. This is not
an OS sandbox: the process has normal user filesystem/network permissions.
The driver's capabilities explicitly report `sandbox: false`. AgentRole
metadata must not be interpreted as OS enforcement. Live steering and an
attached Maki terminal are outside this adapter's scope.

## Switching existing sessions

Changing Gusgus's default affects newly opened sessions. An existing idle
session can be migrated by changing its profile to `maki` and clearing its
`lastConversationRef` while holding the worker store lock and checking that
no run is starting, running, or recovering. Preserve session identity,
messages, and historical run profiles. Codex's private conversation context
cannot be resumed by Maki; the first Maki run begins a new harness history.

## Validation

`tests/test_maki_driver.bash` exercises real worker dispatch with a fake Maki
executable: exact model/workspace arguments, API-key removal, medium config,
threaded replies, resume, settlement, error-result exit zero, OAuth rejection,
and project-extension rejection. Existing shell-driver tests cover shared
worker recovery and lifecycle behavior.

For authenticated acceptance, run:

```bash
TRASHTALK_TEST_MAKI_LIVE=1 bash tests/test_maki_live.bash
```

This uses two real model turns in an isolated store and workspace. It verifies
an inbox reply, the same resumed Maki session ID, recall of a marker from the
first turn, and settlement of both deliveries. It does not send messages to
the user's real inbox.
