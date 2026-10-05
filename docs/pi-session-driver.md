# pi session driver

`Agent::PiDriver` runs agent sessions on the [pi](https://pi.dev) coding agent.
Written against pi 0.99.2.

## Selecting pi

```toml
# ~/.config/trashtalk/config
gusgus.profile = "pi"
pi.model = "omlx/Qwen3.6-35B-A3B-4bit"   # optional; empty uses pi's own default
# pi.excludeTools defaults to the pi-background-tasks tools (bg_delegate, bg_run, ...)
# pi.extensionPaths = "/path/to/ext.ts,/path/to/other.js"   # load only these
```

or `TRASHTALK_GUSGUS_PROFILE=pi`. Gusgus supplies the profile, and delegated
Assignment specialists inherit the delegating run's profile, so one setting
moves both. Existing sessions keep their recorded profile; run
`@ Gusgus fresh: "$PWD"` to open one on pi. Pi's conversation ids are not
interchangeable with other harnesses, so context does not carry over.

`@ Trash doctor` reports the pi executable and version when the profile is
`pi`. Install and login are pi's own: run `pi` and `/login`, or configure a
provider key or local provider in `~/.pi/agent`. Trashtalk does not strip
provider environment variables from pi.

## Protocol

One detached process per delivery batch:

```
bash -c 'cd "$1" && shift && exec "$@"' pi-driver <workspace> pi \
  --mode json --print [--model M] [--exclude-tools T,...] \
  [--no-extensions -e PATH ...] [--session-id REF]
```

The worker's prompt is the process's stdin. Pi streams JSONL events to stdout;
stderr is kept as diagnostics. The first event, `session`, carries pi's session
id, which the driver records as the conversation reference. The next run for the
same session passes it back with `--session-id`, in the same workspace, so pi
resumes its own history.

A run succeeded when the log's last `agent_end` has no `willRetry` and the last
assistant message did not stop with `error` or `aborted`. Exit zero alone is
not success. A provider failure is reported from that message's
`errorMessage`; failures before the first turn (no API key, unknown model) are
the stderr tail.

Pi runs with the user's OS permissions and no sandbox (`sandbox: false`). Stop
sends INT, then TERM, to the recorded process like the other drivers.

## Extensions

Pi loads the user's packages by default, which is what makes a provider
supplied by an extension work. Some packages register tools that overlap with
Trashtalk's Assignment flow. Two settings control that:

- `pi.excludeTools` (comma-separated, passed as `--exclude-tools`) disables
  individual tools. It defaults to every `pi-background-tasks` tool:
  `bg_delegate,bg_result,bg_run,bg_run_pi_attested,bg_status,bg_logs,bg_kill`.
  Subagents started through them would run outside Assignment's lifecycle,
  stop and review controls. Set it to an empty string to allow them.
- `pi.extensionPaths` (comma-separated files) switches to an allowlist: the
  driver adds `--no-extensions` and one `-e` per path, so packages installed
  later cannot add tools. Include any extension that supplies your provider,
  e.g. `~/.pi/agent/npm/node_modules/pi-omlx-picker/index.ts`.

Verified against pi 0.99.2 by asking the model to list its tools: the default
shows `bg_delegate` and the other `bg_*` tools, `--exclude-tools` removes the
named ones, and `--no-extensions -e <picker>` leaves only `read, bash, edit,
write` with the omlx provider still working. Exclusion names are exact, so a
newer version of the package that adds tools needs them added to the list.

Tests: `tests/test_pi_driver.bash` uses a fixture `pi` on `PATH`.
