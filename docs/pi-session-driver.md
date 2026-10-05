# pi session driver

`Agent::PiDriver` runs agent sessions on the [pi](https://pi.dev) coding agent.
Written against pi 0.99.2.

## Selecting pi

```toml
# ~/.config/trashtalk/config
gusgus.profile = "pi"
pi.model = "omlx/Qwen3.6-35B-A3B-4bit"   # optional; empty uses pi's own default
pi.extensions = true                      # false adds --no-extensions
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
  --mode json --print [--model M] [--no-extensions] [--session-id REF]
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
supplied by an extension work. Some packages register their own delegation or
memory tools that overlap with Trashtalk's Assignment flow; set
`pi.extensions = false` for runs that must not see them. That also disables
providers those packages supply.

Tests: `tests/test_pi_driver.bash` uses a fixture `pi` on `PATH`.
