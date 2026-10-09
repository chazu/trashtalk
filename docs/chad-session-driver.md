# chad session driver

`Agent::ChadDriver` runs agent sessions on the [chad](https://github.com/nathansutton/chad)
local coding agent (`chad-code`, MLX inference on Apple Silicon). Written
against chad 2.4.0 by reading its source; **not yet qualified with a live model
run**. Opt-in: it is not a default.

## Selecting chad

```smalltalk
Chaz subclass: Preferences
  GusgusSettings profile: 'chad'
  Agent::ChadSettings model: ''          # empty uses chad's shipped default
```

or `TRASHTALK_GUSGUS_PROFILE=chad`. Delegated Assignment specialists inherit the
delegating run's profile. Existing sessions keep their recorded profile; run
`@ Gusgus fresh: "$PWD"` to open one on chad. `@ Trash doctor` reports the
executable and version. Install with `@ Tools::Chad installCommand`.

| Setting | Env | Default | Meaning |
|---|---|---|---|
| `chad.model` | `TRASHTALK_CHAD_MODEL` | empty | `--model` (HF repo, local dir, GGUF) |
| `chad.thinkBudget` | `TRASHTALK_CHAD_THINK_BUDGET` | 0 | `--think-budget`; 0 omits it |
| `chad.sandbox` | `TRASHTALK_CHAD_SANDBOX` | `off` | see Sandbox |

## Protocol

One detached process per delivery batch:

```
env CHAD_SESSION_DIR=<run base>/chad-sessions/<session> [CHAD_NO_SEATBELT=1] \
  bash -c 'cd <workspace> && exec chad --yolo [--model M] [--think-budget N] \
           [--continue] -- "$(cat prompt.txt)"'
```

- **Prompt.** chad accepts only a positional prompt (no stdin), so the wrapper
  reads the worker's prompt file into it. The prompt is therefore visible in
  `ps`. An empty prompt exits 2 rather than opening chad's interactive TUI.
- **Continuity.** chad stores conversations per directory, and `--continue`
  resumes the newest. Each Trashtalk session gets its own `CHAD_SESSION_DIR`,
  so "newest" is always that session's. Every resume forks to a new chad id.
- **Conversation reference.** chad prints no id. The driver reports the newest
  saved conversation in the session's store after the run. The reference is
  recorded for inspection; resume depends on the store, not the id.
- **Result.** Exit 0 means the task ended on its own; exit 1 a guard or budget
  stopped it; exit 130 an interrupt. Only 0 is a result, and failures name the
  status and keep the stderr tail. As with every driver, exit status does not
  settle a delivery: the agent does that through `trash-send`.
- **Output.** stdout is chad's final answer and stderr the trace; both are kept
  in the run directory.

## Sandbox

In headless mode chad runs yolo, wrapping bash in macOS `sandbox-exec` so writes
stay inside the workspace, temp directories and `~/.chad`. `trash-send` writes
the Trashtalk database outside those, so under the sandbox an agent cannot
deliver results. There is no allowlist setting in chad, so `chad.sandbox`
defaults to `off`, which sets `CHAD_NO_SEATBELT=1` (same posture as pi: the
agent runs with the user's permissions). `on` keeps the sandbox and is only
useful for runs that never need to reach Trashtalk. The driver reports
`sandbox: false` either way.

## Not verified

- That the local model follows the delegation prompt and calls `trash-send`.
- Several chad processes at once on one machine (each loads its own model).
- Behavior across chad versions: `Tools::Chad` pins 2.0.3 for install while this
  was read from 2.4.0.

Tests: `tests/test_chad_driver.bash` uses a fixture `chad` on `PATH`.
