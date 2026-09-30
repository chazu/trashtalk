# Process

Process runs shell commands and retains separate stdout, stderr, and exit code.
Use Future when you want combined output; see [Future](FUTURE.md).

## Managed execution

<!-- smoke: process -->
```bash
proc=$(@ Process for: 'printf ready; printf diagnostic >&2; exit 42')
pid=$(@ "$proc" start)
code=$(@ "$proc" wait)
[[ "$code" == 42 ]]
[[ "$(@ "$proc" output)" == ready ]]
[[ "$(@ "$proc" errors)" == diagnostic ]]
```

`start` returns a supervisor PID immediately. Launch several Process objects
before waiting to run independent commands concurrently. Both captured and
uncaptured sends work. Each launch owns a private directory under `TMPDIR`
(default `/tmp`); `wait` reads its atomic completion receipt, stores the result
on the object, and removes its owned files. Keep the parent runtime alive while
children use its object cache.

| Message | Result |
| --- | --- |
| `Process for: command` | Create a wrapper with status `created` |
| `run` | Run synchronously; return the command's numeric exit code as text |
| `start` | Start asynchronously; return the supervisor PID |
| `wait` | Await and collect output; return the numeric exit code as text |
| `isRunning` | Return `true` or `false`; collect a finished run's result |
| `output`, `errors`, `exitCode` | Read the collected results |
| `succeeded` | Return `true` when the collected exit code is zero |
| `signal: name` | Signal the recorded process group after checking PID identity |
| `terminate`, `kill` | Send TERM/KILL, collect completion, mark the terminal status |
| `info`, `help` | Print details or available operations |

A nonzero **command** exit code is a result of `run`/`wait`, not a failed message
send. Launch, missing-receipt, and collection errors fail the send. `terminate`
waits for the child to honor TERM; use `kill` if forced termination is needed.
A wrapper can run again after completion. Starting an already-running wrapper
is an error.

## Class shortcuts

`Process exec:` delegates to `Shell execAll:` and returns stdout. `Process run:`
delegates to `Shell run:` and returns the shell command's status while preserving
its output. These differ from the instance `run` result contract above.

Process has no PID-only shortcuts. A bare PID does not retain a completion
receipt: Bash `wait` only works for a child of the calling shell, and a PID
returned through a captured message is not such a child. Use a managed Process
for asynchronous completion; `Tool isAlivePid:` answers liveness for a PID.

`Process withLock:receiver:selector:argument:` runs a public send under an OS
lock; its `waiting:` variant bounds lock acquisition. The worker uses this
boundary to serialize changes to its durable queue.
