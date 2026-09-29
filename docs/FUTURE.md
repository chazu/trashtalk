# Future

Future runs a Bash/Trashtalk computation asynchronously and retains its combined
stdout/stderr until cleanup. It shares the parent runtime's object cache; keep
that runtime alive until its Futures finish. Commands run in a fresh Bash with
exported environment/functions; unexported caller locals do not cross that boundary.

<!-- smoke: future -->
```bash
future=$(@ Future for: 'sleep 1; printf ready')
pid=$(@ "$future" start)
result=$(@ "$future" await)
printf '%s\n' "$result"
@ "$future" cleanup
```

| Message | Result |
| --- | --- |
| `Future for: command` | Create an unstarted Future |
| `start` | Launch once and return its supervisor PID immediately |
| `await` | Wait for completion and return combined output |
| `status`, `poll` | `created`, `pending`, `completed`, `failed`, or `cancelled` |
| `isDone` | Shell status 0 for completed/failed/cancelled, 1 otherwise |
| `exitCode` | Child exit code after await; empty while pending |
| `cancel` | Stop running work; return `Cancelled` or `Already completed` |
| `cleanup` | Cancel pending work, remove owned files, delete the cached object |

`await` returns output even when the child command fails. Inspect `exitCode` or
`status` to distinguish success from failure. A missing completion receipt is an
error, never inferred success. Start several Futures before awaiting any to run
independent computations concurrently.

Process and Tool launch a separate Bash process group and atomically publish its
exit receipt. Both captured and uncaptured `start` sends work: completion uses
the receipt, not Bash `wait` on a child of another shell. Creation and start are
separate so callers can retain the object before launching work.

Each Future owns a private directory under `TMPDIR` (default `/tmp`), containing
`stdout.log`, `stderr.log`, `pid`, `pid.start`, and `exit`. Cleanup removes only
those files. Process uses the same mechanism with separate stdout and stderr.
