# Performance and terminal output

Trashtalk keeps message dispatch in Bash. Class messages use compiled metadata;
instance messages resolve their class once per public send. Session files still
carry live object state across command substitutions, and ordinary `new`
retains immediate persistence. These changes do not introduce a daemon or a
second runtime.

## JSON values

Use collection literals followed by `asJson` for temporary structured data:

```smalltalk
classMethod: contextFor: question status: status data: data [
  ^ #{schema_version: 1 question: question
      status: (status jsonValue) data: (data jsonValue)
      flags: #(true false null)} asJson
]
```

The compiler emits one jq invocation for the entire nested value. String
parameters are encoded as strings; number literals, `true`, `false`, and
`null` retain their JSON types (`nil` also represents JSON null). Use
`jsonValue` to insert and validate an already encoded value, including a
number supplied as a string. Malformed typed input fails the method without
printing a partial result. Constructing these values does not allocate or
persist `Json` instances. The existing `Json object` / `Json array` builder
API remains available with its existing object semantics.

Exact argv vectors use the same form:

```smalltalk
argv := #(toolPath '--title' title '--result-json') asJson.
^ @ Tool captureArgvJson: argv input: input
```

Optional fields and arguments can still use `objectAt:putJson:` and
`arrayPush:` on the resulting JSON value. Keep workflow decisions in the DSL;
put process execution and serialization mechanics in shared primitives.

## Diagnostics and progress

Runtime diagnostics use stderr and default to warnings/errors. Set
`TRASHTALK_LOG_LEVEL` to `off`, `error`, `warn`, `info`, `debug`, or `trace`.
`debug` includes class/method resolution; `trace` additionally includes send
arguments. Suppressed logging returns success. Legacy `DEBUG=yes`,
`TRASH_DEBUG=0`, and quiet switches remain supported when no explicit log
level is set. Startup failures remain visible in the CLI launchers.

`TRASHTALK_PROGRESS=auto` shows a single status line for interactive operations
that take longer than 200 ms. Set it to `0` to disable it or `1` to request it
when a controlling terminal exists. Progress writes only to `/dev/tty`, clears
on completion/cancellation, and finishes before an Innards surface starts.
Nested operations do not start another renderer. Headless calls remain quiet.
The REPL marks its interactive context; noninteractive scripts do not enable
automatic progress simply because they were started in a terminal.

Only external/read-only operations may use `_trash_with_progress` internally:
its renderer owns a subshell, so it is not an abstraction for mutating the
caller's shell state. `Tool captureArgvJson:input:progress:` exposes labeled
captured processes through the DSL. The process result still carries the
child's exact exit status, stdout, and stderr.

## Browser data

Single-class queries resolve the source before querying the compiler. Global
queries use per-content records in `trash/.compiled/.symbolcache`. Source
hashes, the compiler fingerprint, and `symbols.jq` determine each cache entry.
Sources are hashed in a batch; cached records are validated in a batch before
anything is returned. Paths come from the current query, so renamed or deleted
sources cannot leave old candidates in the result. Damaged entries are rebuilt.

`Runtime dataForClass:` retrieves persisted candidates together, overlays live
session values, and applies the same lazy migration as `dataFor:` to unloaded
objects. Browser record construction stays in the DSL and emits JSON Lines
without allocating an object for every row.

## Repeating the measurements

```bash
make bash
bin/trash-bench 5 > /tmp/trashtalk-bench.jsonl
jq -s 'group_by(.case) | map({case:.[0].case,
  median_ms: (map(.microseconds) | sort | .[length / 2 | floor] / 1000)})' \
  /tmp/trashtalk-bench.jsonl
```

The benchmark requires Bash 5 for `EPOCHREALTIME`, uses an isolated database,
warms each operation, and exercises the public message surface. It includes
class dispatch, JSON context construction, process capture, scoped/global
symbols, ten object records, and a Codex dry-run when that CLI is installed.
It never makes a model request. `make bench` is the build-and-run convenience
target. Compare timings on the same machine without concurrent builds; these
are wall-clock measurements, not guarantees about every host.

### Measured locally, 2026-09-05

The earlier runtime and JSON-construction changes produced these warm medians
over five samples, before the read/traversal and build-cache work below:

| Public operation | Before | After |
| --- | ---: | ---: |
| Constant class send | 9–13 ms | 1.24 ms |
| Five-field Agent context | 307–637 ms | 4.01 ms |
| Capture `/usr/bin/true` | 265 ms | 23.05 ms |
| One class's symbols | 1.68–2.09 s | 64.11 ms |
| All symbols | 1.77–2.32 s | 85.83 ms |
| Ten instance records | 8.63 s with the truncation fixed | 203.48 ms |
| Codex dry-run preparation | 1.68–2.30 s | 35.30 ms |

The original browser returned only one object; its ten-row baseline required a
temporary split correction. All after samples exited successfully, and benchmark
stderr was empty. The recursive REPL filter search that separately took 5.664 s
was removed; this table does not measure interactive rendering or model latency.

Run runtime checks with `make test-serial` and compiler checks with
`bash lib/run-tests.sh lib/jq-compiler/tests --serial`. Regression tests check
operation counts and output semantics instead of fragile timing assertions.

## Incremental builds and verification

`make` and `make single CLASS=Counter` reuse validated compiled output when its
source, compiler, parent/trait dependencies, and output digest still match.
Receipts live in `.compiled/.buildcache/` (and the corresponding trait directory).
Parents and traits are built before dependents, including on a clean checkout.
Changing source while preserving its mtime still invalidates the receipt.
Invalid source or missing/cyclic dependencies fails the build; the previous
artifact remains installed. Output installation is atomic.

`make verify` builds and runs both runtime and compiler suites. `make test` runs
runtime tests; `make test-compiler` runs compiler tests. Both run in parallel with
a separate disposable checkout, database, compiler cache, and session per test.
Direct `bash tests/test_foo.bash` invocations use the same isolation. User
`.trashrc` is skipped in tests, and HOME is unchanged.

Use `TRASH_TEST_JOBS=4` to bound parallelism, `TRASH_TEST_TIMEOUT=180` to change
the per-file timeout, `TRASH_TEST_KEEP=1` to retain disposable checkouts for
debugging, and `TRASH_TEST_TRACE=1` for Bash traces. The runner's result summary
and exit status include failures and timeouts from either suite.

The [JSON read/traversal primitives](json-values.md) make extraction and iteration
process counts explicit. Diagnostic conversion uses one decoder and one
serializer. Array mapping with a constant class callback uses 13 jq processes
for both one and 25 elements, including runtime persistence; callback work
itself remains additional.

Two consecutive warm `make bash` runs on 2026-09-05 took 3.63 and 3.80 seconds,
with zero classes recompiled. The cache regression separately counts codegen
invocations and checks parent, trait, compiler, and damaged-output invalidation.

Comparing the methods from `b590370` with the new methods in the same isolated
runtime gave these medians over five alternating samples on 2026-09-05:

| Public operation | Previous method | New method |
| --- | ---: | ---: |
| Array map, 25 values, identity callback | 243.07 ms | 99.05 ms |
| Dictionary map, 25 values, identity callback | 607.31 ms | 111.41 ms |
| Browser records, ten Counter instances | 233.66 ms | 159.71 ms |

The comparison checked identical stored collection values and browser records.
The previous Dictionary map also printed intermediate values; its final stdout
line supplied the result ID for that check. The new method returns only the
result ID. These comparisons measure traversal and serialization; callbacks
that invoke processes or mutate persisted state still pay for that work.
