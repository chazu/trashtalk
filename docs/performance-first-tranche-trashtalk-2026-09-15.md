# Performance first tranche, Trashtalk portion (2026-09-15)

**Status:** implemented and measured. This report covers the Trashtalk items of
the [2026-09-14 audit](agent-performance-audit-2026-09-14.md) first tranche:
T1 (tokenizer), T4 (idle worker), T7 plus the low-risk T9 items (Tool request
decoding, instance creation), and T3 plus T11 (compiler-cache generations and
isolated test checkouts). Innards items (I1, I3) are out of scope here. Every
number below was measured on the audit host (Apple Silicon, macOS 14.6, Bash
5.3.15, jq 1.8.1, eight cores) with the shell locale `en_US.UTF-8` and
`LC_ALL` unset, on scratch copies of the checkout and disposable stores. "HEAD"
means commit `70037ab` before this work; "new" means the tranche commits.

## 1. Verified measurements

### T1: the locale hypothesis was false; the substring scan was the cost

The audit proposed pinning `LC_ALL=C` around the scan. The paired command from
the audit (HEAD tokenizer, same source, `jq -S .` digests compared) shows that
the locale is not the lever:

| Source | Bytes | HEAD, UTF-8 | HEAD, `LC_ALL=C` | New scanner | Tokens equal to HEAD (UTF-8) |
| --- | ---: | ---: | ---: | ---: | --- |
| Trash.trash | 51,687 | 27.95 s | 27.94 s | 1.17 s | yes |
| Inbox.trash | 26,736 | 5.68 s | 3.80 s | 0.36 s | yes |
| AgentSession.trash | 24,398 | 4.74 s | 3.31 s | 0.31 s | yes |
| AgentWorker.trash | 23,300 | 4.60 s | 3.17 s | 0.30 s | yes |
| WorkstationRouting.trash | 21,194 | 7.37 s | 4.56 s | 0.45 s | yes |
| Tool.trash | 17,803 | 2.57 s | 1.73 s | 0.25 s | yes |
| All 120 sources (`trash/**/*.trash` and compiler fixtures) | – | 99.8 s | 80.5 s | 17.2 s | 120 of 120 |

Wall time per file, one run each, on an otherwise idle host. Under
`LC_ALL=C` the HEAD scanner also produced *different* tokens for the seven
sources that contain non-ASCII text (Honker, MessagePresentation, Scheduler,
Stream, Trash, Kube/Cluster, Kube/Diff): `${input:i:1}` becomes byte-based, so
columns shift and a multibyte character splits into several `LITERAL` tokens.
The one-line locale pin would therefore have been both ineffective and a
behaviour change. Bash walks the whole string on every `${string:offset:1}`
expansion in either locale, so the scan was O(n²) regardless.

The fix that landed (already on disk from the stopped prior invocation, and
verified here) splits the source once into a character array with one Perl
pass and indexes that array; the scan runs under a function-local `LC_ALL=C`
so `[a-zA-Z_]` classes are ASCII in every environment. Decoded tokens,
including multibyte literals and character-based columns, match HEAD under
UTF-8 for every source. Two defects in the on-disk draft were fixed before
verification: the serializer's `"\u0000"` separator had been replaced by a raw
NUL byte (git showed the file as binary), and the scan's incidental loop
status made `Kube/Cluster.trash` (whose last line is a lone `}` at column 0)
fail to tokenize.

### Builds

Scratch copies of HEAD and of the working tree, rebuilt from physical paths
(see the caveat on symlinked `/tmp`), eight jobs, UTF-8 locale.

| Build | HEAD | New |
| --- | ---: | ---: |
| Cold `make`, 114 classes | 84.2 s | 39.9 s |
| `make` after editing Trash.trash | 27.6 s | 4.5 s |
| `make single CLASS=Counter` after an edit | 1.37 s | 1.63 s |
| Warm `make`, nothing dirty (three interleaved rounds) | 0.70–0.77 s | 0.80–0.93 s |

An earlier pair taken under heavier load read 58.0 s / 36.1 s cold and
23.3 s / 5.3 s after the Trash.trash edit. The warm build carries the prune
(about 25 ms measured alone, plus one jq read of the hash inventory); the rest
of the 0.1 s difference is host noise. The audit's T1 acceptance (Trash.trash
under 2 s to tokenize, `make` after editing it under 8 s) holds.

### Compiler cache footprint (live checkout)

| | Before | After |
| --- | ---: | ---: |
| `trash/.compiled` | 99 MB | 20 MB |
| AST entries | 1,038 (seven compiler generations) | 227 (114 current, 113 previous) |
| Symbol entries | 341 | 0 until the next browser query rebuilds them per class |
| Zero-byte `Class.XXXXXX` staging leftovers | 17 | 0 |

The previous generation is kept for a quick compiler revert; entries whose
source content is no longer current are dropped in both generations. Two
generations of 114 sources sit above the audit's 15 MB target; one generation
is about 10 MB.

### Worker

Empty disposable store, scratch copies with `TRASHDIR` pinned to each copy.

| | HEAD | New |
| --- | ---: | ---: |
| `bin/trash-worker --once` (includes ~100 ms runtime startup) | 210–261 ms | 207–220 ms |
| Warm tick in one long-lived shell (five samples) | 102–106 ms | 66–74 ms |
| External processes per idle tick | 8 (6 sqlite3, 2 perl) | 4 (3 sqlite3, 1 perl) |
| CPU of the whole worker tree over 60 idle seconds | 5.58 s (9.3% of a core, 60 ticks) | 0.97 s (1.6%, about ten ticks including startup) |
| `run/worker/stderr.log` on this host | 91.5 MB, never rotated | 262 KB after the first bounded start; the old contents kept once as `stderr.log.1` |

The remaining idle tick is `pending`, `tickableSessions`, the workstation
subscription query, and the lock's `flock` process. The per-tick DDL, the
`abs_path` Perl process, and the history-wide refresh are gone; backoff then
divides the remaining cost by the interval growth (1, 2, 4, 8, 8 … seconds).

### Public benchmark (`bin/trash-bench 5`, medians in ms)

Two rounds interleaved HEAD → new → HEAD → new so host drift shows up as
round-to-round movement rather than as a before/after difference.

| Case | HEAD round 1 | New round 1 | HEAD round 2 | New round 2 |
| --- | ---: | ---: | ---: | ---: |
| Capture `/usr/bin/true` (`process_capture`) | 26.9 | 18.6 | 35.0 | 17.5 |
| Ten instance records | 42.2 | 43.7 | 44.7 | 47.3 |
| Constant class send | 1.19 | 1.33 | 1.60 | 1.65 |
| Five-field Agent context | 3.59 | 4.44 | 4.54 | 4.44 |
| One class's symbols | 87.0 | 100.9 | 105.1 | 99.6 |
| All symbols | 133.3 | 156.1 | 159.3 | 152.7 |
| Codex dry run | 43.3 | 48.6 | 43.7 | 45.8 |

Capture is 31–50% faster: the decode went from seven jq processes to two, and
what remains is two `mktemp`, the child, `rm`, and the envelope jq, so the
audit's "at least 2×" expectation (which assumed the jq calls dominated) is
not reached. The other cases are unchanged within drift; instance creation
saves one `tr` process per object, which the ten-record case does not measure.

The agent worker end-to-end test (`tests/test_agent_worker.bash`, real
detached shell harnesses, run alone) went from 80.9 s to 59.6 s with all 77
checks passing in both.

### Test checkout setup

| | HEAD | New |
| --- | ---: | ---: |
| Standalone `test-isolated.bash` around `tests/test_env.bash` (three samples) | 534–603 ms | 409–481 ms |
| Same test cloned from the suite's prepared base | – | 244–254 ms |
| Base preparation, once per `make test` | – | 196 ms |

The audit's 1.5 s per test was measured inside its sandbox; on this host the
old setup cost about 0.5 s, of which the per-entry `cp` loop over the cache
and the per-test fingerprint were the larger parts. The suite path now meets
the 0.3 s target.

## 2. Exact changes

### T1 tokenizer (`lib/jq-compiler/tokenizer.bash`)

- `_split_chars` runs one Perl pass that groups UTF-8 sequences (lead byte plus
  continuation bytes) and passes every byte through unchanged; the scan reads
  `${chars[i]}` instead of `${input:i:1}`. Bash arrays are O(1) for the
  sequential access pattern the scanner uses; the one lookahead (`{1..50}`
  sequences) reads at most 256 characters.
- Character classes use `[[ == [a-zA-Z_] ]]` under a function-local
  `LC_ALL=C`; the serializer is locale-independent.
- `tokenize` returns 0 explicitly; only the character split can fail it.
- Perl is now a build dependency (it was already required for conversation log
  projection). `CLAUDE.md` records this.
- Coverage: `lib/jq-compiler/tests/test_tokenizer_locale.bash` tokenizes a
  multibyte fixture under `LC_ALL=C` and under a UTF-8 locale and requires the
  same decoded tokens, checks multibyte comment/string values, character-based
  columns, single-token multibyte literals, ASCII identifier classes, the
  column-0 status regression, and bounds the Trash.trash scan at 20 s (a
  quadratic scan needs more than that even idle).

### T7 and T9 (`trash/Tool.trash`, `lib/trash.bash`)

- `Tool runProcessRequestJson:` decodes the request with one `jq -j` pass that
  validates and emits NUL-separated fields: argv count, argv entries, working
  directory, capture flag, stdin mode, progress label, and inline stdin. The
  field count is exact because every other string is validated NUL-free; a
  rejected request emits nothing. Stdin that itself contains NUL keeps the
  separate `jq -jr '.stdin'` pipe so the child still receives it byte-for-byte.
  A progress label containing NUL is now rejected instead of being silently
  truncated. Error messages and the result envelope are unchanged.
- `_generate_instance_id` and `_to_instance_prefix` use `${name,,}` instead of
  `echo | tr`.
- `_create_instance` skips `_generate_accessor` for fields whose getter the
  compiled artifact already defines (`declare -F`), so only inherited fields
  are generated per creation.
- Coverage: `tests/test_tool_request_decode.bash` pins two jq processes per
  decoded capture (three through the public `captureArgvJson:`, which builds
  the typed request in one jq), inline and NUL-containing stdin, directory
  selection including a path ending in a newline, child status propagation,
  and the three rejection paths. `tests/test_instance_creation_cost.bash` pins
  zero `tr` processes per creation, the identifier prefixes, and that only
  inherited accessors are generated.

### T4 worker (`trash/AgentQueue.trash`, `trash/AgentWorker.trash`, `bin/trash-worker`, `lib/trash.bash`)

- `AgentQueue ensureSchema` is `pragma: direct` and memoized per process keyed
  by store path, the same shape as `AgentSession ensureSchema`. `AgentWorker
  tick` warms it outside the lock and resolves the lock path once per process
  (one Perl process per store instead of per tick).
- `AgentQueue refresh` evicts cached sessions, runs, deliveries, and
  outbox-committed messages (`_env_evict_prefix`, `_env_ids_prefix` in the
  runtime) instead of reloading every historical row into the session cache
  with one file write per row. Later reads load durable state on demand, so a
  tick costs what it touches. Message drafts still being published keep their
  cache entry, as before. Explicit `reload` calls and the guarded reads inside
  reconciliation are unchanged.
- `AgentWorker hasActiveRuns` is one indexed count the service uses to keep
  the base interval while a run is active.
- Making `ensureSchema` direct exposed a pre-existing dispatcher hazard: a
  `pragma: direct` send at call depth 0 ran `((_CALL_DEPTH++))`, which returns
  1 from 0 and ended any caller running under `set -e` (the same happened at
  HEAD with `@ AgentSession ensureSchema` as a script's first send). The depth
  bookkeeping is now plain assignment; `tests/test_direct_errexit.bash` pins it.
- `bin/trash-worker`: idle backoff from `TRASHTALK_WORKER_INTERVAL` (default 1
  s) doubling to `TRASHTALK_WORKER_MAX_INTERVAL` (default 8 s), held at the
  base interval while `hasActiveRuns` is true (tick stdout is not used as the
  signal because the workstation stage may print values); the first failure of a streak is
  logged once and after `TRASHTALK_WORKER_MAX_FAILURES` (default 5)
  consecutive failures the worker exits 1 so launchd's `ThrottleInterval` /
  systemd's `RestartSec` apply; `run/worker/stderr.log` is bounded at start
  when it is the process's stderr (`TRASHTALK_WORKER_LOG_MAX_BYTES`, default 1
  MiB: previous contents kept once as `stderr.log.1`, the newest quarter
  retained in place); the rebuild scan prunes `.astcache`, `.symbolcache`, and
  `.buildcache`. Foreground delivery (`deliverMessage:` → `tickSession:`) is
  untouched.
- Coverage: `tests/test_worker_backoff.bash` (delay sequence and cap, base
  interval with an active run, exit after three failures against a store that
  is a directory, log bounding only when the log is the worker's stderr).
  `tests/test_agent_worker.bash`, `test_agent_service.bash`,
  `test_agent_recovery.bash`, and `test_workstation_worker.bash` cover the
  unchanged delivery, replay, and recovery semantics.

### T3 and T11 (`lib/jq-compiler/build-cache.bash`, `lib/test-isolated.bash`, `lib/run-tests.sh`)

- `_build_prune_caches` runs at the end of every successful `compile-many`
  (including the "artifacts unchanged" path and `make single`). It keeps
  `.astcache` and `.symbolcache` entries for the current sources (the build's
  hash inventory) in the current compiler generation plus the most recently
  used previous generation, and removes `Name.XXXXXX` and `*.tmp` staging files
  older than ten minutes in the artifact, trait, receipt, and cache
  directories. A concurrent build's live staging file is younger than that and
  is kept; a pruned entry only costs a re-parse. Pruning never fails a build.
- `lib/test-isolated.bash` factors the checkout into `prepare_checkout`
  (tar plus one `cp` of the current-generation caches) and `clone_checkout`
  (`cp -c` on macOS, `cp --reflink=auto` elsewhere). `run-tests.sh` computes
  the compiler fingerprint once, prepares one base checkout per run, and
  exports `TRASH_TEST_BASE`; each test clones it. Standalone invocations still
  prepare their own checkout. Every test still gets a private copy of `lib`,
  `bin`, `trash`, and the caches, so isolation is unchanged.
- Coverage: `lib/jq-compiler/tests/test_cache_prune.bash` (generation
  retention across three compiler fingerprints, symbol-cache pruning, stale
  and fresh staging files, unrelated files, the warm-build sweep, and pruning
  of superseded source content). The existing `test_build_cache.bash` and
  `test_symbol_cache.bash` pass unchanged.

## 3. Test results

Both suites were run with `TRASH_TEST_TIMEOUT=300` and eight jobs on the
loaded host described above; "HEAD" ran in the scratch copy, "new" in the
live checkout after the final commit's contents were in place.

| Suite | HEAD | New |
| --- | --- | --- |
| `make test` (runtime) | 377 s, 86 passed, 3 failed | 347 s, 90 passed, 3 failed |
| `make test-compiler` | 96 s, 48 passed | 82 s, 50 passed |

The three runtime failures are the same in both: `test_agent_termination`
("termination can be retried and uses durable PID file"), `test_jcode_driver`
("native tool survives adapter connection loss"), and `test_maki_driver`
("common stop interrupts active Maki"). They fail identically when run alone
against HEAD and against the tranche, all in the harness stop/interrupt paths
that this work does not touch, so they are pre-existing on this host and are
listed as remaining failures rather than regressions. The four extra tests in
the new runtime count are the tranche's own (`test_tool_request_decode`,
`test_instance_creation_cost`, `test_worker_backoff`, `test_direct_errexit`);
the two extra compiler tests are `test_tokenizer_locale` and
`test_cache_prune`.

Suite wall time fell 9%, not the 30% the audit hoped for: on this host the
per-file setup was 0.5 s rather than 1.5 s, and the runtime suite is dominated
by tests that wait on real detached harness processes. An earlier full run
during the heaviest external load showed four load-induced timeouts at the
default 120 s limit in the HEAD copy and one flaky failure in
`test_tool_detach` in the live checkout; both disappeared with the longer
timeout and a quieter host.

Targeted runs during development (all passing at the end): the five tokenizer
and build-cache compiler tests; `test_agent_worker`, `test_agent_service`,
`test_agent_recovery`, `test_workstation_worker`, `test_agent_snapshots`,
`test_assignment_transaction`, `test_process_lock`, `test_process`,
`test_future`, `test_innards_edit`, `test_tool_detach`, `test_env`, and the
new tests. Two regressions were caught and fixed by these runs before the
final suite: the first `refresh` rewrite used a glob and `+=` that the raw
method emitter splits (the eviction became a no-op, leaving stale cached
deliveries), and the first accessor-skip rule keyed on the compiled getter,
which removed the unary setters that `Future for:` and `File` still send.

## 4. Caveats

- The host was not idle during these measurements: another project's Rust
  test build ran at 100% of several cores for long stretches, the owner's
  conversation view and worker service were attached to the live store, and a
  few orphaned shell-driver fixtures from earlier `test_agent_termination`
  runs (`while :; do sleep 0.1; done` loops whose checkouts are gone) were
  still spawning processes. Before/after pairs were run back to back, so
  ratios are more trustworthy than absolute times; the benchmark rounds were
  interleaved for that reason.
- Builds run from a *logical* path under a symlinked directory (macOS `/tmp`
  → `/private/tmp`) name every artifact by its full path
  (`__tmp__tranche__new__trash__Trash`) because the build planner compares
  make's physical `CURDIR` with Bash's logical `pwd`. This predates the
  tranche and does not affect `~/.trashtalk`; the scratch copies were rebuilt
  from their physical paths. Worth a follow-up normalisation in
  `_build_output_for`.
- The tokenizer needs Perl on PATH; it fails with a clear message otherwise.
  Invalid UTF-8 input is grouped per lead byte, which can differ from Bash's
  per-byte treatment of malformed sequences in column numbers only; all
  repository sources are valid UTF-8 and tokenized identically.
- Backoff raises the latency for noticing a finished run and for routing
  outbox rows written by non-ticking senders to at most
  `TRASHTALK_WORKER_MAX_INTERVAL` (8 s). Ticks stay at the base interval while
  a run is active, so chained deliveries after a run exit are not delayed.
  Rebuild detection also waits for the next beat, so a running service may
  take up to 8 s to re-execute after `make`.
- The failure exit assumes a supervisor. A worker started by hand in a
  terminal simply exits after five consecutive failed ticks and prints why.
- Log bounding relies on the inode of the worker's stderr matching
  `run/worker/stderr.log` (`stat` on `/dev/fd/N`); when they differ nothing is
  touched. The first bounding on this host copied the 91.5 MB historical log to
  `run/worker/stderr.log.1`; delete that file when it is no longer wanted.
- Cache pruning keeps at most two generations, and only entries for the
  current sources. Reverting a source edit or parsing a file outside `trash/`
  after a build costs one re-parse (about 0.3 s for a 25 KB class now).
- `AgentQueue ensureSchema` skips its DDL for the rest of the process once it
  has run for a store path. A process that deletes and recreates its store
  file without changing `SQLITE_JSON_DB` would need to unset
  `_AGENT_QUEUE_SCHEMA_DB`; the same already applies to `AgentSession
  ensureSchema`, and no test or runtime path does this.
- `cp -c` needs APFS for clone semantics; on other filesystems it copies,
  which is still one process for the whole tree.

## 5. Follow-up work

- T2: batch parsing and code generation across dirty classes in one jq
  process per stage, and run `build-metadata` per frontier level in one
  driver process. With T1 done, the cold build is bounded by jq program
  compilation and process startup.
- T5/I2: the conversation bridge probe and incremental frames, as the audit's
  second tranche. Unchanged here.
- Symbol queries (T8) could reuse the per-run compiler fingerprint the test
  runner now computes once (`TRASH_TEST_FINGERPRINT` is a test-only export).
- The remaining tokenizer cost for small files is process startup plus one jq
  serializer; a linear scanner in one Perl or jq pass would remove the Bash
  loop entirely if the compiler front end ever becomes the bottleneck again.
- `test_agent_worker.bash` remains the slowest runtime test; it waits on real
  detached harness processes, not on the worker.
