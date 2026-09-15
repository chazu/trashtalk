# Agent performance audit, 2026-09-14

**Status:** read-only investigation and recommendations across Trashtalk, Whisker, and Innards. Nothing was implemented. Every number below is labelled **measured** (observed in this session), **inferred** (derived from reading code and existing documentation), or **hypothesis** (needs profiling before it is acted on).

Scope: what makes ordinary local development and interactive use slower than it needs to be. Model latency, network, and remote hosts are out of scope.

## 1. Executive summary

Trashtalk is the project with the most recoverable latency, and most of it comes from three sources that reinforce each other: a compiler front end whose cost grows with the square of the source size, a runtime whose unit of cost is a spawned process, and three polling loops (worker, conversation bridge, Innards applets) that keep spawning processes and redrawing while nothing is happening. Whisker is already fast for its purpose; its remaining risk is that per-prompt collectors have no timeout. Innards is fast enough per keystroke but burns CPU while idle and re-does whole-window work on every snapshot the bridge sends.

The single largest measured item is the Bash tokenizer: `trash/Trash.trash` alone takes about 24 s of CPU to tokenize and the cost scales quadratically with file size, so a cold `make` has a wall-clock floor of roughly 25 s and editing a large class costs many seconds before any test can run. The evidence points at a locale-dependent Bash substring cost with a one-line verification and a one-line fix.

### Ranked opportunities across all three projects

| Rank | Item | Project | User-facing cost today | Expected gain | Size | Confidence |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | T1 Tokenizer scales quadratically with file size | Trashtalk | 5–24 s CPU per large class per build; cold `make` floor ~25 s | 5–20× faster tokenizing; cold build bounded by jq instead | XS to verify, S to fix | High that it is quadratic (measured); medium-high on the locale cause |
| 2 | T4 Worker tick: process churn every second, per-tick work scales with history, no backoff, 91 MB error log | Trashtalk | Continuous background CPU on the dev machine; failure loops run unthrottled | Idle worker near 0% CPU; bounded logs | S | Medium-high |
| 3 | T5 + I2 Conversation view: 1 Hz probe spawns ~8 processes; on change the whole window is re-projected, re-serialized, and re-wrapped | Trashtalk + Innards | 8–15% of a core idle per open view (est.); 40–100% while an agent streams; stalls at large windows | Idle view near 0; streaming updates O(delta) | M | Medium |
| 4 | T7 `Tool captureArgvJson:` spends 6–7 jq processes per capture | Trashtalk | 23–79 ms per external tool call; every Tool wrapper and the inbox preview hook pays it | 2–3× per capture | S | High |
| 5 | T11 + T3 Test and build feedback overhead: 1.5 s per test file for checkout setup, 99 MB of stale compiler caches, multi-second per-build coordinator floor | Trashtalk | `make test` spends minutes of CPU on setup; `make single` has a seconds-long floor | Setup per test < 0.3 s; caches < 15 MB; measurable `make single` floor | S–M | High (measured) |
| 6 | T6 Store transaction fixed cost (staging database per transaction) | Trashtalk | `@@`, session lookup, delivery paths cost 0.2–1.4 s | 30–50% on those paths | M | Medium |
| 7 | T8 Symbol queries and TAB completion re-run the compiler driver per class and grep every artifact | Trashtalk | 60–300 ms per symbol query; completion of a receiver with a deep hierarchy costs several queries | Completion < 100 ms warm | S | Medium-high |
| 8 | I1 + I3 Innards applets redraw unconditionally at 12.5–25 Hz and re-read the preview file per frame | Innards | Constant idle CPU per open applet; preview churn | Idle 0% | S | High (inspection) |
| 9 | I4 `inmacs`/`inpage` rebuild the syntect syntax set at every launch | Innards | Suspected 50–200 ms startup | Startup < 20 ms | S | Hypothesis |
| 10 | W1 + W2 Whisker collectors have no timeout; the kubernetes segment costs ~29 ms per prompt | Whisker | Prompt stalls in large repositories or with slow kubeconfig; 29 ms on every prompt in the ops view | Bounded prompt latency; ops view ~0 ms | S | Medium (documented numbers) |
| 11 | T13 Inbox preview hook starts a full Trashtalk runtime per hovered message | Trashtalk + Innards | 100–300 ms per hover on an unread message (est.) | Hover cost near 0 | M | Medium |
| 12 | T9 Instance creation micro-costs (`tr` per id, accessor regeneration, jq per field write) | Trashtalk | Small per object; adds up in loops | 10–30% on object-heavy paths | S | High |

## 2. Method, environment, and caveats

**Host and tools (measured):** Apple Silicon macOS 14.6, Bash 5.3.15, jq 1.8.1, Homebrew SQLite selected by the runtime, Python 3.14, Perl 5. Shell locale `en_US.UTF-8` with `LC_ALL` unset. Eight cores according to the project's own 12 September report.

**What ran:** the project's own benchmark scripts (`bin/trash-bench 5`, `bin/trash-bench-interactions 3 browsers|conversations`), the tokenizer on five sources, one isolated runtime test, one isolated compiler test that performs many builds, and `bash -x` traces to count spawned processes. All Trashtalk runs used disposable databases created by those scripts. No file in any repository was changed except this document.

**Two caveats that affect every absolute number:**

1. The session ran inside a sandboxed tool shell that makes process spawning slower, and the owner's own builds and the worker service were active at the same time (compiler caches, receipts, the store, and the worker lock were all modified between 20:01 and 20:08 during the run). Every interaction benchmark case came out **2.3–3.6× slower than the 12 September report, uniformly across cases**, so I treat that as an environment factor and rely on rankings and ratios rather than absolute milliseconds. The tokenizer numbers are CPU user time inside one Bash process and are much less affected.
2. Several diagnostics were blocked by the sandbox and are listed in Appendix B. In particular the Whisker and Innards binaries could not be executed, so those sections rest on code reading and the projects' own documented measurements.

### Measured runtime benchmark (`bin/trash-bench 5`, medians)

| Case | This session | 5 Sep report | Ratio |
| --- | ---: | ---: | ---: |
| Constant class send | 3.4 ms | 1.24 ms | 2.7× |
| Five-field Agent context | 8.5 ms | 4.0 ms | 2.1× |
| Capture `/usr/bin/true` | 79.1 ms | 23.1 ms | 3.4× |
| One class's symbols | 211.8 ms | 64.1 ms | 3.3× |
| All symbols | 297.7 ms | 85.8 ms | 3.5× |
| Ten instance records | 219.5 ms (95–316) | 203.5 ms | 1.1× |
| Codex dry run | 121.0 ms | 35.3 ms | 3.4× |

### Measured interaction benchmark (`bin/trash-bench-interactions 3`, medians)

| Case | This session | 12 Sep "after" | Ratio |
| --- | ---: | ---: | ---: |
| Inspect session admission | 184 ms | 61.5 ms | 3.0× |
| Live session admission | 336 ms | 124.2 ms | 2.7× |
| Resolve current session | 554 ms | 190.5 ms | 2.9× |
| Deliver human inbox mail | 586 ms | 191.2 ms | 3.1× |
| Deliver session inbox mail | 1,369 ms | 477.6 ms | 2.9× |
| Prepare 1 / 10 / 50 / 200 inbox rows | 324 / 469 / 967 / 2,982 ms | 100 / 143 / 331 / 1,076 ms | 2.8–3.3× |
| Prepare 10 / 100 object rows | 104 / 325 ms | 35 / 91 ms | 3.0–3.6× |
| Prepare 10 / 50 session rows | 85 / 99 ms | 31 / 43 ms | 2.3–2.7× |
| Refresh focus frame, 50,000 chunks | 1,074 ms | 336 ms | 3.2× |
| Open empty conversation (fake UI) | 1,294 ms | 373 ms | 3.5× |
| First direct input (one cold sample) | 4,694 ms | 1,429 ms | 3.3× |
| Direct input to busy fixture | 1,895 ms | 620 ms | 3.1× |
| Send through conversation UI handler | 2,046 ms | 639 ms | 3.2× |

The conversations group finished all cases but exited 1 during teardown with `Stop remains unconfirmed; session paused. Retry stop on this run.` from the fake fixture. That is a harness flake worth fixing so `make bench-interactions` can be trusted unattended.

### Measured tokenizer cost (`lib/jq-compiler/tokenizer.bash`, UTF-8 locale)

| Source | Bytes | Lines | Wall | CPU (user) | CPU ÷ bytes² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Counter.trash | 977 | 57 | 0.10 s | 0.06 s | – |
| Tool.trash | 17,803 | 494 | 5.4 s | 3.41 s | 1.08e-8 |
| AgentSession.trash | 24,398 | 538 | 5.3 s | 5.25 s | 0.88e-8 |
| Inbox.trash | 26,736 | 607 | 15.3 s (contended) | 6.86 s | 0.96e-8 |
| Trash.trash | 51,687 | 1,464 | 27.4 s | 24.23 s | 0.91e-8 |

CPU divided by bytes squared is constant within 20% across four files spanning a 3× size range: the scan is quadratic in input size. The 12 September report measured AgentSession.trash at 2.7 s on an idle host, which is consistent with the same shape at half the load.

### Measured process counts (top-level shell only)

A `bash -x` trace of `bin/trash-bench 1` (setup, ten `Counter new`, and two runs of each of seven cases) showed these external commands started by the runtime shell and its subshells: jq 57, sqlite3 14, uuidgen 10, tr 10, cat 10, mktemp 5, rm 4, dirname 3, find 2, mkdir 1. Child `driver.bash` processes (symbol queries) are not traced by this method, so the symbol cases are undercounted.

### Measured build, test, cache, and log facts

- One small isolated runtime test (`tests/test_env.bash`, 21 assertions): 1.73 s wall, 0.37 s user, 0.82 s sys. The disposable-checkout setup dominates.
- `lib/jq-compiler/tests/test_build_cache.bash` (about seventeen build invocations over one- to five-class graphs): 63.4 s wall, 33.3 s user, 16.5 s sys. Roughly 3.5 s per build invocation in this environment, with half the time in the kernel (process creation).
- `trash/.compiled` is 99 MB, of which the AST cache is 96 MB (1,034 entries; the three largest are 744 KB each and are all Trash.trash ASTs for different content hashes under the current compiler fingerprint), the symbol cache 1.9 MB (340 entries), and receipts 440 KB (114 receipts for 115 sources in the Makefile list). The compiled artifacts are the remaining ~1 MB. Entries accumulate per edit and per compiler change; there are also 17 zero-byte staging leftovers named `Class.XXXXXX` dating from 11–14 September.
- `run/` is 132 MB. `run/worker/stderr.log` is 91.5 MB and 934,245 lines: 888,637 lines of `.compiled/Store: line 262: _st…` (a helper missing from a stale worker), 45,347 of the same at line 250, 122 `AgentWorker tick failed; will retry`, 62 `Unknown class 'AgentWorker'`, and 33 `runtime rebuilt; worker restarting` (the last twelve lines are all restarts). `stdout.log` is empty. The stale-worker fix (`216e3b3`, today) stops the root cause; the log is never truncated or rotated.
- The live store is 768 KB with a `class` virtual column and the indexes created by `AgentQueue ensureSchema`.

## 3. Trashtalk findings

### T1. The tokenizer is quadratic in source size

**Evidence (measured):** table above. `tokenizer.bash` walks the input one character at a time with `${input:i:1}` (58 such sites, lines 80–430 and onward) and appends tokens to a Bash array; nothing else in the loop depends on `i`, so the per-character cost is what grows.

**Mechanism (hypothesis, strong):** in a multibyte locale Bash resolves `${var:offset:length}` by walking characters from the start of the string, so each read costs O(offset) and the scan costs O(n²). Under `LC_ALL=C` the same expansion is a direct byte index. The runtime benchmarks and the test runner export `LC_ALL=C`, but `make bash`, `driver.bash`, and `tokenizer.bash` do not (verified by grep), so interactive builds run in the user's UTF-8 locale. The 12 September report's tokenizer numbers were probably taken under UTF-8 as well, since 2.7 s for a 24 KB file is far above a linear scan.

**Proposed change:** at the top of `lib/jq-compiler/tokenizer.bash`, set `LC_ALL=C` for the scanning loop (locally, then restore before serialization if column semantics for non-ASCII text must stay character-based; the serializer already passes bytes to jq unchanged). The compiler fingerprint includes the tokenizer, so every AST cache entry is rebuilt once. If the locale pin is not enough, the second step is a linear scanner (a single Perl or jq pass over the source; Perl is already a required boundary tool).

**Expected impact:** verification predicts AgentSession.trash from 5.3 s to well under 1 s and Trash.trash from 24 s to about 1–2 s of CPU. Cold `make` drops from a ~25 s wall-clock floor to whatever jq parsing and codegen cost (see T2). Confidence: high that the scan is quadratic; medium-high that the locale is the lever. Size: XS to verify, S to land with the regression below. Risk: column numbers in error messages become byte offsets for lines containing non-ASCII characters; `[a-zA-Z_]` classes become strictly ASCII (more deterministic than today). The compiler suite has 47 files and a paired tokenizer comparison already exists in `docs/performance-2026-09-12.md`.

**Measure:** `time LC_ALL=C bash lib/jq-compiler/tokenizer.bash trash/Trash.trash > /dev/null` against the same command without the prefix; decoded token arrays must be identical (`jq -S .` on both). Acceptance: under 2 s for Trash.trash, and `make` after touching Trash.trash under 8 s wall.

**Dependencies:** none. Do this first; it changes every other build measurement.

### T2. Per-class build cost is bounded by process startup and jq program compilation

**Evidence (measured/inferred):** the build-cache test spends about 3.5 s per build invocation on tiny classes, half in the kernel. Each class compile runs `bash driver.bash` (which re-hashes 250 KB of compiler sources for the fingerprint), the tokenizer, `jq -f parser.jq` (34 KB program), `jq -f codegen.jq` (157 KB program), plus three to four small jq calls for metadata and inherited fields (`driver.bash` lines 312–643). jq compiles its program text on every invocation; a 157 KB program is not free (measurement was blocked, see Appendix B). The coordinator (`build-cache.bash`) adds one shasum batch, three to four jq planning calls, and one `bash driver.bash build-metadata` process per frontier node.

**Proposed change (in order of value):** (a) batch codegen and parsing across all dirty classes in one jq process per stage using `inputs` and JSON Lines, instead of one jq per class per stage; (b) run `build-metadata` for a whole frontier level in one driver process (`parse-many` already exists for the browser); (c) cache the compiler fingerprint in a file keyed by the compiler files' size and mtime so child processes stop re-hashing 250 KB.

**Expected impact:** cold build after T1 lands: a further 2–4× on the jq portion; `make single CLASS=X` floor from seconds to well under one second. Confidence: medium (the exact jq compile cost is unmeasured). Size: M for (a), S for (b) and (c). Risk: (a) changes error attribution per class; keep per-class error envelopes in the JSON Lines stream. **Measure:** `time make single CLASS=Counter` after changing one comment in Counter.trash; count processes with the wrapper harness in Appendix A. **Dependencies:** after T1.

### T3. Compiler caches grow without bound and are traversed every second

**Evidence (measured):** 1,034 AST entries for 115 sources means roughly nine retained compiler generations; `.compiled` is 99 MB. `driver.bash` writes a new content-plus-fingerprint entry and never removes old ones. `bin/trash-worker` runs `find trash/.compiled lib -type f -not -path '*/.astcache/*' … -newer stamp` every second; `-not -path` filters output but still descends and stats the ~1,500 cache files. `lib/test-isolated.bash` globs the AST and symbol caches for every test file.

**Proposed change:** at the end of a successful `compile-many`, delete cache entries whose fingerprint suffix is not the current one (keep at most the previous generation for a quick revert); use `-prune` on the cache directories in the worker's `rebuilt` check, or compare the mtime of `trash/.compiled/.buildcache` and `lib` directories instead of walking files. Remove zero-byte `Class.XXXXXX` staging files during the same sweep.

**Expected impact:** disk from 99 MB to roughly 10–20 MB (one AST generation is about 10 MB); the worker's per-second scan from ~1,500 stats to a few dozen; faster per-test cache copies. Confidence: high. Size: S. Risk: pruning during a concurrent build; do it under the same coordinator run, after receipts are written. **Measure:** `du -sh trash/.compiled`; `ls trash/.compiled/.astcache | wc -l` equals the number of sources plus at most one older generation.

### T4. The worker tick spawns many processes per second, scales with history, and never backs off

**Evidence (inferred from code; log measured):** `bin/trash-worker` loops every second: `find` for rebuild detection, then `AgentWorker tick` (`trash/AgentWorker.trash` lines 108–118 and 266–275). An idle tick with an empty store performs: `lockPath` (a `perl` process), `AgentSession ensureSchema`, `Process withLock:` (a second `perl` for `flock`), `AgentQueue refresh` (which calls `AgentQueue ensureSchema`, a DDL round trip with five `CREATE … IF NOT EXISTS` statements, then loads **every** AgentSession, AgentRun, AgentDelivery, and outbox Message row into the session cache with one file write per row), `routePending` (`ensureSchema` again plus a query), `tickableSessions` (a query), and `WorkstationWorker tick` (`AgentFocus human` plus a query). That is roughly 9–12 external processes and 15–20 message dispatches (each one or two subshell forks) per second while nothing is happening, and the `refresh` step grows with the total number of runs and deliveries ever created (there are 85 run directories today). `AgentQueue ensureSchema` runs its own DDL on every `pending`, `persist`, `refresh`, `assign`, and `questionFor` call. `AgentSession ensureSchema` and `WorkstationWorker` do keep per-process memos, but `work:` executes inside the lock's subshell, so any memo set there is discarded when the tick ends (the code comment in `tick` notes this for the session schema and warms it outside the lock; the queue schema gets no such treatment). The 91 MB log shows what happens when a tick fails: the loop retries at full speed with no backoff and launchd never sees a failure because the process never exits.

**Proposed change:** (a) memoize `AgentQueue ensureSchema` per process the way `WorkstationWorker` does; (b) restrict `refresh` to rows for tickable sessions and their active runs, or replace it with a `SELECT count(*), max(rowid)` change probe and reload only when it moves; (c) add idle backoff in `bin/trash-worker`: after a tick that dispatched nothing, sleep 1, 2, 4 … up to a cap (5–10 s), resetting to 1 s whenever a tick did work or a foreground `tickSession:` ran (foreground delivery already ticks immediately, so message latency is unaffected); (d) on N consecutive failed ticks, exit non-zero so launchd's `ThrottleInterval` (5 s) applies and log once, not per tick; (e) truncate or rotate `run/worker/stderr.log` at service start.

**Expected impact:** idle worker from an estimated 10–25% of one core to near zero; bounded logs; per-tick cost independent of history. Confidence: medium-high on structure, the CPU estimate is a hypothesis (measurement blocked, see Appendix A for the command). Size: S. Risk: backoff raises dispatch latency for messages delivered by a process that does not tick in the foreground (for example a remote sender); cap the backoff at a few seconds and keep `TRASHTALK_WORKER_INTERVAL` as the floor. **Measure:** `time SQLITE_JSON_DB=/tmp/empty.db bin/trash-worker --once` minus runtime startup; `ps -o %cpu,cputime -p <worker pid>` after ten idle minutes; line count of `stderr.log` after a day. **Dependencies:** independent; pairs well with T3's `-prune`.

### T5. The conversation bridge polls at 1 Hz with ~8 processes and re-projects the whole window on change

**Evidence (inferred from `lib/tool-duplex.bash` lines 37–72 and `trash/AgentFocus.trash` lines 64–85; costs measured by the interaction benchmark):** every second without user input the bridge sends `AgentFocus changeTokenFor:`, which runs one jq, three to four sqlite3 queries, `AgentDriver runDirFor:`, a Perl stat pass over every file of every run of the session, `shasum`, and `cut`, wrapped in the usual dispatch subshells. When the digest changes (every second while an agent streams, because run logs grow) it runs `frameFor:`: guarded admission (184 ms here, 61 ms in the report) plus the transcript projection (1,074 ms here, 336 ms in the report, for the 50,000-chunk fixture with a warm cache), then serializes the entire 400-entry window to `inagent`, which rebuilds every row (see I2). `load_older` doubles the window up to 100,000 entries, so both the projection and the applet's rebuild scale with history on every refresh.

**Proposed change:** (a) replace the six-process digest with one probe: a single sqlite3 call that returns `PRAGMA data_version`-style change evidence is not available across separate processes (each `_db_sql` opens a fresh connection), so use one query returning `max(rowid)` and `count(*)` for the relevant classes plus a single `stat` of the run log sizes, all in one sqlite3 plus one Perl; (b) make the snapshot protocol incremental: the bridge keeps the last frame and sends `{"type":"delta","append":[…],"replace":[…],"generation":n}` when only new entries arrived, falling back to a full snapshot on rebuild; the transcript cache already knows which records were appended (`lib/transcript-files.pl` returns only new lines); (c) longer term, a persistent SQLite connection (a `coproc sqlite3` owned by the bridge or worker) makes `PRAGMA data_version` usable and removes a process per query.

**Expected impact:** idle view from ~8 processes per second to 2; streaming updates proportional to new text instead of window size; no stalls after `load_older`. Confidence: medium. Size: M (protocol change spans both projects; keep schema version 1 snapshots as the fallback). Risk: delta application bugs in the applet; mitigate with a generation counter and full-snapshot resync on mismatch. **Measure:** per-second process count of an idle attached view (Appendix A harness); time from a log append to the applet redraw; `focus_frame_50000` after `load_older` to 100,000. **Dependencies:** I2 in Innards; independent of T4.

### T6. Every Store transaction builds a staging database

**Evidence (inferred from `lib/store-transaction.bash` lines 133–177):** `_store_transaction_once` creates a temp directory, runs a five-table DDL in a fresh sqlite3, executes the callback in a subshell where each tracked read costs two more sqlite3 processes (existence check, then attach-and-import), then commits with three sqlite3 processes and runs callbacks. Fourteen `.trash` files use `Store transaction:` or `WorkstationSchema transact:`; `Gusgus currentFor:` (554 ms here, 190 ms in the report) is a single read-only transaction, and a session delivery (`send_session`, 1,369 ms here, 478 ms in the report) performs two transactions plus about a dozen queries.

**Proposed change:** (a) copy a pre-built empty `work.db` template (created once per runtime start, or shipped as a fixture) instead of running DDL per transaction; (b) merge the existence check and import into one sqlite3 invocation; (c) route read-only decisions (`currentFor:`, dry-run routing status) through `validateSnapshot:` the way admission already does, which avoids the staging database entirely; (d) cache `AgentFocus human` per process.

**Expected impact:** 30–50% on transaction-heavy paths (`@@`, delegation, delivery). Confidence: medium. Size: M. Risk: the conflict guards and replay semantics are subtle and well tested (`tests/test_store_snapshot.bash`, `test_assignment_transaction.bash`); keep the SQL identical and change only process boundaries. **Measure:** `current_session`, `send_session`, and `access_live` benchmark cases; process count per transaction.

### T7. `Tool captureArgvJson:` runs six to seven jq processes per capture

**Evidence (inferred from `trash/Tool.trash` lines 189–252; measured 79 ms per capture of `/usr/bin/true` here, 23 ms in the report):** `runProcessRequestJson:` validates the request with jq, decodes argv with jq, decodes `working_directory` with jq, tests `.capture` with jq, tests `.stdin` with jq, decodes `.progress` with jq, and builds the result envelope with jq. Every Tool wrapper (`Tools::Inpick`, `Jq`, `Cue`, `Mise`, the code tools) and the inbox preview hook go through this path.

**Proposed change:** one jq invocation that validates and emits every field NUL-separated (argv entries, working directory, capture flag, stdin presence, progress label) read by a single `mapfile -d ''`; keep the final envelope jq. The request is built by the DSL from typed values, so validation and decoding can share a pass safely.

**Expected impact:** from seven jq processes to two per capture; 2–3× on the `process_capture` case and on every wrapper that forwards to it. Confidence: high. Size: S. Risk: low; the request schema is closed and tested. **Measure:** `bin/trash-bench 5` `process_capture` median; jq count per capture from the trace harness. **Dependencies:** none.

### T8. Symbol queries and completion re-run the compiler driver and grep every artifact

**Evidence (measured 212 ms per class-symbol query and 298 ms for all symbols here; 64 and 86 ms in the report; code in `trash/Trash.trash` lines 1044–1062 and 182–195, `lib/trash-completion.bash`):** each `symbolRecordsForClass:` starts a `driver.bash` child that re-hashes the 250 KB compiler fingerprint, hashes the source, validates the cache with jq, and then the caller filters with another jq. TAB completion (`_trash_selectors_recursive`) repeats this for every class in the receiver's inheritance chain, and `listObjects` runs `find` plus one `grep` process per compiled artifact (128 today) to exclude traits on every receiver completion.

**Proposed change:** (a) memoize symbol records per shell session keyed by the artifact directory's mtime and the compiler fingerprint, exported for subshells; (b) compute the compiler fingerprint once and pass it to the driver via environment (it already honours `_COMPILER_VERSION` inside one process); (c) replace the per-file grep in `listObjects` with a single `grep -L` over the directory, or read the class list from the receipts. **Expected impact:** completion under 100 ms warm; symbol queries 3–5× faster. Confidence: medium-high. Size: S. Risk: stale completion after a rebuild if the mtime key is wrong; include `.buildcache` mtime. **Measure:** time `@ Trash symbolRecordsForClass: Counter` twice in one shell; time a TAB completion on `@ AgentSession `.

### T9. Instance creation and field writes pay avoidable processes

**Evidence (measured in the trace: 10 `uuidgen`, 10 `tr`, 10 `cat` for ten `Counter new`; code in `lib/trash.bash` lines 232–236, 1090–1123, 1466–1500):** `_generate_instance_id` lowercases the class name with `echo | tr` (a fork and an exec per object); `_create_instance` re-evaluates `_generate_accessor` for every inherited field on every creation; `_ivar_set` runs one jq per field assignment inside DSL methods, so a method that assigns six fields spawns six jq processes and rewrites the cache file six times.

**Proposed change:** use `${name,,}` for the prefix; skip accessor generation when `declare -F` already finds the getter for that class; keep jq per write (the documented contract in `docs/performance.md` says writes are not deferred to method exit), but consider a compiler-level batch for consecutive literal field assignments through the existing `Runtime assign:to:` path. **Expected impact:** 10–30% on object-heavy paths; small elsewhere. Confidence: high for the first two items. Size: S. **Measure:** `ten_instances` and a loop of `Counter new` with the trace harness.

### T10. Runtime startup cost is paid by every helper process

**Evidence (inferred; measurement blocked):** sourcing `lib/trash.bash` loads four vendor libraries, runs `db_init` (a sqlite3 process) and `_honker_detect` on every start, and `initialize_trash` creates an alias per source file (85). `bin/trash-send`, `bin/trash-receipt` (run by `trash-command` after every wrapped command), the inbox preview hook (once per hovered message), and each `_store_transaction_once` subshell all pay it in full or in part.

**Proposed change:** measure first (`hyperfine 'bin/trash-send Counter description'` with a temp store). Likely wins: make `db_init` conditional on the database file being absent; drop the alias loop (the completion file already covers the interactive case); avoid re-sourcing vendor libraries inside subshells. **Expected impact:** hypothesis of 100–300 ms saved per helper start. Confidence: low until measured. Size: S. **Dependencies:** none.

### T11. Each test file pays about 1.5 s of disposable-checkout setup

**Evidence (measured 1.73 s for a 21-assertion test with 0.82 s in the kernel; code in `lib/test-isolated.bash`):** every test file tars and untars `lib trash tests bin axe schemas Makefile`, runs `driver.bash fingerprint` (cat plus shasum of 250 KB), and copies the current-generation cache entries out of the 1,034-entry directory. With 90 runtime and 53 compiler files that is minutes of CPU per `make verify` spent before any test logic, and on eight jobs it sets a wall-clock floor near 30 s.

**Proposed change:** in `lib/run-tests.sh`, build one read-only base checkout per run and give each test an APFS clone (`cp -c -R`), which is near-instant; compute the fingerprint once and export it; symlink `lib` and `bin` when a test does not mutate them (most do not). Keep `TRASH_TEST_KEEP` semantics. **Expected impact:** per-test setup from ~1.5 s to under 0.3 s; `make test` wall time down by a third or more. Confidence: high. Size: S–M. Risk: tests that mutate sources or compiled output must still get private copies; the clone gives them copy-on-write files, so behaviour is unchanged. **Measure:** wrap `test-isolated.bash` with `EPOCHREALTIME` timestamps and compare the distribution before and after; `time make test`. **Dependencies:** T3 (fewer cache files to copy) helps but is not required.

### T12. Run directories and store history grow without pruning

**Evidence (measured 132 MB in `run/` with 85 run directories and a 30 MB `hosts` tree):** the bridge stats every file of every run of a session each second (`run-files-stamp.pl`), and `AgentQueue refresh` loads every historical run and delivery each tick (T4). Nothing archives finished runs.

**Proposed change:** an explicit `@ AgentRun archiveFinishedOlderThan:` that moves closed run directories out of the scanned tree and, later, a retention policy for store rows. Low priority today; it prevents T4 and T5 costs from creeping up. Size: S. Confidence: medium.

### T13. The inbox preview hook starts a full runtime per hovered message

**Evidence (inferred from `trash/Inbox.trash` lines 297–340 and `src/picker.rs` lines 305–347):** `inpick --preview-hook` executes the hook once per candidate whose preview appears; the hook validates the id with jq and then `exec`s `bin/trash-send … previewed:`, which sources the whole runtime, marks the message viewed (a save), and renders a display object. Scrolling through unread messages therefore costs one runtime start plus a transaction per row.

**Proposed change:** let the picker emit `previewed` intents on its stdout stream (the applet already has a structured result channel) and have the Trashtalk side process them in the already-loaded shell after the picker exits, or in the bridge loop; the display refresh can be done by the picker from a precomputed field. **Expected impact:** hover cost from 100–300 ms (estimate) to near zero. Confidence: medium. Size: M (protocol). **Dependencies:** shares design with T5/I2.

### T14. Notification substrate is a 100 Hz poller

**Evidence (inferred from `lib/vendor/honker.bash` lines 43–85):** `honker_listen` polls `_honker_notifications` with a fresh sqlite3 process every 10 ms by default. Any `Inbox onMessage:` listener therefore spawns about 100 processes per second. This is the mechanism the docs suggest for reacting to wake hints and it is the opposite of an idle-friendly event source.

**Proposed change:** raise the default poll interval to 250–500 ms, and in the medium term provide a real wake primitive (a FIFO or Unix socket written by `honker_notify`'s caller, or a persistent sqlite3 coproc using `PRAGMA data_version`). Size: XS for the interval, M for a real primitive. Confidence: high on the cost, medium on the design.

## 4. Whisker findings

Whisker was not executable in this session (Appendix B). The project's own measurements: 2 ms minimal, 5 ms dev, 29 ms ops per render on a warm cache; 7/17/199 ms cold (`NOTES.md`, `docs/grid-design.md` appendix). These are consistent with the code and I have no reason to doubt them.

### W1. Collectors run synchronously with no timeout

**Evidence (inferred from `src/main.rs` lines 18–113, documented as a known limit in the README):** every prompt runs `git status --porcelain=v1 --branch --untracked-files=normal`, which refreshes the index and scans untracked files; in a large repository or on a slow disk this is hundreds of milliseconds, and a hung custom `command` segment blocks the prompt indefinitely. Outside a repository the `git` process is still spawned and fails.

**Proposed change:** (a) a per-collector timeout (kill the child after, say, 200 ms and render the segment as absent, the same contract a failing segment already has); (b) skip spawning `git` when no `.git` ancestor exists; (c) document `core.untrackedCache` and `core.fsmonitor` for users of large repositories rather than adding async machinery. **Expected impact:** bounded prompt latency; ~3–5 ms saved per prompt outside repositories. Confidence: medium. Size: S. Risk: a timed-out git shows no branch for one prompt; acceptable given the README's own "absent beats misleading" rule. **Measure:** `hyperfine 'target/release/whisker render --view dev --columns 100'` in a small and a large repository; `GIT_TRACE_PERFORMANCE=1 git status …`.

### W2. The kubernetes segment spends ~29 ms per prompt starting kubectl

**Evidence (documented):** `kubectl config view --minify` costs 29 ms warm and 199 ms cold; it runs on every prompt in the ops view. **Proposed change:** cache the extracted context and namespace in a small state file keyed by the mtime and size of each file in `KUBECONFIG` (the same "something writes, the prompt reads" pattern the grid already uses), refreshing only when a kubeconfig changes. **Expected impact:** ops view render from ~29 ms to ~2 ms. Confidence: medium-high. Size: S. Risk: stale context for one prompt after `kubectl config use-context` if the mtime is unchanged; include file size and a short TTL. **Measure:** `hyperfine` on the ops view.

### W3. Two process launches per Alt-O and one per prompt

**Evidence (inferred from `shell/whisker.bash` lines 27–77):** each prompt runs one `whisker render`; Alt-O runs `view next` then `render`. Each launch parses the TOML configuration. At ~2 ms per launch this is not worth changing now; a `render --next` form that prints the new view name on stderr would save one launch per switch. Defer unless profiling shows launch cost dominating.

### W4. Grid image encoding per prompt

**Evidence (documented):** the hand-rolled deflate and base64 run on every render when the grid segment is enabled; the design doc measured it as noise relative to collectors. Defer.

## 5. Innards findings

Innards binaries could not be executed here; findings come from the source. Contract tests exist (`tests/*_contract.rs`) but were not run.

### I1. Every applet redraws on every poll timeout

**Evidence (inferred):** `agent.rs` lines 788–833, `picker.rs` 218–262, `inspector.rs` 123–139, `review.rs` 77–79, and `inline_text.rs` 1319–1348 all follow `loop { draw; poll(80 ms); … }`, so with no input the UI is laid out and rendered 12.5 times per second (25 for `navsplat`, which polls at 40 ms). ratatui diffs cells before writing, so terminal traffic is small, but layout, wrapping, string formatting, and (in `inline_text`) syntect highlighting of the visible lines run on every tick.

**Proposed change:** keep a `dirty` flag set by input events, resize, snapshot arrival, LSP events, or spinner ticks, and draw only when it is set; use a longer poll (250 ms) when nothing is animating. **Expected impact:** idle CPU per open applet from a few percent to ~0. Confidence: high. Size: S. Risk: forgetting to set the flag on a state change; add a debug assertion path or redraw at most once per second as a safety net. **Measure:** `ps -o %cpu -p <pid>` for an idle `inpage` in a pty over 60 s, before and after.

### I2. `inagent` rebuilds every row per snapshot and allocates per character

**Evidence (inferred from `agent.rs` lines 300–376 and 645–663):** `apply` replaces all entries and calls `rebuild`, which re-wraps every entry; `wrap` calls `Span::raw(ch.to_string()).width()` for each character, allocating a String and a Span per character; `viewed_intent` allocates a HashSet of visible ids on every frame. Because the bridge sends a full snapshot every second while an agent streams (T5), a 400-entry window is re-wrapped once per second and a 100,000-entry window (after `load_older`) is re-wrapped on every refresh.

**Proposed change:** (a) use `unicode_width::UnicodeWidthChar::width` directly; (b) keep rows keyed by entry id and re-wrap only entries whose text changed or are new, dropping rows for removed ids; (c) compute `viewed_intent` only when scroll or entries changed; (d) accept the delta protocol from T5 when available. **Expected impact:** snapshot application from O(window) to O(delta); no stalls at large windows. Confidence: medium-high. Size: S for (a)–(c), M with (d). **Measure:** apply a 100,000-entry snapshot in a unit test with `Instant`; end-to-end with the `focus_frame_50000` fixture.

### I3. `inpick` re-reads the preview file every frame and rebuilds all list items

**Evidence (inferred from `picker.rs` lines 431–534 and 622–670):** `draw_preview` calls `std::fs::read_to_string` and splits the whole file on every draw (12.5 Hz), `draw` builds a `ListItem` (three formatted strings) for every match on every draw, and `StaticProvider::search` formats and lowercases a haystack for every candidate on every keystroke. `navsplat/ui.rs` lines 358–409 has the same per-frame file read at 25 Hz.

**Proposed change:** cache the preview lines keyed by (path, mtime, size) or by selected candidate id; precompute the lowercase haystack once per candidate at load; build list items only for the visible window or reuse them until matches change. **Expected impact:** removes ~12–25 file reads per second and most per-frame allocation; keystroke latency unchanged or better for large candidate sets (the symbol browser can produce thousands). Confidence: high. Size: S.

### I4. `inmacs` and `inpage` rebuild the syntect syntax set on every launch

**Evidence (inferred from `inline_text.rs` lines 1255–1293):** `SyntaxHighlighter::new` loads the default set, converts it into a builder, adds the Trashtalk syntax, and calls `build()` on every start, plus `ThemeSet::load_defaults()`. syntect's `SyntaxSetBuilder::build` relinks every syntax and is documented as expensive relative to loading a packed dump. **Hypothesis:** this costs 50–200 ms per launch; it is the most likely reason an inline editor feels slower to open than a bare `less`.

**Proposed change:** build the combined set once at compile time (a `build.rs` that dumps the set with `dump_to_uncompressed_file`, loaded with `from_uncompressed_data`/`include_bytes!`), or add the Trashtalk syntax lazily only for `.trash` files and otherwise use the packed default set directly. Also cache `find_syntax_by_name` once instead of per frame. **Expected impact:** startup under 20 ms if the hypothesis holds. Confidence: hypothesis. Size: S. **Measure:** `hyperfine 'inpage --result-json README.md </dev/null'` (fails without a terminal but only after the syntax set is built) before and after; or a unit test timing `SyntaxHighlighter::new`.

### I5. Per-character spans in the editor renderer

**Evidence (inferred from `inline_text/render.rs` lines 139–171):** `slice_highlighted_line` pushes one `Span` with an owned `String` per visible character (thousands per frame). **Proposed change:** group consecutive characters with identical style into one span. Size: S; defer until I1 lands and profiling shows it matters.

### I6. Cursor-position handshake at startup

**Evidence (inferred from `inline_terminal.rs` lines 307–344):** entering the inline viewport waits up to 2 s for a `CPR` reply. Normal terminals answer within a few milliseconds; under some multiplexers the reply can be delayed, which would show up as a startup stall. Hypothesis only; measure with a pty before changing anything. Resize already reuses the known anchor, which is the right pattern.

## 6. Cross-project opportunities

**X1. Process spawning is the shared cost model.** Trashtalk pays 3–15 ms per external process and 1–3 ms per capture subshell; Whisker pays one launch per prompt plus git and kubectl; Innards' preview hook launches a whole Trashtalk runtime. The highest-value fixes (T4, T5, T7, T8, T13) are all "spawn fewer processes for the same result". A single wrapper-counting harness (Appendix A) measures all of them.

**X2. Polling and refresh.** Four independent loops run while nothing happens: the worker (1 Hz, ~10 processes), the bridge (1 Hz, ~8 processes per open view), Honker listeners (100 Hz when used), and every Innards applet (12.5–25 Hz redraws). The fix pattern is the same everywhere: probe cheaply, back off when idle, redraw only on change, and move toward a real wake source (an appended log line, a FIFO, or a persistent SQLite connection's `data_version`).

**X3. Protocol and UI churn.** The bridge to `inagent` protocol is snapshot-only, so both sides do O(window) work per refresh; the picker's preview hook round-trips through process launches; inbox browsing writes a preview file per message and rebuilds records per browse. An incremental frame protocol (T5/I2) and picker-emitted intents (T13) remove most of it and are worth designing together so both applets share one `delta` shape.

**X4. Build and test feedback.** T1 (tokenizer), T2 (per-class jq startup), T3 (cache growth), and T11 (per-test checkout) compound: today a change to Trash.trash costs ~25 s to rebuild, and a full `make verify` spends minutes on setup. Whisker's and Innards' `cargo` loops are fine; Whisker's `try-it` builds a debug binary each launch, which is expected.

**X5. Repeated filesystem, database, and JSON work.** One jq per field write, six jq per process capture, DDL per queue call, a staging database per transaction, the compiler fingerprint re-hashed per driver process, per-frame file reads in the applets, and per-launch syntax-set builds are all the same shape: work that is recomputed because the result is not kept anywhere. Each fix is small; the value is in doing several.

## 7. Recommended first tranche (five items)

Each item preserves current behaviour and has an acceptance check that can run locally.

1. **T1 tokenizer locale.** Verify with the paired command in Appendix A, then pin `LC_ALL=C` around the scan in `tokenizer.bash`; add a compiler test that tokenizes a fixture with non-ASCII strings under both locales and compares decoded tokens. Acceptance: Trash.trash tokenizes in under 2 s; `make` after editing Trash.trash completes in under 8 s; `make test-compiler` passes.
2. **T4 worker idle cost.** Memoize `AgentQueue ensureSchema`, bound `refresh` to tickable sessions, add capped idle backoff and fail-fast exit after repeated tick failures, truncate the service log on start, and use `-prune` in the rebuild check. Acceptance: `bin/trash-worker --once` against an empty store under 150 ms after runtime startup; idle service under 2% CPU over ten minutes; `run/worker/stderr.log` under 1 MB after a day; `tests/test_agent_worker.bash` and `test_workstation_worker.bash` pass.
3. **T7 single-pass request decoding in `Tool runProcessRequestJson:`** plus the T9 `tr` removal and accessor skip. Acceptance: `process_capture` median at least 2× faster than the same-day baseline; jq count per capture is 2; `tests/test_process.bash`, `test_code_tools.bash`, and the Tool wrapper tests pass.
4. **T3 + T11 cache pruning and shared test checkout.** Prune stale cache generations at the end of `compile-many`, delete staging leftovers, and switch `run-tests.sh` to one base checkout plus APFS clones with the fingerprint computed once. Acceptance: `trash/.compiled` under 15 MB; per-test setup under 0.3 s (measured by timestamps around `test-isolated.bash`); `make verify` wall time reduced by at least 30% with identical pass counts.
5. **I1 + I3 Innards idle redraw and picker caching.** Dirty-flag redraws in all five applets, preview caching by (path, mtime, size), precomputed search haystacks. Acceptance: idle `inpage` and `inagent` at ~0% CPU in a pty for 60 s; contract tests pass; keystroke-to-redraw latency unchanged in the PTY tests.

Second tranche candidates, in order: T5/I2 incremental frames, T6 transaction staging template, T8 symbol memoization, T13 picker intents, W1/W2 collector timeout and kubeconfig cache, I4 syntax-set dump.

## 8. Attractive but premature

- **Option B result ABI and default-on value sends.** The project's own evaluation (`docs/result-passing-design.md`) found under 5% application-level gain, a `set -e` incompatibility in B, and browser regressions with default-on A. Nothing in this audit changes that; the process-count work above yields more for less risk.
- **A resident Trashtalk daemon or a Rust dispatcher.** The docs explicitly reject a daemon; the cost model is process spawning, and T4–T8 recover most of it without a second runtime. Revisit only if, after the first two tranches, dispatch itself (not spawned tools) dominates a profile.
- **Rewriting the jq compiler.** `codegen.jq` is 3,333 lines and well tested. T1 removes the dominant cost; T2's batching removes most of the rest. A compiler rewrite would be weeks of risk for seconds of build time.
- **SQLite indexes on JSON paths.** The store is 768 KB; queries take microseconds and each costs a process anyway. Indexes matter only once the store is tens of MB.
- **Whisker async collectors or a daemon.** The grid design already chose "something else writes, the prompt reads"; a timeout (W1) and a kubeconfig cache (W2) cover the realistic cases.
- **Custom partial redraw or incremental syntect parsing in Innards.** ratatui already diffs cells and highlighting is limited to the visible lines; stopping needless frames (I1) is the whole win.
- **Batching or deferring `_ivar_set` writes to method exit.** It would remove one jq per assignment but violates the documented immediate-write contract and the nested-send read path; only worth it with a compiler-proven "no intervening reads" analysis.
- **Class preloading at runtime start.** Measured neutral in `docs/performance-implementation.md`.

## 9. Reproducible profiling plan

Run these from a normal terminal in `~/.trashtalk` (not inside a sandboxed tool session), with no concurrent builds and the worker service stopped, and record medians of at least five runs. `hyperfine` is not installed; the Bash 5 `EPOCHREALTIME` loops in the project's benchmark scripts are the local equivalent.

**P1. Tokenizer locale (T1).**
```bash
time bash lib/jq-compiler/tokenizer.bash trash/Trash.trash > /tmp/tok-utf8.json
time LC_ALL=C bash lib/jq-compiler/tokenizer.bash trash/Trash.trash > /tmp/tok-c.json
jq -S . /tmp/tok-utf8.json | shasum; jq -S . /tmp/tok-c.json | shasum   # must match
```
Expected if the hypothesis holds: the second run is at least 10× faster; the token arrays are identical (byte columns may differ only on lines with non-ASCII text).

**P2. Process-count harness (T4, T5, T7, T8, T9).** Create wrappers that count executions and forward:
```bash
mkdir -p /tmp/wrap; : > /tmp/wrap/count
for t in jq sqlite3 perl shasum uuidgen jo tr cat sed grep cut sort find mktemp date gdate awk xargs tar python3 cue git; do
  real=$(command -v "$t") || continue
  printf '#!/bin/bash\necho %s >> /tmp/wrap/count\nexec %q "$@"\n' "$t" "$real" > "/tmp/wrap/$t"; chmod +x "/tmp/wrap/$t"
done
export PATH=/tmp/wrap:$PATH SQLITE_JSON_DB=/tmp/audit.db TRASHTALK_RUN_DIR=/tmp/audit-runs TRASHTALK_SKIP_USER_CONFIG=1
source lib/trash.bash; : > /tmp/wrap/count; @ Tool captureArgvJson: '["/usr/bin/true"]' >/dev/null; sort /tmp/wrap/count | uniq -c
: > /tmp/wrap/count; @ AgentWorker tick >/dev/null; sort /tmp/wrap/count | uniq -c
```
The Homebrew sqlite3 is selected by absolute path inside the runtime; set `TRASH_SQLITE3=/tmp/wrap/sqlite3` to count it. Timing must be taken without the wrappers (each adds a Bash start).

**P3. Idle worker and bridge CPU (T4, T5).**
```bash
SQLITE_JSON_DB=/tmp/audit.db TRASHTALK_RUN_DIR=/tmp/audit-runs bin/trash-worker & pid=$!
sleep 120; ps -o %cpu,cputime -p $pid; kill $pid
```
For the bridge, attach a fixture session with `TRASHTALK_BENCH_ROOT`-style isolation as in `bin/trash-bench-interactions`, replace `inagent` with the two-line fake surface from that script, and sample `ps` on the `tool-duplex.bash` process. `sudo fs_usage -w -f exec` (macOS) lists every exec with timestamps if a per-second breakdown is wanted.

**P4. Build floor (T2, T3).**
```bash
time make bash                                   # warm: expect ~0.3 s per the 5 Sep report
printf '\n# audit\n' >> trash/Counter.trash; time make single CLASS=Counter; git checkout trash/Counter.trash
make clean; time make bash                       # cold, before and after T1
```
Count jq processes per class with the P2 harness around `make single`.

**P5. Test setup overhead (T11).** Wrap `lib/test-isolated.bash` invocations in `run-tests.sh` with `EPOCHREALTIME` before and after the `tar` pipeline and before the test body; report the distribution across all files. Compare `time make test` before and after the clone-based checkout.

**P6. Runtime startup (T10).**
```bash
export SQLITE_JSON_DB=/tmp/audit.db TRASHTALK_SKIP_USER_CONFIG=1
for i in 1 2 3 4 5; do /usr/bin/time -p bin/trash-send Counter description; done
bash -x bin/trash-send Counter description 2>&1 | grep -Ec '^\++ (jq|sqlite3|/opt/homebrew/opt/sqlite/bin/sqlite3|perl|find|uuidgen|tr|cat)'
```

**P7. Whisker (W1, W2).**
```bash
cargo build --release --manifest-path ~/dev/rust/whisker/Cargo.toml
cd ~/.trashtalk && for i in 1 2 3 4 5; do /usr/bin/time -p ~/dev/rust/whisker/target/release/whisker render --view dev --columns 100 >/dev/null; done
cd <large repo> && GIT_TRACE_PERFORMANCE=1 git status --porcelain=v1 --branch --untracked-files=normal >/dev/null
/usr/bin/time -p kubectl config view --minify -o jsonpath='{.current-context}'
```

**P8. Innards (I1–I4).**
```bash
cargo build --release --manifest-path ~/dev/rust/innards/Cargo.toml
for i in 1 2 3; do /usr/bin/time -p ~/dev/rust/innards/target/release/inpage --result-json README.md </dev/null; done   # startup incl. syntax set
python3 - <<'EOF'   # idle CPU in a pty
import os, pty, subprocess, time
m, s = pty.openpty(); p = subprocess.Popen(["inpage", "README.md"], stdin=s, stdout=s, stderr=s, preexec_fn=os.setsid)
time.sleep(30); print(subprocess.run(["ps", "-o", "%cpu,cputime", "-p", str(p.pid)], capture_output=True, text=True).stdout); os.write(m, b"q")
EOF
```
For I2, add a `#[test]` that applies a 100,000-entry snapshot to `App` and asserts a wall-clock bound; `cargo flamegraph` or `samply` on `inagent` fed by the `focus_frame_50000` fixture shows `wrap` and `rebuild` directly.

## Appendix A. Commands that produced the measured numbers

- `bash bin/trash-bench 5`, `bash bin/trash-bench-interactions 3 browsers`, `bash bin/trash-bench-interactions 3 conversations` (each with its own disposable store).
- `time bash lib/jq-compiler/tokenizer.bash trash/{Counter,Tool,AgentSession,Inbox,Trash}.trash > /dev/null`.
- `bash -x bin/trash-bench 1 2>&1 | grep -Ew '^\++ (jq|sqlite3|…)' | cut -d' ' -f2 | sort | uniq -c`.
- `time bash lib/test-isolated.bash tests/test_env.bash`; `time bash lib/test-isolated.bash lib/jq-compiler/tests/test_build_cache.bash`.
- `du`, `ls`, `wc`, and `grep` over `trash/.compiled`, `run/worker`, and the tests directory; `make -C ~/.trashtalk -n bash` for the source list; `locale`.

## Appendix B. Diagnostics blocked in this session

The sandbox denied: any command with an environment-variable prefix or `env` (so no isolated-store runs of `trash-send` or `trash-worker --once`, and no `LC_ALL=C` verification of T1); executing binaries outside the three checkouts (all Innards applets) and, for unknown reasons, the Whisker release binary and `git status` timing; `jq -f` (so the jq program-compile cost in T2 is unmeasured); `ps`, `sysctl`, `sw_vers`; `mkdir`/`cp`/`Write` to `/tmp` (so no repository copy for a cold `make` and no read-only copy of the live store for query counts); `sort -k`; and the user's shell rc files (so whether Whisker or Trashtalk hooks are installed in the login shell is unknown). Everything listed in Section 9 covers these gaps and takes a few minutes from an ordinary terminal.
