# Browser and conversation performance, 2026-09-12

Baseline: `e23500ba65b6d2b16e30367c513c5b7660134959`.
Implementation measured: `af93eb30b2e1067c96cdba4eb717c7669875f772`.
Both revisions were built separately and measured sequentially using the same
`bin/trash-bench-interactions` script, with no concurrent builds or tests.

Host: Mac15,12, eight ARM64 cores, macOS 14.6; Bash 5.3.15, jq 1.8.1,
SQLite 3.53.4 (the Homebrew SQLite selected by the runtime). Normal background
applications remained running. These are diagnostic wall-clock measurements,
not a controlled-host qualification or guarantees for other machines.

The numbers below are medians of three samples after one warmup. First direct
input is one cold sample and does not establish a reliable speedup. Small
changes in current-session lookup and session mail are within the range where
host noise matters. Both runs completed all 18 cases and all 52 samples with
zero failures. Collection cases checked exact row counts and object values; the
long transcript checked all 300,000 characters of assistant text.

| Operation | Before (ms) | After (ms) | Ratio |
|---|---:|---:|---:|
| Inspect session admission | 125.29 | 61.53 | 2.04× |
| Live session admission | 343.14 | 124.18 | 2.76× |
| Resolve current session | 201.64 | 190.50 | 1.06× |
| Deliver human inbox mail | 241.50 | 191.19 | 1.26× |
| Deliver session inbox mail | 516.15 | 477.59 | 1.08× |
| Prepare 1 inbox row | 176.16 | 100.37 | 1.76× |
| Prepare 10 inbox rows | 1,451.43 | 143.36 | 10.12× |
| Prepare 50 inbox rows | 7,264.51 | 330.87 | 21.96× |
| Prepare 200 inbox rows | 28,995.50 | 1,075.52 | 26.96× |
| Prepare 10 object rows | 205.75 | 34.97 | 5.88× |
| Prepare 100 object rows | 1,912.18 | 91.29 | 20.95× |
| Prepare 10 session rows | 311.54 | 31.33 | 9.94× |
| Prepare 50 session rows | 1,545.70 | 42.97 | 35.97× |
| Refresh focus frame, 50,000 chunks | 954.11 | 336.34 | 2.84× |
| Open empty conversation with fake UI | 571.85 | 373.17 | 1.53× |
| First direct input (one cold sample) | 1,563.33 | 1,428.84 | 1.09× |
| Direct input to busy fixture | 767.68 | 620.21 | 1.24× |
| Send through conversation UI handler | 1,362.72 | 639.42 | 2.13× |

The largest gains come from removing per-row parsing and subprocesses. Inbox
presentation uses one ordered reload, one participant query, shared calendar
values and one field decode across the rows. Object and session browsers use
shared projections and preview-file writing. Session snapshot predicates now
use the existing class index.

Conversation admission retains fresh ownership, enabled-state, lifecycle and
membership checks. A guarded read projection replaces the temporary staging
database for read-only access. Its result is withheld until the projection
matches again under the commit lock. Direct input retains the authoritative
check under the worker lock, and the UI no longer repeats it beforehand.

The long-history case measures a warm view cache. It verifies the previously
consumed file bytes by hash and parses only complete appended records. It still
reads the prefix for integrity and still serializes the visible frame. Cache
rebuilds preserve global ordering for out-of-order events, edits, truncation,
file replacement/deletion and window changes. Caches belong to temporary views;
authorization still runs on every frame. Cold attach is not represented by the
warm 50,000-chunk number.

Ordinary mail is still durable Message/AgentDelivery work. It improves less than
UI input, which uses the native conversation transport. Field initialization
shares a cache update only for exact generated setters; custom setters and
advice keep ordered public sends. Delivery obtains the final message from its
existing atomic message/outbox transaction after successful commit.

The fake picker measures preparation through the public API, and the repository
Jcode fixture exercises input admission and acknowledgement. No real model or
terminal rendering is measured. Native first-input startup and the remaining
Store/routing work continue to dominate the smaller improvements.

## Tokenizer

Both tokenizers consumed identical source from the baseline revision. Each of
three paired runs compared the full decoded token arrays, including values,
types and source locations. Times include starting Bash and jq.

| Input | Before (ms) | After (ms) | Ratio |
|---|---:|---:|---:|
| Counter.trash | 432.04 | 22.14 | 19.51× |
| AgentSession.trash | 8,740.96 | 2,703.01 | 3.23× |

Serialization now uses one jq process regardless of token count. Token scanning
itself remains Bash. The full compiler suite covers compatibility and focused
tests cover opaque values, numeric positions and serializer failure propagation.

## Reproduction and evidence

Build each checkout first, then run the benchmark script with its default root
or select another built revision with `TRASHTALK_BENCH_ROOT`. Run revisions
sequentially, without concurrent builds/tests:

```bash
bin/trash-bench-interactions 3 all > /tmp/after.jsonl
TRASHTALK_BENCH_ROOT=/path/to/baseline \
  bin/trash-bench-interactions 3 all > /tmp/before.jsonl
```

For tokenizer comparisons, invoke each checkout's
`lib/jq-compiler/tokenizer.bash` with the same baseline `.trash` file, measure
three invocations, and compare decoded JSON after every pair.

Raw samples: [before](benchmarks/2026-09-12-before.jsonl),
[after](benchmarks/2026-09-12-after.jsonl), and
[tokenizers](benchmarks/2026-09-12-tokenizers.jsonl).
Final `make verify` passed 77 runtime test files and 47 compiler test files,
with zero failures or timeouts.
The [implementation checklist](performance-implementation.md) records the
compatibility boundaries and regression coverage. Runtime changes require a
fresh Trashtalk shell; this work does not replace an already-running shell's
loaded functions.
