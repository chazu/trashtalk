# jq replacement research

**Status:** Research and evaluation plan. No JSON processor is replaced by this
document.

## Decision

Do not replace jq globally. Evaluate jaq first as an opt-in compiler-only
candidate. Do not adopt qj yet.

Trashtalk currently runs jq 1.8.1 from `/opt/homebrew/bin/jq`. The repository
requires jq throughout the compiler, build cache, runtime JSON primitives, UI,
agent transport, and helper scripts. A faster executable is useful only if it
preserves every contract that each affected path relies on.

The compiler is the first useful test boundary because it concentrates many jq
invocations and can be tested with its existing suite. Generated Bash and the
runtime must continue to invoke jq until a separate compatibility evaluation
qualifies a replacement there.

## Current use and expected opportunity

`lib/jq-compiler/driver.bash` invokes jq for token parsing, AST validation,
trait assembly, code generation, symbol records, and batch build planning.
`build-cache.bash` also invokes jq for graph, receipt, and NUL-delimited batch
work. These are short-lived processes over mostly small JSON documents.

This matters for interpreting vendor benchmarks:

- The compiler does not process multi-gigabyte NDJSON files.
- It often supplies input through stdin, command substitution, or process
  substitution.
- It requires `-f`, `-n`, `-R`, `-s`, `-c`, `-r`, `-j`, `-e`, `--arg`,
  `--argjson`, and `--slurpfile` semantics.
- Correct failure status, compact JSON output, NUL handling, and numeric values
  matter more than a high throughput result on a large file.

The largest likely gain is lower process startup and filter evaluation time
across a full cold compile. SIMD file scanning and parallel NDJSON processing
will not transfer directly to the compiler workload.

## Candidates

### jq 1.8.1

jq remains the compatibility reference and default. It is installed locally as
version 1.8.1. jaq's upstream documentation notes that jq's startup cost has
improved substantially since jq 1.6, so old jq 1.6 comparisons are not a sound
reason to replace the installed version.

Keep jq as the oracle for differential tests and as the fallback executable.

### jaq 3.1.1

[jaq](https://github.com/01mf02/jaq) is an MIT-licensed Rust implementation
focused on jq compatibility, correctness, and speed. Homebrew provides jaq
3.1.1, but it is not installed locally.

Its upstream benchmark report compares jaq 3.0, jq 1.8.1, and gojq. jaq reports
that it wins 20 of 31 listed benchmarks while jq 1.8.1 wins five. Upstream also
reports materially faster startup than jq 1.6. These are useful signals, not
Trashtalk measurements.

**Fit:** promising for compiler-only evaluation. It is mature enough to install
from Homebrew and has a clear license.

**Risk:** the compiler relies heavily on CLI options and jq-language behavior,
including `--slurpfile` with process substitution and NUL-delimited input.
Passage of a simple filter suite is insufficient. jaq must pass all compiler,
cache, JSON primitive, and generated-code compatibility tests before it can be
selected.

### qj 0.1.4

[qj](https://github.com/6/qj) is a Rust jq-compatible processor backed by
simdjson. Its README claims full jq official-suite coverage and reports 25 to
190 times faster throughput on NDJSON/JSONL file workloads. It also reports two
to 25 times faster performance on some large single-document JSON workloads.

The claims do not map directly to Trashtalk. qj says stdin/pipeline workloads
have smaller gains, slurp mode has only roughly two to three times the jq
performance, and its biggest gains depend on mmap, automatic parallelism, and
on-demand field extraction. Trashtalk commonly uses stdin, `-s`, and temporary
or process-substitution files.

**Fit:** research candidate only. Its file-based NDJSON performance could help
future transcript or event-log tooling, not the current compiler's typical
workload.

**Risks:** qj is very new. GitHub reports repository creation on 2026-02-17 and
its newest release as v0.1.4 on 2026-02-23. Homebrew has no qj formula. The
repository metadata did not report an SPDX license as of this research. Its
compatibility and benchmark claims are upstream claims, not independently
reproduced here. qj also documents different numeric behavior above 2^53 unless
`QJ_JQ_COMPAT=1` is set, and a higher memory tradeoff for its fast file path.

Do not make qj a required Trashtalk dependency or run it in the runtime path.
Reconsider it only after it has a stable distribution, explicit license, and a
passing differential suite on representative Trashtalk inputs.

### gojq

[gojq](https://github.com/itchyny/gojq) is another jq-compatible implementation.
jaq's upstream comparison includes it, but does not establish it as faster than
jq 1.8.1 for this workload. It is not a priority unless jaq fails compatibility
or packaging needs.

## Compatibility boundary

Do not add a system-wide alias, rename another executable to `jq`, or replace
all literal jq calls. That would alter unrelated runtime, UI, and agent
protocols without a focused qualification path.

Instead, add an opt-in compiler executable setting, tentatively
`TRASHTALK_COMPILER_JQ`. It must:

- default to the current `jq` executable;
- accept an absolute executable path after validating it;
- affect only `lib/jq-compiler/driver.bash` and its build-cache children;
- remain visible in compiler and build-cache fingerprints;
- leave generated Bash literal `jq` commands unchanged;
- fail clearly when the selected executable lacks a required option or differs
  from jq on a checked fixture.

A later runtime-wide replacement needs an independent design. `codegen.jq`
emits literal jq commands into generated Bash, and the runtime has many direct
jq calls. This is a separate compatibility and deployment surface.

## Required qualification

Before selecting jaq, run these checks with jq and the candidate and compare
exit status, stdout bytes, and stderr class where the contract exposes it:

1. Run `make test-compiler`, `make test`, and `make verify` with jq, then with
   `TRASHTALK_COMPILER_JQ` set to the candidate.
2. Add a focused differential suite for compiler scripts. Cover `parser.jq`,
   `codegen.jq`, `build-plan.jq`, `symbols.jq`, and `senders.jq`.
3. Cover every used CLI feature, especially `--slurpfile` over process
   substitution, `-Rsc` NUL data, `-rj` output, `--argjson`, `-e`, and multiple
   input files.
4. Compare generated artifacts byte-for-byte. Compare build receipts and the
   dependency graph JSON structurally where timestamps or paths make raw bytes
   inappropriate.
5. Run malformed JSON, missing-file, invalid-filter, and failing-filter cases.
   Preserve the existing caller-visible nonzero status and diagnostics shape.
6. Run generated Bash JSON primitive tests with ordinary jq unchanged. This
   confirms that a compiler-only experiment did not alter emitted runtime code.

Treat any semantic or error-contract difference as a rejection until a narrow,
explicit compatibility shim is designed and tested. Do not hide differences
behind an automatic fallback because a mixed compiler can make builds
non-reproducible.

## Benchmark plan

Measure on the same machine with cold and warm cache states. Record processor
version, command, input fixture hashes, wall time, user and system CPU time,
maximum resident memory, and number of jq processes.

| Workload | Why it matters |
| --- | --- |
| `driver.bash compile` for small, medium, and large `.trash` files | Captures parser and code generator startup cost. |
| Cold `make bash` | Captures full graph planning and compilation. |
| Warm `make bash` | Captures cache and receipt overhead. |
| `compile-many` with the normal worker count | Captures concurrent short jq processes. |
| `build-plan.jq` with synthetic dependency graph | Captures graph and receipt work. |
| Runtime JSON primitive microbenchmarks | Establishes baseline only. Do not substitute candidates in this phase. |

Use `hyperfine` or an equivalent repeatable runner with warmups and at least ten
runs. Test jq 1.8.1 against the candidate version. Report medians and spread,
not a single best result.

Adopt jaq for compiler-only use only if it passes the full qualification suite,
produces equivalent artifacts and receipts, has no worse p95 cold or warm build
time, and provides a meaningful measured improvement. A starting decision bar
is a 10 percent median cold-build improvement with no p95 regression. Revise
the bar after recording the jq baseline.

## Recommendation

1. Keep jq 1.8.1 as the default and reference implementation.
2. Add the narrow compiler executable seam only with its fingerprint and
   differential tests.
3. Install and evaluate jaq 3.1.1 in an isolated test environment first.
4. Do not adopt qj for the compiler or runtime now. Re-evaluate it for large
   NDJSON tools after its distribution and licensing mature.

## Evidence and limitations

Research was performed on 2026-09-28. Local checks found jq 1.8.1 installed;
jaq and qj were absent. Homebrew reports jaq 3.1.1 with an MIT license and no
qj formula. qj repository metadata and all performance results above are
upstream-reported. No candidate was installed, no Trashtalk candidate benchmark
was run, and no compatibility result is claimed.

Sources:

- [jaq README and benchmarks](https://github.com/01mf02/jaq)
- [qj README, compatibility, and benchmarks](https://github.com/6/qj)
- [qj compatibility matrix](https://github.com/6/qj/blob/main/docs/COMPATIBILITY.md)
- Local repository inspection: `lib/jq-compiler/driver.bash`,
  `lib/jq-compiler/build-cache.bash`, `lib/jq-compiler/codegen.jq`, and
  `lib/trash.bash`
