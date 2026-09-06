# Result transport experiment

This is an opt-in, disposable experiment, not a production result ABI. The
runner copies the repository, compiles DSL fixtures, and changes only the
copies. It uses isolated SQLite databases and session files, without network
or model requests. `make bash` prepares ordinary compiled artifacts first.

```bash
python3 experiments/result-passing/run.py --semantics-only --output /tmp/result-semantics.json
python3 experiments/result-passing/run.py --samples 24 --output /tmp/result-run1.json
python3 experiments/result-passing/run.py --samples 24 --output /tmp/result-run2.json
python3 -B experiments/result-passing/focused.py --samples 36 --output /tmp/result-focused.json
python3 -B experiments/result-passing/strict_mode.py --output /tmp/result-strict.json
```

The harness needs Python 3, Bash 5's `EPOCHREALTIME`, and the usual Trashtalk
dependencies. The prototypes themselves retain the Bash 4 runtime baseline.

## Variants

- `C_native`: original compiled assignments; the performance comparison baseline.
- `C`: existing public `@` and its two captures, through a common assignment
  helper. This control quantifies the helper's overhead.
- `A_all`: remove the inner capture for every rewritten assigned send. This is
  an intentionally unrestricted upper-bound probe, including raw methods.
- `A_guard`: remove it only for seven explicitly selected scalar-return methods.
  Original receiver preparation, dispatch, and cleanup remain in use.
- `B_guard`: the same seven methods write to a frame-local result destination.
  It retains the normal dispatcher with an invocation adapter and restores the
  caller's last-result variable and call-stack storage. Unsupported calls reuse
  receiver preparation while retaining the ordinary capture/echo policy. Direct
  fallbacks retain their single capture. Field reads still have their existing
  capture costs. Missing arguments fall back before invocation. Numeric values
  are validated immediately after each field read; invalid values run the old
  scalar body inside its original captures, so arithmetic cannot abort the caller.
- `B_naive`: the initial B implementation repeats receiver preparation by calling
  public `@` again on fallback. It measures the cost of that implementation choice.

The seven methods are the fixture's constant, identity, getter, and arithmetic
returns, plus Counter's description/getValue/getStep. Capabilities are tied to
the compiled source hash. Recompiled overrides invalidate them. Class loading,
inheritance/traits, legacy methods, profiling, advice, handler/ensure frames,
and xpg_echo/errtrace/functrace modes use conservative fallbacks where necessary.
The numeric checks conservatively accept decimal integers of at most 18
characters; other values use the existing behavior. A deterministic write between
receiver resolution and the field read checks that a stale type check is not used.

This is not an effect checker. The runner verifies the selected emitted shapes,
then provides bounded prototype implementations. Arbitrary replacement of Bash
functions outside compilation/reload is not protected by a source-hash marker.
A production implementation would need a deliberate invalidation contract.
Candidate A preserves assignment syntax and changes its captured entry point;
candidate B assigns through its destination entry point directly. Neither uses
the generic bridge internally. Raw local declarations retain their original
assignment/status policy. This textual experiment lowering is not a production
compiler pass; shipping the change requires AST-based lowering.

## Protocol and decision rule

Compare returned bytes, incidental stdout, stderr, status, last-result value,
caller locals/directory/options/traps, runtime stacks/error state, live state,
and persisted state. Temporary paths, diagnostic line numbers, and profiling
durations are normalized; method output is not normalized. The unrestricted
probe may differ; guarded variants and the native control must match.

Measure seven workloads with warm class metadata and isolated persistent workers.
Rotate variant order each round; run only one timed workload at a time. The
first round is warmup and discarded. Validate every sample outside the timer.
Report median per-operation time, p95 of batch means (not request p95), paired
percentage changes, and a deterministic paired-bootstrap interval. Repeat the
run independently. The host is shared; record load and avoid concurrent tests.

Prefer the least invasive compatible option that repeats a >=5% median gain
on an application workload (class map, Block map, or browser records), with no
repeatable >5% regression on another. Microbenchmarks alone do not justify an
ABI change. A direct result ABI needs at least ten additional percentage points
of application benefit over A to justify its larger compatibility burden.
If neither guarded option meets that bar, retain C. Confidence intervals and
the native control qualify the conclusion; these are local measurements.

Pilot runs check harness operation and are excluded from final results. A pilot
found that the shared collection primitive uses unquoted assignments; the final
lowering handles both forms and traces all 25 callbacks to verify activation.
Another pilot led to B's shared-preparation fallback, compared with the naive
version in the final runs. Twenty-four rounds balance the six variant orders.
The second full run had a noisy class-map interval. A focused follow-up uses
36 rounds of three maps per batch to check that application result with less
per-sample timing noise; it does not change either candidate implementation.
The separate strict-mode probe exposes a remaining failure in both B prototypes:
with caller `set -e`, dispatch's post-increment returns a failure status and exits
before even a constant method returns. A and the original path retain the capture
boundary and complete. The probe makes the existing EXIT cleanup nonfatal so its
status does not mask the method result. It saves a trace of B's failing command.
This known failure is retained as evidence, not hidden by another prototype guard.
To reproduce the failed pre-validation B prototype, add `--unchecked-b` to a
`--semantics-only` run. It deliberately exits nonzero after saving the differences.
See the design document for measured results and the resulting decision.
