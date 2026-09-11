# September 2026 cleanup

**Status: implemented.** This pass makes the documentation and supported surface
match the Bash runtime, and removes dormant implementations.

## Compatibility changes

- Bash **4.4+** is the minimum: the existing exact-argv process primitive uses
  NUL-delimited `mapfile -d`. Startup, compiler preflight, doctor, and installation
  instructions now agree. Make resolves Bash from PATH on each platform. Bash 3.2 and Zsh are unsupported.
- `Agent` retains its Axe/Codex one-shot facade. The old tmux session methods,
  `ClaudeAgent`, and `TmuxSession` are removed. Use `AgentIdentity` and
  `AgentSession`; plain `@@` uses Gusgus/Jcode. `Tools::Tmux` remains available.
  A read-only audit found no stored instances or user source/config references
  to the removed classes in the local installation. External scripts may need
  migration; this was not a global caller audit.
- The placeholder `Environment` class is removed. Use `Runtime dataFor:` for
  session objects or `Store getInstance:` for durable JSON. Runtime deletion
  removes cache only; Persistable deletion removes cache and Store.
- `GrpcClient` supports unary `call:with:` / `call:`, `listServices`,
  `listMethods:`, `describe:`, `enableTLS`, and `enablePlaintext`. Each request
  runs grpcurl through Tool with exact argv and preserves stderr/exit status.
  The unsupported streaming callbacks, inert pooling flags, reflection toggle,
  and nonfunctional `withProto:` constructor are removed. This wrapper currently
  requires server reflection; it does not manage connections or local schemas.
- `pragma: direct` and `primitive` remain. Removed backend pragmas (`bashOnly`,
  `procyonOnly`, `procyonNative`) fail compilation with an explicit explanation;
  review their bodies before removing those pragmas from private source.

After updating, run `make clean bash` and start a fresh Bash session. Cleaning
removes compiled files for retired classes; reloading an existing shell alone
can leave old function definitions visible. Durable objects are not deleted.

## Completed scope

| # | Cleanup | Evidence / result |
| --- | --- | --- |
| 1 | Replace backend comparison; archive native closure plans | Current compiler capability reference; `docs/archive/`; obsolete compiler TODO stub removed |
| 2 | Correct language limitations | Production tests cover selectors, negative arguments, raw qualified names, and block boundaries |
| 3 | Explain initial persistence, cache, saves, reload, deletion, transactions | `persistence.md`; matching runtime/trait comments |
| 4 | Repair installation and Bash minimum | README, runtime/compiler checks, doctor |
| 5 | Update agent and message UI guidance | Persistent vs one-shot APIs, live attachment and composer |
| 6 | Index docs, label status, expand idioms | `docs/README.md`; Assignment traits/collaborators; repaired class-cluster link |
| 7 | Test the production compiler | Copied expression implementations replaced; failures exit nonzero; compiler diagnostics preserved; per-run scratch |
| 8 | Remove dead codegen helpers | Unused condition/control-flow cluster and other definition-only helpers removed |
| 9 | Retire alternate compiler modules | Unused expr-parser/codegen, IR, grammar, and PEG files removed from source and fingerprint |
| 10 | Remove native-era debris | Unused configuration backup, markers, reset calls, and comments removed; active pragmas retained |
| 11 | Make profiling Bash-only | Inclusive timing reports, no native advice or unused in-memory accumulator; analyzer status checks |
| 12 | Remove placeholder Environment | Future example uses Runtime; no live data migration |
| 13 | Make gRPC behavior honest | Narrow Tool boundary, offline argv/error tests, explicit optional live test |
| 14 | Delete orphan Yutani demo | No YutaniSession implementation existed |
| 15 | Retire tmux agent API | Local compatibility audit; one-shot facade and Tools::Tmux retained |
| 16 | Simplify ignore rules | New lib sources visible; actual generated output stays ignored |

## Validation

Final validation passed **67 runtime and 43 compiler test files**, with no
failures or timeouts (`TRASH_TEST_TIMEOUT=360 TRASH_TEST_JOBS=4 make test` and
`make test-compiler`, including their build prerequisites). The production
compiler, gRPC process boundary, and profiler have focused regressions. The
isolated suites cover
persistence, Assignment transactions, queued agents, and live-view fixture tests.
The real gRPC server test is explicitly opt-in with `TRASH_TEST_GRPC_LIVE=1`;
fixture success is not proof of a live server or TLS connection.

Additional checks exercised the documented create/mutate/save/reload/deletion
journey against an isolated database, verified doctor on Bash 5.3.15, and
confirmed that macOS Bash 3.2 rejects both runtime and compiler before loading.
The 4.4 floor follows required Bash features; the full suite was run on 5.3.15.
Relative documentation links resolve, new lib source files are visible to Git,
and generated binaries and compiled classes remain ignored.
