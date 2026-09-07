# CUE and mise in Trashtalk: a brainstorm

Trashtalk now ships two exact-argv tool wrappers:

- `Tools::Cue` (`trash/Tools/Cue.trash`) wraps the CUE CLI: validation,
  unification with defaults, export between JSON/YAML/CUE, and `cue eval`/`def`.
- `Tools::Mise` (`trash/Tools/Mise.trash`) wraps mise: per-project tool
  versions, `mise exec`, project environment variables, and `mise.toml` tasks.

Both build on the shared result helpers added to `Tool` (`baseArgv`,
`argv:appendEach:`, `resultForArgv:command:failureOutcome:`, `stdoutOf:`,
`succeeded:`, `missingToolResult:`). Every core message returns the same
versioned envelope:

```json
{"schema_version":1,"tool":"cue","command":"vet","outcome":"validation_error",
 "exit_code":1,"stdout":"","stderr":"port: conflicting values ..."}
```

Convenience messages (`export:`, `list`, `current:`, `envValue:`) project the
envelope to plain stdout so they compose in Bash command substitution.

This document collects ways the two tools could pull their weight inside
Trashtalk. Nothing here is committed to; it is a menu.

---

## Why these two tools fit

Trashtalk carries almost all of its structured state as JSON text: instance
variables in SQLite, result envelopes from tools and agents, proposals, and
snapshots. It has no schema language of its own, and validation today is ad hoc
jq predicates scattered through raw methods. CUE is a schema-and-data language
whose CLI reads JSON on stdin and returns crisp exit codes and diagnostics. That
is exactly the boundary Trashtalk already knows how to talk to.

Trashtalk also depends on a handful of external binaries (`jq`, `jo`,
`sqlite3`, `uuidgen`, optionally `axe`, `codex`, `tmux`, the Innards tools) and
increasingly runs agents against other people's repositories. mise is the
standard answer to "which versions of which tools does this directory want, and
what environment should commands run under". It also happens to be a task
runner with a JSON-listable task graph.

---

## Part 1: CUE

### 1.1 Schemas for persisted instances

Every class already carries a JSON default template and its instance data lives
in SQLite as JSON. A class could optionally ship a CUE definition next to its
`.trash` source:

```cue
// trash/schemas/Counter.cue
#Counter: {
    value: int & >=0 | *0
    step:  int & >0  | *1
}
```

Ideas that fall out of this:

- **`Object validate`**: a base-class message that runs
  `@ Tools::Cue vet: schemaFile json: (@ Environment get: id)` and returns the
  envelope. Instances can check themselves before a risky mutation, and
  `Persistable` could refuse to store an instance that fails its schema.
- **Schema-driven defaults**: `unify:json:` fills CUE defaults. `Object new`
  could hand a partial JSON object to CUE and let it produce the fully
  defaulted instance, replacing the template merge for classes that opt in.
- **Schema migration**: `tests/test_schema_migration.bash` already exercises
  changing instanceVars over time. A CUE definition per schema version lets the
  runtime detect "this stored instance no longer satisfies the current
  definition" instead of silently reading missing fields as empty.
- **Inheritance as unification**: CUE `#Child: #Parent & {...}` mirrors
  Trashtalk subclassing. Generating the CUE hierarchy from class metadata
  (`__Class__parent`, ivar defaults) is a small compiler extension.

### 1.2 Contracts for tool and agent result envelopes

`Tool`, `Tools::Axe`, `Tools::Codex`, `SourceProposal`, and
`ObjectEditProposal` all emit `schema_version: 1` envelopes, and consumers
`jsonUnpack:` the fields they expect. A single `docs/schemas/results.cue` could
define each envelope once:

```cue
#ToolResult: {
    schema_version: 1
    tool:           string
    command:        string
    outcome:        "success" | "missing_tool" | "validation_error" |
                    "evaluation_error" | "command_error" | "task_error" |
                    "process_error"
    exit_code:      int & >=0 & <=255
    stdout:         string
    stderr:         string
}
```

- **Test assertions**: `TestCase` could gain `assert: json matches: '#ToolResult'`
  that shells out to `cue vet`, so the tests for Axe, Codex, Cue, and Mise stop
  re-asserting field-by-field.
- **Agent output gating**: `Agent`/`AgentSession` receive LLM output that is
  supposed to be JSON of a known shape. Vetting it against a CUE definition
  before acting is a stronger check than `String isJson:` and yields
  human-readable diagnostics to feed back to the model on retry.
- **Cross-language contracts**: the Innards tools (`indiff`, `inpick`,
  `inpage`, `ininspect`) speak JSON on stdin/stdout to Trashtalk. Sharing one
  CUE file between the two repositories makes the wire format versioned and
  testable on both sides.

### 1.3 Configuration and the `.trashrc`

Trashtalk reads shell-style configuration (`TRASHTALK_AGENT_BACKEND`,
`TRASHTALK_VALUE_SEND`, test knobs). A CUE-checked config could:

- validate `.trashrc`-style settings exported as JSON (`@ Env allWithPrefix:
  'TRASHTALK_'` is already there) against a definition of known keys and
  allowed values, and produce a `doctor` finding for typos;
- render the same config to YAML or JSON for other tools with `convert:from:to:`;
- describe `axe/agents/*.toml` (tool allowlists, read-only constraints) so the
  test that currently greps for `write_file|edit_file|run_command` becomes a
  schema assertion.

### 1.4 Kube package: typed snapshots and policy

`Kube::Resource` stores raw `kubectl` JSON and `Kube::Snapshot` captures sets of
them. CUE has first-class Kubernetes usage:

- **Policy as schema**: "every Deployment must set resource limits" is a CUE
  constraint; `@ Tools::Cue vet: policyFile json: podJson` turns
  `Kube::Diff` into a policy checker, not only a change lister.
- **Config generation**: CUE can export manifests from a small set of inputs;
  a Trashtalk class could own the inputs (environment, replica count) and ask
  CUE for the concrete YAML to apply.
- **Snapshot normalisation**: unifying a resource with a "projection" schema
  (only the fields we care about) before persisting gives stable diffs that
  ignore `resourceVersion`, `managedFields`, and timestamps.

### 1.5 Data plumbing utilities

Small but immediately useful:

- `@ Tools::Cue convert: yaml from: 'yaml' to: 'json'` gives Trashtalk a YAML
  reader without adding `yq` as a dependency.
- `cue export --out text` templates strings from data; a `Template` helper
  could build on it.
- `cue def` renders a schema for documentation; the browser/inspector classes
  could show "what is this class's contract" alongside its methods.

### 1.6 Longer range: CUE as an IDL for Trashtalk protocols

`Protocol` declares required selectors. CUE could additionally declare the
argument and result shapes of those selectors, giving the compiler a place to
emit runtime checks (`method: valueWith: data` requires `data` to match
`#Event`). This is the point where CUE stops being a tool and becomes part of
the language surface, and it is worth prototyping on one protocol before
generalising.

---

## Part 2: mise

### 2.1 `make doctor` and dependency management

Trashtalk requires `jq`, `jo`, `sqlite3`, and `uuidgen`, and `Tool ensure`
tries to `brew install` things. A checked-in `mise.toml` would pin the versions
the test suite is known to pass with:

```toml
[tools]
jq = "1.7"
go = "1.23"       # for cue / axe via go install
"go:cuelang.org/go/cmd/cue" = "latest"

[env]
TRASHTALK_TEST_JOBS = "4"
```

- **Reproducible environments**: `@ Tools::Mise installAll` in a session
  start hook, CI, or `make doctor` replaces platform-specific install commands.
- **`Tool installCommand` fallback**: the base class could try
  `@ Tools::Mise install: toolName` before the brew/apt string, since mise
  knows how to fetch most CLIs from its registry, GitHub releases, `go`,
  `cargo`, `npm`, and `pipx`.
- **`doctor` output**: `@ Tools::Mise doctor` and `@ Tools::Mise list` give the
  existing diagnostics command a structured inventory of installed versions.

### 2.2 Running agents against other projects

`Agent`, `AxeAgent`, and `CodexAgent` run one-shot LLM commands in a working
directory. That directory has its own toolchain expectations, and today the
agent inherits whatever is on the caller's PATH.

- **Correct tools per repository**: `@ Tools::Mise execArgv: argv inDirectory:
  repo` runs a command with the versions that repository pins, without
  activating mise in the calling shell. A `Process`/`Shell` option
  `withMiseFor: directory` could route every agent-spawned command through it.
- **Environment discovery**: `@ Tools::Mise envInDirectory: repo` returns the
  project's `[env]` as JSON, which can be handed to the agent as context
  ("this project sets DATABASE_URL, RAILS_ENV") or used to populate an
  `Env`-like object for the session.
- **Trust as a gate**: mise refuses to load untrusted config. Surfacing
  `trust:` as an explicit, logged step fits the existing "never modify user
  configuration silently" stance of the Axe and Codex wrappers.

### 2.3 mise tasks as a project task graph

`mise tasks ls --json` returns names, descriptions, sources, and dependencies.
That is a ready-made task model.

- **`Project` class**: an object per directory that answers `tasks`,
  `runTask:`, `env`, and `tools`, backed by `Tools::Mise` and persisted like
  any other instance. Agents get a uniform "what can I run here" API across
  repositories that use mise, instead of guessing at Makefiles and package
  scripts.
- **Innards picker integration**: feed the task list to `Tools::Inpick` for an
  interactive "run a task" command inside the ambient session.
- **Task results as envelopes**: `runTask:` already returns `{outcome,
  exit_code, stdout, stderr}`. `Scheduler` and `Future` could run tasks
  asynchronously and post the envelope to an `Inbox` or `EventBus` when done.
- **Trashtalk's own Makefile**: the `make verify`, `make bench`, and `make
  test-serial` targets could be mirrored as mise tasks with `depends`, which
  makes them discoverable to agents via the same `tasks` message that works on
  any other project.

### 2.4 Per-session toolchains and reproducible benchmarks

`docs/performance.md` reports measured process counts and timings. Those
numbers depend on the `jq` and `bash` versions in play.

- Record `@ Tools::Mise list` (versions and install paths) alongside benchmark
  output so results are comparable across machines.
- Run `make bench` under `@ Tools::Mise exec: 'jq@1.6' argv: ...` versus
  `'jq@1.7'` to measure the effect of a dependency upgrade without touching the
  system.

### 2.5 Environment as data

`Env` exposes the process environment; mise exposes the *intended* environment
for a directory. Combining them:

- `envValue:` gives Trashtalk code a way to read project secrets or endpoints
  declared in `mise.toml` without requiring the user to have activated mise in
  their shell.
- `envFor: 'bash'` returns export lines that `Shell` could evaluate with
  `pragma: direct` to activate a project inside the current runtime, which is
  the mise equivalent of `source .env`.

---

## Part 3: Using them together

- **Validated project manifests**: a CUE schema for `mise.toml` (tool names,
  version syntax, task shape) lets Trashtalk vet a project's mise config before
  trusting it. `convert:from:to:` handles the TOML-to-JSON step if CUE's TOML
  reader is used, or `mise config ls --json` provides the already-parsed view.
- **Schema-checked task results**: mise task output that is expected to be
  JSON (a test report, a build manifest) gets vetted against a CUE definition
  before Trashtalk stores it as a snapshot.
- **Bootstrapping**: `mise.toml` pins `cue` itself. `@ Tools::Mise installAll`
  followed by `@ Tools::Cue vet:` is a two-message setup for any of the schema
  work in Part 1, with no reliance on `go install` being on PATH.
- **Agent harness recipe**: for a repository the agent has not seen before,
  `Tools::Mise` reports the tools, environment, and tasks; `Tools::Cue` checks
  the agent's structured plan or edit proposal against the repository's
  declared contracts; the existing `SourceProposal` gate applies the change.

---

## Suggested first steps

1. Add `mise.toml` to this repository pinning `jq`, `jo`, `sqlite3`, `go`, and
   `cue`, and teach `make doctor` to call `@ Tools::Mise doctor`.
2. Write `docs/schemas/results.cue` for the `Tool` envelope and add a
   `TestCase` assertion that uses `Tools::Cue vet:json:`; convert the Cue and
   Mise tests to use it.
3. Prototype `Object validate` on one class (`Counter`) with a hand-written
   CUE definition, then decide whether generating definitions from class
   metadata is worth a compiler pass.
4. Prototype a `Project` class over `Tools::Mise` and wire it into
   `AxeAgent`'s working-directory context so agents see project tasks and
   environment.
