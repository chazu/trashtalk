# Innards as an Ambient UI/IDE Layer for Trashtalk: Revised Plan

*Implementation plan, September 2026. This is a companion to
`docs/innards-ambient-ide.md`: the original remains the exploratory design and
inventory; this document revises the execution plan.*

## 1. Decision summary

Proceed with Innards as Trashtalk's inline UI layer, with four boundaries:

1. **Innards owns terminal presentation.** It draws bounded inline surfaces,
   reads `/dev/tty`, and reports structured outcomes on stdout.
2. **Trashtalk owns the model and workflow.** Classes, source, compilation,
   tests, objects, and events remain accessible through message sends.
3. **External CLIs own the LLM harness.** Trashtalk wraps either the
   [Axe CLI](https://github.com/jrswab/axe) or the official Codex CLI instead of
   implementing providers, authentication, retry policy, token budgets,
   memory, tool loops, or sub-agent delegation itself.
4. **The DSL is the default implementation language.** Raw methods are allowed
   for real process, TTY, file-descriptor, trap, and shell boundaries. Domain
   decisions and orchestration remain in `method:`/`classMethod:` wherever
   possible. Repeated raw work is a signal to improve the DSL or a reusable
   primitive.

Trashtalk is Bash-only. Native Procyon compilation and Bash/native parity are
not design constraints.

The first tracer bullet remains the annotated edit loop. The first agent slice
is much smaller than the original proposal: a read-only, one-shot `@@` command
implemented over a selectable CLI backend, with an Innards view for the result.

## 2. Baseline

### 2.1 Trashtalk

Trashtalk already provides:

- a jq-based compiler with located AST nodes and an AST cache;
- reflection and source lookup through `Trash` and `Runtime`;
- an edit, compile, Bash-syntax-check, reload, and test loop;
- persistent objects and event/concurrency abstractions;
- `Tool` and `Process` abstractions over external commands;
- preliminary `Agent`, `ClaudeAgent`, and `AgentSession` classes; and
- `@@`, currently a stub in the Bash runtime.

The current `Trash edit:` loop demonstrates the value of the first slice, but
also its debt: it detects edits with mtimes, writes compiler failures into the
source as comments, contains editor-specific line-jump logic, and then removes
the injected comments before compiling.

The current agent classes are tmux automation around an interactive provider
CLI. They can send keystrokes but cannot reliably receive structured results.
They should not be extended into an in-process LLM harness.

### 2.2 Innards

The `chazu/innards` checkout at `~/dev/rust/innards` is currently at
`25de24d`. It contains:

- `inmacs`, an inline Emacs-style editor;
- `inpage`, a read-only inline pager;
- `navsplat`, a Rust/rust-analyzer symbol picker; and
- reusable editor, picker, preview, navigation-stack, and inline-terminal code.

The important invariant is that an Innards surface occupies a temporary region
below the prompt without entering the alternate screen or destroying existing
scrollback.

### 2.3 Axe

Axe is a small Unix-style LLM harness. Its current documented surface includes:

- `axe run <agent>` with a prompt or piped stdin;
- project-local agent discovery under `axe/agents/`;
- JSON output and meaningful exit codes;
- multiple providers, budgets, retries, skills, memory, and sub-agents;
- optional file, command, web, and MCP tools; and
- a dry-run mode for inspecting resolved context without making an LLM call.

Those capabilities are owned by Axe. Trashtalk should depend only on a small,
versioned CLI contract and must continue to work when Axe is absent.

Installation and credential setup are explicit user operations, separate from
the integration.

### 2.4 Codex

The official Codex CLI provides a non-interactive `codex exec` surface and can
authenticate through a ChatGPT subscription. The Trashtalk adapter is a second
one-shot backend, not a replacement provider implementation: it supplies
context on stdin, captures only the final answer, and pins execution to an
ephemeral read-only sandbox. It requires `codex login status` to identify a
ChatGPT login and removes API-key variables from the child environment.

## 3. Responsibility map

| Concern | Owner |
|---|---|
| Inline rendering, keys, selection, annotations | Innards |
| Classes, methods, source, objects, compilation, tests | Trashtalk |
| External program invocation | Trashtalk `Tool`/`Process` boundary |
| Model providers, authentication, prompts, tool loop, retries, budgets | Axe or Codex CLI |
| Human approval of proposed edits or commands | Innards + Trashtalk |
| Applying accepted source changes and running gates | Trashtalk |
| Long-lived domain events | Trashtalk streams/events |
| LLM memory | External harness; do not duplicate transcripts in Trashtalk |

The composition is deliberately process-oriented:

```text
shell / trash REPL
  -> pure Trashtalk facade and workflow
      -> narrow Tool/Process shell boundary
          -> inmacs / inpage / inpick / indiff / Axe / Codex
              UI: /dev/tty
              data: stdin
              result: stdout JSON
              diagnostics: stderr
```

No daemon, FFI, or shared Rust library is required across repositories.

## 4. DSL-first integration policy

New integration code follows this decision sequence:

1. Express the behavior as ordinary Trashtalk messages.
2. Reuse `Tool`, `Process`, `File`, `String`, `Array`, and other existing
   abstractions.
3. If the remaining problem is genuinely an OS boundary, add one small raw
   method at that boundary and expose a DSL-facing method above it.
4. If substantial control flow, parsing, quoting, or data transformation would
   be raw, stop and assess a DSL/compiler enhancement.
5. If the same raw pattern appears a second time, consolidate it rather than
   copying Bash.

Examples of justified raw boundaries include opening `/dev/tty`, preserving a
child process's exit status while capturing stdout, installing a shell trap,
and performing file-descriptor redirection. Choosing which editor to use,
interpreting an editor outcome, converting diagnostics, choosing whether to
compile, and selecting an agent workflow are domain logic and should be DSL.

The existing string-based `Tool run:`/`Shell exec:` path eventually reaches
`eval`. Do not construct new command templates by interpolating source paths,
symbols, prompts, or object values. Add an argv-preserving process operation or
otherwise pass untrusted data through stdin/files. This is both a safety fix
and a useful DSL primitive.

## 5. Shared process contract

All drivable Innards surfaces should follow one contract:

- open `/dev/tty` themselves for input and rendering;
- reserve stdin for data and stdout for results;
- send human/debug diagnostics to stderr;
- emit versioned JSON or JSON Lines rather than delimiter-encoded records;
- distinguish cancellation from failure; and
- never emit terminal escape sequences on stdout.

Every record includes `schema_version: 1`. File positions are one-based and use
`line`, `column`, `end_line`, and `end_column` consistently.

### 5.1 Editor outcome

`inmacs --result-json` emits exactly one record:

```json
{
  "schema_version": 1,
  "outcome": "saved",
  "path": "trash/Counter.trash",
  "changed": true,
  "cursor": {"line": 12, "column": 4},
  "edit_count": 3
}
```

`outcome` is one of:

- `saved`: changed content was saved;
- `unchanged`: the editor closed without a content change;
- `discarded`: dirty content was explicitly discarded; or
- `cancelled`: the operation was cancelled before editing.

Exit codes communicate process-level success or cancellation; callers use the
record for workflow decisions. Suggested codes are 0 for `saved`/`unchanged`,
3 for `discarded`/`cancelled`, and 1 for operational errors.

### 5.2 Diagnostics

Diagnostics are JSON Lines or a JSON array with:

```json
{
  "schema_version": 1,
  "source": "trashtalk-compiler",
  "severity": "error",
  "message": "unterminated string literal",
  "path": "trash/Counter.trash",
  "line": 14,
  "column": 8,
  "end_line": 14,
  "end_column": 12
}
```

The compiler/test adapter should produce this schema once. Innards renders it;
it does not parse compiler or TAP text itself.

### 5.3 Atomic save

Atomic saving must define more than temporary-file-plus-rename:

- create the temporary file beside the target;
- preserve target permissions;
- detect a target changed since it was opened and ask before overwriting;
- define symlink behavior explicitly;
- flush the temporary file before rename; and
- leave the original untouched on cancellation or failure.

Extended attributes and ownership should be tested on supported platforms and
documented if they cannot be preserved.

## 6. Innards work

### 6.1 Foundation

Refactor `inmacs` and `inpage` around a public configuration and result API:

```rust
innards::inline_text::{Config, Editor, Outcome, run_with}
```

Move argument parsing into the binaries, use `clap` consistently, and add:

- `--column`, `--title`, `--status`, and `--tab-width`;
- `--syntax`;
- `--annotations`;
- stdin input and explicit output destinations;
- `--result-json`; and
- atomic/conflict-aware save behavior.

Add a Trashtalk syntax definition, two-space indentation, and copy-current-
indentation-on-Enter. Highlighting is presentation only; the jq tokenizer and
parser remain authoritative.

### 6.2 Generic picker

Extract the reusable picker from `navsplat` as `inpick`. Prefer JSON Lines
candidate records:

```json
{
  "schema_version": 1,
  "id": "Array>>at:put:",
  "path": "trash/Array.trash",
  "line": 49,
  "column": 2,
  "label": "Array>>at:put:",
  "kind": "method",
  "detail": "instance method"
}
```

Do not expose `cmd:TEMPLATE` substitution as the primary extension mechanism.
For the first version, pipe a complete candidate set into `inpick` and use file
previews. If dynamic side panes later prove necessary, define a structured
provider protocol that passes the selected record as JSON to an executable
with a fixed argv.

Keep the existing LSP behavior behind a provider interface so `navsplat`
becomes a thin Rust-oriented entry point without making Trashtalk implement
LSP.

### 6.3 Additional surfaces

Build additional binaries only behind demonstrated workflows:

- `indiff`: review a proposal and return accepted hunks or a resulting file;
- `ininspect`: navigate JSON/object records and return explicit edit proposals;
- `intail`: inspect bounded live output while it owns the terminal;
- `inprompt`: collect or confirm input.

Use an `in-`/`in*` name for the inspector rather than the globally generic
`inspect` binary name.

None of these surfaces directly executes callback strings or applies domain
changes. They return selections and proposals to Trashtalk.

## 7. Trashtalk IDE API

### 7.1 Tool wrappers

Add small wrappers under `Tools`:

- `Tools::Inmacs`
- `Tools::Inpage`
- `Tools::Inpick`
- later, `Tools::Indiff`, `Tools::Ininspect`, `Tools::Intail`, and
  `Tools::Inprompt`

Each wrapper declares the executable name and translates a typed Trashtalk
request into the shared process contract. Normal control flow and result
interpretation remain pure DSL. A single reusable raw process method owns TTY
and descriptor handling if the DSL cannot express it.

`Trash doctor` reports Innards as optional. Missing tools fall back to
`$VISUAL`/`$EDITOR`, `${PAGER:-less}`, `fzf`, or plain terminal input as
appropriate.

Do not automatically page all long dispatcher output in the first slice: that
can change pipeline behavior and surprise scripts. Add explicit `Console page:`
and evaluate automatic paging separately using the condition that both stdin
and stdout are interactive terminals.

### 7.2 Annotated edit loop

Refactor the current edit loop into DSL-sized operations:

- `sourceFileFor:`
- `editFile:diagnostics:line:`
- `compileClass:`
- `diagnosticsFromCompileResult:`
- `reloadCompiledClass:`
- `runTestsFor:`
- `diagnosticsFromTestResult:`

The workflow is:

```text
locate source
  -> open editor with current diagnostics
  -> unchanged/cancelled? stop
  -> compile to a temporary artifact
  -> Bash syntax check
  -> install compiled artifact and reload
  -> run class tests
  -> success: finish
  -> failure: convert diagnostics and reopen editor
```

The source file is never modified to carry diagnostics. Installation of a
compiled artifact remains transactional: do not replace the last working
artifact until compilation and `bash -n` pass.

### 7.3 Symbols and browsing

Generate the first symbol index directly from the cached compiler AST. Method
locations already exist; verify their accuracy rather than adding a second
location field. Do not add a SQLite index until measurement shows parsing the
existing cache is too slow.

`implementorsOf:` may compare normalized selectors. `sendersOf:` must analyze
parsed message sends, especially multi-part keyword selectors; token equality
is insufficient.

The first browser supports:

- classes, traits, methods, and instance variables;
- file previews;
- opening the selected source location; and
- implementors and senders once their queries are semantically correct.

Selection and preview records remain JSON throughout the integration.

## 8. Agent harness integration

### 8.1 Layer one: `Tools::Axe`

`Tools::Axe` is a faithful wrapper around the external CLI. It owns no agent
policy. Its initial surface should include:

- `name` -> `axe`;
- `isInstalled` and `version`;
- `run:prompt:workingDirectory:`;
- `run:input:workingDirectory:`;
- `dryRun:prompt:workingDirectory:`; and
- access to the original Axe exit code and JSON output.

Arguments must be passed as argv and prompts/context through stdin where
possible. Do not interpolate prompts into a command string.

Preserve Axe's documented exit distinctions: success, runtime error,
configuration error, provider/network error, and budget exhaustion. The facade
may explain them but must not collapse them into one generic failure.

Installation is explicit. `ensure` may explain how to install Axe, but invoking
`@@` must not silently install a binary or initialize credentials.

### 8.2 Layer two: agent facade

Put convenience and Trashtalk vocabulary one layer above the Tool wrapper. The
exact class name can be settled during implementation; the intended split is:

```text
Tools::Axe       exact process/CLI adapter
AxeAgent         named Axe agent/configuration adapter
Tools::Codex     exact subscription-backed Codex CLI adapter
CodexAgent       Codex context adapter
Agent            configured convenience facade used by @@
```

The facade provides messages such as:

- `ask:` using the configured one-shot backend;
- `run:withInput:` for a named specialized agent;
- `review:` for a read-only review workflow; and
- `dryRun:` to show resolved context before an LLM request.

Configuration uses a project-local `axe/agents/` directory where practical.
Trashtalk stores only references needed for its UI—run id, agent name, status,
timestamps, and result location. Each external harness owns its model
configuration and memory. The existing tmux-oriented `ClaudeAgent` path can be
deprecated after the one-shot paths work; it need not be preserved as their
execution model.

### 8.3 Minimal `@@`

The first useful `@@` behavior is deliberately small:

```text
@@ "why did this command fail?"
  -> Agent ask:
  -> AxeAgent or CodexAgent
  -> exact external CLI invocation with explicit context on stdin
  -> normalize final result and exit status
  -> show final text in an Innards pager
  -> leave final answer in scrollback
```

Context is explicit: the user's message, current directory, last command status,
and `$__` when available. Do not install a global `DEBUG` trap or capture all
shell history in the first version.

Axe's documented JSON mode is a result envelope, not a guaranteed streaming
event protocol. The first slice therefore does not pretend to stream tokens.
If Axe later exposes stable streaming JSON, `intail` can render it. Otherwise a
small progress indicator followed by the final result is sufficient.

### 8.4 Read-only safety profile

The first project-local Axe agent enables no mutation or shell-execution tools.
It receives context through stdin and may use only explicitly selected read-only
capabilities. In particular:

- do not enable Axe's `write_file`, `edit_file`, or `run_command` tools;
- set the working directory explicitly;
- use `axe run <agent> --dry-run` to inspect resolved context during development;
- do not log provider credentials or the expanded environment; and
- do not expose a generic Trashtalk `send` tool initially.

File tools being rooted to a working directory is useful, but it is not an
approval system. Any later mutation path must add an explicit proposal and
human-review boundary.

### 8.5 Subscription-backed Codex profile

`TRASHTALK_AGENT_BACKEND=codex` selects `CodexAgent`. `Tools::Codex` invokes
`codex exec` with an ephemeral session, ignored user tool configuration, an
explicit working directory, and a read-only sandbox. The adapter accepts only
a ChatGPT-authenticated CLI status and removes API-key variables from the child
environment, preventing an accidental switch to per-token API billing. Its
dry-run mode is local: it returns the exact argv and stdin without starting
Codex.

### 8.6 Proposed changes

After read-only `@@` is proven, add one proposal workflow:

1. A specialized Axe agent returns a versioned JSON result containing a unified
   diff and the hash of every source file it was based on.
2. `indiff` presents the proposal without applying it.
3. Trashtalk verifies that source hashes still match.
4. Accepted hunks are applied to a temporary copy.
5. `.trash` changes pass the normal compile, syntax, reload, and test gate.
6. Only a successful candidate replaces the working source/artifact; failures
   reopen `inmacs` with diagnostics or leave the proposal unapplied.

Command proposals remain text returned to the user. Pre-filling a Readline
buffer is optional and must never synthesize an Enter key.

### 8.7 Later capabilities

Only add these after the basic wrapper is useful:

- Axe memory-backed conversations;
- background runs and result notifications;
- Trashtalk event publication for run lifecycle;
- specialized review/test agents;
- a narrow read-only Trashtalk MCP server; and
- opt-in shell-history capture with redaction and retention rules.

If an MCP server is added, use specific capability tools such as `source`,
`symbols`, and `methods`, not a generic message-send escape hatch. Compilation
and tests are potentially mutating/side-effecting and require a separate policy.

## 9. Revised milestones

### Milestone 0: contracts and terminal proof

**Innards**

- Implement the shared `/dev/tty` plus stdout JSON contract in a small spike.
- Prove cancellation, exit codes, signal cleanup, and scrollback restoration.
- Add PTY-level tests and manually exercise direct shell and tmux use.

**Exit:** a caller can pipe input to an Innards binary, interact on the terminal,
capture one valid JSON result, and recover a usable terminal after success,
cancellation, or a signal.

**Status (2026-09-03): complete in the Innards worktree.** The standalone
contract has PTY coverage for piped input, clean stdout JSON, normal close,
dirty-edit discard, Ctrl-C, SIGTERM, exit codes, and terminal-mode restoration.
Direct-shell and tmux exercises also passed, including inline viewport cleanup
that retained the pre-launch tmux scrollback.

### Milestone 1: annotated editor vertical slice

**Innards**

- Shared CLI/config/result API for `inmacs` and `inpage`.
- Outcome enum, conflict-aware atomic save, annotations, syntax selection,
  Trashtalk grammar, and two-space indentation.

**Trashtalk**

- Minimal reusable TTY/process boundary.
- `Tools::Inmacs` and `Tools::Inpage`.
- DSL-first annotated `Trash edit:` workflow and `$EDITOR` fallback.
- Optional Innards line in `Trash doctor`.

**Exit:** edit success, unchanged exit, explicit discard, compiler failure, Bash
syntax failure, test failure, missing-Innards fallback, and terminal recovery
are all tested. The source never contains generated diagnostic comments.

**Status (2026-09-03): complete in both worktrees.** Innards now exposes the
shared configuration/result API, versioned annotations, conflict-aware atomic
saves, stdin-to-explicit-output editing, and bundled Trashtalk presentation.
Trashtalk has exact-argv `Tools::Inmacs`/`Tools::Inpage` wrappers and a
DSL-first edit transaction with narrow compiler, Bash-validation, install, test,
and fallback boundaries. Automated coverage includes all listed outcomes plus
namespaced classes; a real PTY exercise also completed edit, save, compile,
reload, and test with terminal cleanup.

### Milestone 2: browser

- JSONL `inpick` and a reusable provider interface.
- AST-derived symbol records with no new persistent cache initially.
- Class/method picker with source preview and open-at-location.
- Correct implementors query, followed by parsed senders analysis.

**Exit:** a user can browse and open every class/method category represented in
the AST, including namespaced classes and multi-keyword selectors.

Implemented: Innards now provides a JSONL `inpick` surface backed by a reusable
provider API and the same guarded inline terminal foundation as the editor and
pager. Trashtalk derives classes, traits, instance/class variables, and every
method category from cached jq-compiler ASTs, preserves complete keyword
selectors, previews and opens exact source locations, and provides parsed
implementors and senders queries. Persisted instances are also available as
structured picker records, with an `fzf` fallback when `inpick` is absent.
Automated coverage includes namespaced symbols, source positions, raw/DSL/test
methods, false-positive sender fragments, JSON-preserving selection, fallback
selection, instance data, and PTY terminal restoration.

### Milestone 3: one-shot `@@` backends

- `Tools::Axe` with typed invocation, JSON parsing, dry-run, and exit mapping.
- One project-local read-only Axe agent configuration.
- `Agent`/`AxeAgent` convenience layer.
- `@@` one-shot question and final-result presentation through Innards.
- Clear missing-binary and missing-configuration guidance.

**Exit:** a dry run proves the exact context; a live run returns a parsed result;
provider, configuration, and budget failures remain distinguishable; no Axe
mutation or command tool is enabled.

**Status (2026-09-03): implemented and deterministically verified.**
`Tools::Axe` now invokes exact argv with context on stdin, preserves Axe's
documented exit distinctions, validates successful JSON, and returns a stable
Trashtalk envelope. `AxeAgent` supplies a versioned context containing the
question, explicit working directory, prior status, and `$__`; `Agent` owns
answer/error presentation; and `@@` routes the final text through `inpage` plus
scrollback. The checked-in `trashtalk-readonly` agent enables only
`list_directory` and `read_file`. Current Axe omits resolved dry-run context
from its JSON encoding, so this adapter intentionally captures the complete
human-readable `--dry-run` output instead of combining `--dry-run` with
`--json`. Fake-process coverage proves exact context, parsed live-shaped
results, malformed output handling, missing-tool guidance, and exit codes
0–4. A credentialed provider call remains an environment acceptance check,
not an automated repository test.

**Codex extension (2026-09-04): implemented and deterministically verified.**
`Tools::Codex` wraps the official non-interactive CLI contract and normalizes
its final stdout into the same run envelope. `CodexAgent` reuses the common
versioned context, while `Agent` selects `axe` or `codex` through
`TRASHTALK_AGENT_BACKEND`. The Codex path is ephemeral and read-only, ignores
user tool configuration, requires ChatGPT authentication, and strips API-key
variables before execution. Fake-process coverage proves exact argv/stdin,
backend selection, local dry runs, auth rejection, missing-tool behavior, and
`@@` presentation. A live subscription-backed response remains an environment
acceptance check.

### Milestone 4: review surface and proposals

- `indiff` as a presentation-only review surface.
- Versioned proposal schema with source hashes.
- Apply-to-temporary-copy and compile/test gate.
- No direct execution of agent-authored commands.

**Exit:** stale proposals cannot overwrite newer work, rejected hunks make no
changes, and accepted `.trash` changes either pass the full gate or leave the
working source/artifact recoverable.

**Status (2026-09-03): implemented and deterministically verified.** Innards
now provides `indiff`, a presentation-only unified-diff surface whose only
outputs are versioned accept, reject, and cancel decisions. Trashtalk adds a
closed, single-file SourceProposal v1 schema, a read-only Axe proposer profile,
and a separate explicit review/apply transaction. The transaction validates
repository-relative paths and matching diff headers, checks the base SHA-256
after review and immediately before commit, patches a private copy, compiles
with the jq compiler, validates generated Bash, runs tests from the candidate
artifact, and installs source plus artifact with rollback backups. Automated
coverage proves rejection and cancellation make no changes, stale proposals
cannot overwrite concurrent edits, malformed schemas/reviewer output and
command fields fail closed, compile/test failures preserve both files, and a
passing accepted proposal reloads successfully. Version 1 intentionally limits
each proposal to one file; multi-file atomicity is deferred until a workflow
proves it necessary.

### Milestone 5: optional ambient features

- `intail`, `ininspect`, and `inprompt` only as required by proven workflows.
- Run lifecycle events and a prompt-adjacent status line.
- Background harness jobs and unread-result picker.
- Optional read-only MCP tools.

This milestone is intentionally not required for the IDE or `@@` to be useful.

**Status (2026-09-03): partially activated by an observed workflow gap.** The
instance browser exposed full UUIDs and raw Runtime JSON in a one-line fallback,
making the useful ivar state unreadable. Instance records now use compact
identities and ivar summaries, and `ininspect` provides nested JSON navigation
plus typed scalar edit proposals. Trashtalk validates a closed result schema,
the target identity, the original state, and the selected old value before
applying a proposal; Runtime metadata is view-excluded and Innards never
mutates the object. The other ambient surfaces, background jobs, prompt hook,
events, and MCP boundary remain inactive until independently justified.

## 10. Verification matrix

| Boundary | Automated proof | Human acceptance |
|---|---|---|
| Innards terminal | PTY tests for output separation, resize, cancellation, signal cleanup | direct shell, tmux, SSH, rlwrap |
| Saving | unchanged/discard/conflict tests; permissions and symlink cases | edit a real class without scrollback damage |
| Diagnostics | schema/parser/render/navigation tests | compile and test failures are understandable in place |
| Trashtalk integration | compiler tests and edit-loop integration tests | `$EDITOR` fallback and hot reload |
| Picker | JSONL/property tests; namespaced and keyword-selector fixtures | browse a representative image quickly |
| Object inspector | tree/result unit tests; PTY restoration; stale/malformed proposal integration tests | inspect and edit a nested real object in a shell |
| Axe wrapper | fake `axe` executable covering exit codes and malformed JSON | `axe run <agent> --dry-run`, then one credentialed run |
| Codex wrapper | fake `codex` covering exact argv/stdin, auth gate, failures, and dry-run isolation | `codex login status`, then one subscription-backed `@@` |
| Proposal gate | stale-hash, reject, partial accept, failed compile/test tests | review one nontrivial change without hidden writes |

Tests that need a provider, SSH host, or human terminal remain acceptance gates;
they are not implied by unit-test success.

## 11. Explicit non-goals

The revised plan does not initially:

- build an LLM provider adapter in Trashtalk;
- parse provider-specific streaming formats;
- maintain a second copy of an external harness's conversation memory;
- capture every shell command;
- give an agent a generic message-send or shell-execution capability;
- run a permanent Innards daemon;
- make all long `@` output page automatically; or
- require every proposed Innards surface before shipping the editor/browser.

## 12. Resolved implementation decisions

Milestone 1 resolved:

- The reusable exact-argv/stdin boundary lives on `Tool`.
- `inmacs` rejects editable symlinks and uses conflict-aware atomic replacement.
- The crate remains named `navsplat` while the public binaries carry the
  Innards surface names.

Milestone 3 resolved:

- `trashtalk-readonly` is the checked-in default `@@` profile.
- `TRASHTALK_AGENT_BACKEND` selects `axe` (default) or `codex`.
- The Codex backend requires ChatGPT authentication and strips API-key
  variables before an ephemeral read-only run.
- Legacy `AgentSession` code remains available but outside the one-shot path.
- Current Axe JSON maps through `content`; dry-run context uses Axe's complete
  human-readable representation.

Milestone 4 resolved:

- SourceProposal v1 is a closed single-file schema applied with `patch`.
- `indiff` records explicit per-hunk choices.
- No accepted hunk reaches the working tree until every candidate gate passes;
  the structured `indiff` decision is the required confirmation.

Milestone 5 first optional slice resolved:

- `ininspect` receives only declared ivar state and returns typed leaf-edit
  proposals; it does not execute callbacks or mutate objects.
- Trashtalk owns optimistic validation and Runtime mutation.
- Cross-object reference following remains deferred rather than accepting a
  configured command template.

## 13. Recommended next action

Keep provider credentials and installed binaries outside repository tests.
The remaining acceptance work is operational: install the verified Innards
checkout, inspect and edit one nested real object, run one live `@@` through
each configured backend, and review one nontrivial source proposal in a real
shell.
Only add another Milestone 5 slice in response to a separate observed gap.
