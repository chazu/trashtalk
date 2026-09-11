# Code and session Tool adapters

**Status:** Current CLI adapters; persistent memory and a Chad session driver remain future work.

`Tools::Roam`, `Tools::AstGrep`, `Tools::Cass`, and `Tools::Chad` are available
through ordinary class messages. Their methods build exact argument arrays in
the DSL. `Tool` supplies process capture and child-only directory selection;
the `Tools::JsonOutput` trait handles structured CLI results.

These are executable adapters. Repository/local-copy/worktree domain objects,
shared memory, and a persistent Chad session driver remain separate work under
the [delegation plan](agent-delegation-implementation.md).

## Setup and result contract

Build with `make bash`, then use a fresh Trashtalk shell. In an existing shell,
`@ Trash reloadClass: Tool` loads the new directory-aware process primitive.
Executables must be on `PATH`. Query/run methods report `missing_tool` with
installation guidance; they never install a dependency themselves.

```bash
@ Tools::Roam version
@ Tools::AstGrep version
@ Tools::Cass version
@ Tools::Chad version

# Explicit installation uses the existing Tool interface.
@ Tools::Roam ensure
@ Tools::AstGrep ensure
```

Roam's install recipe pins `roam-code==14.1.0` through uv; Chad's pins
`chad-code==2.0.3`. ast-grep uses Homebrew. Cass provides its upstream Cargo
installation recipe. These commands require the corresponding package manager.

Code/session query methods return a JSON object with
`schema_version`, `tool`, `command`, `outcome`, `exit_code`, `stdout`, `stderr`,
and `data`. `data` is the parsed native object/array, or null when unavailable.
Both original streams and the actual child exit code are preserved. A
successful process emitting malformed or incorrectly shaped JSON produces
`invalid_output`; ordinary failures produce `command_error`.

```bash
result=$(@ Tools::Cass search: 'assignment persistence' inWorkspace: "$PWD")
printf '%s\n' "$result" | jq '{outcome, data}'
```

Bad arguments are precondition failures: the message returns nonzero before
launching a process. Check the envelope's outcome for completed invocations;
shell success alone does not mean the native command succeeded. A missing
working directory is captured with child exit code 72 and diagnostics.

## Roam

Pass the **checkout root**, including for linked worktrees. Each invocation
sets `ROAM_DB_DIR=.roam` relative to that directory, overriding inherited or
project-configured database locations. This keeps separate checkouts' indexes
separate. The caller's directory and environment are preserved.

```bash
@ Tools::Roam indexInDirectory: "$PWD"
@ Tools::Roam statusInDirectory: "$PWD"
@ Tools::Roam search: 'parse_config' inDirectory: "$PWD"
@ Tools::Roam context: 'parse_config' inDirectory: "$PWD"
@ Tools::Roam impact: 'parse_config' inDirectory: "$PWD"
```

Indexing calls `roam index`. Queries set `ROAM_NO_AUTO_INDEX=1`, so a missing or
incomplete index returns `index_missing` and its recovery payload. Refresh
explicitly after changes. This adapter preserves native index-age and other
metadata; it does not certify that every result reflects the current dirty
checkout. Roam may maintain its own query/cache metadata.

The wrapper requests JSON with an 8,000-token output budget and preserves
Roam's native search/impact limits. Native `summary.partial_success` becomes
the `partial` outcome. Inspect the payload's truncation and freshness details.
The adapter covers graph queries only; the compiler's `Trash symbolRecords`,
`implementorsOf:`, and `sendersOf:` remain the source-level tools for `.trash`.

Upstream contracts: [Roam CLI](https://github.com/Cranot/roam-code/blob/v14.1.0/src/roam/cli.py)
and [index refusal](https://github.com/Cranot/roam-code/blob/v14.1.0/src/roam/commands/resolve.py).

## ast-grep

```bash
@ Tools::AstGrep search: 'greet($A)' language: python inDirectory: "$PWD"
@ Tools::AstGrep search: 'greet($A)' language: python \
  files: '["src/example.py","tests/example test.py"]' inDirectory: "$PWD"
@ Tools::AstGrep search: 'greet($A)' language: python in: 'greet("hello")'
```

The first two methods search files; `in:` supplies source text on stdin.
`files:` accepts a single path or JSON array. Searches use `run --json=compact`
with an explicit language. Patterns and filenames stay literal, including
shell metacharacters. This surface exposes search; native rewriting and
interactive editing are not part of these methods.

Native exit 1 with an empty match array produces `no_matches`, preserving exit
1 and `data: []`. Parse/process errors remain errors. Match ranges retain
ast-grep's **zero-based** line/column coordinates and byte offsets; presentation
code must convert coordinates when displaying one-based editor locations.
Searches return the native match array without an additional result cap, so
select files or a narrow pattern for large repositories.

Upstream contracts: [run](https://ast-grep.github.io/reference/cli/run) and
[JSON output](https://ast-grep.github.io/guide/tools/json).

## cass

The adapter targets
[coding_agent_session_search](https://github.com/Dicklesworthstone/coding_agent_session_search).

```bash
@ Tools::Cass status
@ Tools::Cass index
@ Tools::Cass search: 'why did that migration fail?'
@ Tools::Cass search: 'migration' inWorkspace: "$PWD"
@ Tools::Cass search: 'migration' inWorkspace: "$PWD" agent: codex \
  limit: 10 dataDirectory: '/path/to/cass-index'
@ Tools::Cass view: '/path/from/search/source_path.jsonl' line: 42
```

Search defaults to lexical mode, 20 hits, a five-second native query timeout,
and 2,000 characters per content/snippet field. The configurable hit limit is
1–1,000. It requests robot metadata and disables daemon use; search does not
request indexing or model setup. Native source references, line numbers,
freshness, warnings, and truncation information remain in `data`.

`statusInDataDirectory:` and `indexInDataDirectory:` select an alternate store.
A data directory controls **storage**, while `inWorkspace:` filters sessions by
their recorded workspace path. It is not yet a logical Repository association.
Unscoped `search:` searches cass's available history. `view:line:` follows a
local source reference with five surrounding lines.

`index` is an explicit incremental indexing request over cass's configured
sources. Selecting a fresh data directory does not restrict source discovery.
Connector availability and source access are determined by the installed cass
version; the wrapper does not add a new history connector.

Cass can emit structured failures on stderr. These remain on stderr in the
envelope and are also parsed into `data`; native missing-index exit 3 becomes
`index_missing`. A successful `status` command can still report an unhealthy or
uninitialized index in its payload.

## Chad

The adapter targets [nathansutton/chad](https://github.com/nathansutton/chad),
published as `chad-code`. Its ordinary backend is local MLX inference. Model
requirements, downloads, persistence, and native tool policy belong to Chad.

```bash
@ Tools::Chad levers                       # metadata; no model load
@ Tools::Chad dryRun: 'inspect the parser' workingDirectory: "$PWD"
@ Tools::Chad plan: 'inspect the parser' workingDirectory: "$PWD"
@ Tools::Chad run: 'implement the parser fix' workingDirectory: "$PWD"
@ Tools::Chad run: 'inspect the parser' model: '/path/to/model' workingDirectory: "$PWD"
@ Tools::Chad continueLatest: 'add a regression test' workingDirectory: "$PWD"
```

`run:` starts a headless task, whose native behavior auto-approves mutating
tools. `plan:` explicitly selects Chad's read-only plan mode. A run can download
the selected/default model if it is absent. `dryRun:` shows argv, empty stdin,
directory, mode, and continuation selection without launching anything.

The inspected CLI consumes a **positional prompt**, so the adapter passes it
as one argument after `--`, with EOF on stdin. Prompts consequently appear in
the process argument list. Empty/whitespace prompts are rejected to prevent
accidental interactive startup. Native run output is text: the result includes
the common harness fields `backend: "chad"` and `result.content`, plus
`result.format: "text"`, mode, and continuation information. Output is not
parsed into a fabricated final-answer or session-ID protocol.

`continueLatest:` means the most recent saved conversation in that directory;
Chad forks it. It does not target a Trashtalk AgentSession ID and is unsuitable
for concurrent identities sharing one directory. This Tool is not registered
as a resident `AgentDriver` or selected as Gusgus's harness. Exit 0 means the
process finished successfully; it does not complete an Assignment.

## Verification

`tests/test_code_tools.bash` and `tests/test_chad_tool.bash` exercise exact
arguments, directory/environment isolation, stdin, missing tools, malformed
output, native errors, and continuation/mode selection using deterministic
executables. `tests/test_code_tools_live.bash` is an opt-in disposable-repository
check against actual Roam, ast-grep, and cass binaries:

```bash
TRASHTALK_CODE_TOOLS_LIVE=1 bash tests/test_code_tools_live.bash
```

The live check exercises separate Roam indexes for a checkout and dirty linked
worktree, ast-grep file/stdin searches, and cass status, missing-index errors,
and source viewing. Successful indexed cass search and index invocation are
covered with deterministic executables; the live check does not ingest personal
session history. Chad metadata is also checked if its executable is available.

Checked versions: Roam 14.1.0, ast-grep 0.45.3, cass 0.6.23. Chad 2.0.3's
version/help/metadata interface was checked; model generation and a persistent
Chad session lifecycle have not been qualified. Temporary dependency installs
used for these checks do not place new tools on the normal user `PATH`.
