# Innards as an Ambient UI/IDE Layer for Trashtalk

*Design report, September 2026. Covers the `chazu/innards` and `chazu/trashtalk`
repositories as of `innards@25de24d` and `trashtalk@18e7683`.*

## 1. Summary

Trashtalk already has the bones of a Smalltalk-style development environment:
a message-passing runtime with introspection (`@ Trash methodsFor:`,
`sourceFor:`, `hierarchyFor:`), a compiler that emits a located JSON AST, an
edit/compile/test loop (`@ Trash edit:`), persistent objects in SQLite, and a
pub/sub substrate (Honker-backed `EventBus`, `Stream`, `Actor`, `Scheduler`).
What it lacks is a *view*. The README says the whiz-bang TUI IDE was jettisoned
and that an acme-like, unobtrusive alternative is wanted.

Innards is that alternative waiting to happen. Its one big idea is the ratatui
**inline viewport**: every tool opens *below the current prompt*, does its job,
and disappears, leaving scrollback intact. That is exactly the "ambient" quality
a shell-native IDE needs. Today innards is three Rust binaries aimed at Rust
development (`navsplat` for rust-analyzer symbols, `inmacs` editor, `inpage`
pager), with no programmatic surface beyond argv and a single exit code.

This report proposes:

1. Turning the innards binaries into **drivable, scriptable surfaces** with a
   stable CLI, JSON in/out, and meaningful exit codes, so Trashtalk can call
   them like any other tool and read back what the user did.
2. Generalising `navsplat` into a **symbol picker with pluggable sources**, so
   Trashtalk's own AST (not an LSP) can feed a class/method browser, senders,
   and implementors view.
3. Adding a small set of **new inline surfaces** (inspector, live log, diff
   review, prompt/confirm) that map one-to-one onto Smalltalk IDE tools and
   onto the needs of an agent harness.
4. A **Trashtalk-side API** (`Tools::Inmacs`, `Tools::Inpick`, an `Ide` facade
   on `Trash`) with graceful fallbacks to `$EDITOR`, `less`, and `fzf`.
5. Using the combination as an **ambient agentic harness**: `@@` becomes a real
   conversation with a coding agent whose context is the shell session, whose
   tools are message sends, whose proposals are rendered as inline diffs and
   pre-filled commands, and whose background work surfaces as prompt-adjacent
   notifications rather than a full-screen app.

The recommended first slice is small: give `inmacs` annotations, exit codes,
stdin/stdout modes, and a `.trash` grammar, then rewrite `@ Trash edit:` to use
them. That alone removes the error-comment-injection hack in the edit loop and
gives Trashtalk a native editor in about a week of work.

## 2. What exists today

### 2.1 Innards (`chazu/innards`, fork of `rdaum/innards`)

| File | Lines | What it is |
|------|------:|------------|
| `src/inline_text.rs` | 1551 | Editor/pager core: `ropey` buffer, `syntect` highlighting, Emacs keys, incremental search, mark/kill/yank, undo/redo, fill-paragraph, viewport resize |
| `src/inline_text/render.rs` | 157 | Frame drawing: bordered block, line numbers, status line, selection highlight |
| `src/lsp.rs` | 867 | Hand-rolled JSON-RPC client: `workspace/symbol`, `textDocument/references`, call hierarchy, `$/progress` |
| `src/bin/navsplat.rs` + `navsplat/ui.rs` | 1305 | Picker: debounced query, symbol list, preview pane, side pane (refs/callers/callees), navigation stack, clipboard |
| `src/bin/inmacs.rs`, `inpage.rs` | 12 | Thin `main`s calling `inline_text::run(Mode)` |

Strengths worth preserving:

- **Inline viewport discipline.** `TerminalGuard` enters raw mode, creates a
  `Viewport::Inline(height)` terminal, and on drop clears the region and
  restores the cursor. Nothing above the prompt is touched.
- **Resizable in place.** Alt-Up/Down shrinks or grows the region with a
  correctly anchored redraw (`resize_anchor_row`).
- **A navigation model.** `navsplat` has a promoted-symbol stack (select a hit
  in the side pane, promote it, Backspace pops). That is precisely the
  drill-down model a Smalltalk inspector needs.
- **A machine-readable mode already.** `navsplat symbols QUERY` prints
  `file:line:col<TAB>name [Kind]` lines without a TUI. This is the seed of the
  programmatic API.
- **No global state, no config files, no daemon.** Everything is argv-driven.

Gaps relative to being a driveable UI layer:

- `inline_text::run` parses `env::args()` itself; there is no library entry
  point that takes a `Config`. Only `FILE`, `--height`, `--line`/`+LINE` are
  accepted, and the parser is hand-written while `navsplat` uses `clap`.
- `Outcome` has one variant, `Quit`. The caller cannot tell "saved" from
  "abandoned"; `@ Trash edit:` currently works around this by comparing
  `stat` mtimes before and after.
- No stdin input, no stdout result. Content must live in a file; the result
  is only the file on disk.
- Nothing can be shown to the user except the file and a one-line status. There
  is no way to pass diagnostics (line, column, message) for gutter marks.
- `SyntaxHighlighter::new` picks a grammar by file extension via syntect's
  defaults, so `.trash` renders as plain text.
- The LSP client hardcodes `Command::new("rust-analyzer")` and the picker is
  Rust-only in name and defaults. The client itself is generic JSON-RPC.
- The crate is named `navsplat` in `Cargo.toml`, the tab key inserts four spaces
  (Trashtalk convention is two), and saves are whole-file, non-atomic writes.
- `navsplat` has no unit tests; `inline_text` has a good set.

### 2.2 Trashtalk (what the UI layer would sit on)

Already present and directly useful:

- **Located AST.** `lib/jq-compiler/driver.bash parse FILE` emits JSON with
  `location.line/col` for the class, every instance variable, and every method,
  plus the raw token stream (with `line`/`col`) for each body. A symbol index
  for a picker is a `jq` one-liner away, and the driver already caches ASTs in
  `.compiled/.astcache`.
- **Introspection messages.** `listObjects`, `listTraits`, `methodsFor:`,
  `methodsIn:category:`, `hierarchyFor:`, `sourceFor:`, `hashFor:`,
  `inspect:`, `hasTestsFor:`, `runTestsFor:` (TAP output), `eval:`.
- **Edit loop.** `@ Trash edit:` (in `trash/Trash.trash`) runs `$EDITOR`,
  compiles, validates with `bash -n`, hot-reloads with `reloadClass:`, runs
  `testMethod:`s, and loops on failure. It is `pragma: direct` and redirects the
  editor to `/dev/tty`, which is the pattern any innards-launching method must
  follow.
- **Events and background work.** `EventBus`, `Observable`, `Stream` (durable,
  cross-process, consumer offsets), `Actor`, `Scheduler`, `Future`, `Coproc`,
  `FIFO`, `Process`.
- **Agent scaffolding.** `Agent`, `ClaudeAgent`, `AgentSession` (persisted in
  SQLite with `sessionName`, `spawnedBy`, `status`, `messageCount`),
  `Tools::Tmux` for creating sessions and `send-keys`.
- **REPL tooling.** `bin/trash-repl`, `bin/trash` (rlwrap wrapper), `$__` last
  result, `lib/trash-completion.bash` (receivers and methods).
- **Tool abstraction.** `Tool subclass: Object` with `name`, `installCommand`,
  `ensure`, `path`, `run:`, `runAsync:`; `Tools::Jq`, `Tools::Netcat`,
  `Tools::Tmux` as examples.

Gaps and drift the UI work should fix or route around:

- **The edit loop mutates the source file** to show errors: it prepends a
  `# == COMPILE ERRORS ==` comment block, then strips it on the next save.
  Editor line-jumping is a `case` over editor names. Both exist only because
  the editor has no side channel for diagnostics.
- **`@@` is a stub** (`lib/trash.bash:2223`) that echoes `[Agent] message`.
  The `Agent` classes talk to a tmux session with `send-keys` only; nothing
  captures a response, and `ClaudeAgent toolCommand` is the bare interactive
  `claude` binary.
- **`ReplServer` does not exist.** `emacs/trashtalk-mode.el` speaks a socket
  protocol (`PING`, `EVAL:`, `BASE64:`, `INFO:`, `METHODS:`, `RELOAD:` with
  `OK:`/`ERROR:` replies and `\x1f` as the newline escape) to
  `/tmp/trashtalk-repl.sock`, and the README lists the class, but there is no
  `ReplServer.trash` in the tree. Likewise `bin/trash` references
  `lib/rlwrap-completion.pl`, which is missing.
- **`docs/twin.md`** describes the earlier windowed-IDE direction (Twin text
  window manager, `@ Twin inspect:`); no `Twin.trash` remains. The document is
  still a good statement of *what* tools were wanted: inspector, class browser,
  workspace, stream windows.
- **Message sends run in a subshell** unless the method is `pragma: direct`.
  Any method that opens a terminal UI must be a `rawMethod:` with
  `pragma: direct`, which also means it cannot be natively compiled.

## 3. Design principles

1. **Inline or nothing.** No alternate screen, no full-screen mode. Every
   surface is a bounded region under the prompt and its output, if any, ends
   up in scrollback. This is the innards contract; extend it, do not break it.
2. **Every surface is a process with a CLI.** Trashtalk is Bash; the cheapest,
   most robust integration is `spawn → read stdout → check exit code`. No shared
   libraries, no FFI, no daemon required for the base case.
3. **Machine-readable in and out.** Inputs are files, stdin, or a JSON file
   path. Outputs are newline-delimited JSON on stdout (or plain lines where a
   single value suffices) plus a documented exit code. The user-facing drawing
   goes to `/dev/tty`, never to stdout, so stdout stays clean for the caller.
4. **Trashtalk is the model, innards is the view.** Class metadata, source,
   instances, events, and sessions live in Trashtalk. Innards renders and
   collects input. It may call back into Trashtalk (for previews, reload,
   tests) but only through the public message-send surface.
5. **Degrade gracefully.** Every Trashtalk method that wants an innards surface
   falls back to `$EDITOR`, `${PAGER:-less}`, `fzf`, or plain `read -r` when the
   binary is missing. `@ Trash doctor` reports innards as optional, like honker.
6. **Loose coupling.** Innards stays a general-purpose crate (it should remain
   upstreamable to `rdaum/innards`, and it is GPL-3.0 while trashtalk carries no
   licence file). A CLI boundary keeps both projects independent.

## 4. Innards enhancements

### 4.1 Tier 0: make the existing binaries drivable

These are small, mechanical changes to `inline_text.rs` and its `main`s.

**Unified CLI.** Replace `Config::parse` with `clap` (already a dependency) and
share the argument set between `inmacs` and `inpage`:

```text
inmacs [OPTIONS] [FILE]
  +LINE, --line N          initial cursor line (exists)
  --height N               viewport rows (exists)
  --col N                  initial cursor column
  --title TEXT             replaces "inmacs: path" in the border
  --status TEXT            initial status-line text
  --syntax NAME            force a grammar (e.g. trash, bash, json)
  --tab-width N            spaces per Tab (default 4; Trashtalk passes 2)
  --annotations FILE       JSON diagnostics to show as gutter marks (see 4.1.3)
  --readonly               open editor keys disabled (inpage behaviour)
  --stdin, -               read initial content from stdin
  --output PATH|-          write buffer on save to PATH or stdout instead of FILE
  --result-json            print an outcome record on stdout at exit (see 4.1.2)
  --autosave-on-quit       Ctrl-X Ctrl-C saves if dirty instead of discarding
```

**Outcome reporting.** Extend `Outcome` and the process exit code:

| Exit | Meaning |
|-----:|---------|
| 0 | Quit after a save (or no changes were made) |
| 3 | Quit with unsaved changes discarded |
| 4 | Quit with nothing changed and `--require-change` set |
| 1 | Error (unreadable file, bad args) |

With `--result-json`, stdout carries one line:

```json
{"saved":true,"path":"trash/Counter.trash","dirty_at_exit":false,
 "cursor":{"line":12,"col":4},"edits":3}
```

This removes the `stat` mtime dance in `@ Trash edit:` and lets callers decide
whether to compile.

**Annotations.** A JSON file of diagnostics:

```json
[{"line":14,"col":8,"severity":"error","message":"unterminated string literal"},
 {"line":9,"severity":"warning","message":"unused local 'tmp'"}]
```

Rendered as a coloured gutter mark, an underline on the range if `end_col` is
given, and the message in the status line when the cursor is on that line.
`Alt-N`/`Alt-P` jump between annotations. This is the side channel that lets
the compiler talk to the editor without rewriting the source file, and it is
the same shape a test failure, a lint, or an agent's review comment would use.

**A Trashtalk grammar.** Add `syntaxes/Trashtalk.sublime-syntax` and load it
alongside the syntect defaults (`SyntaxSetBuilder::add_from_folder`, or embed
with `include_str!`). The language is small enough that a first grammar is an
afternoon: `#` comments; `'…'`, `"…"` (with `$var` and `$(…)` interpolation),
and `'''…'''` strings; the header keywords `subclass:`, `trait`,
`instanceVars:`, `classInstanceVars:`, `include:`, `package:`, `requires:`,
`category:`, `alias:for:`; method keywords `method:`, `classMethod:`,
`rawMethod:`, `rawClassMethod:`, `testMethod:`, `rawTestMethod:`,
`before:do:`, `after:do:`; `pragma:`; `@`, `^`, `:=`, `| locals |`, block
params `[:x :y |`; capitalised class names and `Pkg::Class`. Raw method bodies
can embed the bundled Bash grammar between `[` and the matching `]`. The Emacs
mode's font-lock table is a ready checklist.

**Library shape.** Rename the package to `innards`, keep the binary names, and
expose `innards::inline_text::{Config, Editor, run_with(Config)}` so future
surfaces (4.3) can embed the editor widget instead of shelling out to it.
Make `save` write to a temp file and rename, so a crash mid-write cannot
truncate a class file.

### 4.2 Tier 1: a generic picker and a Trashtalk symbol source

`navsplat` is about 90% of a class browser. The UI (query box, list, preview,
side pane, promoted-symbol stack) has nothing Rust-specific in it; only the
symbol *source* does.

**Extract `inpick`.** A new binary that reads candidates from stdin or a
command and reuses `navsplat/ui.rs`:

```text
inpick [OPTIONS] [QUERY]
  --from CMD            run CMD (once, or per keystroke with --live) for candidates
  --preview MODE        file | cmd:TEMPLATE | none
  --side NAME=CMD       add a side pane (Alt-<first letter>) fed by CMD; {file}
                        {line} {name} {kind} {detail} are substituted
  --multi               allow multi-select (Tab marks)
  --height N
  --json                print selected records as JSON lines instead of raw lines
```

Input line format is the one `navsplat symbols` already prints, extended with
optional tab-separated fields:

```text
file:line:col<TAB>name<TAB>kind<TAB>detail
```

`--preview file` shows the file centred on `line` exactly as today.
`--preview cmd:'trash-send Trash sourceFor: {name}'` lets Trashtalk supply the
preview (for classes without a stable file location, or for instance data).

**Symbol provider trait.** In `lsp.rs`, put the picker's needs behind a trait:

```rust
pub trait SymbolProvider {
    fn symbols(&self, query: &str) -> Result<RequestId>;
    fn references(&self, sym: &Symbol, key: String) -> Result<RequestId>;
    fn incoming_calls(&self, sym: &Symbol, key: String) -> Result<RequestId>;
    fn outgoing_calls(&self, sym: &Symbol, key: String) -> Result<RequestId>;
}
```

with two implementations: `LspProvider { command: String }` (today's client,
with `rust-analyzer` as the default command instead of a hardcode) and
`ExecProvider { symbols_cmd, references_cmd, callers_cmd, callees_cmd }` that
runs shell commands and parses the line format above. `navsplat` becomes
`inpick --provider lsp:rust-analyzer`; Trashtalk uses the exec provider and
never has to implement LSP.

**The Trashtalk side.** Add to `Trash` (or a new `Ide` class):

```smalltalk
# Emit picker lines for every class, method and ivar. Uses the AST cache.
rawClassMethod: symbols [
  local f
  for f in "$TRASHDIR"/*.trash "$TRASHDIR"/*/*.trash "$TRASHDIR"/traits/*.trash; do
    "$SCRIPT_DIR/jq-compiler/driver.bash" parse "$f" 2>/dev/null \
      | jq -r --arg f "${f#"$TRASHDIR"/}" '
          .class as $c
          | "\($f):\($c.location.line):\($c.location.col)\t\($c.name)\t\(if $c.isTrait then "Trait" else "Class" end)\t\($c.parent // "")",
            ($c.methods[] | "\($f):\(.location.line // 0):0\t\($c.name)>>\(.selector)\t\(if .kind=="class" then "ClassMethod" else "Method" end)\t\(if .raw then "raw" else "" end)"),
            ($c.instanceVars[] | "\($f):\(.location.line):\(.location.col)\t\($c.name).\(.name)\tField\t\(.default.value // "")")'
  done
]
```

(The parser records `location` on class and ivars today; adding it to method
nodes is a one-line change in `grammar/method.jq`. Cache the joined output in
SQLite keyed by source hash so `browse` is instant.)

Then the Smalltalk browser tools fall out:

| Message | Implementation |
|---------|----------------|
| `@ Trash browse` | `@ Trash symbols \| inpick --preview file --side 'senders=trash-send Trash sendersOf: {name}' --side 'implementors=trash-send Trash implementorsOf: {name}'`; Enter opens `inmacs +{line} {file}` |
| `@ Trash implementorsOf: sel` | filter symbol lines whose selector matches |
| `@ Trash sendersOf: sel` | scan each method body's token stream for `KEYWORD`/`IDENTIFIER` tokens equal to the selector; emit `file:line:col` |
| `@ Trash instancesOf: Class` | `findAll` lines with a `cmd:` preview of `@ Runtime dataFor:` |
| `@ Trash pickMethod: Class` | `methodsFor:` reshaped as lines; returns the selector |

Selection results come back on stdout, so `class=$(@ Trash browse)` composes
with the rest of the shell.

### 4.3 Tier 2: new inline surfaces

Each is a small binary built from the same widgets. Listed in the order they
pay off.

**`inspect` (object inspector).** Input: JSON on stdin (an instance record from
`@ Runtime dataFor:`). Renders a key/value tree; nested JSON expands in place;
a value matching the instance-id pattern (`^[a-z]+(_[a-z]+)*_[0-9a-f-]+$`) is a
reference and Enter drills into it by running a configured command
(`--follow 'trash-send Runtime dataFor: {value}'`), pushing onto the same
navigation stack `navsplat` uses. `e` on a leaf opens it in an embedded
`inmacs` buffer and prints `{"path":["value"],"new":"42"}` on exit so
`@ $obj inspect` can apply it with `_ivar_set`. This is `docs/twin.md`'s
`@ Twin inspect:` with drill-down, inline.

**`intail` (live view).** Follows a file, FIFO, or command output and renders
the last N lines in the viewport with auto-scroll, a pause key, and search from
`inline_text`. `q` exits and, with `--keep`, leaves the final N lines in
scrollback. Pairs with `Stream`, `EventBus`, `Coproc readLinesDo:`, and
`TRASH_PROFILE_FILE`. Because an inline ratatui region holds the tty in raw
mode, it cannot stay pinned while the user keeps typing at the prompt; see 6.4
for the truly ambient (non-modal) counterpart.

**`indiff` (review).** Input: a unified diff on stdin or `--old FILE --new FILE`.
Side-by-side or unified rendering with hunk navigation, and per-hunk
accept/reject (`y`/`n`/`a`/`q`). Output: the accepted patch on stdout, or the
resulting file with `--apply`. Exit 0 if anything accepted, 3 if all rejected.
This is the gate every agent-proposed edit passes through (section 6).

**`inprompt` (input and confirm).** One-line or multi-line input under the
prompt with a completion source (`--complete CMD`, called with the current
word), history file, and a `--confirm "text"` mode that draws a yes/no strip
and exits 0/1. Replaces `read -r` in `removeObject:` and in every place the
agent harness needs approval. With `--complete 'trash-send Trash completions:
{word}'` it is also the REPL input line, reusing `lib/trash-completion.bash`
logic without rlwrap.

**Status strip (non-modal).** Not a binary: a `PROMPT_COMMAND` hook that
prints one dim line above each prompt from a small state file or a Honker
stream (agent count, last event, failing tests). Cheap, always visible, and the
only truly ambient element that survives while the user is typing. `inpick`
over the same stream gives the drill-in. See 6.4.

### 4.4 Tier 3: a session protocol (optional)

Spawning a process per surface is fine for the editor, picker, and diff. Two
cases want something longer-lived:

- previews and side panes that call back into Trashtalk on every keystroke,
  where `trash-send` paying to source `lib/trash.bash` each time is too slow;
- agent streams, where many small updates arrive over minutes.

Rather than invent a new protocol, finish the one the Emacs mode already
expects: implement `ReplServer.trash` (a `Coproc`/`Tools::Netcat`-style Unix
socket listener in the user's shell process, `pragma: direct`, with the
`EVAL:`/`INFO:`/`METHODS:`/`RELOAD:` verbs and `OK:`/`ERROR:` replies). Innards
surfaces then take `--repl /tmp/trashtalk-repl.sock` and use it for previews,
reload-on-save, and test runs. One protocol serves Emacs, innards, and any
future editor. Add `SYMBOLS:` and `EVENTS:` (subscribe to an `EventBus` name,
stream `EVT:` lines) as the two verbs the UI needs beyond what Emacs uses.

If a long-running innards process is ever wanted (one tty owner multiplexing
surfaces), do it as `innards serve` speaking newline-delimited JSON on stdio
with the same verbs as the CLI surfaces (`open`, `pick`, `inspect`, `diff`,
`confirm`, `notify`). Do not start here; the per-process model covers the
first year.

## 5. Trashtalk-side API

### 5.1 Tool wrappers

One `Tool` subclass per binary, all in a `Tools` package so they sit beside
`Tools::Tmux`:

```smalltalk
package: Tools

Inmacs subclass: Tool
  classMethod: name [ ^ "inmacs" ]
  classMethod: installCommand [ ^ "cargo install --git https://github.com/chazu/innards" ]

  # Edit a file with optional diagnostics. Returns the result JSON.
  # Falls back to $EDITOR when inmacs is missing (then returns {"saved":null}).
  rawClassMethod: edit: file annotations: annFile line: line [
    pragma: direct
    local file="$1" ann="$2" line="${3:-1}"
    if [[ "$(@ "$_RECEIVER" isInstalled)" == "true" ]]; then
      inmacs --syntax trash --tab-width 2 --result-json \
        ${ann:+--annotations "$ann"} +"$line" "$file" </dev/tty >/dev/tty 2>&1 \
        3>&1 1>/dev/tty  # result JSON goes to fd 3 -> caller; see note
    else
      ${EDITOR:-vi} +"$line" "$file" </dev/tty >/dev/tty
      echo '{"saved":null}'
    fi
  ]
```

(The fd juggling above is the one awkward part of "draw on the tty, report on
stdout": innards should open `/dev/tty` itself for drawing and input, exactly
as `fzf` does, so callers can simply capture stdout. Make that the default and
the wrapper becomes `result=$(inmacs --result-json … "$file")`.)

`Tools::Inpage`, `Tools::Inpick`, `Tools::Inspect`, `Tools::Indiff`,
`Tools::Inprompt` follow the same shape. `@ Trash doctor` gains an `innards`
line (OK / WARN optional, like honker).

### 5.2 Rewriting `@ Trash edit:`

The loop keeps its structure (compile, `bash -n`, reload, run tests, repeat)
but stops mutating the file:

```smalltalk
rawMethod: edit: class_name [
  pragma: direct
  local file ann="/tmp/trash_edit_$$.json" line=1 result
  file=$(@ "$_RECEIVER" _sourceFileFor: "$class_name") || return 1
  : > "$ann"
  while true; do
    result=$(@ Tools::Inmacs edit: "$file" annotations: "$ann" line: "$line")
    [[ "$(jq -r .saved <<<"$result")" == "false" ]] && { echo "Abandoned"; rm -f "$ann"; return 1; }
    if @ "$_RECEIVER" _compileAndReload: "$class_name" into: "$ann"; then
      if @ "$_RECEIVER" _runTests: "$class_name" into: "$ann"; then
        rm -f "$ann"; return 0
      fi
    fi
    line=$(jq -r '.[0].line // 1' "$ann")
  done
]
```

`_compileAndReload:into:` converts the compiler's `LINE:COL: message` lines
(the format the current loop already greps for) into annotation records;
`_runTests:into:` converts TAP `not ok` lines the same way (a `testMethod:`'s
location comes from the AST). The user sees red gutter marks and the message
in the status bar instead of a comment block in their source. When `inmacs` is
missing, the fallback path prints the diagnostics to the terminal before
re-opening `$EDITOR`, which is still better than editing the file.

`@ Trash new:` needs no change beyond calling the new `edit:`.

### 5.3 Browser, inspector, pager

```smalltalk
rawClassMethod: browse [                # class/method browser
  pragma: direct
  local pick
  pick=$(@ "$_RECEIVER" symbols | inpick --preview file \
           --side 'senders=trash-send Trash sendersOf: {name}' \
           --side 'implementors=trash-send Trash implementorsOf: {name}') || return
  @ Tools::Inmacs edit: "$TRASHDIR/${pick%%:*}" annotations: "" line: "$(cut -d: -f2 <<<"$pick")"
]

rawMethod: inspect [                    # on Object, replaces the text dump
  pragma: direct
  @ Runtime dataFor: "$_INSTANCE" | inspect --follow 'trash-send Runtime dataFor: {value}' \
     --edit 'trash-send Runtime setIvar: {key} of: '"$_INSTANCE"' to: {value}'
]

rawClassMethod: page: text [            # Console
  pragma: direct
  if command -v inpage >/dev/null; then printf '%s\n' "$1" | inpage --syntax trash -
  else printf '%s\n' "$1" | "${PAGER:-less}"; fi
]
```

Set `TRASH_PAGER=inpage` and have the `@` dispatcher page results longer than
the terminal height when stdout is a tty, so `@ Trash methodsFor: Trash`
becomes readable without leaving the prompt.

### 5.4 Events into the UI

`Observable` and `EventBus` already emit; the UI just needs a sink. Add an
`Ide` class (or methods on `Trash`) that:

- writes a compact one-line summary of selected events (`compiled`, `tests
  failed`, `agent finished`) into `$TRASHDIR/.ide/status`, read by the prompt
  hook (6.4);
- exposes `@ Ide tail: 'stream-name'` as `intail --follow-cmd 'trash-send
  Stream tailLines: name'`.

## 6. An ambient agentic harness in the shell

### 6.1 What "ambient" means here

The agent lives *next to the prompt*, not in a full-screen chat. You type a
command, it fails, you type `@@ why`, a short region opens under the prompt,
the answer streams into it, the region closes and the answer stays in
scrollback. Proposed changes arrive as a diff you accept hunk by hunk;
proposed commands arrive pre-filled at your prompt so the shell, not the agent,
executes them. Long tasks run in the background and announce themselves in a
one-line strip above the next prompt. Trashtalk supplies sessions,
persistence, events, and the tool surface; innards supplies every visible
pixel.

### 6.2 Where today's scaffolding falls short

- `@@` echoes its argument; `Agent`/`ClaudeAgent` send keystrokes into a tmux
  window running interactive `claude` and cannot read a reply.
- Nothing records shell context (recent commands, exit codes, cwd), so the
  agent cannot answer "why did that fail" without being told.
- There is no approval surface; `Shell exec:` is `eval "$1"`, so wiring an
  agent's output straight to it would be an arbitrary-code-execution path.
- The agent has no structured way to *use* Trashtalk: no tool schema, no
  session-scoped `@` access.

### 6.3 Proposed architecture

```
 shell (bash, trash.bash sourced)
 ├─ PROMPT_COMMAND / DEBUG trap ─▶ Stream 'shell.events'  (cmd, cwd, exit, duration)
 ├─ @@ msg ─▶ ClaudeAgent ask: ─▶ Coproc: claude -p --output-format stream-json
 │                                  --resume <AgentSession.externalId>
 │                                  --mcp-config <trash-mcp>
 │            events ─▶ EventBus 'agent.<session>' ─▶ intail (inline, modal)
 │            final text ─▶ scrollback
 │            file edits ─▶ indiff ─▶ accept ─▶ @ Trash edit:-style compile+test gate
 │            commands ─▶ inprompt --prefill ─▶ user presses Enter ─▶ shell runs it
 ├─ @ Agent spawn: ─▶ Actor/Future in background ─▶ Stream 'agent.results'
 └─ prompt hook ─▶ status strip: "2 agents running · 1 result · tests: green"
```

**Context capture.** A `Shell::History` class installs a `DEBUG` trap plus
`PROMPT_COMMAND` hook that appends `{ts, cwd, cmd, exit, ms}` to a Honker
`Stream` named `shell.events` (falling back to a JSONL file when honker is
absent). `@@` sends the last N events, `$__`, and `pwd` as context. Nothing is
sent anywhere until the user invokes `@@`.

**Conversation.** `ClaudeAgent ask:` stops driving tmux and runs the CLI in
non-interactive mode via `Coproc for:` with `claude -p "$msg"
--output-format stream-json --resume "$externalId"`. `AgentSession` gains an
`externalId` ivar (the provider's session id) so `@@` resumes the same thread
across shell sessions; `sessionName` and tmux remain for the "attach to a live
agent" case. `Coproc readLinesDo:` forwards each JSON event to
`EventBus 'agent.<id>'`; the conversation surface is `intail` on that bus with
`--keep` so the final assistant text remains in scrollback. `Agent` stays
provider-agnostic: `toolCommand`, `streamArgs:`, and `parseEvent:` are the
overridable seams, so another CLI agent is a subclass, not a rewrite.

**Proposals, not actions.** The harness never executes agent output. Two
surfaces carry all agent effects:

- *Edits* arrive as unified diffs (from the agent's own edit tool events, or by
  having the agent write to a scratch copy under `$TRASHDIR/.ide/proposals/`).
  `indiff` shows them; accepted hunks are applied and, for `.trash` files,
  pushed through the same compile → `bash -n` → reload → tests gate as
  `@ Trash edit:`. A failing gate re-opens `inmacs` with annotations.
- *Commands* arrive as text and are placed into the shell's input line with
  `inprompt --prefill` (or `READLINE_LINE` via `bind -x`); the user edits or
  presses Enter. The shell is the executor and its history is the audit log.

`Shell exec:` and friends stay as they are for user code; agent code paths
must not call them with agent-authored strings.

**Tools are message sends.** Expose Trashtalk to the agent as an MCP server,
`bin/trash-mcp`, speaking JSON-RPC on stdio with a handful of tools:
`send(receiver, selector, args)`, `symbols(query)`, `source(class)`,
`methods(class)`, `compile(class)`, `run_tests(class)`, `inspect(instance)`.
Each is a thin wrapper over `trash-send`, and `methodsFor:` output doubles as
the tool description. This makes the object system the agent's harness: the
agent browses classes with the same index the human browses, edits through the
same gate, and its tool calls are visible live in `intail` because
`bin/trash-mcp` publishes each call to `EventBus 'agent.tools'`.

**Background agents.** `@ Agent spawn: 'task'` already exists in shape. Back it
with `Actor` (mailbox, at-least-once) or `Future` for one-shots; results go to
`Stream 'agent.results'`. `@@ results` opens `inpick` over unread results with
the full transcript as preview. `Scheduler` gives cron-style agents ("every
morning, summarise failing tests") with leader election when several shells are
open.

### 6.4 The ambient status strip

An inline ratatui region cannot coexist with a live prompt on the same tty, so
the always-on part of the harness must be plain text drawn by the shell. A
`PROMPT_COMMAND` hook reads `$TRASHDIR/.ide/status` (written by `Ide`
subscribers to the agent and test event buses) and prints at most one dim line:

```
· 1 agent running (refactor Counter) · 1 result unread · Counter tests: 2/2
$ 
```

Nothing is printed when there is nothing to say. `@@ results`, `@ Trash
browse`, and `@ Ide tail:` are the drill-ins. This is the same shape as a git
prompt segment and costs one `cat` per prompt. In tmux, the same text can be
mirrored to the status line via `tmux set -g status-right`, which `Tools::Tmux`
already knows how to drive.

### 6.5 Safety defaults

- Agent effects are diffs and pre-filled commands only; no direct execution.
- `.trashrc` gains `AGENT_ALLOW_PATHS` (defaults to `$TRASHDIR` and the current
  repo) and `AGENT_TOOLS` (defaults to the read-only MCP tools plus `compile`
  and `run_tests`); `send` is opt-in.
- All agent I/O is logged to `Stream 'agent.<id>'` and is browsable after the
  fact with `inpick`/`inpage`.
- Context sent to the agent is shown once (`inpage`) the first time a session
  is created, so the user knows what leaves the machine.

## 7. Roadmap

| Step | Repo | Work | Depends on |
|-----:|------|------|------------|
| 1 | innards | `clap` CLI for `inmacs`/`inpage`; exit codes; `--result-json`; `--stdin`/`--output`; open `/dev/tty` for drawing; atomic save; crate rename | — |
| 2 | innards | `--annotations`; gutter marks; Alt-N/P; `--status`, `--title`, `--tab-width` | 1 |
| 3 | innards | `Trashtalk.sublime-syntax`; `--syntax` | 1 |
| 4 | trashtalk | `Tools::Inmacs`, `Tools::Inpage`; rewrite `@ Trash edit:`/`new:` to use annotations; `doctor` line; paging of long `@` output | 1–3 |
| 5 | trashtalk | Method `location` in `grammar/method.jq`; `@ Trash symbols`, `sendersOf:`, `implementorsOf:` with SQLite cache | — |
| 6 | innards | `SymbolProvider` trait; `ExecProvider`; `inpick` binary sharing `navsplat/ui.rs`; `navsplat` becomes a thin wrapper | 1 |
| 7 | trashtalk | `@ Trash browse`, `pickMethod:`, `instancesOf:` | 5, 6 |
| 8 | innards | `inspect`, `intail`, `inprompt` | 6 |
| 9 | trashtalk | `Object inspect` via `inspect`; `Ide tail:`; status file + `PROMPT_COMMAND` strip | 8 |
| 10 | trashtalk | `ReplServer.trash` (protocol already specified by the Emacs mode); `SYMBOLS:`/`EVENTS:` verbs; restore or drop `lib/rlwrap-completion.pl` reference in `bin/trash` | 5 |
| 11 | innards | `indiff` with hunk accept/reject | 1 |
| 12 | trashtalk | `Shell::History` capture; `AgentSession externalId`; `ClaudeAgent ask:` over `Coproc` + stream-json; `@@` wired to it; `intail` conversation surface | 8, 11 |
| 13 | trashtalk | `bin/trash-mcp`; agent proposals through `indiff` and `inprompt --prefill`; background agents on `Actor`/`Stream`; `@@ results` | 12 |

Steps 1–4 are the recommended first milestone: they are independent of the
picker work, immediately improve daily use of `@ Trash edit:`, and validate
the "draw on tty, report on stdout" contract everything else relies on.

## 8. Risks and open questions

- **tty ownership.** Surfaces must open `/dev/tty` for input and drawing so
  stdin/stdout are free for data. crossterm supports this via
  `crossterm::tty::IsTty` and a `File` backend for the ratatui
  `CrosstermBackend`; it needs testing under tmux, SSH, and when the shell is
  itself inside `rlwrap`.
- **Spawn cost.** `trash-send` sources `lib/trash.bash` on every call. For
  per-keystroke previews that is too slow; `ReplServer` (step 10) or
  precomputed preview files are the fixes. `docs/self-hosting-evolution.md`
  already measured process-spawn overhead dominating small operations.
- **Grammar maintenance.** A sublime-syntax for Trashtalk duplicates knowledge
  in `tokenizer.bash` and the Emacs mode. Acceptable for highlighting; do not
  try to make it authoritative.
- **Two-space vs four-space tabs, `Tab` behaviour, and no auto-indent** in
  `inmacs` will be felt immediately when editing `.trash`. Step 2 should
  include "copy previous line's indentation on Enter".
- **`pragma: direct` everywhere.** Every UI-launching method is a raw method
  bypassing the subshell, so it cannot be natively compiled and must guard its
  own error handling. Keep these methods thin wrappers around the binaries.
- **Agent CLI drift.** `claude -p`, `--output-format stream-json`, and
  `--resume` are the current non-interactive interface; keep provider details
  in `ClaudeAgent` only, behind `Agent`'s seams.
- **Licensing.** innards is GPL-3.0; trashtalk has no `LICENSE` file. The CLI
  boundary means trashtalk only *invokes* innards, which keeps the projects
  separable, but trashtalk should pick a licence before any code is shared.
- **Upstream.** `Cargo.toml` still points at `rdaum/innards`. Decide whether
  Tier 0 and `inpick` go upstream (they are Rust-agnostic and useful to any
  shell user) or whether `chazu/innards` becomes a deliberate fork.

## 9. Appendix: interface summary

**Binaries and exit codes**

| Binary | Input | Output (stdout) | Exit |
|--------|-------|-----------------|-----:|
| `inmacs` | FILE or stdin; `--annotations` | `--result-json` record | 0 saved · 3 discarded · 1 error |
| `inpage` | FILE or stdin | none | 0 |
| `inpick` | candidate lines (stdin/`--from`) | selected line(s) or `--json` | 0 · 130 cancelled |
| `inspect` | JSON on stdin; `--follow`, `--edit` | edit records | 0 · 130 |
| `intail` | file, FIFO, or `--follow-cmd` | none (`--keep` leaves lines in scrollback) | 0 |
| `indiff` | unified diff / `--old --new` | accepted patch or `--apply` | 0 accepted · 3 none |
| `inprompt` | `--prefill`, `--complete CMD`, `--confirm` | entered text | 0 · 1 declined · 130 |

**Candidate line** (`inpick`, `symbols`): `file:line:col<TAB>name<TAB>kind<TAB>detail`

**Annotation record**: `{"line":N,"col":N,"end_col":N,"severity":"error|warning|info","message":"…"}`

**Result record** (`inmacs --result-json`): `{"saved":bool,"path":"…","dirty_at_exit":bool,"cursor":{"line":N,"col":N},"edits":N}`

**ReplServer verbs** (from `emacs/trashtalk-mode.el`, to be implemented):
`PING`, `EVAL:code` (or `EVAL:BASE64:…`), `INFO:name`, `METHODS:Class`,
`RELOAD:Class`; proposed `SYMBOLS:query`, `EVENTS:bus`. Replies `OK:`,
`INFO:`, `METHODS:`, `ERROR:`, `EVT:`; newlines in payloads encoded as `\x1f`.
