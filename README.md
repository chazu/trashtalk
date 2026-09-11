<p align="center">
  <img src="https://github.com/chazu/trashtalk/blob/main/img/logo.png">
</p>


# Trashtalk

A Smalltalk-inspired message-passing system for Bash.

Trashtalk implements message passing, inheritance, traits, aspect-oriented programming and persistent instances - with bash.

## Why would you do this?

I'm not a big fan of bash. I think POSIX is the computing environment we deserve, not the one we need. Bash's ubiquity is its strongest selling point, so strong in fact that bash scripting remains the more-or-less correct choice for a lot of situations, especially in my line of work. This really gets my goat.

I've seen others twist bash/sh into strange loops to give themselves superpowers - both in-person and from afar: a few small tricks, conventions or utilities can become a force-multiplier for software authorship. _Personal_ software authorship. Trashtalk started as a minimal message-passing implementation in bash, intended as an experiment in the direction of enabling expressive personal tool-making in the ugly substrate of shell-scripting.

It lingered in my dotfiles repo for years.

Then LLMs came. I said "Hey Claude, what do you think about this gewgaw over here?" Claude said "You're absolutely right!" and we were off - it morphed into a DSL transpiled into bash, then I added a compiler written in golang to provide native compilation for a subset of the DSL, then I started trying to add a TUI-based Smalltalk-style IDE on top of it. Things continued to get weirder and weirder, each day I travelled half the distance between here and v1.0, and eventually it dawned on me that I'd gone too far, so I dialed back Trashtalk and jettisoned the non-bash bits. More precisely, I spun them off into their own projects. Anyhow, here we are.

## What's it good for?

So far I've only really used Trashtalk to work on Trashtalk. I'll let you know when that changes. Until then, some things I'm thinking about doing include:

- Exploring the idea of an acme-like editor as a substitute for the whiz-bang TUI I tried so desperately to make work
- Building multi-process CLI tools using the Actor/Stream/EventBus classes (backed by [Honker](https://github.com/russellromney/honker) — pub/sub, work queues, and durable streams in SQLite)

If you have any ideas that aren't terribly rude, I'd love to hear them!

## Architecture

Trashtalk uses a **DSL compiler** that transforms Smalltalk-inspired source files (`.trash`) into namespaced Bash functions. This way, we implement message passing without polluting the global namespace. Or, well, we pollute it _in a principled fashion_.

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│  Source (.trash)│────▶│   Compiler   │────▶│ Compiled (bash) │
│                 │     │              │     │                 │
│ Counter subclass│     │  jq-compiler │     │ __Counter__     │
│   method: inc   │     │              │     │   increment()   │
└─────────────────┘     └──────────────┘     └─────────────────┘
                                                      │
                                                      ▼
                                             ┌─────────────────┐
                                             │   Dispatcher    │
                                             │                 │
                                             │ @ Counter inc   │
                                             │       ▼         │
                                             │ __Counter__     │
                                             │   increment()   │
                                             └─────────────────┘
```

### Key Components

- **DSL Compiler** (`lib/jq-compiler/`) - Bash tokenizer and jq parser/code generator that transforms `.trash` source files into executable Bash
- **Dispatcher** (`lib/trash.bash`) - Routes `@` message sends to the appropriate namespaced function
- **Source Files** (`trash/*.trash`) - Human-readable class definitions
- **Compiled Files** (`trash/.compiled/`) - Generated Bash code loaded by the runtime

## Installation

### Requirements

- **Bash 4.4+** — macOS ships bash 3.2 at `/bin/bash`; install a modern one with
  `brew install bash`, put it first on PATH (`export PATH="$(brew --prefix)/bin:$PATH"`),
  then start it with `exec bash`. Build scripts also resolve `bash` from PATH.
- Required tools: `jo`, `jq`, `sqlite3`, `uuidgen`; builds also need `make` and `shasum`
  - macOS: `brew install jo jq sqlite` (`uuidgen` is built in)
  - Debian/Ubuntu: `sudo apt install bash jo jq sqlite3 uuid-runtime make libdigest-sha-perl`

Clone or copy this repository to `~/.trashtalk`:

```bash
git clone https://github.com/chazu/trashtalk.git ~/.trashtalk
```

Compile the bundled classes (required before first use — the runtime dispatches
to generated bash in `trash/.compiled/`):

```bash
cd ~/.trashtalk && make
```

Add the following to your Bash startup file (`~/.bashrc`; source it from
`~/.bash_profile` if you use login shells). Trashtalk must run in Bash:

```bash
source ~/.trashtalk/lib/trash.bash
```

Start a fresh Bash session or source the file above, then verify:

```bash
@ Trash doctor
@ Trash info
```

The Bash floor comes from NUL-delimited `mapfile -d` in the Tool process
boundary. macOS system Bash 3.2 and Zsh are not supported runtimes. See the
[documentation index](docs/README.md) for current APIs, designs, and historical notes.

## Troubleshooting

If anything misbehaves, run the built-in diagnostics first:

```bash
@ Trash doctor      # or: make doctor
```

It checks bash version (needs 4.4+), required tools (`jo`/`jq`/`sqlite3`/`uuidgen`),
whether the sqlite3 in use can load the optional honker extension, and whether
classes have been compiled — and prints a clear OK/WARN/FAIL line for each.
It checks Jcode, the default session harness. Install it from [jcode.sh](https://jcode.sh)
if needed, then use `@ Jcode login` for OpenAI subscription authentication.
When `TRASHTALK_GUSGUS_PROFILE=maki` is selected, doctor installs Maki if missing
and verifies its executable. Maki login is `@ Maki loginToProvider: 'openai'`.

Common fixes:

- **`declare: -A: invalid option` / nothing works on macOS** — you're on the
  system bash 3.2. `brew install bash` and use it (`exec "$(brew --prefix)/bin/bash"`).
- **`unknown command "load"` / honker errors** — your `sqlite3` lacks extension
  support (Apple's does). `brew install sqlite`, then
  `export TRASH_SQLITE3="$(brew --prefix sqlite)/bin/sqlite3"`.
- **`Unknown class '...'`** — run `make` to compile the classes.

`@ Trash help` lists all system commands. Tab completion for `@` loads
automatically in interactive shells.

## Quick Start

```bash
# Send a message to an object
@ Trash info

# Create a counter instance
counter=$(@ Counter new)
@ $counter setValue 5
@ $counter increment 3
@ $counter show

# Create an array
arr=$(@ Array new)
@ $arr push hello
@ $arr push world
@ $arr show

# System introspection
@ Trash listObjects
@ Trash methodsFor Counter
@ Trash help
```

## DSL Syntax

Classes are defined in `.trash` files using a Smalltalk-inspired syntax:

### Basic Class Definition

```smalltalk
# Counter - A simple counter class
Counter subclass: Object
  include: Debuggable
  instanceVars: value:0 step:1

  method: increment [
    | newValue |
    newValue := $(( $(_ivar value) + $(_ivar step) ))
    _ivar_set value "$newValue"
    echo "$newValue"
  ]

  method: setValue: val [
    _ivar_set value "$val"
  ]

  method: show [
    echo "Counter value: $(_ivar value)"
  ]
```

### DSL Elements

| Element | Syntax | Description |
|---------|--------|-------------|
| Class declaration | `ClassName subclass: SuperClass` | Declare a class with inheritance |
| Trait declaration | `TraitName trait` | Declare a trait (mixin) |
| Include trait | `include: TraitName` | Mix in a trait |
| Instance variables | `instanceVars: name:default` | Declare instance vars with defaults |
| Dependencies | `requires: 'path/to/file.bash'` | Source external dependencies |
| Method | `method: name [body]` | Define an instance method |
| Method with args | `method: foo: x bar: y [body]` | Keyword-style arguments |
| Class method | `classMethod: name [body]` | Define a class method |
| Raw method | `rawMethod: name [body]` | Pass-through (no transformation) |
| Test method | `testMethod: name [body]` | Define an inline test (see Testing) |
| Local variables | `\| var1 var2 \|` | Declare local variables |
| Assignment | `var := value` | Assign to variable |
| Self reference | `@ self methodName` | Message to self |

### Method Body Transformations

The compiler transforms DSL constructs to Bash:

```smalltalk
# DSL syntax:
method: example: arg [
  | result |
  result := $(some_command)
  @ self debug: "Got result: $result"
  @ OtherClass doSomething: "$result" with: "$arg"
]

# Compiles to:
__MyClass__example() {
  local arg="$1"
  local result
  result=$(some_command)
  @ "$_RECEIVER" debug "Got result: $result"
  @ OtherClass doSomething_with "$result" "$arg"
}
```

### Raw Methods

Use `rawMethod:` for code that shouldn't be transformed (heredocs, traps, complex bash):

```smalltalk
rawMethod: createConfig: name [
  cat > "$CONFIG_DIR/$name" << 'EOF'
# Configuration file
setting=value
EOF
  echo "Created config: $name"
]
```

### Traits

Traits provide reusable behavior without inheritance:

```smalltalk
Debuggable trait

  method: debug: message [
    [[ "${TRASH_DEBUG:-1}" == "0" ]] && return 0
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] DEBUG ($_RECEIVER): $message" >&2
  ]

  method: inspect [
    echo "Object: $_RECEIVER"
    echo "Class: $_SUPERCLASS"
  ]
```

### Aspect-Oriented Programming (AOP)

Trashtalk supports before/after advice for cross-cutting concerns like logging, validation, or notifications:

```smalltalk
Account subclass: Object
  instanceVars: balance:0

  method: withdraw: amount [
    balance := balance - amount
  ]

  method: deposit: amount [
    balance := balance + amount
  ]

  # Run before withdraw: executes
  before: withdraw: do: [
    @ self log: "Attempting withdrawal"
  ]

  # Run after deposit: completes
  after: deposit: do: [
    @ self notifyBalanceChanged
  ]
```

Advice hooks execute automatically - `before:do:` runs prior to the method, `after:do:` runs after it returns.

### Inline Testing

Trashtalk supports defining tests directly in class files using `testMethod:`.
When `inmacs` is on `PATH`, `@ Trash edit: ClassName` opens the source in the
Innards inline editor with Trashtalk syntax highlighting and two-space
indentation. Saving compiles to a temporary artifact, checks the generated Bash,
installs and reloads the class, then runs its inline tests. Compiler and test
failures reopen the editor as annotations at the relevant source line; they are
never inserted into the `.trash` source.

If Innards is unavailable, the edit command falls back to `$VISUAL`, then
`$EDITOR`, then `vi`. The fallback still uses the same compile, validation,
reload, and test pipeline after the file changes. `@ Trash doctor` reports
Innards availability as an optional capability.

### Class, Method, and Instance Browser

With `inpick` on `PATH`, Trashtalk derives browser records directly from the
canonical jq compiler AST and previews the selected source without maintaining
a second index. `fzf` is used as a fallback when Innards is unavailable.

```bash
@ Trash browse                         # choose any symbol and open its source
@ Trash browseClass: Counter           # browse one class and open a selection
@ Trash pickMethod: Counter            # return a structured method selection
@ Trash browseImplementorsOf: 'at:put:'
@ Trash browseSendersOf: 'at:put:'
@ Trash browseInstancesOf: Counter     # persisted instances and compact ivar state
@ Trash inspectInstancesOf: Counter    # choose an instance, then inspect it
```

Class, trait, instance-variable, class-variable, instance-method, class-method,
and test-method records carry exact source positions. Namespaced classes and
complete multi-keyword selectors remain intact. Browser selection results are
JSON; commands that open source feed the chosen path and line into the same
transactional edit/compile/test loop described above.

### Object Inspector

With `ininspect` on `PATH`, any persisted object can open as a navigable inline
tree. Containers expand in place and `e` on a scalar edits it as a JSON value:

```bash
counter=$(@ Counter create)
@ "$counter" inspectInteractive
```

Innards only returns an edit proposal. Trashtalk checks that the object and its
selected value have not changed, rejects unknown or command-bearing fields,
and then applies the typed value through `Runtime`. Runtime metadata is not
offered as editable state. Plain `@ "$counter" inspect` remains the textual
fallback and never requires Innards.

```smalltalk
Counter subclass: Object
  instanceVars: value:0 step:1

  method: increment [
    value := value + step.
    ^ value
  ]

  method: setStep: s [
    step := s
  ]

  testMethod: testIncrement [
    pragma: primitive
    local c result
    c=$(@ Counter new)
    result=$(@ "$c" increment)
    _assert_eq "$result" "1" "increment returns 1"
    @ "$c" destroy
  ]

  testMethod: testCustomStep [
    pragma: primitive
    local c
    c=$(@ Counter new)
    @ "$c" setStep: 5
    _assert_eq "$(@ "$c" increment)" "5" "custom step works"
    @ "$c" destroy
  ]
```

#### Assertion Functions

Tests use TAP (Test Anything Protocol) assertions:

| Function | Description |
|----------|-------------|
| `_assert_eq "$actual" "$expected" "desc"` | Assert values are equal |
| `_assert_neq "$actual" "$unexpected" "desc"` | Assert values are not equal |
| `_assert_true "$value" "desc"` | Assert value is non-empty |
| `_assert_false "$value" "desc"` | Assert value is empty |
| `_assert_contains "$haystack" "$needle" "desc"` | Assert string contains substring |
| `_assert_ok "command" "desc"` | Assert command succeeds (exit 0) |

#### Running Tests

```bash
# Run tests for a class
@ Trash runTestsFor: Counter

# Check if a class has tests
@ Trash hasTestsFor: Counter

# Tests run automatically during edit flow
@ Trash edit: Counter
```

Output follows TAP format:

```
# Running tests for Counter
ok 1 - increment returns 1
ok 2 - custom step works
1..2
# All 2 tests passed
```

## Code and session tools

`Tools::Roam` provides checkout-local indexing and code-graph queries;
`Tools::AstGrep` searches syntax patterns; `Tools::Cass` searches existing agent
sessions. `Tools::Chad` wraps the local Chad harness for explicit headless tasks
and plan mode. All use exact argument vectors and preserve process diagnostics.
See [code and session Tool adapters](docs/code-and-session-tools.md) for setup,
examples, result contracts, and qualification limits.

## Live agent conversations

Use `@ AgentSession browse` and choose **Attach to conversation**, or send
`focus` / `attach` to an existing session. The Innards `inagent` applet shows
backlog and live harness output, offers an inbox-backed composer, and detaches
without stopping work. Message actions also offer **Attach to sender session**
when the sender can be resolved. See [session view controls and setup](docs/agent-session-view.md).

## Gusgus: the assistant behind `@@`

`@@` talks to Gusgus, a persistent assistant with one conversation
per workspace (the git repository root, or the directory itself outside a
repository). It sends your message and returns immediately; Gusgus works in a
managed Jcode session by default and answers into your inbox, in the same thread as
your question. Replying to that message continues the same conversation.

```bash
false
__='the command produced this output'
@@ 'why did that fail?'                 # prints the message id and returns

inbox=$(@ Trash userInbox)
@ $inbox list                            # Gusgus's reply appears here
@ $inbox show: $msg
@ $msg reply: 'and how do I fix it?'     # resumes the same conversation
@ $inbox thread: $msg                    # the whole exchange, oldest first

@@ --fresh 'unrelated question'          # close this workspace's session, start another
@@ --one-shot 'question'                 # stateless one-shot path (below)
@@ --dry-run 'question'                  # show the one-shot context, no model call
@ Gusgus help
```

`inbox=$(@ Trash userInbox)` returns your persisted `Inbox` instance, using
`TRASHTALK_USER` with `$USER` as the fallback. Other inboxes are instances of
the same class, retrieved with `@ Inbox named: 'gusgus'`.

`@ "$inbox" browse` opens that inbox in Innards: `inpick` lists the messages
with a rendered preview of each. Displaying a preview marks that message read
and clears its unread dot. Opening a thread marks its messages read too.
**Ctrl-D** archives the highlighted message
and refreshes the inbox; archived messages remain available in their threads.
**Enter** opens actions for reply, viewing the thread in `inpage`, archive,
or back. Reply composes
in `inmacs` and sends the saved text into the thread, which resumes Gusgus
when the message came from a session. Without Innards the same loop falls
back to `fzf` and `$EDITOR`.

`@ "$inbox" count` returns its total non-archived messages, including read
messages. `@ "$inbox" unreadCount` counts only unread messages.
`@ Inbox count` counts stored inbox instances. The `Inbox` class does not
implicitly select the current user's inbox.

Each `@@` becomes an `AgentDelivery` on the workspace's `AgentSession`.
`AgentWorker` notifies the configured harness with inbox message references;
the agent reads their contents from Inbox and uses `AgentRun result:forDelivery:`
and `settle:` to respond and acknowledge work. Every run gets a private launcher
for the common `trash-send` API. Busy sessions queue messages for the next prompt.
Jcode is the default and uses a resident daemon, resuming the same native
conversation across runs. Jcode and Maki run with your normal OS permissions.
`@@` and inbox replies request foreground ticks. For queued work to continue
without another command, run `bin/trash-worker` or install and start its user
service with `bin/trash-worker-service install` and `bin/trash-worker-service start`.
`@ AgentSession browse` opens session activity, messages, run logs, and explicit
pause/resume/retry actions in Innards. See [agent operations](docs/agent-operations.md)
for recovery behavior, service controls, and validation. Gusgus uses OpenAI
OAuth and medium reasoning effort. Configure with
`TRASHTALK_JCODE_MODEL` (default `gpt-5.6-terra`),
`TRASHTALK_USER` (your inbox name, default `$USER`), and
`TRASHTALK_GUSGUS_PROFILE` (`jcode` by default; `maki` for Maki; `codex` or the legacy
`assistant-low-power` for Codex; `shell` for a script in
`TRASHTALK_SHELL_DRIVER`). Codex uses `TRASHTALK_CODEX_MODEL`
(default `gpt-5.6-terra`). Maki uses `TRASHTALK_MAKI_MODEL`
(default `openai/gpt-5.6-terra`). Jcode uses existing OpenAI subscription login.
`@ Jcode login` starts interactive authentication. `@ "$run" stop` pauses its
session and stops that exact run; agents use `AgentRun stop:` with `agent.stop`
role authority. Profiles are captured when a session opens;
changing the default does not migrate existing conversations. See the
[Jcode driver design](docs/jcode-session-driver.md),
[Maki driver design](docs/maki-session-driver.md), and
`docs/headless-agent-sessions-design.md`.

`Assignment` adds durable work owned by an identity, with explicit session
selection, progress, inbox questions, and atomic completion. Its first slice is
manual: published work is held from harness dispatch. Follow the
[Assignment walkthrough](docs/assignments.md) to try it from Bash.

## One-shot agent questions

`@@ --one-shot` sends one explicit, read-only request through the selected
external agent harness with no memory. Axe is the default; the official Codex
CLI is also supported. The request includes the question, current working
directory, previous command status, and `$__` when it is set. The final answer
opens in `inpage` when available and is also printed into shell scrollback.

```bash
false
__='the command produced this output'
@@ --one-shot 'why did that fail?'

# Inspect the selected backend's exact context without making an LLM call.
@@ --dry-run 'what context would you receive?'
```

Choose the backend in `~/.trashrc`:

```bash
# Default: Axe with the checked-in project-local agent profile.
TRASHTALK_AGENT_BACKEND=axe

# Official Codex CLI using a ChatGPT subscription login.
TRASHTALK_AGENT_BACKEND=codex
```

For Codex, run `codex login` and select the ChatGPT login, then verify it with
`codex login status`. The adapter refuses API-key authentication and removes
`CODEX_API_KEY` and `OPENAI_API_KEY` from the child process so selecting this
backend cannot silently fall back to per-token API billing. It invokes
`codex exec` ephemerally, ignores user tool configuration, and fixes the
sandbox to read-only. See OpenAI's documentation for
[authentication](https://learn.chatgpt.com/docs/auth) and
[non-interactive Codex](https://learn.chatgpt.com/docs/non-interactive-mode).

Trashtalk never installs Axe or initializes credentials implicitly. Install it
explicitly with `go install github.com/jrswab/axe@latest`, configure the
provider required by `axe/agents/trashtalk-readonly.toml`, and use `@ Trash
doctor` to check availability. The checked-in agent enables only Axe's
`list_directory` and `read_file` tools—no file mutation, shell commands, or
subagents.

`@@` preserves Axe's status distinctions: `1` runtime, `2` configuration, `3`
provider/network, and `4` budget exhaustion. Missing Axe returns `127`.
Codex failures preserve their original process status; a non-ChatGPT login is
reported as configuration exit `2`, and a missing Codex CLI as `127`.

## Reviewed source proposals

Source mutation is a separate operation from `@@`. A specialized read-only Axe
agent can propose a one-file `.trash` unified diff, but cannot apply it:

```bash
run=$(@ Agent propose: 'make value return 2' for: Counter)
proposal=$(printf '%s' "$run" | jq -r .result.content)
@ Agent reviewAndApplyProposal: "$proposal"
```

Proposal schema v1 is closed and deliberately narrow:

```json
{
  "schema_version": 1,
  "kind": "trashtalk_source_patch",
  "files": [{
    "class_name": "Counter",
    "path": "trash/Counter.trash",
    "base_sha256": "<64 lowercase hex characters>",
    "diff": "--- a/trash/Counter.trash\n+++ b/trash/Counter.trash\n..."
  }]
}
```

`indiff` only displays the diff and records zero-based accepted/rejected hunk
indices. After an acceptance, Trashtalk validates that complete decision,
rechecks the source hash, applies only accepted hunks to a temporary copy,
compiles it with the canonical jq compiler, validates generated Bash, runs the
candidate's tests, checks the hash again, and then installs source and artifact
with rollback backups. Rejection, cancellation, stale hashes, invalid paths or
headers, and failed gates leave the working source and compiled artifact
unchanged. Command fields and multi-file proposals are rejected; no
agent-authored command is executed.

## Compiling Classes

Compile a single class:

```bash
make single CLASS=MyClass
```

Compile all classes:

```bash
make compile
```

Or use the compiler directly:

```bash
lib/jq-compiler/driver.bash compile trash/MyClass.trash > trash/.compiled/MyClass
```

## Profiling

Trashtalk includes a built-in profiling system to help identify performance bottlenecks and optimize method dispatch.

### Enabling Profiling

Runtime diagnostics default to warnings/errors on stderr. Use
`TRASHTALK_LOG_LEVEL=debug` for method-resolution diagnostics or `trace` to
include message arguments. Interactive slow operations show delayed progress
on `/dev/tty`; `TRASHTALK_PROGRESS=0` disables it. See
[performance and terminal output](docs/performance.md) for JSON-value
construction, browser caching, and the isolated `bin/trash-bench` harness.

Set `TRASH_PROFILE=1` to enable profiling output:

```bash
# Profile to stderr
TRASH_PROFILE=1 @ Counter new

# Profile to a file
TRASH_PROFILE=1 TRASH_PROFILE_FILE=profile.log @ MyApp run
```

### Profile Output Format

Profiling logs entry and exit points with timing:

```
[1767909948.119] → Counter.new [bash]
[1767909948.295] ← Counter.new [bash] 176ms
```

- `→` marks method entry
- `←` marks method exit with elapsed time
- Routes identify Bash dispatch (`bash`) or caller-shell methods (`bash:direct`).

### Environment Variables

| Variable | Description |
|----------|-------------|
| `TRASH_PROFILE=1` | Enable profiling output |
| `TRASH_PROFILE_FILE=path` | Write to file instead of stderr |
| `TRASH_PROFILE_DEPTH=N` | Only log calls up to depth N |
| `TRASH_PROFILE_MIN_MS=N` | Only log calls taking >= N milliseconds |

### Profile Analyzer

Use `bin/trash-profile-analyze` to generate reports from profile logs:

```bash
# Generate profile data
TRASH_PROFILE=1 @ MyApp run 2>profile.log

# Analyze the profile
bin/trash-profile-analyze profile.log
```

The analyzer reports dispatch routes, slowest individual calls, the most-called
methods, and classes by call count. Method times include nested calls, so their
sum is not wall-clock duration. The report's timestamp span covers the first
through last completed call. Use these measurements to choose a representative
workflow to benchmark; the report does not infer subprocess counts or recommend
another runtime.

## Core Classes

| Class | Description |
|-------|-------------|
| `Object` | Root class with new, findAll, count methods |
| `Trash` | System introspection and management |
| `Store` | SQLite-backed instance persistence |
| `Array` | Dynamic array with push, pop, map, filter |
| `Counter` | Simple counter with increment/decrement |
| `File` | File system operations (read, write, temp files) |
| `Future` | Async computation with result retrieval |
| `Process` | External OS process management (subprocess-like) |
| `ReplServer` | Socket-based REPL server for Emacs integration |
| `Honker` | Pub/sub, work queues, streams, locks, rate limiting (requires honker extension) |
| `EventBus` | Observer pattern via ephemeral pub/sub |
| `Actor` | Mailbox-style actors with background dispatch and at-least-once delivery |
| `Stream` | Cross-process durable streams with consumer offset tracking |
| `Scheduler` | Cron-based periodic tasks with leader election |
| `Inbox` | Durable named inboxes for messages between agents, humans, and processes |
| `Message` | A persisted message: sender, recipient, kind, status, thread |
| `Gusgus` | The persistent assistant behind `@@`: one session per workspace |
| `AgentSession` | Durable agent conversation bound to an identity, archetype, role, and workspace |
| `AgentIdentity`, `AgentArchetype`, `AgentRole` | Who an agent is, what it is for, and what it may do |
| `AgentRun`, `AgentDelivery` | One harness process, and the durable input batch it was offered |
| `AgentWorker` | Foreground dispatch and reconciliation: claim, launch, settle |
| `JcodeDriver`, `MakiDriver`, `CodexDriver`, `ShellDriver` | Common session drivers for resident Jcode, Maki SDK, Codex, and test scripts |

### Traits

| Trait | Description |
|-------|-------------|
| `Persistable` | Save/reload, deletion, and Store queries |
| `Debuggable` | Debug logging, inspection, ancestry tracing |
| `Assignment::Authority`, `Assignment::Reporting`, `Assignment::Presentation` | Assignment authorization, progress/questions, and views |
| `Observable` | Event emission, subscription, and atomic save+emit for any class |

## Message Sending

```bash
# Basic syntax
@ <Receiver> <selector> [args...]

# Examples
@ Trash info                      # No arguments
@ Counter new                     # Returns instance ID
@ "$counter" incrementBy: 5            # Instance method with arg
@ Store getField: "$id" field: name # Public keyword message
```

## Instance Persistence

`new` immediately saves an initial SQLite record. Subsequent mutations update
the session cache and need an explicit `save` to become durable:

```bash
counter=$(@ Counter new)
@ "$counter" value: 42
@ "$counter" save
@ Counter findAll                 # List stored Counter instances
@ Counter find: 'value > 10'       # Query durable state
@ Counter count
```

`Persistable` supplies save, reload, deletion, and query methods. See
[object persistence](docs/persistence.md) for cache freshness, deletion, and
Store transactions.

## Honker Integration

Trashtalk optionally integrates with [Honker](https://github.com/russellromney/honker), a SQLite loadable extension that adds pub/sub, work queues, durable streams, distributed locks, rate limiting, and cron scheduling — all backed by the same SQLite database used for instance persistence. No extra processes or external brokers needed.

### Installation

The easiest way is the bundled installer, which clones honker, builds it with `cargo`, and drops the artifact into `~/.trashtalk/lib/vendor/honker/`:

```bash
bin/install-honker              # build from main + install
bin/install-honker --ref v0.2   # pin to a tag/branch/commit
```

Requires `cargo` (Rust toolchain) and a `sqlite3` built with loadable-extension support. On macOS, the system `sqlite3` does **not** allow `.load`; install Homebrew's and put it ahead on `PATH`:

```bash
brew install sqlite
export PATH="$(brew --prefix sqlite)/bin:$PATH"
```

Or install manually:

```bash
# Option 1: Project-local
cp libhonker_ext.dylib ~/.trashtalk/lib/vendor/honker/  # macOS
cp libhonker_ext.so ~/.trashtalk/lib/vendor/honker/     # Linux

# Option 2: System-wide
cp libhonker_ext.dylib /usr/local/lib/   # macOS
cp libhonker_ext.so /usr/local/lib/      # Linux

# Option 3: Explicit path
export HONKER_EXT=/path/to/libhonker_ext
```

Trashtalk auto-detects honker at startup. Everything works without it — honker-dependent classes degrade gracefully, and tests skip automatically.

### EventBus — Observer Pattern

```bash
# Create a named event bus
bus=$(@ EventBus named: 'orders')

# Subscribe with a handler block
handler=$(@ Block params: '["payload"]' code: 'echo "Got: $payload"' captured: '{}')
@ $bus on: 'created' do: $handler

# Emit events
@ $bus emit: 'created' payload: '{"id":42,"total":99.50}'
@ $bus emit: 'shipped'                # no payload

# Clean up
@ $bus shutdown
```

### Observable Trait — Events on Any Class

```smalltalk
Order subclass: Object
  include: Persistable
  include: Observable
  instanceVars: status:'pending' total:0
```

```bash
order=$(@ Order new)
@ $order on: 'completed' do: $handler    # subscribe to this instance
@ $order emit: 'completed'               # fire event
@ $order saveAndEmit: 'saved'            # atomic persist + event in one transaction
```

### Actor — Mailbox Message Processing

Each actor has a named queue. Messages are processed sequentially by a background dispatcher. Honker provides at-least-once delivery with retries and dead-lettering.

```bash
actor=$(@ Actor named: 'order-processor')
@ $actor start                            # start background dispatcher

@ $actor send: 'processOrder' with: '42'
@ $actor send: 'cleanup'
@ $actor pendingCount                     # check mailbox depth

@ $actor stop
```

### Stream — Cross-Process Durable Streams

Two separate trashtalk programs sharing the same database can communicate through streams. Messages survive crashes, and consumers track their position with offsets.

```bash
# Terminal 1 (producer)
producer=$(@ Stream named: 'metrics')
@ $producer publish: '{"cpu":42,"host":"web1"}'

# Terminal 2 (consumer)
consumer=$(@ Stream named: 'metrics' consumer: 'dashboard')
msgs=$(@ $consumer read)
@ $consumer ack: 5                        # advance offset
@ $consumer consumeDo: $handler           # continuous background consumption
```

### Scheduler — Cron Tasks with Leader Election

```bash
@ Scheduler every: '*/5 * * * *' call: 'cleanup_fn' named: 'cleanup'
@ Scheduler start                         # start tick loop
@ Scheduler stop
```

Multiple processes can run the scheduler — honker's leader election ensures each task fires exactly once.

### Locks and Rate Limiting

```bash
@ Honker lock: 'deploy'                  # acquire distributed lock
@ Honker unlock: 'deploy'

allowed=$(@ Honker rateLimit: 'api-call' limit: 100 window: 60)
```

## Inboxes

An `Inbox` is a durable, named mailbox that agents, humans, and deterministic
processes all share the same way. Messages are ordinary `Persistable` objects,
so they survive process exit and can be listed, read, replied to, and archived
from any trashtalk process using the same Store. Inboxes are created on first
use; names may contain letters, digits, `_ . : -`.

```bash
# An agent (or cron job) reports to a human
@ Inbox send: 'all 71 tests pass' to: 'chazu' from: 'maki:abc123' subject: 'done' kind: 'result'
@ Inbox alert: 'disk 95%' to: 'chazu' from: 'cron'

# An agent asks a question and waits for the answer to land in its own inbox
q=$(@ Inbox ask: 'ok to force-push?' to: 'chazu' from: 'maki:abc123')

# The human reads and replies from the REPL
inbox=$(@ Inbox named: 'chazu')
@ $inbox list                     # unread messages, one line each
@ $inbox show: $q                 # full message; marks it read
@ $q reply: 'yes'                 # lands in maki:abc123's inbox, same thread

# The agent finds the answer
@ $(@ Inbox named: 'maki:abc123') unread
@ $(@ Inbox named: 'maki:abc123') thread: $q      # question + reply, oldest first
```

Queries return instance ids, one per line: `unread`, `unreadCount`,
`questions` (unread, kind `question`), `messages` / `messages: n` (recent,
non-archived, newest first), `thread: id`. Bulk actions: `readAll`, and per
message `markRead` / `archive`. Kinds are free-form; `note`, `question`,
`alert`, and `result` are the conventions the helpers use.

### Wakeups

Delivery transports live outside the core. With the honker extension, an
inbox can run a `Block` for every future delivery; the block receives the
message as JSON. Put whatever reaches you there: `mosquitto_pub`, `tmux
send-keys`, a desktop notifier, or a message send into another Trashtalk
object.

```bash
handler=$(@ Block params: '["payload"]' \
  code: 'mosquitto_pub -t "inbox/$(jq -r .to <<<"$payload")" -m "$(jq -r .body <<<"$payload")"' \
  captured: '{}')
@ $inbox onMessage: $handler      # background listener; returns its pid
@ $inbox stopListening
```

Without honker, `send`/`read`/`reply` work unchanged; `onMessage:` warns and
returns an empty pid. `@ Inbox help` and `@ Message help` list every message.

## Development loop

Run `bin/trash` for the REPL. It uses Bash Readline for editing, Up/Down history,
and Tab completion of classes, live object variables, and methods; no rlwrap
filter is required. History defaults to `~/.trash_history`; override it with
`TRASHTALK_HISTORY_FILE`. `bin/trash --help` shows examples, and piped message
input works without terminal setup.

`make` skips unchanged compiled classes and rebuilds parent/trait dependencies
before their dependents. `make single CLASS=Counter` uses the same build cache.
Run `make verify` to build and check both runtime and compiler suites in isolated
parallel test checkouts. See [performance](docs/performance.md) for controls and
[JSON values](docs/json-values.md) for typed reads, bulk field binding, and
collection traversal primitives.

## Dependencies

Vendored in `lib/vendor/`:
- `sqlite-json.bash` - SQLite-based JSON document store and key-value persistence
- `honker.bash` - Bash wrapper for the Honker SQLite extension (pub/sub, queues, streams)
- `tuplespace/` - Event coordination (legacy; can be upgraded to honker via `tuplespace-honker.bash` shim)
- `bsfl.sh` - Bash utility functions
- `fun.sh` - Functional programming utilities

External tools (install separately):
- `jo` - JSON output from shell
- `jq` - JSON processor
- `sqlite3` - Database engine
- `uuidgen` - UUID generation (usually pre-installed)
- `libhonker_ext` - Honker SQLite extension (optional — enables EventBus, Actor, Stream, Scheduler)

## Emacs Integration

Trashtalk includes a major mode for Emacs with syntax highlighting, indentation, and REPL integration for interactive development.

### Installation

Add to your `init.el`:

```elisp
(add-to-list 'load-path "~/.trashtalk/emacs")
(require 'trashtalk-mode)
```

Or with `use-package`:

```elisp
(use-package trashtalk-mode
  :load-path "~/.trashtalk/emacs"
  :mode "\\.trash\\'")
```


## File Structure

```
~/.trashtalk/
├── emacs/
│   └── trashtalk-mode.el    # Emacs major mode with REPL support
├── lib/
│   ├── trash.bash           # Main runtime & dispatcher
│   ├── jq-compiler/         # jq-based DSL compiler
│   │   ├── driver.bash      # CLI entry point
│   │   ├── tokenizer.bash   # Source → JSON tokens
│   │   ├── parser.jq        # Tokens → AST
│   │   └── codegen.jq       # AST → Bash code
│   └── vendor/              # Vendored dependencies
│       ├── sqlite-json.bash # SQLite JSON document store
│       ├── honker.bash      # Honker extension wrapper
│       └── tuplespace/      # Legacy event coordination
├── trash/
│   ├── *.trash              # DSL source files
│   ├── .compiled/           # Compiled output
│   │   └── traits/          # Compiled traits
│   └── traits/              # Trait source files
│       ├── Debuggable.trash
│       ├── Observable.trash # Event emission mixin
│       └── ...
└── tests/                   # Test scripts
```

## Version

Supposedly v1.0.0

## Author

Chaz Straney
