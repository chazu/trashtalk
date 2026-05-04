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

- **DSL Compiler** (`lib/jq-compiler/`) - jq-based two-pass compiler that transforms `.trash` source files into executable Bash
- **Dispatcher** (`lib/trash.bash`) - Routes `@` message sends to the appropriate namespaced function
- **Source Files** (`trash/*.trash`) - Human-readable class definitions
- **Compiled Files** (`trash/.compiled/`) - Generated Bash code (also copied to `trash/` for runtime)

## Installation

Clone or copy this repository to `~/.trashtalk`:

```bash
git clone <repo-url> ~/.trashtalk
```

Add the following to your `.bashrc` or `.zshrc`:

```bash
source ~/.trashtalk/lib/trash.bash
```

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

Trashtalk supports defining tests directly in class files using `testMethod:`. Tests run automatically when using `@ Trash edit: ClassName` - if tests fail, you're returned to the editor.

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
[1767909948.119] → Counter.new [native]
[daemon] Counter.new 44ms route=fallback reason=no_plugin
[1767909948.248] → Counter.new [native→bash]
[1767909948.295] ← Counter.new [native→bash] 153ms
```

- `→` marks method entry
- `←` marks method exit with elapsed time
- Route types: `native`, `bash`, `native→bash`, `bash:direct`

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

The analyzer generates a report showing:

- **Dispatch Routing**: Breakdown of native vs bash execution
- **Slowest Methods**: Top 10 methods by execution time
- **Most Called Methods**: Top 10 methods by call count
- **Classes by Call Count**: Which classes are used most
- **Recommendations**: Suggestions for optimization (e.g., classes that would benefit from native plugins)

Example output:

```
================================================================================
                        TRASHTALK PROFILE REPORT
================================================================================

Run duration: 2.5 seconds
Total method calls: 150
Total method time: 2340ms

DISPATCH ROUTING
----------------
  [native→bash]          120 calls ( 80%)   avg    15ms   total   1800ms
  [bash]                  30 calls ( 20%)   avg    18ms   total    540ms

SLOWEST METHODS (top 10)
------------------------
     153ms  Dictionary.new                           [native→bash]
      89ms  Array.map                                [bash]
      ...

RECOMMENDATIONS
---------------
  1. Dictionary has 45 calls but no native support - prioritize for dylib
```

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

### Traits

| Trait | Description |
|-------|-------------|
| `Debuggable` | Debug logging, inspection, ancestry tracing |
| `Observable` | Event emission, subscription, and atomic save+emit for any class |

## Message Sending

```bash
# Basic syntax
@ <Receiver> <selector> [args...]

# Examples
@ Trash info                      # No arguments
@ Counter new                     # Returns instance ID
@ $counter increment 5            # Instance method with arg
@ Store getField_field "$id" name # Keyword method (compiled form)
```

## Instance Persistence

Instances are stored in SQLite via the Store class:

```bash
# Create and persist
counter=$(@ Counter new)
@ $counter setValue 42

# Find later
@ Counter findAll                 # List all Counter instances
@ Counter find "value > 10"       # Query with predicate
@ Counter count                   # Count instances
```

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
