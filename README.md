<p align="center">
  <img src="https://github.com/chazu/trashtalk/blob/main/img/logo.png">
</p>


# Trashtalk

A Smalltalk-inspired message-passing system for Bash.

Trashtalk brings object-oriented programming concepts to shell scripting: message passing, inheritance, traits, and instance persistence - all compiled to namespaced Bash functions.

## Architecture

Trashtalk uses a **DSL compiler** that transforms Smalltalk-inspired source files (`.trash`) into namespaced Bash functions. This avoids polluting the global namespace while providing clean OOP semantics.

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
| `Tuplespace` | Linda-style tuple coordination (SQLite backend) |
| `Twin` | Window manager integration (twsendmsg) |

### Traits

| Trait | Description |
|-------|-------------|
| `Debuggable` | Debug logging, inspection, ancestry tracing |

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

## Dependencies

Vendored in `lib/vendor/`:
- `sqlite-json.bash` - SQLite-based JSON document store and key-value persistence
- `tuplespace/` - Event coordination
- `bsfl.sh` - Bash utility functions
- `fun.sh` - Functional programming utilities

External tools (install separately):
- `jo` - JSON output from shell
- `jq` - JSON processor
- `sqlite3` - Database engine
- `uuidgen` - UUID generation (usually pre-installed)

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

### REPL Server

The REPL server provides interactive evaluation, hot reloading, and introspection from Emacs.

**Start the server** in a terminal:

```bash
@ ReplServer start
```

**Connect from Emacs** with `C-c C-z` in any `.trash` buffer.

### Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c` | `trashtalk-eval-defun` | Evaluate method at point |
| `C-c C-r` | `trashtalk-eval-region` | Evaluate selected region |
| `C-c C-l` | `trashtalk-eval-line` | Evaluate current line |
| `C-c C-b` | `trashtalk-eval-buffer` | Evaluate entire buffer |
| `C-c C-k` | `trashtalk-reload-current-file` | Recompile and reload class |
| `C-c C-z` | `trashtalk-repl-connect` | Connect to REPL server |
| `C-c C-i` | `trashtalk-info-at-point` | Show info for symbol at point |
| `C-c C-m` | `trashtalk-methods-for-class` | List methods for a class |

### REPL Protocol

The server uses a simple line-based protocol over a Unix socket (`/tmp/trashtalk-repl.sock`):

```
Request:  COMMAND:payload
Response: STATUS:result
```

Commands: `EVAL`, `COMPLETE`, `INFO`, `METHODS`, `RELOAD`, `PING`, `QUIT`

You can also interact with the server from the command line:

```bash
echo "PING" | nc -U /tmp/trashtalk-repl.sock
echo "EVAL:@ Counter new" | nc -U /tmp/trashtalk-repl.sock
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
├── trash/
│   ├── *.trash              # DSL source files
│   ├── .compiled/           # Compiled output
│   │   └── traits/          # Compiled traits
│   └── traits/              # Trait source files
└── tests/                   # Test scripts
```

## Version

0.1.0

## Author

Chaz Straney
