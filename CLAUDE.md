# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Trashtalk?

Trashtalk is a Smalltalk-inspired DSL compiler and runtime for Bash. It transforms `.trash` source files into executable code, providing OOP semantics (classes, inheritance, traits, instance persistence).

## Critical Invariants

### Primitive vs Non-Primitive Classes

**This is the most important architectural constraint.**

| Class Type | Pragma | Methods | Traits | Execution |
|------------|--------|---------|--------|-----------|
| **Primitive** | `pragma: primitiveClass` | ALL must be `rawMethod`/`rawClassMethod` | NOT allowed | Bash (rawMethods) or Native (Procyon) |
| **Non-Primitive** | (none) | ZERO raw methods | Allowed | Pure DSL, compiles to native |

**Primitive classes** wrap system/external functionality and must have **semantically parallel implementations** in both:
- Bash: via `rawMethod`/`rawClassMethod` (using shell commands, grpcurl, etc.)
- Native: via Procyon codegen (Go implementations in `pkg/codegen/primitives.go`)

**Current primitive classes**: Object, File, String, Shell, Console, GrpcClient, Http, Time, Env, Block, Coproc, FIFO, Store, Runtime, Protocol, Tool, Future

**Non-primitive classes** are pure Trashtalk DSL. They use primitive classes for any system interaction.

**Current goal**: Convert all non-primitive classes to pure Trashtalk that executes fully in native mode.

## Architecture

### Compilation Pipeline

```
.trash source → jq-compiler (parse) → AST JSON → Procyon → Output
                                                    │
                                    ┌───────────────┴───────────────┐
                                    │                               │
                              --mode=bash                    --mode=plugin
                                    │                               │
                                    ▼                               ▼
                           Compiled Bash              Go source → .dylib plugin
                         (trash/.compiled/)         (trash/.compiled/.build/)
```

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| jq-compiler | `lib/jq-compiler/` | Tokenizes and parses `.trash` → AST JSON |
| Procyon | `lib/procyon/procyon` | Code generator (bash and native backends) |
| Procyon source | `~/dev/go/procyon` | Go project with parser, IR, codegen |
| tt | `lib/tt` | Loads native plugins, handles dispatch |
| Runtime | `lib/trash.bash` | Bash dispatcher, routes `@` message sends |
| Compiled bash | `trash/.compiled/*` | Generated bash functions |
| Native plugins | `trash/.compiled/*.dylib` | Compiled Go shared libraries |

### Procyon

Procyon is the code generator that produces both bash and native Go code from Trashtalk AST.

**Repository**: `~/dev/go/procyon`

**Modes**:
- `--mode=bash`: Generates bash functions (default compilation target)
- `--mode=plugin`: Generates Go code that compiles to `.dylib` plugins

**Key files in Procyon**:
- `pkg/codegen/codegen.go` - Main code generation
- `pkg/codegen/primitives.go` - Native implementations for primitive classes
- `pkg/codegen/codegen_grpc.go` - GrpcClient native implementations

### Runtime Execution Model

1. **Bash mode**: `source lib/trash.bash` loads runtime, dispatcher routes `@` sends to compiled bash functions
2. **Native mode**: `tt` daemon loads `.dylib` plugins, handles dispatch with fallback to bash for uncompiled methods
3. **Hybrid**: Native daemon can call bash methods and vice versa; instances stored in shared SQLite

## Build Commands

```bash
make                      # Full build (bash + plugins + daemon)
make bash                 # Compile to bash only
make plugins              # Build native .dylib plugins
make daemon               # Build tt daemon
make legacy               # Use legacy jq-compiler only (no Procyon)
make single CLASS=Counter # Compile single class
make single CLASS=Yutani/Widget  # Namespaced class
make test                 # Run all tests
make clean                # Remove build artifacts
```

## Testing

```bash
make test                 # Run all tests, show pass/fail summary
make test-verbose         # Run tests with bash -x tracing
```

Test files are in `tests/test_*.bash`.

## Runtime Usage

```bash
source lib/trash.bash              # Load the runtime
@ Trash info                       # System info
counter=$(@ Counter new)           # Create instance
@ $counter increment 5             # Call instance method
```

## DSL Syntax Quick Reference

```smalltalk
# Non-primitive class (pure DSL, no rawMethods)
Counter subclass: Object
  instanceVars: value:0 step:1

  method: increment [
    | newValue |
    newValue := value + step.
    value := newValue.
    ^ newValue
  ]

  classMethod: create [
    @ Counter new
  ]

# Primitive class (all rawMethods, no traits)
MyPrimitive subclass: Object
  pragma: primitiveClass
  instanceVars: data:''

  rawMethod: doSomething [
    # Raw bash code here
    echo "result"
  ]

  rawClassMethod: create [
    local instance
    instance=$(@ MyPrimitive new)
    echo "$instance"
  ]
```

### Key Patterns

**JSON extraction** (use String primitive):
```smalltalk
value := @ String jsonPath: 'session.id' from: jsonResponse.
```

**Handler/closure pattern** (for callbacks in non-primitive classes):
```smalltalk
# Store handler in ivar, implement valueWith: to receive callbacks
method: onEventDo: handler [
  eventHandler := handler.
  @ someService streamWithCallback: self
]

method: valueWith: data [
  (eventHandler notEmpty) ifTrue: [
    @ eventHandler value: data
  ]
]
```

**Predicates that work**:
```smalltalk
(value isEmpty) ifTrue: [...]      # Works
(value notEmpty) ifTrue: [...]     # Works
```

## Key Transformations

| DSL | Compiles To |
|-----|-------------|
| `Counter subclass: Object` | Class metadata + function stubs |
| `method: foo [body]` | `__Counter__foo() { body }` |
| `method: at: x put: y [...]` | `__Counter__at_put() { local x="$1"; local y="$2"; ... }` |
| `\| var1 var2 \|` | `local var1 var2` |
| `var := value` | `var="value"` |
| `@ self method` | `@ "$_RECEIVER" method` |

## Runtime Context Variables

Set by dispatcher during message sends:
- `$_RECEIVER` - Object/class receiving the message
- `$_SELECTOR` - Method name being called
- `$_CLASS` - Class context
- `$_INSTANCE` - Instance ID (for instance methods)

## Instance Persistence

Instances stored in SQLite (`instances.db`) as JSON. Instance IDs are lowercase class name + UUID:
- Non-namespaced: `counter_abc123`
- Namespaced: `myapp_counter_abc123` (for `MyApp::Counter`)

## Namespaces

```smalltalk
package: MyApp

Counter subclass: Object
  method: increment [ ... ]
```

- Qualified references: `@ OtherPkg::Counter new`
- Compiled function: `__MyApp__Counter__increment`
- Instance ID: `myapp_counter_uuid`

## Issue Tracking

We use **bd** (beads) for issue tracking.

```bash
bd ready                  # Find available work
bd show <id>              # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>             # Complete work
bd sync                   # Sync with git (run at session end)
```

### Priorities
- `0` - Critical (P0)
- `1` - High
- `2` - Medium
- `3` - Low
- `4` - Backlog

## External Dependencies

Required: `jo`, `jq`, `sqlite3`, `uuidgen`, `grpcurl` (for GrpcClient bash fallback)

## Known Issues

- **Method name collision**: Keyword methods (e.g., `skip:`) and unary methods with same base name compile to same bash function
- **Negative numbers in arguments**: Compiler may mangle `0 -1` into `0-1`
- **ifTrue: with non-predicate expressions**: `(@ String contains:...) ifTrue:` doesn't work correctly (returns "true"/"false" strings but ifTrue: uses `-n` test)
