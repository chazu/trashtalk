# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Trashtalk?

Trashtalk is a Smalltalk-inspired DSL compiler and runtime for Bash. It transforms `.trash` source files into executable Bash functions, providing OOP semantics (classes, inheritance, traits, instance persistence).

## Architecture

### Compilation Pipeline

```
.trash source → jq-compiler → Compiled Bash
                    │
              ┌─────┴─────┐
              │           │
         tokenizer    codegen
          (bash)        (jq)
              │           │
              └─────┬─────┘
                    ▼
           trash/.compiled/*
```

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| jq-compiler | `lib/jq-compiler/` | Tokenizes and parses `.trash` → generates Bash |
| Runtime | `lib/trash.bash` | Bash dispatcher, routes `@` message sends |
| Compiled classes | `trash/.compiled/*` | Generated bash functions |

### jq-compiler

The jq-compiler is a three-stage pipeline:
- **Tokenizer** (`tokenizer.bash`): Converts .trash source to JSON tokens
- **Parser** (`parser.jq`): PEG-style combinators parse tokens to JSON AST
- **CodeGen** (`codegen.jq`): Generates bash functions from AST

### Runtime Execution Model

1. `source lib/trash.bash` loads the runtime
2. The `@` dispatcher routes message sends to compiled bash functions
3. Instances are stored in SQLite as JSON

## Build Commands

```bash
make              # Compile all classes to bash
make bash         # Same as above
make single CLASS=Counter  # Compile single class
make test         # Run all tests
make clean        # Remove build artifacts
```

## Testing

```bash
make test         # Run all tests, show pass/fail summary
make test-verbose # Run tests with bash -x tracing
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
# Class with regular methods (DSL transformation applied)
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

# Class with raw methods (no DSL transformation, direct Bash)
MyClass subclass: Object
  instanceVars: data:''

  rawMethod: doSomething [
    # Raw bash code here
    echo "result"
  ]

  rawClassMethod: create [
    local instance
    instance=$(@ MyClass new)
    echo "$instance"
  ]
```

### When to Use rawMethod vs method

- Use `method:` for most code - it handles variable inference and message transformation
- Use `rawMethod:` when you need:
  - Heredocs, traps, or complex Bash constructs
  - Process substitution (`<(...)`)
  - Complex loops or conditionals
  - Direct control over Bash execution

### Key Patterns

**JSON extraction** (use String primitive):
```smalltalk
value := @ String jsonPath: 'session.id' from: jsonResponse.
```

**Handler/closure pattern** (for callbacks):
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

## Pragmas

### `pragma: direct`

Bypasses subshell capture, allowing methods to modify variables in the calling shell:

```smalltalk
rawMethod: setGlobalCounter [
  pragma: direct
  GLOBAL_COUNTER="modified"
]
```

Use for methods that need to modify shell state.

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

Required: `jo`, `jq`, `sqlite3`, `uuidgen`

## Known Issues

- **Method name collision**: Keyword methods (e.g., `skip:`) and unary methods with same base name compile to same bash function
- **Negative numbers in arguments**: Compiler may mangle `0 -1` into `0-1`
- **ifTrue: with non-predicate expressions**: `(@ String contains:...) ifTrue:` doesn't work correctly (returns "true"/"false" strings but ifTrue: uses `-n` test)
