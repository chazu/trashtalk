# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Trashtalk?

Trashtalk is a Smalltalk-inspired DSL compiler and runtime for Bash. It transforms `.trash` source files into namespaced Bash functions, providing OOP semantics (classes, inheritance, traits, instance persistence) without polluting the global namespace.

## Build Commands

```bash
make                      # Compile all .trash files
make compile              # Same as above
make single CLASS=Counter # Compile a single class
make compile-traits       # Compile only traits
make clean                # Delete .compiled directory
make watch                # Auto-recompile on file changes
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
@ Trash methodsFor Counter         # List methods for a class
```

## Architecture

### Compilation Pipeline

```
.trash source → Tokenizer → Parser → Code Generator → Compiled Bash
```

- **Compiler**: `lib/trash-compiler.bash` - tokenizes, parses AST, generates code
- **Runtime/Dispatcher**: `lib/trash.bash` - routes `@` message sends to functions
- **Source files**: `trash/*.trash` and `trash/traits/*.trash`
- **Compiled output**: `trash/.compiled/` (also copied to `trash/` for runtime)

### Key Transformations

| DSL | Compiles To |
|-----|-------------|
| `Counter subclass: Object` | Class metadata + function stubs |
| `method: foo [body]` | `__Counter__foo() { body }` |
| `method: at: x put: y [...]` | `__Counter__at_put() { local x="$1"; local y="$2"; ... }` |
| `\| var1 var2 \|` | `local var1 var2` |
| `var := value` | `var="value"` |
| `@ self method` | `@ "$_RECEIVER" method` |

### Runtime Context Variables

Set by dispatcher during message sends (local to each `send()` frame):
- `$_RECEIVER` - Object/class receiving the message
- `$_SELECTOR` - Method name being called
- `$_CLASS` - Class context
- `$_INSTANCE` - Instance ID (for instance methods)

These are `local` variables using Bash's dynamic scoping, so nested message sends get their own copies that are automatically restored on return.

### Error Handling & Advice APIs

```bash
# Error handling
_throw "ErrorType" "message"      # Signal an error
_on_error "ErrorType" "handler"   # Register handler (use "*" for catch-all)
_pop_handler                      # Remove handler when leaving protected region
_ensure "cleanup_command"         # Register cleanup that runs on frame exit

# Debugging
_print_stack_trace                # Print call stack to stderr
_CALL_STACK                       # Array of "Class.selector" entries
_CALL_DEPTH                       # Current call depth

# Method advice (AOP)
_add_before_advice "Class" "selector" "handler"  # Run before method
_add_after_advice "Class" "selector" "handler"   # Run after method
_remove_advice "Class" "selector"                # Remove advice
```

### Instance Persistence

Instances stored in SQLite (`instances.db`) as JSON. Instance IDs are lowercase class name + UUID (e.g., `counter_abc123`).

## Key Files

- `lib/trash.bash` - Main dispatcher (`send()` function handles method lookup)
- `lib/trash-compiler.bash` - DSL compiler (tokenizer lines 41-255, parser lines 329-576)
- `lib/vendor/sqlite-json.bash` - Database layer
- `lib/vendor/tuplespace/` - Process coordination

## DSL Syntax Quick Reference

```smalltalk
# Class definition
Counter subclass: Object
  include: Debuggable
  instanceVars: value:0 step:1

  method: increment [
    | newValue |
    newValue := $(( $(_ivar value) + 1 ))
    _ivar_set value "$newValue"
  ]

  classMethod: create [
    @ Counter new
  ]

  rawMethod: withHeredoc [
    # No transformation - use for heredocs, traps
  ]
```

## External Dependencies

Requires: `jo`, `jq`, `sqlite3`, `uuidgen`

## Known Issues

- **Tuplespace vendor path**: `Tuplespace.trash` references wrong path to `tuplespace.bash`
- **Find predicate queries**: `@ Trash find "value > 5"` parsing needs revision
- **Inherited _vars array**: jq parsing fails in some inheritance test scenarios
- Test framework (`TestCase.trash`) is partially implemented
- Two-pass AST-based compiler is in progress (`lib/jq-compiler/`)
