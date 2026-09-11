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
make              # Build changed classes; validate and reuse unchanged output
make bash         # Same as above
make single CLASS=Counter  # Compile single class
make verify       # Build and run both isolated suites in parallel
make test         # Run isolated runtime tests in parallel
make test-compiler # Run isolated compiler tests in parallel
make test-serial  # Run tests sequentially
make test-verbose # Run tests with bash -x tracing
make clean        # Remove build artifacts
make bench        # Build and measure public messages with isolated object state
```

## Testing

```bash
make verify       # Build and run both suites, show pass/fail summaries
make test-verbose # Run tests with bash -x tracing
```

Compiler test files are in `lib/jq-compiler/tests/test_*.bash`.

Tests run in separate disposable checkouts with isolated databases and caches,
including standalone test invocations. `TRASH_TEST_JOBS` and
`TRASH_TEST_TIMEOUT` control parallelism and per-file timeouts. See
`docs/performance.md` for retaining a failed checkout or enabling traces.

Compiled classes carry JSON default templates. Object creation merges inherited
templates in one serializer and retains immediate persistence. Each instance
send decodes its scalar fields once; field reads reuse that decode only while
the exact session data remains unchanged. Containers are read on demand. Block
invocation decodes its metadata together
and retains public getter dispatch when those getters are overridden.

Builds validate source and artifact hashes in a batch, plan shared dependencies
once, and run only changed classes in parallel. See `docs/performance.md` for
the measured process counts. `TRASHTALK_VALUE_SEND=1` enables guarded removal of
one capture in eligible compiled assignments and collection callbacks; it is off
by default. Set it for both compilation and execution, and start a fresh runtime
after rebuilding. Public `@` keeps its result conventions. See
`docs/result-passing-design.md` for eligibility, reload/invalidation, and results.

## Runtime Usage

```bash
source lib/trash.bash              # Load the runtime
@ Trash info                       # System info
counter=$(@ Counter new)           # Create instance
@ "$counter" incrementBy: 5             # Call instance method
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
- DSL methods can now handle: ivar accessors (`^ ivar`), predicate returns (`^ path fileExists`),
  ivar assignment from params (`ivar := param`) and literals (`ivar := "value"`),
  message sends to self (`@ self stop`), and string literal returns (`^ "hello"`)
- String work stays in the DSL: `s startsWith: p`, `s withoutPrefix: p`, `s upTo: ':'`,
  `s afterLast: '/'`, `s replaceAll: a with: b`, `s size`, `s asUppercase`, `s trimmed`,
  `s lines`, `s firstLine` compile to parameter expansion with no dispatch
- Failure stays in the DSL: `@ SomeError signal: 'msg'` fails the method;
  `(@ x foo) ifFailed: [:e | ...]` guards a send; `@ e signal` re-raises
- Iteration and dispatch stay in the DSL: `ids linesDo: [:id | ...]` over newline lists,
  `x caseOf: { 'a' -> [...]. #('b' 'c') -> [...] } otherwise: [...]` over literals
- A method's stdout is its value: non-tail sends discard output; `pragma: stream`
  keeps it for methods that print several lines
- A body that is one call to a Bash function is a declaration:
  `classPrimitive: notify: channel payload: data calls: honker_notify`
- Use `rawMethod:` only when you need:
  - Bash builtins the intrinsics do not cover (e.g. `IFS`, `read`, `mapfile`, extglob)
  - External commands (`curl`, `stat`, `printf`, `cat`, etc.)
  - Runtime context variables (`$_CLASS`, `$_INSTANCE`, `$_RECEIVER`, `$_SELECTOR`)
  - Heredocs, traps, process substitution, or complex Bash control flow
  - Pipe chains or redirections

### Key Patterns

**Temporary JSON values** (one jq call, no persistent builder objects):
```smalltalk
context := #{schema_version: 1 question: question
             status: (status jsonValue) data: (data jsonValue)} asJson.
argv := #(toolPath '--result-json' '--title' title) asJson.
```

Dynamic leaves are strings unless marked `jsonValue`; literal numbers,
booleans, nulls, and nested collections retain JSON types. Invalid typed input
fails the method. See `docs/performance.md` for output contracts and benchmarks.

**JSON reads** (preserve types and distinguish absent fields):
```smalltalk
value := jsonResponse jsonAt: 'session.id'.
present := jsonResponse jsonHas: 'session.id'.
jsonResponse jsonUnpack: #('status' 'message') into: [:status :message |
  @ Console print: message
]
```

Use `jsonTextAt:` for decoded text, and `jsonAt:ifAbsent:` for an encoded default
only when absent. Legacy `String jsonPath:from:` remains available. See
`docs/json-values.md` for paths, bulk binding, and one-pass collection traversal.

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

**Predicates that work** (in ifTrue:, return, and assignment contexts):
```smalltalk
(path fileExists) ifTrue: [...]    # Conditional
^ path isFile                      # Return "true"/"false"
result := path isEmpty             # Assign "true"/"false"
```

Supported predicates: `fileExists`, `isFile`, `isDirectory`, `isFifo`, `isSymlink`,
`isReadable`, `isWritable`, `isExecutable`, `isEmpty`, `notEmpty`,
`isSocket`, `isBlockDevice`, `isCharDevice`.

## Key Transformations

| DSL | Compiles To |
|-----|-------------|
| `Counter subclass: Object` | Class metadata + function stubs |
| `method: foo [body]` | `__Counter__foo() { body }` |
| `method: at: x put: y [...]` | `__Counter__at_put() { local x="$1"; local y="$2"; ... }` |
| `| var1 var2 |` | `local var1 var2` |
| `var := value` | `var="value"` (local) or `_ivar_set var "value"` (ivar) |
| `^ ivar` | `echo "$(_ivar ivar)"; return` |
| `^ path fileExists` | `[[ -e "$path" ]] && echo "true" \|\| echo "false"; return` |
| `result := path isEmpty` | `result="$([[ -z "$path" ]] && echo true \|\| echo false)"` |
| `@ self method` | `@ "$_RECEIVER" method` |
| `base := path afterLast: '/'` | `base="${path##*'/'}"` |
| `(s startsWith: p) ifTrue: [...]` | `if [[ "$s" == "$p"* ]]; then ...; fi` |
| `@ StateError signal: 'msg'` | `_throw 'StateError' 'msg'; return 1` |
| `(@ x save) ifFailed: [^ '']` | `if ! { @ "$x" save >/dev/null; }; then _clear_error; echo ""; return; fi` |
| `ids linesDo: [:id \| ...]` | `mapfile -t` into an array, then `for id in ...; do ...; done` |
| `x caseOf: { 'a' -> [...] }` | `case "$x" in 'a') ...;; esac` |
| `@ x reload.` (non-tail) | `@ "$x" reload >/dev/null` |
| `classPrimitive: ping calls: fn` | `__Class__class__ping() { fn; }` |

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

### `pragma: stream`

Keeps the output of every statement in a `method:` body. Without it, a send in a
non-tail statement is an effect and its stdout is discarded. Use it for help
text, listings, and reports built from several `Console print:` sends.

### `pragma: direct`

Bypasses subshell capture, allowing methods to modify variables in the calling shell:

```smalltalk
rawMethod: setGlobalCounter [
  pragma: direct
  GLOBAL_COUNTER="modified"
]
```

Use for methods that need to modify shell state.

## External Dependencies

Required: Bash 4.4+, `jo`, `jq`, `sqlite3`, `uuidgen`; builds also need `make` and `shasum`.

## Language boundaries

See [LANGUAGE.md](LANGUAGE.md#limitations-and-regression-coverage) for current
limitations and [compiler capabilities](docs/COMPILER_CAPABILITIES.md) for their
regression coverage. Unary/keyword selector collisions, negative arguments, and
qualified raw-method references are fixed; do not introduce their old workarounds.

See [persistence](docs/persistence.md) for immediate initial persistence, cached
mutations, explicit saves, and guarded Store transactions. Start with the
[documentation index](docs/README.md) when deciding whether a design is current.
