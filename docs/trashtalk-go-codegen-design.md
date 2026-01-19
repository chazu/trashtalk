# Procyon - Design Document

*Procyon: the genus for raccoons - because what goes better with trash?*

## Overview

This document describes the design for **Procyon**, a tool that generates native Go code from Trashtalk class definitions. The generated Go binaries interoperate with the Bash runtime, sharing instance storage via SQLite.

## Goals

1. **Speed**: Native Go execution for performance-critical classes
2. **Interoperability**: Seamless interaction between Go and Bash classes
3. **Self-describing binaries**: Embedded source code and content hash for versioning
4. **Incremental adoption**: Compile individual classes without rewriting the whole system

## Non-Goals (for v1)

1. Compiling raw methods containing arbitrary Bash
2. Full Smalltalk block/closure semantics
3. Replacing the Bash runtime entirely
4. Optimizing cross-class calls (still goes through Bash for Bash classes)

## Key Decisions

- **Traits**: Supported in v1
- **Runtime code**: Inlined into generated output (no separate module to import)
- **Code generation**: Use [jennifer](https://github.com/dave/jennifer) for programmatic Go code generation
- **Testing strategy**: Acceptance tests first (AST → expected Go output), then unit tests for components
- **Philosophy**: This is an experiment. Keep it simple. Bash remains the primary runtime. Native compilation is an optimization, not a replacement.
- **Fallback behavior**: When a method can't be compiled, warn the user at generation time but allow fallback to Bash at runtime. The warning should clearly explain why the method couldn't be compiled.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         trashtalk repo                          │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐ │
│  │ .trash file │───▶│ tokenizer   │───▶│ parser.jq           │ │
│  └─────────────┘    │ (bash)      │    │ (outputs AST JSON)  │ │
│                     └─────────────┘    └──────────┬──────────┘ │
└──────────────────────────────────────────────────│─────────────┘
                                                    │
                                                    ▼ AST JSON (stdin)
┌─────────────────────────────────────────────────────────────────┐
│                         procyon repo                            │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                       procyon CLI                            ││
│  │  ┌──────────┐    ┌──────────────┐    ┌──────────────────┐  ││
│  │  │ AST      │───▶│ IR Builder   │───▶│ Go Code          │  ││
│  │  │ Parser   │    │              │    │ Generator        │  ││
│  │  └──────────┘    └──────────────┘    └──────────────────┘  ││
│  └─────────────────────────────────────────────────────────────┘│
│                              │                                   │
│                              ▼                                   │
│                        main.go output                            │
└─────────────────────────────────────────────────────────────────┘
```

## Input: AST Structure

The jq parser produces JSON with this structure:

```json
{
  "type": "class",
  "name": "Counter",
  "parent": "Object",
  "instanceVars": [
    {"name": "value", "default": {"type": "number", "value": "0"}},
    {"name": "step", "default": {"type": "number", "value": "1"}}
  ],
  "traits": [],
  "methods": [
    {
      "type": "method",
      "kind": "instance",        // or "class"
      "raw": false,              // raw methods can't be compiled
      "selector": "increment",
      "args": [],
      "body": {
        "type": "block",
        "tokens": [...]          // token stream, not expression tree
      }
    }
  ]
}
```

### Key Observation: Method Bodies

Method bodies are currently **token streams**, not parsed expression trees. The bash codegen processes these tokens directly. We have three options:

| Option | Approach | Pros | Cons |
|--------|----------|------|------|
| A | Parse tokens in Go codegen | Go is good at parsing; no jq changes | Duplicates parsing logic |
| B | Extend jq parser to emit expression AST | Single source of truth | jq is awkward for complex parsing |
| C | Add expression parser in jq, optional | Backwards compatible | More jq complexity |

**Recommendation**: Option A - parse token streams in Go. The token stream is well-defined and Go has better tooling for expression parsing.

## Output: Generated Go Structure

```go
package main

import (
    "database/sql"
    _ "embed"
    "encoding/json"
    // ...
)

//go:embed Counter.trash
var _sourceCode string
var _contentHash = "sha256:..."

// Instance struct from instanceVars
type Counter struct {
    Class     string   `json:"class"`
    CreatedAt string   `json:"created_at"`
    Vars      []string `json:"_vars"`
    Value     int      `json:"value"`
    Step      int      `json:"step"`
}

func main() {
    // CLI dispatch: <receiver> <selector> [args...]
}

// Generated methods
func (c *Counter) Increment() int {
    c.Value += c.Step
    return c.Value
}

// Method dispatch
func dispatch(c *Counter, selector string, args []string) (string, error) {
    switch selector {
    case "increment":
        return strconv.Itoa(c.Increment()), nil
    // ...
    default:
        return "", ErrUnknownSelector
    }
}
```

## Translation Rules

### Instance Variables

| Trashtalk | Go |
|-----------|-----|
| `instanceVars: value:0 step:1` | `type Counter struct { Value int; Step int }` |
| `value` (read) | `c.Value` |
| `value := x` | `c.Value = x` |

### Expressions

| Trashtalk | Go |
|-----------|-----|
| `a + b` | `a + b` |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` | `a / b` |
| `^ value` | `return value` |

### Local Variables

| Trashtalk | Go |
|-----------|-----|
| `\| x y z \|` | `var x, y, z int` (or inferred type) |
| `x := 5` | `x = 5` |

### Message Sends

| Trashtalk | Go |
|-----------|-----|
| `@ self increment` | `c.Increment()` |
| `@ self setValue: 5` | `c.SetValue(5)` |
| `@ OtherClass method` | `sendMessage("OtherClass", "method")` (shells out) |

### Control Flow

| Trashtalk | Go |
|-----------|-----|
| `condition ifTrue: [block]` | `if condition { block }` |
| `condition ifFalse: [block]` | `if !condition { block }` |
| `condition ifTrue: [a] ifFalse: [b]` | `if condition { a } else { b }` |
| `[condition] whileTrue: [block]` | `for condition { block }` |

## Type Inference

Trashtalk is dynamically typed; Go is statically typed. Strategy:

1. **Instance variables with numeric defaults** → `int`
2. **Instance variables with string defaults** → `string`
3. **Instance variables with no default** → `interface{}`
4. **Arithmetic expressions** → `int`
5. **String literals** → `string`
6. **Method return types** → inferred from `^` expressions

For v1, focus on numeric types. Complex types can return `interface{}` and be handled at runtime.

## Interop: Calling Bash Classes from Go

When Go code needs to call a Bash class:

```go
func sendMessage(receiver, selector string, args ...string) (string, error) {
    // Build command: source trash.bash && @ receiver selector args...
    cmd := exec.Command("bash", "-c",
        fmt.Sprintf("source %s/lib/trash.bash && @ %s %s %s",
            trashtalkRoot, receiver, selector, strings.Join(args, " ")))
    output, err := cmd.Output()
    return strings.TrimSpace(string(output)), err
}
```

This is slow but correct. Future optimization: a persistent Bash coprocess.

## Interop: Calling Go Classes from Bash

Already implemented in the POC. The dispatcher in `lib/trash.bash` checks for `.native` binaries and calls them with:

```bash
$native_binary "$instance_id" "$selector" "$@"
```

Exit code 200 means "unknown selector, fall back to Bash."

## Limitations and Unsupported Features

### Cannot Compile

1. **Raw methods** (`rawMethod:`) - contain arbitrary Bash
2. **Subshell expressions** (`$(...)`) - need Bash evaluation
3. **Bash-specific syntax** - heredocs, traps, etc.
4. **Dynamic method dispatch to self** - `@ self perform: selector`

### Strategy for Unsupported Code

When the codegen encounters unsupported constructs:

1. **Warn** and skip the method (fall back to Bash at runtime)
2. Or **fail** if `--strict` flag is set

Example warning output:
```
procyon: Counter.trash
  ✓ increment - compiled
  ✓ decrement - compiled
  ✓ getValue - compiled
  ⚠ processData - skipped: contains subshell expression $(...)
  ⚠ initialize - skipped: raw method

Generated 3/5 methods. 2 will fall back to Bash.
```

## CLI Interface

```bash
# Basic usage - reads AST from stdin
./driver.bash parse Counter.trash | procyon > counter/main.go

# With options
procyon --class Counter --output counter/ < ast.json
procyon --strict  # fail on unsupported constructs
procyon --dry-run # show what would be generated

# Compile and build
procyon < ast.json | go build -o Counter.native
```

## Project Structure

```
procyon/
├── cmd/
│   └── procyon/
│       └── main.go           # CLI entry point
├── pkg/
│   ├── ast/
│   │   └── types.go          # Go types for AST JSON
│   ├── parser/
│   │   └── expr.go           # Token stream → expression parser
│   ├── ir/
│   │   └── ir.go             # Intermediate representation
│   └── codegen/
│       └── codegen.go        # IR → Go code (using jennifer)
├── testdata/
│   ├── counter/
│   │   ├── input.json        # AST from parser
│   │   └── expected.go       # Expected generated code
│   └── ...
├── go.mod
└── README.md
```

## Testing Strategy

### Acceptance Tests (High-Level)

Each test case is a directory in `testdata/` containing:
- `input.json` - AST from the jq parser
- `expected.go` - Expected generated Go code

The test runner:
1. Reads `input.json`
2. Runs it through the codegen
3. Compares output to `expected.go`
4. Optionally: compiles the generated code to verify it's valid Go

```go
func TestCodegen(t *testing.T) {
    dirs, _ := filepath.Glob("testdata/*")
    for _, dir := range dirs {
        t.Run(filepath.Base(dir), func(t *testing.T) {
            input := readFile(dir, "input.json")
            expected := readFile(dir, "expected.go")

            ast := ast.Parse(input)
            actual := codegen.Generate(ast)

            if actual != expected {
                t.Errorf("mismatch:\n%s", diff(expected, actual))
            }
        })
    }
}
```

### Unit Tests

Added as we build each component:
- `pkg/ast/` - parsing AST JSON
- `pkg/parser/` - token stream → expression trees
- `pkg/codegen/` - individual generation functions

### Integration Tests

After M1: compile generated Counter, run against same SQLite DB as Bash version, verify identical behavior.

## Milestones

### M1: Minimal Viable Generator
- Parse AST JSON
- Generate struct from instanceVars
- Generate simple arithmetic methods (no control flow)
- Generate dispatch switch
- Embed source and hash
- Acceptance test framework in place

### M2: Control Flow
- ifTrue:/ifFalse: → if/else
- whileTrue: → for loops
- Early return (^)

### M3: Message Sends & Traits
- @ self method → direct method call
- @ self keyword: arg → method call with args
- @ OtherClass method → shell out to Bash
- Trait method inlining

### M4: Polish
- Better error messages
- --strict mode
- Type inference improvements
- Documentation

## Deferred Decisions

These will be decided when we encounter them, keeping the simplest working approach:

1. **Trait method resolution**: Inline vs standalone - decide when implementing traits
2. **Error handling style**: `(result, error)` vs panic/recover - decide when implementing exceptions
3. **Complex type inference**: Start with `int` for arithmetic, `string` for strings, expand as needed

---

## Appendix: Example Translation

### Input: Counter.trash

```smalltalk
Counter subclass: Object
  instanceVars: value:0 step:1

  method: increment [
    | newVal |
    newVal := value + step
    value := newVal
    ^ newVal
  ]
```

### Output: main.go (excerpt)

```go
type Counter struct {
    Class     string   `json:"class"`
    CreatedAt string   `json:"created_at"`
    Vars      []string `json:"_vars"`
    Value     int      `json:"value"`
    Step      int      `json:"step"`
}

func (c *Counter) Increment() int {
    var newVal int
    newVal = c.Value + c.Step
    c.Value = newVal
    return newVal
}
```
