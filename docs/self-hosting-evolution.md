# Analysis: Trashtalk Self-Hosting and Procyon Integration

*Analysis prepared by language architect agent, January 2026*

## Current Architecture Summary

### Trashtalk Compilation Pipeline
```
.trash source
    |
    v
[tokenizer.bash] -- Bash script (841 lines)
    |
    v
JSON tokens
    |
    v
[parser.jq] -- jq filter (893 lines)
    |
    v
JSON AST
    |
    v
[codegen.jq] -- jq filter (~1200+ lines)
    |
    v
Compiled Bash functions
```

### Trashtalk Runtime (`lib/trash.bash` - 1877 lines)
Core responsibilities:
1. **Message Dispatcher** (`send()`) - Routes `@ receiver selector args` to functions
2. **Instance Management** - Creates, loads, saves instances via `_env_*` functions
3. **Class Registry** - Tracks sourced classes, metadata variables (`__ClassName__superclass`, etc.)
4. **Method Lookup** - Inheritance chain traversal, trait lookup
5. **Context Management** - `_RECEIVER`, `_SELECTOR`, `_CLASS`, `_INSTANCE` variables
6. **Error Handling** - `_throw`, `_on_error`, `_ensure`, advice system

### Procyon Pipeline
```
JSON AST (from jq parser)
    |
    v
[Procyon] -- Go program
    |
    v
main.go (generated)
    |
    v
go build
    |
    v
.native binary
```

### Current Interop Model
- **Shared Storage**: Both Bash and Go use SQLite (`instances.db`)
- **Exit Code Protocol**: Exit code 200 = "unknown selector, fall back to Bash"
- **Shell-out for Cross-calls**: Go's `sendMessage()` spawns `trash-send` script
- **Native Binary Discovery**: `send()` checks for `.native` file before Bash dispatch

---

## Analysis: What Can Be Self-Hosted?

### Tier 1: High Feasibility (Can be done incrementally)

#### 1. Instance Storage Layer
**Current**: `_env_*` functions in Bash + SQLite via `sqlite-json.bash`

**Self-hosting approach**: Create a `MemoryEnvironment` and `Store` class in Trashtalk:

```smalltalk
MemoryEnvironment subclass: Object
  classInstanceVars: instances:{}

  classMethod: get: instanceId [
    "Return JSON data for instance from memory"
  ]

  classMethod: set: instanceId to: data [
    "Store instance data in memory"
  ]

  classMethod: exists: instanceId [
    "Check if instance exists"
  ]
```

**Challenge**: The storage layer is called from the *dispatcher itself*, creating a chicken-and-egg problem. Solution: Keep a minimal Bash bootstrap that can load the first classes.

#### 2. Class Metadata Registry
**Current**: Bash associative arrays and dynamic variables (`__Counter__superclass`, etc.)

**Self-hosting approach**: Create a `ClassRegistry` class:

```smalltalk
ClassRegistry subclass: Object
  classInstanceVars: classes:{}

  classMethod: register: className withMetadata: metadata [
    "Store class superclass, traits, instanceVars"
  ]

  classMethod: superclassOf: className [
    ^ @ self classes at: className at: 'superclass'
  ]

  classMethod: instanceVarsFor: className [
    "Collect inherited vars from class chain"
  ]
```

**Feasibility**: High. This is mostly data manipulation that Trashtalk handles well.

#### 3. Compiler Tokenizer
**Current**: `tokenizer.bash` (841 lines of Bash)

**Self-hosting approach**: The tokenizer is a state machine over a string. Trashtalk can express this:

```smalltalk
Tokenizer subclass: Object
  instanceVars: input position line col tokens

  method: tokenize: source [
    input := source.
    position := 0.
    tokens := #().
    [self atEnd] whileFalse: [self scanToken].
    ^ tokens
  ]

  method: scanToken [
    | char |
    char := self currentChar.
    (char = $') ifTrue: [^ self scanString].
    (char isDigit) ifTrue: [^ self scanNumber].
    "..."
  ]
```

**Challenge**: String manipulation in Bash is slow. Tokenizing in pure Trashtalk would be significantly slower than the current Bash implementation.

**Solution**: This is a perfect candidate for Procyon compilation. The tokenizer has:
- No bash-specific constructs (no `$()`, heredocs, etc.)
- Simple data structures (array of token objects)
- Deterministic control flow

### Tier 2: Medium Feasibility (Requires more infrastructure)

#### 4. Compiler Parser
**Current**: `parser.jq` (893 lines of jq)

The parser is a PEG-style recursive descent parser. It could be written in Trashtalk:

```smalltalk
Parser subclass: Object
  instanceVars: tokens position result

  method: parseClassHeader [
    self skipNewlines.
    (self currentToken type = 'IDENTIFIER') ifTrue: [
      | className |
      className := self currentToken value.
      self advance.
      "..."
    ].
  ]
```

**Challenge**: The jq parser is highly functional with pattern matching. Trashtalk's imperative style would make this more verbose but still feasible.

**Opportunity**: A Trashtalk parser could enable runtime metaprogramming - parsing and compiling code dynamically.

#### 5. Compiler Code Generator
**Current**: `codegen.jq` (~1200+ lines)

The codegen walks the AST and emits Bash. This is essentially:
- Pattern matching on AST node types
- String concatenation to build output
- Context tracking (current class, method kind, etc.)

Could be self-hosted with similar structure to the parser.

### Tier 3: Core Bootstrap (Most challenging)

#### 6. Message Dispatcher (`send()`)
**Current**: 300+ lines in `lib/trash.bash`

This is the heart of Trashtalk. Self-hosting it faces the fundamental bootstrap problem:
- To run a Trashtalk method, you need the dispatcher
- To run the dispatcher (if written in Trashtalk), you need the dispatcher

**Possible approaches**:

**A. Minimal Bootstrap Dispatcher**
Keep a ~50 line minimal Bash `send()` that can:
1. Load compiled class files
2. Call the first method
3. Hand off to a self-hosted dispatcher for subsequent calls

**B. Procyon-Compiled Dispatcher**
Write the dispatcher in Trashtalk, compile to Go, use that as the bootstrap:
```
@ Dispatcher send: receiver selector: sel args: args
```

The Go binary would handle method lookup, context management, and delegation.

**C. Two-Phase Boot**
1. Phase 1: Tiny Bash boot that loads `Dispatcher.native`
2. Phase 2: Dispatcher written in Trashtalk, compiled by Procyon

---

## Proposed Architecture Evolution

### Phase 1: Data Layer Self-Hosting

1. **Create `Environment` class** in Trashtalk with both memory and SQLite backends
2. **Compile it with Procyon** so it's fast
3. **Update `lib/trash.bash`** to call the native binary for storage operations
4. **Keep `_env_*` as fallback** if native binary not available

This provides immediate benefits:
- Faster instance operations (Go vs Bash)
- Storage logic becomes inspectable/modifiable Trashtalk code
- First step toward self-hosting

### Phase 2: Registry and Metadata

1. **Create `ClassRegistry` class** tracking class metadata
2. **Create `MethodLookup` class** handling inheritance traversal
3. **Compile both with Procyon**
4. **Update dispatcher** to query these classes instead of Bash variables

### Phase 3: Compiler Self-Hosting

1. **Port Tokenizer to Trashtalk** and compile with Procyon
2. **Port Parser to Trashtalk** - creates Trashtalk AST objects
3. **Port CodeGen to Trashtalk** - either Bash codegen or direct Procyon integration

At this point, the compiler can compile itself (bootstrapping milestone).

### Phase 4: Dispatcher Migration

1. **Write `MessageDispatcher` class** in Trashtalk
2. **Compile with Procyon** to create native dispatcher
3. **Minimal Bash bootstrap** just invokes the native dispatcher

---

## Procyon Enhancement Proposals

### 1. Improved Interop - FFI Style Dispatch

Instead of shelling out for cross-class calls, use a shared memory or socket protocol:

```go
// In generated Go code
func sendMessage(receiver, selector string, args ...string) (string, error) {
    // First, check if we have a native binary for the receiver's class
    nativePath := filepath.Join(compiledDir, class + ".native")
    if _, err := os.Stat(nativePath); err == nil {
        // Direct binary call (fast path)
        return callNativeBinary(nativePath, receiver, selector, args)
    }
    // Fall back to Bash (slow path)
    return shellOut(receiver, selector, args)
}
```

### 2. Library Mode for Procyon

Instead of generating standalone binaries, generate Go packages that can be linked:

```go
// counter/counter.go
package counter

type Counter struct {
    Value int
    Step  int
}

func (c *Counter) Increment(n int) int {
    c.Value += n
    return c.Value
}
```

Then a main dispatcher binary links all class packages:

```go
// dispatcher/main.go
import (
    "trashtalk/counter"
    "trashtalk/person"
    // ...
)

func main() {
    // Single process, no shell-out overhead
}
```

### 3. Bidirectional AST

Currently: jq parser -> JSON AST -> Procyon -> Go

Proposal: Define Trashtalk AST classes that both the jq parser and Procyon understand:

```smalltalk
ClassNode subclass: ASTNode
  instanceVars: name parent traits instanceVars methods

MethodNode subclass: ASTNode
  instanceVars: selector kind args body
```

These become Go structs through Procyon, and the jq parser outputs JSON that matches this schema.

### 4. Incremental Compilation

Track source hashes in the AST and compiled output. Only recompile changed methods:

```smalltalk
Compiler subclass: Object

  method: compileIfChanged: className [
    | currentHash storedHash |
    currentHash := @ self hashSource: className.
    storedHash := @ ClassRegistry hashFor: className.
    (currentHash = storedHash) ifFalse: [
      @ self compile: className
    ]
  ]
```

---

## Bootstrapping Strategy

### Stage 0: Current State
- Compiler: jq + Bash
- Runtime: Bash
- Native: Optional Procyon binaries

### Stage 1: Native Data Layer (weeks)
- Port `Environment` and `Store` to Trashtalk
- Compile with Procyon
- Runtime calls native for storage, falls back to Bash

### Stage 2: Native Registry (weeks)
- Port `ClassRegistry` and `MethodLookup`
- Compile with Procyon
- Runtime calls native for lookups

### Stage 3: Self-Hosted Compiler (months)
- Port Tokenizer, Parser, CodeGen to Trashtalk
- Compiler can compile itself
- Keep jq version as bootstrap compiler

### Stage 4: Native Dispatcher (months)
- Port `send()` to Trashtalk
- Compile to native with Procyon
- Minimal Bash shim just starts native dispatcher

### Stage 5: Full Native (long-term)
- All core classes compiled to single native binary
- Bash used only for shell integration and debugging
- Can still fall back to Bash for dynamic/reflective code

---

## Implementation Status (January 2026)

### Completed: Environment.trash PoC

We implemented the first proof-of-concept:

1. **Created `Environment.trash`** with class methods for storage operations
2. **Extended Procyon** to generate specialized SQLite code for Environment class methods
3. **Built `Environment.native`** binary that handles db operations
4. **Added native acceleration** to `sqlite-json.bash` (db_get, db_put, db_find_by_class)

### Performance Finding: Process Spawn Overhead

**Key insight**: Individual native calls are NOT faster than Bash+sqlite3 CLI!

| Operation (100 iter) | Native | Bash |
|---------------------|--------|------|
| db_get | 0.616s | 0.468s |
| db_find_by_class (50 iter) | 0.263s | 0.234s |

**Why?** Each native call spawns a new Go process. The sqlite3 CLI is already native, so the process creation overhead outweighs any Go vs Bash speed difference.

**Conclusion**: For native acceleration to be beneficial, we need:
- **Library Mode** - generate linkable packages, single binary for all classes
- **Daemon Mode** - long-running process that handles requests via socket/pipe
- **Batch operations** - amortize startup cost over multiple queries

This validates the "Procyon Enhancement Proposals" section below.

---

## Recommended First Steps

1. **Create `Environment.trash`** with memory storage using Trashtalk dictionaries:

```smalltalk
Environment subclass: Object
  classInstanceVars: instances:{}

  classMethod: get: id [
    ^ (_cvar instances) at: id ifAbsent: [nil]
  ]

  classMethod: set: id to: data [
    (_cvar instances) at: id put: data
  ]
```

2. **Test compilation with Procyon** to verify the generated Go works

3. **Update `lib/trash.bash`** `_env_get`/`_env_set` to call native if available

4. **Measure performance** difference between Bash and native storage

5. **Iterate** based on what breaks and what works

---

## Key Insights

1. **The dispatcher is the hardest part** because of the bootstrap problem. Consider keeping a minimal Bash dispatcher that can load and run a Trashtalk-written dispatcher.

2. **Procyon is the key enabler** for self-hosting. Without native compilation, a Trashtalk-in-Trashtalk compiler would be too slow to be practical.

3. **Incremental migration is essential**. Each component should be independently replaceable while the rest continues working.

4. **SQLite is the shared state** between Bash and Go. This is a good foundation for gradual migration.

5. **Exit code 200 protocol is clever** but adds latency. Consider a daemon mode where a single Go process handles all native methods.

---

## Conclusion

Trashtalk has a solid foundation for self-hosting evolution:

- The jq-based compiler produces clean ASTs
- Procyon can compile significant subsets of Trashtalk to native code
- The SQLite storage layer provides shared state
- The exit-code protocol enables graceful fallback

The most practical path forward is:
1. Start with data layer self-hosting (low risk, immediate benefits)
2. Build toward a native registry and lookup system
3. Eventually tackle the compiler and dispatcher

This preserves Bash compatibility (essential for shell integration) while enabling performance where it matters and making the system more self-describing.
