# Trashtalk Compiler Capabilities

This document provides a comprehensive comparison of the three compilation backends:
1. **jq-compiler** - Pure jq/bash tokenizer → parser → codegen pipeline
2. **Procyon Bash Backend** - Go-based IR → Bash code generator
3. **Procyon Native Plugin** - Go-based compilation to native C-shared plugins

---

## Quick Comparison Matrix

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|-------------|--------------|----------------|
| **Output Format** | Bash functions | Bash functions | Go c-shared plugin |
| **Execution** | Interpreted | Interpreted | Native + late-bound |
| **Performance** | Baseline | Baseline | 10-100x faster |
| **Dependencies** | jq, bash | Go (compile time) | Go runtime |

---

## 1. Language Features

### Classes & Inheritance

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Basic class definition | ✓ | ✓ | ✓ |
| Inheritance (`subclass:`) | ✓ | ✓ | ✓ |
| Traits (`include:`) | ✓ | ✓ | ✓ (trait methods merged) |
| Namespaces (`Package::Class`) | ✓ | ✓ | ✓ |
| Package declaration | ✓ | ✓ | ✓ |
| Import statements | ✓ (parsed) | ✓ (parsed) | ✓ (parsed) |

### Instance & Class Variables

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Instance variables | ✓ | ✓ | ✓ (Go struct fields) |
| Default values (numeric) | ✓ | ✓ | ✓ (int type) |
| Default values (string) | ✓ | ✓ | ✓ (json.RawMessage) |
| Class instance vars | ✓ | ✓ | ✓ |
| Inherited ivar access | ✓ | ✓ | ✓ (via SQLite lookup) |
| Auto getters/setters | ✓ | ✓ | ✓ |

### Methods

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Instance methods | ✓ | ✓ | ✓ |
| Class methods | ✓ | ✓ | ✓ |
| Unary selectors | ✓ | ✓ | ✓ |
| Keyword selectors | ✓ | ✓ | ✓ |
| Multi-keyword selectors | ✓ | ✓ | ✓ |
| Raw methods | ✓ | ✓ | ✗ (skipped) |
| Primitive methods | ✓ | ✓ | ✓ (200+ native) |

---

## 2. Expressions & Operators

### Arithmetic

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Addition (`+`) | ✓ | ✓ | ✓ |
| Subtraction (`-`) | ✓ | ✓ | ✓ |
| Multiplication (`*`) | ✓ | ✓ | ✓ |
| Division (`/`) | ✓ | ✓ | ✓ |
| Modulo (`%`) | ✓ | ✓ | ✓ |
| Unary minus (`-x`) | ✓ | ✓ | ✓ |

### Comparison & Logic

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Equality (`=`, `==`) | ✓ | ✓ | ✓ |
| Inequality (`!=`, `~=`) | ✓ | ✓ | ✓ |
| Less than (`<`) | ✓ | ✓ | ✓ |
| Greater than (`>`) | ✓ | ✓ | ✓ |
| Less or equal (`<=`) | ✓ | ✓ | ✓ |
| Greater or equal (`>=`) | ✓ | ✓ | ✓ |
| Logical AND (`&&`) | ✓ | ✓ | ✓ |
| Logical OR (`\|\|`) | ✓ | ✓ | ✓ |
| Logical NOT (`!`) | ✓ | ✓ | ✓ |
| Regex match (`=~`) | ✓ | ✓ | ✗ |

### Strings & Literals

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Single-quoted strings | ✓ | ✓ | ✓ |
| Double-quoted strings | ✓ | ✓ | ✓ |
| Triple-quoted strings | ✓ | ✓ | ✓ |
| String concatenation (`,`) | ✓ | ✓ | ✓ |
| Symbols (`#name`) | ✓ | ✓ | ✓ |
| Array literals (`#(...)`) | ✓ | ✓ | ✓ |
| Dictionary literals (`#{...}`) | ✓ | ✓ | ✓ |

---

## 3. Control Flow

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| `ifTrue:` | ✓ | ✓ | ✓ |
| `ifFalse:` | ✓ | ✓ | ✓ |
| `ifTrue:ifFalse:` | ✓ | ✓ | ✓ |
| `ifNil:` | ✓ | ✓ | ✓ |
| `ifNotNil:` | ✓ | ✓ | ✓ |
| `whileTrue:` | ✓ | ✓ | ✓ |
| `whileFalse:` | ✓ | ✓ | ✓ |
| `timesRepeat:` | ✓ | ✓ | ✓ |
| `try:catch:` | ✓ | ✓ | ✗ (falls back) |

---

## 4. Blocks & Iteration

### Block Syntax

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Zero-arg blocks `[body]` | ✓ | ✓ | ✓ |
| Single-arg blocks `[x \| body]` | ✓ | ✓ | ✓ |
| Multi-arg blocks `[x y \| body]` | ✓ | ✓ | ✓ |
| Block param syntax `[:x \| ...]` | ✓ | ✓ | ✓ |

### Iteration Methods

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| `do:` (each iteration) | ✓ | ✓ | ✓ (native loop) |
| `collect:` (map) | ✓ | ✓ | ✓ (native loop) |
| `select:` (filter) | ✓ | ✓ | ✓ (native loop) |
| `detect:` (find first) | ✓ | ✓ | ✓ (native loop) |
| `detect:ifNone:` | ✓ | ✓ | ✓ |
| `inject:into:` (reduce) | ✓ | ✓ | ✓ |

### Block Invocation

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| `value` (zero-arg) | ✓ | ✓ | ✓ (via invokeBlock) |
| `valueWith:` (one-arg) | ✓ | ✓ | ✓ (via invokeBlock) |
| `valueWith:and:` (two-arg) | ✓ | ✓ | ✓ (via invokeBlock) |
| Inline block compilation | ✓ | ✓ | ✓ |
| Dynamic block execution | ✓ | ✓ | ✓ (shells to bash) |

---

## 5. Message Sends

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Self sends (`@ self msg`) | ✓ | ✓ | ✓ (direct call) |
| Instance sends (`@ obj msg`) | ✓ | ✓ | ✓ (sendMessage) |
| Class sends (`@ Class msg`) | ✓ | ✓ | ✓ (sendMessage) |
| Cascading (`@ obj a; b; c`) | ✓ | ✓ | ✗ |
| Super sends (`@ super msg`) | ✓ | ✓ | ✗ |

---

## 6. JSON Primitives

### Array Operations

| Operation | jq-compiler | Procyon Bash | Procyon Native |
|-----------|:-----------:|:------------:|:--------------:|
| `arrayLength` | ✓ | ✓ | ✓ |
| `arrayFirst` | ✓ | ✓ | ✓ |
| `arrayLast` | ✓ | ✓ | ✓ |
| `arrayAt:` | ✓ | ✓ | ✓ |
| `arrayPush:` | ✓ | ✓ | ✓ |
| `arrayAtPut:` | ✓ | ✓ | ✓ |
| `arrayRemoveAt:` | ✓ | ✓ | ✓ |
| `arrayIsEmpty` | ✓ | ✓ | ✓ |

### Object Operations

| Operation | jq-compiler | Procyon Bash | Procyon Native |
|-----------|:-----------:|:------------:|:--------------:|
| `objectAt:` | ✓ | ✓ | ✓ |
| `objectAtPut:` | ✓ | ✓ | ✓ |
| `objectLength` | ✓ | ✓ | ✓ |
| `objectHasKey:` | ✓ | ✓ | ✓ |
| `objectKeys` | ✓ | ✓ | ✓ |
| `objectValues` | ✓ | ✓ | ✓ |
| `objectRemoveKey:` | ✓ | ✓ | ✓ |
| `objectIsEmpty` | ✓ | ✓ | ✓ |

---

## 7. Pragmas

| Pragma | jq-compiler | Procyon Bash | Procyon Native |
|--------|:-----------:|:------------:|:--------------:|
| `pragma: primitiveClass` | ✓ | ✓ | ✓ (skips class) |
| `pragma: primitive` | ✓ | ✓ | ✓ (native impl) |
| `pragma: bashOnly` | ✓ | ✓ | ✓ (skips method) |
| `pragma: procyonOnly` | ✗ | ✓ | ✓ |
| `pragma: procyonNative` | ✗ | ✓ | ✓ |
| `pragma: direct` | ✓ | ✗ | ✗ |

---

## 8. Advanced Features

### Method Advice (AOP)

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| `before:` advice | ✓ | ✗ | ✗ |
| `after:` advice | ✓ | ✗ | ✗ |
| Method aliasing | ✓ | ✗ | ✗ |

### Error Handling

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| `_throw` | ✓ | ✓ | ✗ (falls back) |
| `_on_error` | ✓ | ✓ | ✗ (falls back) |
| `_ensure` | ✓ | ✓ | ✗ (falls back) |
| `_pop_handler` | ✓ | ✓ | ✗ (falls back) |

### Introspection

| Feature | jq-compiler | Procyon Bash | Procyon Native |
|---------|:-----------:|:------------:|:--------------:|
| Source embedding | ✓ | ✓ | ✗ |
| Method categories | ✓ | ✗ | ✗ |
| Metadata variables | ✓ | ✓ | ✓ (in code) |

---

## 9. Native-Only Features (Procyon Plugin)

These features are unique to or significantly enhanced in native plugin mode:

| Feature | Description |
|---------|-------------|
| **C-shared exports** | `GetClassName()`, `Dispatch()` exported for FFI |
| **SQLite integration** | Direct database access without subprocess |
| **Type-safe structs** | Instance vars as Go struct fields |
| **Native loop compilation** | `do:`, `collect:`, `select:` as Go for-loops |
| **Direct self-calls** | Self message sends bypass runtime dispatch |
| **200+ primitive natives** | File, String, Shell, etc. in Go |

---

## 10. Limitations Summary

### jq-compiler
- **No type checking** - All values are strings at runtime
- **Method name collision** - `skip:` and `skip` compile to same function
- **Negative number arguments** - May mangle to `0-1` from `0 -1`
- **Performance** - Interpreted bash, subprocess overhead

### Procyon Bash Backend
- **No AOP** - Before/after advice not supported
- **No method categories** - Categories not preserved in output
- **Same runtime limitations** - Still bash-interpreted

### Procyon Native Plugin
- **No raw methods** - Must use `bashOnly` pragma
- **No cascades** - `@ obj a; b; c` not supported
- **No super sends** - `@ super method` not compiled
- **No exception APIs** - `_throw`, `_ensure` cause fallback
- **No regex match** - `=~` operator not implemented
- **Limited closures** - Dynamic blocks invoke bash runtime
- **No source embedding** - Source not included in binary

---

## 11. When to Use Each

### Use jq-compiler when:
- You need AOP features (before/after advice)
- You need method categories for introspection
- You're debugging the compilation process
- You need maximum compatibility

### Use Procyon Bash Backend when:
- You want Go-based compilation (faster build times)
- You need identical runtime behavior to jq-compiler
- You're preparing code for eventual native compilation

### Use Procyon Native Plugin when:
- **Performance is critical** - 10-100x faster execution
- You're building GUI/TUI applications (Yutani)
- You have CPU-intensive operations
- You want to minimize subprocess overhead
- Your code doesn't use AOP or cascades

---

## 12. Migration Checklist

To prepare code for native compilation:

1. **Remove rawMethod/rawClassMethod** - Convert to pure Trashtalk
2. **Replace subshells** - Use assignment: `result := @ obj method` not `result=$(@ obj method)`
3. **Remove bashOnly pragmas** - Unless truly bash-dependent
4. **Avoid cascades** - Use separate message sends
5. **Remove super sends** - Call inherited methods directly
6. **Remove AOP advice** - before/after not supported
7. **Test with native build** - `make native-single CLASS=YourClass`

---

## Version Information

- **jq-compiler**: ~6,700 lines, 40+ token types
- **Procyon Bash Backend**: ~1,000 lines Go
- **Procyon Native Plugin**: ~1,500 lines Go
- **Primitive Registry**: 200+ native implementations
- **Test Coverage**: 36 test suites (jq), comprehensive Go tests
