# Trashtalk Roadmap

## High Priority

### Better Compiler Error Messages
**Status:** Nearly complete in jq-compiler

Done:
- ✅ Track line numbers during tokenization (all tokens have line/col)
- ✅ Source locations in AST nodes (class, method, instanceVar, include, requires)
- ✅ Parser warnings for unknown tokens with location info
- ✅ Error recovery via synchronization points
- ✅ bash -n syntax validation with --check flag
- ✅ Show source context around errors with caret pointing to column

Example output:
```
Parse warnings in /tmp/test.trash:
  2:2: Unexpected token in class body [unknown_token]
       2 |   unknownKeyword here
             ^
```

Remaining:
- Suggest fixes for common mistakes (e.g., "did you mean method:?")
- Integrate jq-compiler as default compiler

### Two-Pass Compiler (AST)
**Status:** ✅ Complete - implemented in `lib/jq-compiler/`

The jq-compiler implements a full two-pass architecture:
1. **Pass 1:** Tokenizer (bash) → JSON token array with line/col info
2. **Pass 2:** Parser (jq) → AST → Codegen (jq) → Bash output

Features:
- 243 tests passing (tokenizer, parser, codegen, integration)
- Handles all .trash files including complex ones (Process.trash, Trash.trash)
- Raw method support for heredocs, traps, signals
- Keyword method transformation (multi-keyword → underscore-joined)
- Nested DSL transformation in subshells
- Error recovery and warnings

Remaining:
- Integrate as default compiler (replace lib/trash-compiler.bash)
- Performance testing on large files

### Pure Smalltalk Syntax in Method Bodies
Currently method bodies use bash syntax with `$()` escapes:
```smalltalk
method: increment [
  current := $(@ self getValue)      # bash escape
  newVal := $((current + step))      # bash arithmetic
]
```
Goal: Pure Smalltalk syntax that compiles to bash:
```smalltalk
method: increment [
  current := self getValue.
  newVal := current + step.
  self setValue: newVal.
  ^ newVal
]
```

## Medium Priority

### Simple Test Framework
**Status:** Unblocked - global variable issue is now fixed

Prototype exists in `TestCounter.trash`. Can now have proper `TestCase` base class:
```smalltalk
TestCounter subclass: TestCase
  method: testIncrement [
    | c |
    c := $(@ Counter new)
    @ self assert: $(@ $c increment) equals: 1
  ]
```

### REPL Improvements
- Command history
- Tab completion for class/method names
- `@ Trash browse Counter` - interactive object browser

### Documentation Generation
- Extract comments from `.trash` files
- Generate markdown API docs automatically

### Lazy Loading of Compiled Classes
- Only source class files when first accessed
- Reduces startup time for large systems

## Lower Priority

### Method Aliasing
```smalltalk
alias: size for: count
alias: length for: count
```

### Caching Layer
Every getter does a full `db_get`, every setter does `db_get` + `db_put`. Could cache instance data during a method call chain to reduce DB round-trips.

### Batch Saves / Dirty Tracking
- Only write to DB when explicitly saved
- Track which fields changed
- Support transactions (all-or-nothing writes)

### Relationships Between Instances
Instances referencing other instances:
```bash
@ $order getCustomer    # Returns another instance ID
@ $order setCustomer $customer_id
```
Maybe with lazy loading and reference integrity.

### Type Validation
Add type hints to instance_vars:
```bash
instanceVars: count:integer items:array name:string
```
Validate on set, reject invalid types.

### Schema Versioning
Handle upgrades to instance structure over time. What happens when you add a new instance_var to a class with existing instances?

### Before/After Hooks (Method Advice)
**Status:** Basic infrastructure implemented via `_add_before_advice`, `_add_after_advice`, `_remove_advice`

DSL syntax still needed:
```smalltalk
before: save do: [@ self validate]
after: delete do: [@ self notifyObservers]
```

### Module/Package System
```smalltalk
package: MyApp
  import: 'networking/HttpClient'
  import: 'data/JsonParser'
```

## Ambitious / Long-term

### Self-Hosting
Could more of Trashtalk be written in Trashtalk? The Store object is a start, but what about the parser, method dispatch, etc.?

### Blocks / Closures
Support for passing blocks of code:
```smalltalk
@ $array select: [ :each | each > 5 ]
@ $array do: [ :each | echo $each ]
@ $array inject: 0 into: [ :sum :x | $((sum + x)) ]
```

### Exception Handling
**Status:** Basic infrastructure implemented via `_throw`, `_on_error`, `_ensure`, `_pop_handler`

DSL syntax still needed:
```smalltalk
method: riskyOperation [
  try: [
    @ self doSomethingDangerous
  ] catch: [ :error |
    @ self log: "Failed: $error"
  ]
]
```

### Protocols / Interfaces
```smalltalk
Counter implements: Enumerable, Comparable
```

### Class-Side Instance Variables
Variables shared across all instances of a class (like class variables in Smalltalk).
```smalltalk
Counter subclass: Object
  classVars: instanceCount:0
```

### Method Categories
Organize methods into categories for better introspection:
```smalltalk
category: "accessing"
  method: getValue [...]
  method: setValue: val [...]

category: "arithmetic"
  method: increment [...]
```

---

## Known Issues

### Heredoc Compilation in rawMethod Sections
When `rawMethod:` sections contain heredocs, the compiler indents the `EOF` terminators, breaking bash syntax. Heredoc terminators must start at column 1. This affects `Trash.trash` methods like `createObject:super:` and `quickCreate:template:`.

---

## Recently Completed

### jq-compiler Two-Pass AST Compiler (2024-12-16)
Implemented a complete two-pass compiler in `lib/jq-compiler/`:
- **Tokenizer** (bash): 35+ token types with line/col tracking
- **Parser** (jq): PEG-style parser producing JSON AST
- **Codegen** (jq): AST to bash code generation
- **243 tests** covering tokenizer, parser, codegen, integration
- **Features:** Error recovery, source locations, nested DSL in subshells, raw methods
- **All .trash files compile** with valid bash syntax
- **Error context display:** Shows source line with caret pointing to error column

### Known Issues Fixed (2024-12-16)
**Find Predicate Query Syntax:** Now working correctly. `@ Counter find 'value > 5'` properly filters instances. The `Object.find()` method parses predicates using regex and generates correct SQL queries.

**Instance _vars Array in Inheritance:** Fixed. All 6 inheritance tests pass. Child classes correctly inherit instance variables from parents, the `_vars` JSON array includes all inherited and own vars, and setters work for both inherited and own variables.

### Tuplespace Fixes (2024-12-15)
**Problem 1:** `tuplespace.bash` overwrote global `$SCRIPT_DIR` variable, causing wrong path lookups.
**Solution:** Renamed internal variables to `_TUPLESPACE_DIR`/`_TUPLESPACE_PARENT`.

**Problem 2:** Trait methods not found - dispatcher didn't look in traits for methods like `debug`.
**Solution:** Added trait method lookup in `send()` dispatcher after class/instance method lookup.

**Problem 3:** Method name mismatches - keyword methods like `put: args:` compiled to `put_args`, but callers expected `put`.
**Solution:** Added `rawMethod:` wrappers in `Tuplespace.trash` for simple varargs-style API (`put`, `get`, `take`, `count`, `putKV`, `getKV`, `putEvent`, `test`).

### Context Stack System & Global Variable Fix (2024-12-15)
**Problem:** `$_RECEIVER`, `$_CLASS`, `$_INSTANCE`, `$_SELECTOR` were global/exported variables that got corrupted by nested message sends.

**Solution:** Changed from `export` to `local` variables in `send()`. Bash's dynamic scoping means called methods see the local values, and nested `send()` calls get their own copies that are automatically restored on return.

**Added infrastructure:**
- **Call stack tracking:** `_CALL_STACK` array with `_print_stack_trace()` for debugging
- **Ensure handlers:** `_ensure "cleanup_cmd"` registers cleanup code that runs when frame exits
- **Error handling:** `_throw`, `_on_error`, `_pop_handler`, `_clear_error` for exception-like handling
- **Method advice:** `_add_before_advice`, `_add_after_advice`, `_remove_advice` for AOP-style hooks

**Also fixed:**
- Removed `is_a Object` from Object class (was causing infinite loop in `hierarchyFor`)
- Fixed test bug: `@ $counter increment 5` → `@ $counter incrementBy 5`

### Quick Wins Batch (2024-12-14)
- **Makefile**: Build workflow with `make compile`, `make test`, `make watch`, `make single CLASS=Name`
- **Per-class reload**: `@ Trash reloadClass Counter` and `@ Trash compileAndReload Counter`
- **Compiler error line numbers**: Parse errors now show file and line number
- **Private method enforcement**: Methods starting with `_` can only be called from same class

### DSL Class Conversion (2024-12-14)
Converted all core classes to DSL format:
- `Store.trash` - SQLite persistence (class methods)
- `Array.trash` - Dynamic arrays with instance vars
- `Debuggable.trash` - Trait for debugging
- `Tuplespace.trash` - Linda-style coordination
- `Trash.trash` - System introspection (uses rawMethod for heredocs)
- `Process.trash` - Background processes (uses rawMethod for traps/signals)

Added compiler features:
- `rawMethod:` / `rawClassMethod:` - Pass-through without transformation
- `requires:` directive for sourcing dependencies
- `trait` keyword for trait definitions
- Keyword message transformation in method bodies

### Namespace Pollution Fix & DSL Compiler (2024-12-14)
**Problem**: Trash methods like `find()` were polluting the global bash namespace.

**Solution**: Created a compilation pipeline that generates namespaced functions:
- Methods compile to `__ClassName__methodName` (e.g., `__Counter__increment`)
- Dispatcher prefers compiled classes

### Instance Method Dispatch Fix (2024-12-14)
Added `$_CLASS` export to `send()` function for proper instance method dispatch.

### Edit Method on Object (2024-12-13)
Added `edit` method to Object that opens class definitions in `$EDITOR`.

### Instance Variable Defaults (2024-12-13)
Added default value syntax: `instanceVars: count:0 step:5 name:hello`

### Inheritance of instance_vars (2024-12-13)
Child classes automatically inherit instance variables from parent classes.

### Object Persistence Methods (2024-12-13)
Added `findAll`, `find`, `count`, `save`, `delete`, `asJson`, `exists` to Object.

---

*Last updated: 2024-12-16 (Known issues fixed, heredoc bug identified)*
