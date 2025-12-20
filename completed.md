# Trashtalk - Completed Work

This file tracks completed features and fixes. Moved from todo.md to reduce clutter.

---

## Compiler Infrastructure

### jq-compiler Two-Pass AST Compiler (2024-12-16)
Implemented a complete two-pass compiler in `lib/jq-compiler/`:
- **Tokenizer** (bash): 35+ token types with line/col tracking
- **Parser** (jq): PEG-style parser producing JSON AST
- **Codegen** (jq): AST to bash code generation
- **278 tests** covering tokenizer, parser, codegen, integration
- **Features:** Error recovery, source locations, nested DSL in subshells, raw methods
- **All .trash files compile** with valid bash syntax

Architecture uses **island parsing** - a well-established pattern for DSLs with embedded host language code.

### Better Compiler Error Messages (2024-12-16)
- Track line numbers during tokenization (all tokens have line/col)
- Source locations in AST nodes (class, method, instanceVar, include, requires)
- Parser warnings for unknown tokens with location info
- Error recovery via synchronization points
- `bash -n` syntax validation with `--check` flag
- Show source context around errors with caret pointing to column

### Compiler Integration (2024-12-16)
- Removed old `lib/trash-compiler.bash` (882 lines)
- Updated `Trash.trash` `compileAndReload:` method to use jq-compiler
- Updated Makefile for traits directory creation
- All tests pass

---

## Smalltalk Syntax - Phase 1 & 2

### Expression Parser with Ivar Inference (2024-12-17)
Implemented full Pratt parser for Smalltalk-style expressions:
- **Instance variable inference:** `value + step` → `$(_ivar value) + $(_ivar step)`
- **Instance variable assignment:** `value := value + 5` → `_ivar_set value "..."`
- **Arithmetic with precedence:** `x + y * 2` respects mathematical precedence
- **Cascade syntax:** `@ self inc; inc; inc.` sends multiple messages to same receiver
- **Expression args in keywords:** `@ self at: x + 1 put: y * 2`
- **Runtime support:** Added `_ivar` and `_ivar_set` functions to lib/trash.bash
- **Smart parser selection:** Legacy code uses legacy parser, new Smalltalk code uses expression parser

### Collection Literals - Phase 5 (2024-12-17)
- Symbol syntax: `#symbol` → `"symbol"` (bare value in assignments/returns)
- Array literals: `#(1 2 3)` → `("1" "2" "3")` (bash indexed array)
- Dictionary literals: `#{key: value}` → `([key]="value")` (bash associative array)
- 24 new tests for collection literals
- Limitation: Arrays/dicts can only be assigned to local vars, not ivars (needs serialization)

---

## Runtime Infrastructure

### Context Stack System & Variable Fix (2024-12-15)
**Problem:** `$_RECEIVER`, `$_CLASS`, `$_INSTANCE`, `$_SELECTOR` were global variables that got corrupted by nested message sends.

**Solution:** Changed from `export` to `local` variables in `send()`. Bash's dynamic scoping means called methods see the local values, and nested `send()` calls get their own copies.

**Added infrastructure:**
- **Call stack tracking:** `_CALL_STACK` array with `_print_stack_trace()`
- **Ensure handlers:** `_ensure "cleanup_cmd"` for cleanup on frame exit
- **Error handling:** `_throw`, `_on_error`, `_pop_handler`, `_clear_error`
- **Method advice:** `_add_before_advice`, `_add_after_advice`, `_remove_advice`

### Tuplespace Fixes (2024-12-15)
- Fixed `tuplespace.bash` overwriting global `$SCRIPT_DIR`
- Added trait method lookup in `send()` dispatcher
- Fixed keyword method name mismatches with `rawMethod:` wrappers

---

## DSL & Class System

### DSL Class Conversion (2024-12-14)
Converted all core classes to DSL format:
- `Store.trash` - SQLite persistence
- `Array.trash` - Dynamic arrays
- `Debuggable.trash` - Trait for debugging
- `Tuplespace.trash` - Linda-style coordination
- `Trash.trash` - System introspection
- `Process.trash` - Background processes

Added compiler features:
- `rawMethod:` / `rawClassMethod:` - Pass-through without transformation
- `requires:` directive for sourcing dependencies
- `trait` keyword for trait definitions
- Keyword message transformation in method bodies

### Namespace Pollution Fix (2024-12-14)
Methods compile to `__ClassName__methodName` (e.g., `__Counter__increment`). Dispatcher prefers compiled classes.

### Test Framework (2024-12-16)
- Fixed `_get_class_instance_vars` to read compiled metadata
- Fixed `_get_parent_class` to read compiled metadata
- `TestCase` base class with inheritance working
- `@ TestCounter runAll` passes all assertions

---

## Quick Wins (2024-12-14)

- **Makefile**: `make compile`, `make test`, `make watch`, `make single CLASS=Name`
- **Per-class reload**: `@ Trash reloadClass Counter` and `@ Trash compileAndReload Counter`
- **Compiler error line numbers**: Parse errors show file and line number
- **Private method enforcement**: Methods starting with `_` only callable from same class
- **Instance variable defaults**: `instanceVars: count:0 step:5`
- **Inheritance of instance_vars**: Child classes inherit from parents
- **Object persistence**: `findAll`, `find`, `count`, `save`, `delete`, `asJson`, `exists`
- **Edit method**: Opens class definitions in `$EDITOR`

---

## Bug Fixes

### Process Substitution (2024-12-17)
`< <(...)` was incorrectly collapsed to `<<(...)`. Fixed in codegen.jq.

### Character Class Spacing (2024-12-17)
`[Yy]$` was becoming `[Yy ]$`. Fixed gsub to handle letters.

### Find Predicate Query (2024-12-16)
`@ Counter find 'value > 5'` now properly filters instances.

### Instance _vars Array in Inheritance (2024-12-16)
Child classes correctly inherit instance variables from parents.

---

---

## Collection Literals in Instance Variables (2024-12-17)

Arrays and dictionaries can now be stored as instance variables:
- Codegen detects ivar target and generates JSON serialization
- `data := #(1 2 3)` → `_ivar_set data '["1","2","3"]'`
- `config := #{a: 1}` → `_ivar_set config '{"a":"1"}'`
- Local variables still use efficient bash array syntax

Runtime helpers added:
- `_ivar_array` - Get array ivar as quoted values
- `_ivar_dict` - Get dict ivar as bash declare statement
- `_ivar_array_at` - Get single array element by index
- `_ivar_dict_at` - Get single dict value by key

11 new tests added, 289 total tests passing.

---

## Object References as Instance Variables (2024-12-17)

Instance IDs are strings and can be stored directly in ivars. Runtime helpers added:
- `_ivar_ref` - Get object reference from ivar (documents intent)
- `_ivar_ref_valid` - Check if reference points to existing instance
- `_ivar_ref_class` - Get class of referenced object
- `_ivar_send` - Send message to object stored in ivar

---

## Block Closures (2024-12-17)

Full Block class implementation with variable capture:

**Syntax:**
- `[:x | x + 1]` - Block with one parameter
- `[:x :y | x + y]` - Block with two parameters
- `[@ self doSomething]` - Block with no parameters

**Implementation:**
- **Tokenizer:** Added `BLOCK_PARAM` token for `:x` syntax in block parameter lists
- **Parser:** Recognizes `block_literal` (with params) and `block` (no params) AST types
- **Codegen:** Generates `$(@ Block params_code_captured ...)` expressions
- **Runtime:** `Block.trash` class with `value`, `valueWith:`, `valueWith:and:` methods
- Blocks capture `_RECEIVER` for `@ self` references within block body
- Block body wraps last expression in `echo` for return values

**Array iteration methods added:**
- `do:` - Execute block for each element
- `collect:` - Map block over elements, return new Array
- `select:` - Filter elements where block returns non-empty
- `inject:into:` - Reduce/fold using block

**Files modified:**
- `lib/jq-compiler/tokenizer.bash` - BLOCK_PARAM token
- `lib/jq-compiler/codegen.jq` - block_literal and block handling (~lines 669-724)
- `trash/Block.trash` - NEW runtime class
- `trash/Array.trash` - Iteration methods

**Tests:** 9 tests in `lib/jq-compiler/tests/test_blocks.bash`

---

## Control Flow (2024-12-17)

Smalltalk-style control flow with inlined blocks:

**Boolean messages:**
- `ifTrue:` - Execute block if receiver is truthy
- `ifFalse:` - Execute block if receiver is falsy
- `ifTrue:ifFalse:` - Conditional with both branches

**Loop constructs:**
- `whileTrue:` - Execute block while condition is true
- `timesRepeat:` - Execute block N times

**Example:**
```smalltalk
(count > 0) ifTrue: [@ self decrement]
[running] whileTrue: [@ self processNext]
5 timesRepeat: [@ self tick]
```

**Implementation:** Control flow blocks are inlined at compile time (not Block objects) for efficiency. The codegen generates bash `if`/`while`/`for` constructs directly.

**Tests:** `lib/jq-compiler/tests/test_control_flow.bash`

---

## Runtime Bug Fix: Class Name Path Resolution (2024-12-17)

**Problem:** `_SUPERCLASS` was being set to full path (e.g., `/Users/chazu/.trashtalk/trash/Object`) instead of class name, causing errors like `__/Users/chazu/.trashtalk/trash/Object__instanceVars: invalid variable name`.

**Solution:** Added path sanitization at start of `_get_class_instance_vars` and `_get_parent_class` functions:
```bash
class_name="${class_name##*/}"
```

This extracts just the basename from any path, fixing inheritance and instance variable resolution.

---

## Class Instance Variables (2024-12-18)

Variables shared across all instances of a class, stored in kv-bash.

**Syntax:**
```smalltalk
Counter subclass: Object
  classInstanceVars: instanceCount:0 defaultStep:1
  instanceVars: value:0

  classMethod: count [
    ^ instanceCount
  ]

  method: initialize [
    instanceCount := instanceCount + 1.
  ]
```

**Implementation:**
- **Parser:** Added `classInstanceVars:` as sync point and parsing case
- **Codegen:** Generates `__ClassName__classInstanceVars` metadata and `__ClassName__initClassVars()` function
- **Runtime:** Added `_cvar` and `_cvar_set` functions using kv-bash storage
- **Expression parser:** Auto-infers cvars like ivars - `counter` → `$(_cvar counter)`, `counter := x` → `_cvar_set counter "$x"`

**Storage:**
- Keys: `__ClassName__cvar__varname` in kv-bash
- Initialized on first class access (not re-initialized if already set)
- No inheritance - each class has its own cvars

**Design decisions:**
- No cvar inheritance (each class declares its own)
- No auto-generated accessors (use `_cvar`/`_cvar_set` directly)

**Files modified:**
- `lib/jq-compiler/parser.jq` - Added `classInstanceVars:` parsing
- `lib/jq-compiler/codegen.jq` - Added metadata, initializer, cvar inference
- `lib/trash.bash` - Added `_cvar`, `_cvar_set`, initializer call in send()

**Tests:** 9 tests in `lib/jq-compiler/tests/test_class_instance_vars.bash`

---

## Protocols (2024-12-18)

Go-style ad-hoc polymorphism. Classes don't declare they implement a protocol; if they have the required methods, they conform.

```smalltalk
Enumerable subclass: Protocol
  requires: do:
  requires: collect:
  requires: inject: into:

@ Array conformsTo: Enumerable   "=> true"
@ Counter conformsTo: Enumerable "=> false"
```

---

## Method Categories (2024-12-18)

Organize methods into categories for better introspection:

```smalltalk
category: "accessing"
  method: getValue [...]
  method: setValue: val [...]

category: "arithmetic"
  method: increment [...]
```

- Compiler tracks category membership in `__ClassName__methodCategories` metadata
- `@ Trash categoriesFor ClassName` - list all categories
- `@ Trash methodsIn_category ClassName categoryName` - list methods in category

---

## Interactive Environment Features (2024-12-18)

### Canonical Object Printing
Objects print as `<ClassName instanceId>`:
```bash
@ $counter printString  # => <Counter counter_abc123>
@ $array printString    # => <Array array_def456>
```

Object base class (`trash/Object.trash`) provides:
- `printString` - canonical `<Class id>` format
- `class` - returns class name
- `id` - returns instance ID
- `isKindOf: className` - checks inheritance chain
- `conformsTo: protocolName` - checks protocol conformance

### Last Result Variable (`$__`)
Each `@` command stores its result in `$__` (double underscore):
```bash
@ Counter new           # => counter_abc123, $__ = counter_abc123
@ $__ increment         # => 1, $__ = 1
```

Note: `$_` couldn't be used because it's a bash special variable.

### Inspection Protocol
Object class (`trash/Object.trash`) provides:
- `inspect` - detailed inspection showing class, id, and all instance variables
- `asJson` - returns raw JSON data for the instance
- `findAll` (class method) - returns all instances of the class
- `count` (class method) - returns count of instances
- `find` (class method) - find instances matching a predicate (e.g., `@ Counter find "value > 5"`)

---

## Test Framework - TestCase (2024-12-18)

Full xUnit-style test framework in `trash/TestCase.trash`:

**Assertions:**
- `assert: actual equals: expected` / `assert_equals` - equality check
- `assert: actual notEquals: expected` / `assert_not_equals` - inequality
- `assertTrue:` / `assert_true` - boolean true check
- `assertFalse:` / `assert_false` - boolean false check
- `assertNil:` / `assert_nil` - nil/empty check
- `assertNotNil:` / `assert_not_nil` - non-nil check
- `assert: haystack contains: needle` / `assert_contains` - string containment
- `assert: actual matches: pattern` / `assert_matches` - regex matching
- `assert: actual greaterThan: expected` / `assert_greater_than` - numeric >
- `assert: actual lessThan: expected` / `assert_less_than` - numeric <
- `assert: actual greaterOrEqual: expected` - numeric >=
- `assert: actual lessOrEqual: expected` - numeric <=

**Test Control:**
- `skip:` / `skip` - skip test with optional reason
- `pending:` / `pending` - mark test as pending
- `fail:` / `fail` - explicit failure

**Usage:**
```smalltalk
MyTest subclass: TestCase
  rawMethod: testSomething [
    @ "$_RECEIVER" assert_equals "hello" "hello"
    @ "$_RECEIVER" assert_true "1"
  ]

# Run with:
@ MyTest runAll
```

---

## Legacy Code Cleanup (2024-12-18)

All legacy `is_a` + `instance_vars` + function-style syntax has been removed:

**Deleted:**
- `trash/*.legacy` archive files (Array.legacy, Store.legacy, Trash.legacy, Tuplespace.legacy)

**Updated to modern `.trash` syntax:**
- `tests/test_instance_var_defaults.bash` - now creates/compiles `.trash` files dynamically
- `Trash.trash` template methods (`createObject:super:`, `quickCreate:template:`) - now generate `.trash` files and compile them
- `Trash.trash` introspection methods (`methodsFor:`, `hierarchyFor:`) - now use compiled metadata

**Removed legacy fallbacks from `lib/trash.bash`:**
- `_get_class_instance_vars()` - no longer parses `instance_vars` from legacy files
- `_get_parent_class()` - no longer parses `is_a` from legacy files

**Still used (not legacy):**
- `is_a()` and `instance_vars()` functions - used by runtime for accessor generation from compiled metadata
- Runtime class files (`trash/ClassName` without extension) - these are compiled output copies

---

## Actor/Process Separation (2024-12-19)

Refactored concurrency architecture to distinguish between internal concurrency and external process management:

**Actor Class** (`trash/Actor.trash`) - Erlang/Go-style concurrency:
- `@ Actor spawn: ClassName` - spawn isolated actor running a message loop
- `@ Actor sendTo: id message: selector` - async message send via Tuplespace
- `@ Actor getFrom: id timeout: seconds` - wait for response
- `@ Actor terminate: id` - graceful shutdown
- `@ Actor status: id`, `listActors`, `listRunning`, `cleanupAll`, `stats`
- Actors run in separate bash subshells, communicate only via Tuplespace
- No shared state, one message processed at a time

**Process Class** (`trash/Process.trash`) - External OS process management:
- `@ Process for: "command"` - create managed process wrapper
- Instance methods: `run`, `start`, `wait`, `isRunning`, `terminate`, `kill`, `signal:`
- Output capture: `output`, `errors`, `exitCode`, `succeeded`
- Class methods for quick execution: `exec:`, `run:`, `spawn:`, `waitPid:`, `isRunningPid:`, `killPid:`
- Similar to Python's subprocess module

**Documentation:**
- `ACTOR.md` - Complete Actor API reference
- `PROCESS.md` - Complete Process API reference
- `TUPLESPACE.md` - Updated to reference Actor for inter-actor communication

**Dispatcher fix:** Added keyword method parsing to build compound selectors from `@ obj method: arg1 key2: arg2` syntax.

---

## File Class & Twin Integration (2024-12-19)

Built foundational classes for Twin windowing environment:

**File Class** (`trash/File.trash`) - File system operations:
- Instance methods: `read`, `write:`, `writeLine:`, `append:`, `appendLine:`
- Queries: `exists`, `isFile`, `isDirectory`, `isFifo`, `size`, `modificationTime`
- Path operations: `path`, `directory`, `basename`, `extension`, `stem`
- Operations: `delete`, `copyTo:`, `moveTo:`, `touch`, `info`
- Class methods for quick ops: `@ File read:`, `@ File write:to:`, `@ File exists:`, `@ File delete:`
- Temp files: `@ File temp`, `@ File tempWithPrefix:`
- FIFOs: `@ File mkfifo:`

**Twin Class** (`trash/Twin.trash`) - Window manager integration:
- `@ Twin open: "command"` - Open terminal running command
- `@ Twin edit: "/path"` - Open file in $EDITOR
- `@ Twin inspect: $obj` - Open object inspector window
- `@ Twin workspace` - Open code workspace
- `@ Twin openStream:` / `sendTo:data:` / `closeStream:` - FIFO-based streaming
- `@ Twin isAvailable` - Check if Twin is installed

**Object Enhancements:**
- Added `inspectTo:` method for writing inspection to a file

**Runtime Fixes:**
- Fixed class method dispatch priority: when receiver is a class (not instance), class methods are now checked BEFORE instance methods. This fixes `@ File exists: "/path"` calling the class method instead of the shadowing instance method.
- Fixed class pre-sourcing in `@` function to ensure classes are loaded before dispatch.

**Documentation:**
- `FILE.md` - Complete File API reference

---

*Last updated: 2024-12-19*
