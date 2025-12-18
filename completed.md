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

*Last updated: 2024-12-17*
