# Trashtalk TODO

**Test Status: All tests passing (127+ tests across 6 suites)**

See `completed.md` for finished work.

---

## Yutani Windowing Environment
Pending stabilisation of the yutani project, possible fallback to twin if it turns into a shitshow

## Concurrency & Coordination

Base APIs implemented - see ACTOR.md, PROCESS.md, TUPLESPACE.md

### Actor Enhancements
- Supervision trees (restart strategies)
- Actor linking (linked actors die together)
- Named actors (lookup by name instead of ID)
- Actor groups and broadcast messaging

### Process Enhancements
- Piping between processes (shouldn't this be an actor concern?)
- Process groups
- Timeout support for long-running commands
- generally making sure we dont lose track of processes

### Tuplespace Enhancements
- Pattern matching with wildcards
- Lease-based expiration
- Distributed tuplespace (multi-machine)

---

## Module System

### Packages / Namespaces
```smalltalk
package: MyApp
  import: 'networking/HttpClient'
  import: 'data/JsonParser'
```
- Namespace isolation to avoid class name collisions
- `MyApp::Counter` vs `OtherLib::Counter`
- Package-level visibility (public/private classes)
- Dependency declaration and loading order

---

## Medium Priority

### REPL / @ Improvements
- Command history persistence
- Tab completion for class/method names
- wrap @ in rlwrap or similar
- `@ Trash browse Counter` - interactive object browser, possibly with gum

### Documentation Generation
- Extract comments from `.trash` files
- Generate markdown API docs automatically
- Method signatures with argument names

### Lazy Loading of Compiled Classes
- Only source class files when first accessed
- Reduces startup time for large systems

---

## Lower Priority

### SQLite Index Automation
- Automatically create indexes for frequently queried instance variables
- Track query patterns and suggest/create indexes
- `@ Store ensureIndex: Counter on: #value`
- Index maintenance on schema changes
- Consider: automatic index creation when `find:` predicates are used

### Caching Layer
Every getter does `db_get`, every setter does `db_get` + `db_put`. Could cache instance data during method call chain.

### Batch Saves / Dirty Tracking
- Only write to DB when explicitly saved
- Track which fields changed
- Support transactions

### 'Image' Versioning
- WAL logs for sqlite - snapshots and implicit version control

### Type Validation
```smalltalk
instanceVars: count:integer items:array name:string
```
Validate on set, reject invalid types.

### Schema Versioning
Handle upgrades when you add new instance_var to class with existing instances.

## Performance & Maintenance

### Performance Optimization
- Only if profiling shows problems
- Bash tokenizer is O(n) per character in some versions
- Acceptable for files <2000 lines
- Consider awk/sed for hot paths if needed

### Modularization
- Split codegen.jq only if it exceeds ~800 lines
- Potential: `header.jq`, `transforms.jq`, `method.jq`, `main.jq`

---

## Ambitious / Long-term

### Self-Hosting
Could more of Trashtalk be written in Trashtalk? The Store object is a start. What about parser, method dispatch?

### Windowing Environment
See `windowing-ideas.md` for research on building an Acme-like terminal environment:
- Multi-panel tiled layout
- Text execution (any text is runnable)
- Structural regular expressions for code queries
- Trashtalk-aware editing

---

## Critical: Known Issues (Must Fix for v1.0)

These bugs break expected behavior and should be fixed before Trashtalk is considered complete.

### Heredoc in rawMethod
When `rawMethod:` contains heredocs, compiler indents `EOF` terminators, breaking bash syntax. Affects `Trash.trash` methods like `createObject:super:`.

### ~~Method Name Collision (Keyword vs Unary)~~ ✓ FIXED
~~Keyword methods (e.g., `skip:`) and unary methods with the same base name (e.g., `skip`) compile to the same bash function name (`__ClassName__skip`), causing the second definition to overwrite the first.~~

**Fixed:** Keyword methods now compile with trailing underscore (e.g., `skip:` → `__Class__skip_`, `skip` → `__Class__skip`). The runtime dispatcher also updated to match.

### Negative Numbers in Arguments
The compiler may mangle arguments when a negative number follows another argument. For example, `0 -1` can become `0-1` (single argument instead of two).

**Workaround:** Avoid negative number literals in method calls. Use variables or different test values:
```smalltalk
# BAD - may be mangled:
@ obj assert_greater_than 0 -1

# GOOD - use positive numbers or variables:
@ obj assert_greater_than 1 0
```

### String Defaults in instanceVars
The compiler strips colons from string default values. For example, `instanceVars: name:unknown status:active` compiles to `name unknown status active` instead of preserving the defaults.

**Impact:** String defaults don't work; only numeric defaults are reliable.

**Workaround:** Use numeric defaults (0, 1, etc.) or leave variables without defaults and initialize in the `new` method.

---

*Last updated: 2024-12-20 - All tests passing, exception handling DSL added*
