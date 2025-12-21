# Trashtalk TODO

**Test Status: All tests passing (127+ tests across 6 suites)**

See `completed.md` for finished work.

---

## Twin Windowing Environment

### Twin Configuration & Startup
- Figure out socket server configuration (had to start manually)
- Create ergonomic startup script (`trashwin`? `trashsh`? `trash-ui`?)
- Document Ghostty/terminal compatibility (TERM=xterm-256color workaround)
- Investigate why `@ Twin start` can't work (command substitution limitation)

---

## Concurrency & Coordination

Base APIs implemented - see ACTOR.md, PROCESS.md, TUPLESPACE.md

### Actor Enhancements
- Supervision trees (restart strategies)
- Actor linking (linked actors die together)
- Named actors (lookup by name instead of ID)
- Actor groups and broadcast messaging

### Process Enhancements
- Piping between processes
- Process groups
- Timeout support for long-running commands

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

### REPL Improvements
- Command history persistence
- Tab completion for class/method names
- `@ Trash browse Counter` - interactive object browser

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

## Known Issues

### Heredoc in rawMethod
When `rawMethod:` contains heredocs, compiler indents `EOF` terminators, breaking bash syntax. Affects `Trash.trash` methods like `createObject:super:`.

### Method Name Collision (Keyword vs Unary)
Keyword methods (e.g., `skip:`) and unary methods with the same base name (e.g., `skip`) compile to the same bash function name (`__ClassName__skip`), causing the second definition to overwrite the first.

**Impact:** When both exist, calling the keyword version actually executes the unary version, potentially causing infinite recursion if the unary version delegates to the keyword version.

**Workaround:** Inline implementations in both methods rather than having one delegate to the other:
```smalltalk
# BAD - causes infinite loop:
rawMethod: skip: reason [...]
rawMethod: skip [
  @ "$_RECEIVER" skip: "No reason"  # Calls itself!
]

# GOOD - inline both:
rawMethod: skip: reason [
  # ... full implementation ...
]
rawMethod: skip [
  # ... full implementation with default reason ...
]
```

**Proper fix:** Compiler should generate distinct function names, e.g., `__ClassName__skip_` for `skip:` and `__ClassName__skip` for `skip`.

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
