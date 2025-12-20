# Trashtalk TODO

**Test Status: 7 test suites, 35+ bash tests, 22 TestCase assertions (100% passing)**

See `completed.md` for finished work.

---

## Concurrency & Coordination

### Actor Supervision ✓ (Actor API implemented, documented in ACTOR.md)
Build on the Actor concurrency primitive:
- Supervision trees (restart strategies)
- Actor linking (linked actors die together)
- Named actors (lookup by name instead of ID)
- Actor groups and broadcast messaging

### Process Enhancements ✓ (Process API implemented, documented in PROCESS.md)
Extend the external process management:
- Piping between processes
- Process groups
- Timeout support for long-running commands

### Tuplespace Patterns ✓ (Tuplespace API documented in TUPLESPACE.md)
Additional coordination patterns:
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
- `$it` support (see above)

### Documentation Generation
- Extract comments from `.trash` files
- Generate markdown API docs automatically
- Method signatures with argument names

### Lazy Loading of Compiled Classes
- Only source class files when first accessed
- Reduces startup time for large systems

---

## Lower Priority

### Method Aliasing
```smalltalk
alias: size for: count
alias: length for: count
```

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

### Before/After Hooks DSL
Infrastructure exists (`_add_before_advice`, etc.). Need DSL syntax:
```smalltalk
before: save do: [@ self validate]
after: delete do: [@ self notifyObservers]
```

### Exception Handling DSL
Infrastructure exists (`_throw`, `_on_error`, etc.). Need DSL syntax:
```smalltalk
try: [
  @ self doSomethingDangerous
] catch: [ :error |
  @ self log: "Failed: $error"
]
```

---

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

*Last updated: 2024-12-18 - Moved completed items to completed.md*
