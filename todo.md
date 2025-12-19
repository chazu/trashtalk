# Trashtalk TODO

**Test Status: 7 test suites, 35+ bash tests, 22 TestCase assertions (100% passing)**

See `completed.md` for finished work.

---

## Interactive Environment Features

These enable the "output becomes input" paradigm for the Trashtalk environment.

### Canonical Object Printing - COMPLETE
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

### Last Result Variable (`$__`) - COMPLETE
Each `@` command stores its result in `$__` (double underscore):
```bash
@ Counter new           # => counter_abc123, $__ = counter_abc123
@ $__ increment         # => 1, $__ = 1
@ $__ printString       # Error: "1" is not an instance

# For repeated access, save to a variable:
c=$(@ Counter new)
@ $c increment          # => 1
@ $c increment          # => 2
```
Note: `$_` couldn't be used because it's a bash special variable.

### Inspection Protocol - COMPLETE
Object class (`trash/Object.trash`) now provides:
- `inspect` - detailed inspection showing class, id, and all instance variables
- `asJson` - returns raw JSON data for the instance
- `findAll` (class method) - returns all instances of the class
- `count` (class method) - returns count of instances
- `find` (class method) - find instances matching a predicate (e.g., `@ Counter find "value > 5"`)

Example:
```bash
@ $counter inspect
# a Counter
#   id: counter_abc123
#   value: 5
#   step: 1

@ Counter findAll      # => counter_abc123 counter_def456
@ Counter count        # => 2
@ Counter find "value > 5"  # => counter_abc123
```

### Legacy Code Cleanup - COMPLETE
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

### Test Framework (TestCase) - COMPLETE
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

## Language Features - High Priority

### Block Closures - COMPLETE
See `completed.md` for implementation details.

### Control Flow - COMPLETE
See `completed.md` for implementation details.

### Object References as Instance Variables - COMPLETE
Instance IDs are strings and can be stored directly in ivars. Runtime helpers added:
- `_ivar_ref varname` - Get object reference from ivar (documents intent)
- `_ivar_ref_valid varname` - Check if reference points to existing instance
- `_ivar_ref_class varname` - Get class of referenced object
- `_ivar_send varname method args...` - Send message to object in ivar

Example:
```smalltalk
method: setCustomer: cust [
  customerId := cust.
]
method: getCustomerName [
  ^ $(_ivar_send customerId getName)
]
```

Future: Cascading deletes, reference integrity checks on save.

### Collection Literals in Instance Variables (Phase 5b) - COMPLETE
Arrays and dictionaries can now be stored as instance variables via JSON serialization:
- `data := #(1 2 3)` → `_ivar_set data '["1","2","3"]'`
- `config := #{a: 1}` → `_ivar_set config '{"a":"1"}'`

Runtime helpers:
- `_ivar_array varname` - Get array ivar as space-separated quoted values
- `_ivar_dict varname` - Get dict ivar as bash declare statement
- `_ivar_array_at varname index` - Get single array element
- `_ivar_dict_at varname key` - Get single dict value

Local variables still use bash array syntax for efficiency.

---

## Class System Features

### Class Instance Variables - COMPLETE
See `completed.md` for implementation details.

### Protocols - COMPLETE
Go-style ad-hoc polymorphism. Classes don't declare they implement a protocol; if they have the required methods, they conform.
```smalltalk
Enumerable subclass: Protocol
  requires: do:
  requires: collect:
  requires: inject: into:

@ Array conformsTo: Enumerable   "=> true"
@ Counter conformsTo: Enumerable "=> false"
```
See `completed.md` for implementation details.

### Method Categories - COMPLETE
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
- See `completed.md` for implementation details

---

## Concurrency & Coordination

### Process API
Design and document the Process class API for spawning and managing background processes:
- `@ Process spawn: ObjectName` - spawn object as background process
- `@ Process sendTo: processId message: selector` - async message send
- `@ Process waitFor: processId` - block until process completes
- Process lifecycle (start, stop, status)
- Error handling and process supervision
- Usage patterns and examples

### Tuplespace API & Patterns
Design and document the Tuplespace coordination primitives:
- `@ Tuplespace put: tuple` / `@ Tuplespace get: pattern` - basic operations
- Pattern matching semantics (wildcards, partial matches)
- Blocking vs non-blocking reads
- Common patterns: producer/consumer, request/response, pub/sub
- Integration with Process for inter-process communication
- Performance considerations and cleanup

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

*Last updated: 2024-12-18 - Legacy code cleanup complete*
