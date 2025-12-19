# Trashtalk TODO

**Test Status: 112+ tests passing (100%)**

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

### Last Result Variable (`$it`)
- REPL stores last result in `$it` (or `$_`)
- Enables fluid exploration: `@ Counter new` → `@ $it increment` → `@ $it increment`
- Runtime support in workspace/REPL context
- Compiler may need awareness for `$it` as special variable

### Inspection Protocol
- Standard `inspect` method returns object state as dictionary
- Example: `@ obj inspect` → `#{class: Counter id: abc123 value: 5 step: 1}`
- Powers the Inspector panel in the environment
- Consider: `instanceVariables` method to list ivar names

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

---

*Last updated: 2024-12-17 - Block closures and control flow complete*
