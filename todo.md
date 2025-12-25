# Trashtalk TODO

See `completed.md` for finished work.

---

## Yutani Windowing Environment
Pending stabilisation of the yutani project, possible fallback to twin if it turns into a shitshow

## Concurrency & Coordination

Base APIs implemented - see FUTURE.md, PROCESS.md, TUPLESPACE.md

### Future Enhancements
- Future combinators (all, any, race)
- Timeout support for futures
- Future chaining/composition

### Process Enhancements
- Piping between processes
- Process groups
- Timeout support for long-running commands
- generally making sure we dont lose track of processes

### Tuplespace Enhancements
- Pattern matching with wildcards
- Lease-based expiration
- Distributed tuplespace (multi-machine)

## Medium Priority

### REPL / @ Improvements
- Completion for bare @ function
- Completion for repl when invoked via `@ Trash repl`

### Documentation Generation
- Extract comments from `.trash` files
- Generate markdown API docs automatically
- Method signatures with argument names

### Lazy Loading of Compiled Classes
- Only source class files when first accessed
- Reduces startup time for large systems

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
- Allow ephemeral object instances - don't always save them to db (new vs create)
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

### Procyon
Go-based code generator takes output from jq compiler and produces a compiled native object which can interoperate with trashtalk

---

*Last updated: 2025-12-22 - String defaults in instanceVars clarified*
