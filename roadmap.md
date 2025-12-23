# Trashtalk Roadmap

## Pre-0.0:
 - Review collection classes - add set, dictionary as objects
 - Build out Tool class to wrap external commands
 - Are we using kv for tuplespace or recfiles?
 - Work out installation
 - What other classes are necessary?
 - Allow ephemeral object instances - don't always save them to db (new vs create)
 - Completion for bare @ function
 - Add second path for user classes, separate from built-in classes
 - Extract comments from `.trash` files
 - Generate markdown API docs automatically
 - Method signatures with argument names
 - Index/maintenance on schema changes
 - Handle upgrades when you add new instance_var to class with existing instances.

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

## Yutani Windowing Environment
Pending stabilisation of the yutani project, possible fallback to twin if it turns into a shitshow

### Procyon
Go-based code generator takes output from jq compiler and produces a compiled native object which can interoperate with trashtalk

### REPL / @ Improvements
- Completion for repl when invoked via `@ Trash repl`

### Documentation Generation
- Extract comments from `.trash` files
- Generate markdown API docs automatically
- Method signatures with argument names

### Lazy Loading of Compiled Classes
- Only source class files when first accessed
- Reduces startup time for large systems

### SQLite Index Automation
- Automatically create indexes for frequently queried instance variables
- Track query patterns and suggest/create indexes
- `@ Store ensureIndex: Counter on: #value`

- Consider: automatic index creation when `find:` predicates are used

### Caching Layer
Every getter does `db_get`, every setter does `db_get` + `db_put`. Could cache instance data during method call chain.

### Batch Saves / Dirty Tracking
- Track which fields changed
- Support transactions

### 'Image' Versioning
- WAL logs for sqlite - snapshots and implicit version control

### Type Validation
```smalltalk
instanceVars: count:integer items:array name:string
```
Validate on set, reject invalid types.

### Performance Optimization
- Only if profiling shows problems
- Bash tokenizer is O(n) per character in some versions
- Acceptable for files <2000 lines
- Consider awk/sed for hot paths if needed

### Modularization
- Split codegen.jq only if it exceeds ~800 lines
- Potential: `header.jq`, `transforms.jq`, `method.jq`, `main.jq`

### Self-Hosting
Could more of Trashtalk be written in Trashtalk? The Store object is a start. What about parser, method dispatch?
