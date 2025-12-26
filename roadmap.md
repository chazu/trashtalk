# Trashtalk Roadmap

## Pre-0.0:
 - Do we have problems with multiple processes running at once?
 - Ensure all documentation is correct
 - Review collection classes - add list (or array), set, dictionary as objects
 - Build out Tool class to wrap external commands
 - Work out installation
 - What other classes are necessary?
   - HttpClient and HttpServer
 - Allow ephemeral object instances - don't always save them to db (new vs create)
 - Add second path for user classes, separate from built-in classes
 - Extract comments from `.trash` files
 - Generate markdown API docs automatically
 - Method signatures with argument names
 - Index/maintenance on schema changes
 - Handle upgrades when you add new instance_var to class with existing instances.
 - rework object IDs - include hash of class instead of class name?
 - Completion for bare @ function
 - do we want some syntactic sugar for sending output to /dev/null?

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

## Yutani Windowing Environment
Pending stabilisation of the yutani project, possible fallback to twin if it turns into a shitshow

### Procyon
Go-based code generator takes output from jq compiler and produces a compiled native object which can interoperate with trashtalk

### Script Execution Enhancements
Currently `@ Trash eval:` wraps code in a temporary class. Future improvements:
- Extend compiler to handle standalone expressions (true Trashtalk syntax without class wrapper)
- Full Trashtalk interpreter for interactive evaluation without compilation step
- REPL integration with expression-level eval
- Script file format (.trash-script) that doesn't require class boilerplate

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
