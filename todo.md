# Trashtalk Roadmap

## High Priority

### Compile Remaining Core Classes
The DSL compiler and namespace isolation are working. Next step is to compile the remaining core classes to prevent namespace pollution:
- `Trash` - System introspection class (has `find` method that shadows bash)
- `Array`, `Store`, `Process` - Other classes with potential conflicts
- Create `.trash` DSL versions and compile them to `trash/.compiled/`

### Improve DSL Compiler
The compiler (`lib/trash-compiler.bash`) works but has limitations:
- Complex bash constructs in method bodies need manual handling
- Add support for `if/then/else`, `while`, `case` statements
- Better error messages for syntax errors
- Consider a two-pass approach: parse to AST, then generate bash

### Pure Smalltalk Syntax in Method Bodies
Currently method bodies use bash syntax with `$()` escapes:
```smalltalk
method: increment [
  current := $(@ self getValue)      # bash escape
  newVal := $((current + step))      # bash arithmetic
]
```
Goal: Pure Smalltalk syntax that compiles to bash:
```smalltalk
method: increment [
  current := self getValue.
  newVal := current + step.
  self setValue: newVal.
  ^ newVal
]
```

## Medium Priority

### Keyword Message Syntax
Support Smalltalk-style keyword messages:
```bash
@ [myCounter at: 3 put: "foo"]
@ [myArray from: 1 to: 5]
@ [store find: "Counter" where: "value > 5"]
```
Would require extending trash-parser.bash to handle colon-delimited keywords.

## Lower Priority

### Caching Layer
Every getter does a full `db_get`, every setter does `db_get` + `db_put`. Could cache instance data during a method call chain to reduce DB round-trips.

### Batch Saves / Dirty Tracking
- Only write to DB when explicitly saved
- Track which fields changed
- Support transactions (all-or-nothing writes)

### Relationships Between Instances
Instances referencing other instances:
```bash
@ $order getCustomer    # Returns another instance ID
@ $order setCustomer $customer_id
```
Maybe with lazy loading and reference integrity.

### Type Validation
Add type hints to instance_vars:
```bash
instance_vars count:integer items:array name:string
```
Validate on set, reject invalid types.

### Schema Versioning
Handle upgrades to instance structure over time. What happens when you add a new instance_var to a class with existing instances?

### Self-Hosting
Could more of Trashtalk be written in Trashtalk? The Store object is a start, but what about the parser, method dispatch, etc.?

### Blocks / Closures
Support for passing blocks of code:
```bash
@ $array select: [ :each | each > 5 ]
@ $array do: [ :each | echo $each ]
```

### Class-Side Instance Variables
Variables shared across all instances of a class (like class variables in Smalltalk).

### Method Categories
Organize methods into categories for better introspection:
```bash
category "accessing"
getValue() { ... }
setValue() { ... }

category "arithmetic"
increment() { ... }
```


## Recently Completed

### Namespace Pollution Fix & DSL Compiler (2024-12-14)
**Problem**: Trash methods like `find()` were polluting the global bash namespace, making the POSIX `find` utility unusable after sourcing Trash.

**Solution**: Created a compilation pipeline that generates namespaced functions:
- **New file**: `lib/trash-compiler.bash` - Compiles `.trash` DSL files to namespaced bash
- **Naming convention**: Methods compile to `__ClassName__methodName` (e.g., `__Counter__increment`)
- **Updated dispatcher**: `send()` and `method_missing()` now prefer compiled classes
- **Compiled classes**: `trash/.compiled/Counter`, `trash/.compiled/Object`

**DSL Syntax** (keyword-based, Option B):
```smalltalk
Counter subclass: Object
  instanceVars: value:0 step:1

  method: increment [
    | current step newVal |
    current := $(@ self getValue)
    step := $(@ self getStep)
    newVal := $((current + step))
    @ self setValue: $newVal
    ^ $newVal
  ]

  classMethod: description [
    ^ "A simple counter"
  ]
```

**Usage**:
```bash
# Compile a class
bash lib/trash-compiler.bash compile trash/Counter.trash trash/.compiled/Counter

# Dispatcher automatically uses compiled version
counter=$(@ Counter new)
@ $counter increment  # Uses __Counter__increment, not find()
find . -name "*.md"   # Still works! bash find not shadowed
```

### Instance Method Dispatch Fix (2024-12-14)
**Problem**: Instance methods like `@ $counter find 'x > 5'` failed because `$_RECEIVER` was the instance ID, but `find()` expected a class name.

**Solution**: Added `$_CLASS` export to `send()` function:
- `$_RECEIVER` = actual receiver (instance ID or class name)
- `$_CLASS` = always the class name
- `$_INSTANCE` = instance ID if receiver is instance, empty otherwise
- Updated `Object.find`, `Object.findAll`, `Object.count` to use `$_CLASS`

### Edit Method on Object (2024-12-13)
Added `edit` method to Object that opens class definitions in `$EDITOR`:
```bash
@ Counter edit        # Opens trash/Counter in $EDITOR
@ $counter edit       # Opens the class file for this instance's type
```
- Works for both class names and instance IDs
- Falls back to `vi` if `$EDITOR` not set
- Error handling for missing class files

### Instance Variable Defaults (2024-12-13)
Added default value syntax to `instance_vars`:
```bash
instance_vars count:0 step:5 name:hello enabled:true items:[]
```
- Supports integers, floats, booleans, strings, empty arrays/objects
- Counter updated to use `instance_vars value:0` (no manual init needed)
- Tests: `tests/test_instance_var_defaults.bash` (7 tests)

### Inheritance of instance_vars (2024-12-13)
Child classes now automatically inherit instance variables from parent classes:
```bash
# Vehicle declares: instance_vars wheels:4 brand
# Car inherits wheels/brand and adds: instance_vars color:red engine
car=$(@ Car new)
@ $car getWheels  # 4 (inherited with default)
@ $car setBrand "Toyota"  # inherited setter works
```
- Multi-level inheritance supported (tested 3 levels)
- Child can override parent defaults
- Inherited vars included in `_vars` array
- Tests: `tests/test_instance_var_inheritance.bash` (6 tests)

### Object Persistence Methods (2024-12-13)
Added persistence methods to Object base class, inherited by all subclasses:
- **Class methods**: `findAll`, `find 'predicate'`, `count`
- **Instance methods**: `save`, `delete`, `class`, `id`, `asJson`, `exists`

```bash
@ Counter findAll              # All Counter instances
@ Counter find 'value > 5'     # Query with predicate
@ Counter count                # Count instances
@ $counter delete              # Remove from DB
@ $counter asJson              # Get raw JSON
```

### Trash Class Updated (2024-12-13)
- Converted `findAll`, `find`, `countInstances` to use SQLite via Store
- Now delegates to Object methods: `@ Trash findAll Counter` → `@ Counter findAll`
- Both API styles work interchangeably

### Bug Fixes (2024-12-13)
- Fixed `wrapped_readlink` breaking `file_defines_function` on macOS
- Fixed argument quoting in `@` and `send` functions (predicates with spaces now work)
- Improved method dispatch to correctly handle generated accessors vs inherited methods

---

*Last updated: 2024-12-14* (DSL compiler, namespace pollution fix, instance method dispatch fix)
