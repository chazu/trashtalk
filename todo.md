# Trashtalk Roadmap

## Medium Priority

### Keyword Message Syntax
Support Smalltalk-style keyword messages:
```bash
@ [myCounter at: 3 put: "foo"]
@ [myArray from: 1 to: 5]
@ [store find: "Counter" where: "value > 5"]
```
Would require extending trash-parser.bash to handle colon-delimited keywords.

### More Smalltalk-like Object Definitions
Explore making class definitions less bash-like:
```smalltalk
Counter subclass: Object [
    | count step |

    increment [
        count := count + step
    ]
]
```
Could be a DSL that compiles to bash, or a custom parser.

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

*Last updated: 2024-12-13* (Edit method, instance var defaults, instance var inheritance)
