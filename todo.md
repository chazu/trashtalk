# Trashtalk Roadmap

## High Priority

### Instance Variable Defaults
Allow default values in instance_vars declaration:
```bash
instance_vars count:0 step:1 name:""
```
Would simplify `new()` methods significantly.

### Update Trash Class
The Trash object has `findAll`, `find`, `countInstances` that currently scan kv-bash. Should use Store/sqlite-json now. Dogfooding the new persistence layer.

### Edit Message on Object
Add an `edit` method to Object that opens the class definition in `$EDITOR`:
```bash
@ Counter edit        # Opens trash/Counter in $EDITOR
@ $instance edit      # Opens the class file for this instance's type
```

## Medium Priority

### Inheritance of instance_vars
If `Counter is_a ValueHolder` and ValueHolder declares `instance_vars value`, Counter should inherit those variables. Currently each class must redeclare everything.

### Migration Script
- Clear out old kv-bash instance data (`~/.kv-bash/counter_*`, etc.)
- Or migrate existing instances to SQLite
- Keep kv-bash only for stack frames

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

## Ideas to Explore

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

---

*Last updated: 2024-12-13*
