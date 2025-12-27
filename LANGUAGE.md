# Trashtalk Language Reference

Trashtalk is a Smalltalk-inspired DSL that compiles to Bash. Classes are defined in `.trash` files and compiled to namespaced Bash functions.

## Class Definition

```smalltalk
Counter subclass: Object
  include: Debuggable           # Optional: include traits
  instanceVars: value:0 step:1  # Instance variables with optional defaults

  # Methods go here...
```

- Classes inherit from a superclass (use `Object` as base)
- Instance variables are declared with `instanceVars:`, optionally with default values (`name:default`)
- String defaults use quotes: `instanceVars: name:'unknown' status:'pending'`
- Traits are mixed in with `include:`

## Namespaces (Packages)

Classes can be organized into namespaces:

```smalltalk
package: Tools

Netcat subclass: Tool
  # ...
```

Reference namespaced classes with `::`:

```smalltalk
@ Tools::Netcat listen: 8080
@ Tools::Curl get: "https://example.com"
```

## Methods

### Regular Methods

Transformed by the compiler (local vars, assignment, message sends):

```smalltalk
method: increment [
  | current newVal |              # Declare local variables
  current := $(@ self getValue)   # Assignment with :=
  newVal := $((current + 1))
  @ self setValue: $newVal
  ^ $newVal                       # Return with ^
]

method: at: index put: value [    # Keyword method with arguments
  @ self doSomethingWith: $index and: $value
]
```

### Raw Methods

No transformation - use when you need full Bash control (heredocs, traps, complex loops):

```smalltalk
rawMethod: complexOperation [
  local x y
  for item in "${array[@]}"; do
    echo "$item"
  done
]
```

### Class Methods

Called on the class itself, not instances:

```smalltalk
classMethod: description [
  ^ "A counter that counts"
]

rawClassMethod: new [
  local id=$(_generate_instance_id Counter)
  _create_instance Counter "$id"
  echo "$id"
]
```

## Traits

Traits are reusable method collections. Define a trait:

```smalltalk
Debuggable trait

  method: inspect [
    @ Console print: "Instance: $_RECEIVER"
  ]

  method: log: message [
    echo "[$(date)] $message"
  ]
```

Include traits in classes:

```smalltalk
Counter subclass: Object
  include: Debuggable
  include: Persistable
```

Built-in traits:
- `Debuggable` - inspection and debugging methods
- `Persistable` - database persistence (save, delete, findAll, etc.)

## Message Sends

The `@` operator sends messages to objects:

```smalltalk
@ self increment                    # Unary message (no args)
@ self setValue: 42                 # Keyword message (one arg)
@ self at: 0 put: "hello"           # Keyword message (two args)
@ Counter new                       # Class message
@ $myCounter getValue               # Message to instance in variable
```

### Cascades

Send multiple messages to the same receiver with `;`:

```smalltalk
@ self reset; increment; increment.   # Three messages to self
```

## Variables

### Local Variables

Declare with `| var1 var2 |` at method start:

```smalltalk
method: example [
  | x y result |
  x := 10
  y := 20
  result := $((x + y))
]
```

### Instance Variables

Auto-generated getters/setters:

```smalltalk
instanceVars: count:0 name

# In methods:
@ self getCount            # Getter
@ self setCount: 5         # Setter
```

In `rawMethod:` blocks, use `_ivar` and `_ivar_set`:

```smalltalk
value := $(_ivar count)
_ivar_set count "$newValue"
```

### Object References

Store references to other objects in instance variables:

```smalltalk
# Store a reference
_ivar_set_ref owner "$person_id"    # Validates reference exists

# Retrieve and use
owner=$(_ivar_ref owner)            # Get the reference
@ $owner getName                    # Send message

# Convenience: send directly
result=$(_ivar_send owner getName)  # Same as above

# Check validity
if _ivar_ref_valid owner; then
  class=$(_ivar_ref_class owner)    # Get class of referenced object
fi
```

### Class Instance Variables

Shared across all instances of a class:

```smalltalk
Counter subclass: Object
  classInstanceVars: totalCreated:0

  classMethod: new [
    totalCreated := totalCreated + 1
    ...
  ]
```

## Control Flow

Control flow uses blocks `[...]` and is inlined at compile time:

```smalltalk
# Conditionals
(x > 5) ifTrue: [@ self doSomething].
(x > 5) ifFalse: [@ self doOther].
(x > 5) ifTrue: [a := 1] ifFalse: [a := 0].

# Loops
5 timesRepeat: [@ self tick].
[running] whileTrue: [@ self process].
```

## Blocks (Closures)

For passing code to methods like `do:`, `collect:`, `select:`:

```smalltalk
[:x | x + 1]                # Block with one parameter
[:x :y | x + y]             # Block with two parameters
[@ self doSomething]        # Block with no parameters

# Usage with Array:
@ myArray do: [:each | @ Console print: each]
@ myArray collect: [:x | x * 2]
@ myArray select: [:x | x > 5]
@ myArray inject: 0 into: [:sum :x | sum + x]
```

## Collection Literals

```smalltalk
#symbol                     # Symbol (becomes a string)
#(1 2 3)                    # Array literal
#{key: value}               # Dictionary literal
```

## Persistence

Classes that include the `Persistable` trait can be saved to SQLite:

```smalltalk
Counter subclass: Object
  include: Persistable
  instanceVars: value:0
```

Usage:

```smalltalk
counter := $(@ Counter new)          # Create in memory
@ $counter increment
@ $counter save                      # Persist to database

# Or create and persist in one step:
counter := $(@ Counter create)

@ $counter delete                    # Remove from database

# Queries
@ Counter findAll                    # All instance IDs
@ Counter count                      # Number of instances
@ Counter find 'value > 5'           # Query with predicate
```

## Error Handling

Trashtalk provides structured error handling:

```smalltalk
# Throw an error
_throw "ErrorType" "Error message"

# Handle errors
_on_error "ErrorType" "handler_function"
# ... code that might throw ...
_pop_handler

# Catch-all handler
_on_error "*" "my_catch_all"

# Cleanup that runs on frame exit
_ensure "cleanup_command"
```

Example:

```smalltalk
rawMethod: safeOperation [
  _on_error "NetworkError" "@ self handleNetworkError"
  @ self riskyNetworkCall
  _pop_handler
]

rawMethod: handleNetworkError [
  echo "Network failed, using fallback"
  @ self useFallback
]
```

## Futures (Async)

Run computations in the background:

```smalltalk
# Create a future
future := $(@ Future for: '@ self expensiveCalculation')

# Start it
@ $future start

# Do other work...

# Get result (blocks until done)
result := $(@ $future await)

# Check without blocking
@ $future poll          # Returns: pending/completed/failed
@ $future isDone        # Returns 0 if done, 1 if pending

# Cancel
@ $future cancel

# Cleanup
@ $future cleanup
```

## Method Advice (AOP)

Add behavior before/after methods without modifying them:

```smalltalk
# Run before a method
_add_before_advice "Counter" "increment" "log_call"

# Run after a method
_add_after_advice "Counter" "increment" "log_result"

# Remove advice
_remove_advice "Counter" "increment"
```

## Source Introspection

Access embedded source code and hashes:

```smalltalk
# Get source code of a class
@ Trash sourceFor: Counter

# Get SHA-256 hash of source
@ Trash hashFor: Counter
```

## Complete Example

```smalltalk
# Task.trash - A simple task tracker
Task subclass: Object
  include: Persistable
  instanceVars: title done:0 priority:1

  rawClassMethod: new [
    local id=$(_generate_instance_id Task)
    _create_instance Task "$id"
    echo "$id"
  ]

  classMethod: titled: t [
    | task |
    task := $(@ Task new)
    @ $task setTitle: "$t"
    ^ $task
  ]

  method: complete [
    @ self setDone: 1
    ^ self
  ]

  method: isComplete [
    | d |
    d := $(@ self getDone)
    (d = 1) ifTrue: [^ "yes"] ifFalse: [^ "no"]
  ]

  method: describe [
    | t p |
    t := $(@ self getTitle)
    p := $(@ self getPriority)
    ^ "[$p] $t"
  ]
```

Usage:

```bash
source lib/trash.bash
task=$(@ Task titled "Write docs")
@ $task describe            # => [1] Write docs
@ $task complete
@ $task save                # Persist to database
@ $task isComplete          # => yes
@ Task findAll              # => task_abc123
```

## Tips

- Use `rawMethod:` when you need heredocs, traps, or complex Bash constructs
- Use `method:` for most code - it handles variable inference and message transformation
- End statements with `.` when followed by another statement
- Use `^` to return values from methods
- `@ self` refers to the current receiver
- The `$__` variable holds the result of the last `@` command

## Known Issues

- **Method name collision**: Keyword methods (e.g., `skip:`) and unary methods with the same base name (e.g., `skip`) compile to the same function. Avoid this pattern.
- **Negative numbers in arguments**: Arguments like `0 -1` may be mangled. Use variables instead of negative literals in method calls.

## Development Workflow

```bash
@ Trash new MyClass               # Create new class skeleton and open in $EDITOR
@ Trash edit Counter              # Edit existing class, auto-recompile on save
@ Trash compileAndReload Counter  # Manual compile (safe - won't break on errors)
@ Trash methodsFor Counter        # List methods
@ Trash sourceFor Counter         # View embedded source
@ Trash hashFor Counter           # View source hash
@ $instance inspect               # Show instance details
```
