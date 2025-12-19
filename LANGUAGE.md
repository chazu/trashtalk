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
- Traits are mixed in with `include:`

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

In `method:` blocks, you can also use `_ivar` and `_ivar_set`:

```smalltalk
value := $(_ivar count)
_ivar_set count "$newValue"
```

### Class Instance Variables

Shared across all instances of a class:

```smalltalk
Counter subclass: Object
  classInstanceVars: totalCreated:0

  classMethod: new [
    totalCreated := totalCreated + 1   # Auto-inferred in methods
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

## Instance Persistence

Instances are automatically persisted to SQLite:

```smalltalk
counter := $(@ Counter new)          # Create and persist
@ $counter increment                 # Modify (auto-saved)
@ $counter delete                    # Remove from database

@ Counter findAll                    # All instance IDs
@ Counter count                      # Number of instances
@ Counter find 'value > 5'           # Query with predicate
```

## Complete Example

```smalltalk
# Task.trash - A simple task tracker
Task subclass: Object
  include: Debuggable
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

## Development Workflow

```bash
@ Trash new MyClass               # Create new class skeleton and open in $EDITOR
@ Trash edit Counter              # Edit existing class, auto-recompile on save
@ Trash compileAndReload Counter  # Manual compile (safe - won't break on errors)
@ Trash methodsFor Counter        # List methods
@ $instance inspect               # Show instance details
```
