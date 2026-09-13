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

### Declared Primitives

A method whose whole body would be one call to a Bash function is declared, not
written. The compiler generates the stub and passes the arguments positionally:

```smalltalk
classPrimitive: enqueue: queue payload: data calls: honker_enqueue
classPrimitive: bootstrap calls: honker_bootstrap
primitive: whoami calls: bridge_receiver          # instance side; sees $_RECEIVER
```

The declaration names the shell boundary instead of hiding it in a raw body,
and browsers list it like any other method. Anything that reorders, transforms,
or adds arguments stays a `rawMethod:`.

### Pragmas

Pragmas are method-level directives that modify how a method is compiled or executed. Place them at the start of the method body.

#### `pragma: stream`

A method's stdout is its value, so a send made for its effect in a non-tail
statement discards its output. Methods that intentionally print several lines
(help text, listings, reports) declare `pragma: stream` to keep every
statement's output. See [Statement Values](#statement-values).

```smalltalk
method: help [
  pragma: stream
  @ Console print: 'Usage:'.
  @ Console print: '  @ Counter increment'
]
```

#### `pragma: direct`

By default, method calls run in a subshell (via `$(...)` capture), which isolates side effects. The `pragma: direct` directive bypasses this subshell, allowing methods to modify variables in the calling shell.

```smalltalk
rawMethod: setGlobalCounter [
  pragma: direct
  GLOBAL_COUNTER="modified"
]
```

**How it works:**
1. The compiler emits a marker variable: `declare -g __ClassName__methodName__direct=1`
2. At runtime, the `@` dispatcher checks for this marker
3. If present, calls the method directly instead of capturing in a subshell

**Use cases:**
- Setting shell variables that must persist after the call
- Performance-critical methods where subshell overhead matters
- Methods that coordinate with the calling environment

**Example - Normal method cannot modify caller's variable:**

```smalltalk
rawMethod: normalSet [
  MY_VAR="modified_by_normal"
]
```

```bash
MY_VAR="original"
@ MyClass normalSet
echo "$MY_VAR"  # Still "original" - method ran in subshell
```

**Example - Direct method CAN modify caller's variable:**

```smalltalk
rawMethod: directSet [
  pragma: direct
  MY_VAR="modified_by_direct"
]
```

```bash
MY_VAR="original"
@ MyClass directSet
echo "$MY_VAR"  # Now "modified_by_direct"
```

**Works with keyword methods:**

```smalltalk
rawMethod: setValue: val [
  pragma: direct
  MY_VAR="$val"
]
```

```bash
@ MyClass setValue: "new_value"
echo "$MY_VAR"  # "new_value"
```

**Gotchas:**

1. **Output capture negates the benefit**: If you capture the output with `$(...)`, Bash creates a subshell anyway:
   ```bash
   # This still runs in a subshell due to $() capture
   result=$(@ MyClass directSet)
   echo "$MY_VAR"  # Still "original"
   ```

2. **Mixed class behavior**: You can mix direct and normal methods in the same class. Each method is independent:
   ```smalltalk
   rawMethod: directMethod [
     pragma: direct
     DIRECT_VAR="set"
   ]
   rawMethod: normalMethod [
     NORMAL_VAR="set"  # Will NOT persist
   ]
   ```

3. **Works with namespaced classes**: The marker uses the full qualified name:
   ```smalltalk
   package: MyApp

   Service subclass: Object
     rawMethod: configure [
       pragma: direct
       # Marker: __MyApp__Service__configure__direct=1
     ]
   ```

4. **Only for `rawMethod:`**: Use with `rawMethod:` since you're working at the Bash level. Regular `method:` blocks apply transformations that may interfere.

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

### Method Categories

Organize methods into logical groups (no runtime effect, documentation only):

```smalltalk
category: 'initialization'
  method: setup [
    @ self reset
  ]

category: 'accessing'
  method: getValue [
    ^ value
  ]
```

### Method Aliases

Create alternate names for existing methods:

```smalltalk
alias: size for: count
alias: length for: count
```

### Protocol Requirements

Declare method dependencies (documentation/validation):

```smalltalk
requires: 'lib/database.bash'    # File dependency
requires: do:                     # Required method selector
requires: inject: into:           # Required keyword method
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

Each cascaded message is compiled to a separate `@` call with the same receiver.

### Statement Values

The value of a method is the output of its last statement, or of `^`. A send
in any other statement position is made for its effect, and its output is
discarded (compiled as `>/dev/null`, no capture). This holds inside inline
control flow too: the last statement of a block in tail position is the
method's value, everything else is an effect.

```smalltalk
method: requeue: delivery [
  @ delivery reload.                     # effect: output dropped
  @ delivery transitionTo: 'pending'.    # effect: output dropped
  ^ 'pending'                            # value
]

classMethod: create [
  @ Counter new                          # tail statement: its output is the value
]
```

Blocks passed to ordinary methods (`do:`, `collect:`) keep every statement's
output, since a block's value is its output. Use `pragma: stream` for methods
that print several lines on purpose.

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

Declared at class level with optional defaults:

```smalltalk
instanceVars: count:0 name
```

Auto-generated getters/setters:

```smalltalk
@ self getCount            # Getter
@ self setCount: 5         # Setter
```

**Automatic inference**: In regular `method:` blocks, instance variable names are automatically detected and wrapped with `$(_ivar varname)` when used in expressions:

```smalltalk
method: increment [
  value := value + step    # Compiles to: value=$(( $(_ivar value) + $(_ivar step) ))
]
```

In `rawMethod:` blocks, use explicit access:

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

### Conditionals

```smalltalk
# Basic conditionals
(x > 5) ifTrue: [@ self doSomething].
(x > 5) ifFalse: [@ self doOther].
(x > 5) ifTrue: [a := 1] ifFalse: [a := 0].

# Nil checking
value ifNil: [value := 'default'].
value ifNil: [^ 'missing'] ifNotNil: [^ value].
value ifNotNil: [@ self process: value].
```

### Loops

```smalltalk
# Repeat N times
5 timesRepeat: [@ self tick].

# While loops
[running] whileTrue: [@ self process].
[count < max] whileFalse: [@ self wait].

# Range iteration
1 to: 10 do: [:i |
  @ Console print: i
].

# Newline-separated values (Store finders return one id per line)
(@ AgentDelivery pendingFor: self) linesDo: [:id |
  @ id reload.
  (@ id state) = 'blocked' ifTrue: [^ id]   # ^ returns from the method
].
```

`linesDo:` decodes the lines once into a Bash array and runs the block body in
the method's own shell: no Block object, no subshell, empty lines skipped, and
stdin untouched.

### Literal Dispatch

`caseOf:` compares against string, symbol, or number literals and compiles to
a Bash `case`. An array literal groups several keys; `otherwise:` is optional.

```smalltalk
^ profile caseOf: {
  'shell' -> [ 'ShellDriver' ].
  'maki' -> [ 'MakiDriver' ].
  #('codex' 'assistant-low-power' '') -> [ 'CodexDriver' ]
} otherwise: [ @ WorkerError signal: 'Unknown backend profile ' , profile ]
```

In return position an arm's final value expression is the method's result.
Keys must be literals; a variable key is a compile error.

### Boolean Operators

```smalltalk
# Combine conditions
(x > 0) and: [y > 0] ifTrue: [@ self valid].
(x > 100) or: [y > 100] ifTrue: [@ self overflow].

# Negation
(x > 10) not ifTrue: [@ self small].

# Boolean expressions are values too: they return or assign "true"/"false"
^ (prefix = 'session') or: [prefix = 'agent']
```

## Operators

### Comparison Operators

Trashtalk distinguishes between string and numeric comparisons:

| Operator | Type | Description |
|----------|------|-------------|
| `=` | String | String equality (`[[ "$a" == "$b" ]]`) |
| `~=` | String | String inequality (`[[ "$a" != "$b" ]]`) |
| `==` | Numeric | Numeric equality (`(( a == b ))`) |
| `!=` | Numeric | Numeric inequality (`(( a != b ))`) |
| `>` | Numeric | Greater than |
| `<` | Numeric | Less than |
| `>=` | Numeric | Greater or equal |
| `<=` | Numeric | Less or equal |
| `=~` | Regex | Regex match (`[[ "$str" =~ pattern ]]`) |

```smalltalk
# String comparison
(name = 'Alice') ifTrue: [@ self greet].
(status ~= 'active') ifTrue: [@ self activate].

# Numeric comparison
(count > 0) ifTrue: [@ self process].
(x >= min) and: [x <= max] ifTrue: [@ self inRange].

# Regex matching
(email =~ '^[^@]+@[^@]+$') ifTrue: [@ self validEmail].
(str matches: 'pattern') ifTrue: [@ self matched].
```

### Arithmetic Operators

```smalltalk
a + b
a - b
a * b
a / b
a % b    # Modulo
-a       # Unary minus
```

Precedence: `*`, `/`, `%` bind tighter than `+`, `-`.

### Test Predicates

File and string testing predicates:

```smalltalk
# File tests
(path fileExists) ifTrue: [@ self load].
(path isFile) ifTrue: [@ self readFile].
(path isDirectory) ifTrue: [@ self listDir].
(path isReadable) ifTrue: [@ self open].
(path isWritable) ifTrue: [@ self save].
(path isExecutable) ifTrue: [@ self run].
(path isFifo) ifTrue: [@ self readPipe].
(path isSymlink) ifTrue: [@ self resolve].

# String tests
(str isEmpty) ifTrue: [str := 'default'].
(str notEmpty) ifTrue: [@ self process].
```

### String Intrinsics

String messages on an implicit receiver compile to Bash parameter expansion or
a `[[ ]]` test. They never dispatch, and a local receiver never forks.
Arguments are literal: a `*` in the data is a character, not a pattern.

| Message | Result | Compiles to |
|---------|--------|-------------|
| `s startsWith: p` / `s endsWith: p` / `s includes: p` | `true`/`false` | `[[ "$s" == "$p"* ]]` |
| `s withoutPrefix: p` / `s withoutSuffix: p` | text | `${s#"$p"}` / `${s%"$p"}` |
| `s upTo: d` / `s after: d` | before or after the first `d` | `${s%%"$d"*}` / `${s#*"$d"}` |
| `s upToLast: d` / `s afterLast: d` | before or after the last `d` | `${s%"$d"*}` / `${s##*"$d"}` |
| `s replaceAll: a with: b` / `s replaceFirst: a with: b` | text | `${s//"$a"/"$b"}` |
| `s copyFrom: 2 to: 4` | 1-based inclusive substring | `${s:1:3}` |
| `s first: n` / `s last: n` | leading or trailing characters | `${s:0:n}` / `${s: -n}` |
| `s size` | length, usable in arithmetic | `${#s}` |
| `s asUppercase` / `s asLowercase` | text | `${s^^}` / `${s,,}` |
| `s firstLine` | text up to the first newline | `${s%%$'\n'*}` |
| `s trimmed` | surrounding whitespace removed | one capture, no dispatch |
| `s lines` | JSON array of non-empty lines | one `jq` process |

```smalltalk
base := path afterLast: '/'.
(base startsWith: '.') ifTrue: [^ 'hidden'].
(base size > 64) ifTrue: [@ NameError signal: 'name too long'].
^ base withoutSuffix: '.trash'
```

A message the compiler does not recognize on an implicit receiver is a compile
error. Object messages always use `@ receiver selector`.

## Blocks (Closures)

For passing code to methods like `do:`, `collect:`, `select:`:

```smalltalk
[:x | x + 1]                # Block with one parameter
[:x :y | x + y]             # Block with two parameters
[@ self doSomething]        # Block with no parameters
```

### Block Evaluation

```smalltalk
@ block value                    # No args
@ block valueWith: arg           # One arg
@ block valueWith: a and: b      # Two args
@ block numArgs                  # Number of parameters
```

### Usage with Collections

```smalltalk
@ myArray do: [:each | @ Console print: each]
@ myArray collect: [:x | x * 2]
@ myArray select: [:x | x > 5]
@ myArray inject: 0 into: [:sum :x | sum + x]
```

## String Literals

### Single-Quoted Strings

```smalltalk
'simple string'
```

### Double-Quoted Strings (Interpolated)

```smalltalk
"Value is $(@ self getValue)"
"Count: $count"
```

### Triple-Quoted Strings (Multi-line)

```smalltalk
text := '''This is a
multi-line string
with preserved newlines'''

# Can also be used in instance variable defaults
instanceVars: template:'''default
multi-line
value'''
```

## Collection Literals

```smalltalk
#symbol                     # Symbol (becomes a string)
#(1 2 3)                    # Array literal
#{key: value}               # Dictionary literal
```

## Array Class

Create and manipulate arrays:

```smalltalk
arr := $(@ Array new)
@ $arr push: 'item'
@ $arr at: 0 put: 'first'
item := $(@ $arr at: 0)
size := $(@ $arr size)
last := $(@ $arr pop)
```

### Array Methods

| Method | Description |
|--------|-------------|
| `new` | Create empty array |
| `at:` | Get element at index |
| `at:put:` | Set element at index |
| `push:` | Add to end |
| `pop` | Remove and return last |
| `size` | Get length |
| `show` | Print contents |
| `withValues:` | Initialize with values |
| `do:` | Iterate with block |
| `collect:` | Map with block |
| `select:` | Filter with block |
| `inject:into:` | Reduce with block |

## Dictionary Class

Create and manipulate key-value pairs:

```smalltalk
dict := $(@ Dictionary new)
@ $dict at: 'name' put: 'Alice'
name := $(@ $dict at: 'name')
@ $dict removeAt: 'name'
```

### Dictionary Methods

| Method | Description |
|--------|-------------|
| `new` | Create empty dictionary |
| `at:` | Get value for key |
| `at:put:` | Set value for key |
| `at:ifAbsent:` | Get with default |
| `includesKey:` | Check if key exists |
| `removeAt:` | Remove key-value pair |
| `keys` | Get all keys |
| `values` | Get all values |
| `size` | Number of entries |
| `isEmpty` | Check if empty |
| `clear` | Remove all entries |
| `merge:` | Merge another dictionary |
| `do:` | Iterate with [:key :value] |
| `keysDo:` | Iterate keys only |
| `valuesDo:` | Iterate values only |
| `collect:` | Map values with block |
| `select:` | Filter with block |
| `asJson` | Export as JSON |
| `fromJson:` | Create from JSON (class method) |

## Persistence

Ordinary `new` immediately saves an initial SQLite record, even for classes
without `Persistable`. Later field changes update the session cache. The
`Persistable` trait adds explicit saves, reloads, deletion, and Store queries.

```bash
counter=$(@ Counter new)             # initial defaults already saved
@ "$counter" increment              # update the live cached object
@ "$counter" save                   # persist that change
@ "$counter" reload                 # discard cached changes and load Store data
@ Counter findAll                   # stored instance IDs
@ Counter find: 'value > 5'          # query stored state
@ "$counter" delete                 # remove from Store and this session cache
```

`create` calls `new` and `save`; it is a convenience, not a separate persistence
mode. `Runtime delete:` removes only the cached copy. See
[object persistence](docs/persistence.md) for freshness, deletion, and
`Store transaction:` semantics.

## Error Handling

### Raising and Handling in DSL Methods

A method fails by signalling an error. Any class-like name ending in `Error`
works as the type; `@ self error:` raises a plain `Error`. The method sets the
runtime error state and returns 1, so callers see a failed send.

```smalltalk
method: requeue: delivery [
  ((@ delivery state) ~= 'failed') ifTrue: [
    @ SessionError signal: 'Delivery ' , delivery , ' is not failed'
  ].
  (@ delivery transitionTo: 'pending') ifFailed: [:e | @ e signal ].   # re-raise
  ^ 'pending'
]
```

`ifFailed:` guards one send and runs its block when the send fails. It works
on a statement, an assignment, or a return, and inlines to `if ! { ...; }`:

```smalltalk
(@ store save) ifFailed: [ ^ 'unsaved' ].
run := @ AgentRun current ifFailed: [ @ RunTokenError signal: 'no run token' ].
^ (@ self compute) ifFailed: [:e | ^ 'fallback (' , e , ')' ]
```

The block's optional parameter receives `Type: message`, including errors
raised inside a captured send. `@ e signal` re-raises it unchanged. The error
state is cleared before the block runs, and `try: [...] catch: [:e | ...]`
follows the same rule. Raising from raw code or the REPL: `@ Error signal: 'msg'`.

### Runtime Primitives

Raw methods use the runtime directly:

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

## Test Framework

Trashtalk includes a TestCase class for writing tests:

```smalltalk
MyTests subclass: TestCase

  method: testAddition [
    | result |
    result := 2 + 2
    @ self assert: result equals: 4
  ]

  method: testGreater [
    @ self assert: 10 greaterThan: 5
  ]

  method: testString [
    @ self assert: 'hello world' contains: 'world'
  ]
```

Run tests with:
```smalltalk
@ MyTests runAll
```

### Assertion Methods

| Method | Description |
|--------|-------------|
| `assert:equals:` | Check equality |
| `assert:notEquals:` | Check inequality |
| `assertTrue:` | Check truthy |
| `assertFalse:` | Check falsy |
| `assertNil:` | Check nil/empty |
| `assertNotNil:` | Check not nil |
| `assert:contains:` | Check substring |
| `assert:matches:` | Check regex match |
| `assert:greaterThan:` | Numeric comparison |
| `assert:lessThan:` | Numeric comparison |
| `assert:greaterOrEqual:` | Numeric comparison |
| `assert:lessOrEqual:` | Numeric comparison |
| `skip` | Skip current test |
| `pending` | Mark test as pending |

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

## Return Values

The `^` operator returns a value from a method:

```smalltalk
method: getValue [
  ^ value
]
```

This compiles to:
```bash
echo "$value"; return
```

The returned value is captured via command substitution:
```smalltalk
result := $(@ self getValue)
```

## REPL Features

In the REPL, the `$__` variable stores the result of the last `@` command:

```bash
@ Counter new
@ $__ increment    # Uses result of previous command
@ $__ getValue
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
- The `$__` variable holds the result of the last `@` command (REPL only)
- Instance variables are automatically inferred in regular methods

## Limitations and regression coverage

- **Non-local returns in custom methods:** `^` works inside compiler-recognized
  inline control flow and supported collection iteration. A block passed to an
  arbitrary custom method returns from its own evaluation, not its enclosing
  method. See `test_block_early_return.bash` and `test_blocks.bash`.
- **Failure propagation:** a failed `@` send does not automatically return from
  the method. Guard it with `ifFailed:` or raise with `@ SomeError signal:` at
  effect boundaries. A Store transaction rejects commit after any failed send,
  even if later code succeeds.
- **Keyword sends and trailing keywords:** `ifTrue:`, `ifFailed:`, `linesDo:`,
  and `caseOf:` after a keyword send need parentheses:
  `(@ d transitionTo: 'x') ifFailed: [...]`. A unary send does not.
- **`caseOf:` keys** are literals only; **`linesDo:`** skips empty lines.
- **Arithmetic:** Bash arithmetic operates on integers; JSON numbers do not add
  floating point arithmetic to DSL expressions.

Unary `skip` and keyword `skip:` are distinct selectors; negative arguments and
qualified names such as `@ Pkg::Class method` in raw bodies are supported.
`test_known_issues.bash`, `test_namespaces.bash`, and `test_expr_codegen.bash`
exercise these formerly broken cases through the production compiler.
Raw methods preserve standalone Bash sequence words such as `{1..50}`,
`{-2..2..2}`, and `{a..e..2}`. `test_raw_ranges.bash` executes compiled loops
to verify their iteration counts; `test_agent_conversation.bash` checks direct
input while a worker lock is temporarily held.
See [compiler capabilities](docs/COMPILER_CAPABILITIES.md) for the test map.

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
