# Tuplespace API

Tuplespace provides Linda-style coordination primitives for inter-process communication. Tuples are stored in a shared space where processes can put, get, and take data.

## Concepts

**Tuple**: A typed record with key-value fields. Each tuple has:
- `Type`: Category of the tuple (e.g., "process", "message", "event")
- `ID`: Auto-generated unique identifier
- `Timestamp`: When the tuple was created
- Custom fields as key-value pairs

**Put**: Add a tuple to the space (non-blocking)

**Get**: Read tuples matching criteria without removing them (non-blocking)

**Take**: Read and remove tuples matching criteria (non-blocking)

**Wait**: Block until a matching tuple exists

## Basic Operations

### Put a Tuple

```bash
# Put a tuple with type and key-value pairs
@ Tuplespace put "person" "name" "Alice" "age" "30" "city" "NYC"

# Returns the tuple ID
# => person_1734567890.123_12345
```

### Get Tuples (Read)

```bash
# Get all tuples of a type
@ Tuplespace get "person"

# Get tuples matching a key-value
@ Tuplespace get "person" "city" "NYC"

# Using explicit method
@ Tuplespace get_key_value "person" "name" "Alice"
```

Output is in recutils format:
```
Type: person
ID: person_1734567890.123_12345
Timestamp: 1734567890.123
name: Alice
age: 30
city: NYC
```

### Take Tuples (Read + Remove)

```bash
# Take removes matching tuples from the space
@ Tuplespace take "person" "name" "Alice"

# Using explicit method
@ Tuplespace take_key_value "person" "id" "person_123"
```

### Wait for Tuples (Blocking)

```bash
# Block until a matching tuple exists
@ Tuplespace wait "response" "request_id" "req_123"

# With timeout (seconds)
@ Tuplespace wait_key_value_timeout "response" "request_id" "req_123" 10
```

## Extracting Fields

Tuple records are multi-line text. Extract specific fields with `field_name`:

```bash
# Get a tuple
tuple=$(@ Tuplespace get_key_value "person" "name" "Alice")

# Extract a field
name=$(echo "$tuple" | @ Tuplespace field_name - "name")
age=$(echo "$tuple" | @ Tuplespace field_name - "age")

echo "$name is $age years old"
# => Alice is 30 years old
```

## Key-Value Convenience

For simple key-value storage:

```bash
# Store a value
@ Tuplespace putKV "config.debug" "true"

# Retrieve it
debug=$(@ Tuplespace getKV "config.debug")
```

## Events

Events are tuples with a name and optional data:

```bash
# Put an event
@ Tuplespace putEvent "user_login" "alice@example.com"
@ Tuplespace putEvent_data "process_started" "proc_123"

# Events are just tuples of type "event"
@ Tuplespace get "event" "name" "user_login"
```

## Utility Commands

```bash
# List all tuples
@ Tuplespace list

# Count tuples
@ Tuplespace count              # All tuples
@ Tuplespace count "person"     # By type

# Clear all tuples
@ Tuplespace clear

# System info
@ Tuplespace info

# Initialize (usually automatic)
@ Tuplespace init
```

## Common Patterns

### Request/Response

```bash
# Sender: put a request and wait for response
request_id="req_$(uuidgen)"
@ Tuplespace put "request" "id" "$request_id" "action" "fetch" "url" "http://example.com"

# Wait for response
response=$(@ Tuplespace wait_key_value_timeout "response" "request_id" "$request_id" 30)
result=$(echo "$response" | @ Tuplespace field_name - "result")
```

```bash
# Receiver: take requests and put responses
while true; do
    request=$(@ Tuplespace take "request")
    if [[ -n "$request" ]]; then
        req_id=$(echo "$request" | @ Tuplespace field_name - "id")
        action=$(echo "$request" | @ Tuplespace field_name - "action")

        # Process and respond
        result="done"
        @ Tuplespace put "response" "request_id" "$req_id" "result" "$result"
    fi
    sleep 0.1
done
```

### Producer/Consumer

```bash
# Producer: put work items
for i in {1..10}; do
    @ Tuplespace put "task" "id" "task_$i" "data" "process item $i"
done
```

```bash
# Consumer: take and process work items
while true; do
    task=$(@ Tuplespace take "task")
    if [[ -n "$task" ]]; then
        data=$(echo "$task" | @ Tuplespace field_name - "data")
        echo "Processing: $data"
    fi
    sleep 0.1
done
```

### Pub/Sub with Events

```bash
# Publisher
@ Tuplespace putEvent_data "temperature_reading" "72.5"
```

```bash
# Subscriber (polling)
while true; do
    event=$(@ Tuplespace take "event" "name" "temperature_reading")
    if [[ -n "$event" ]]; then
        temp=$(echo "$event" | @ Tuplespace field_name - "data")
        echo "Temperature: $temp"
    fi
    sleep 1
done
```

## Storage Details

Tuplespace uses recutils for storage:
- Database: `~/.tuplespace/tuples.rec`
- Events directory: `~/.tuplespace/events/`
- Requires `recsel` and `recdel` commands (install via `brew install recutils`)

## Integration with Actor

The Actor class uses Tuplespace internally for:
- Actor registration (`type: "actor"`)
- Message passing (`type: "message"`)
- Response handling (`type: "response"`)
- Lifecycle events (`type: "event"`)

See `@ Actor help` for actor management commands.

## Quick Reference

| Operation | Command |
|-----------|---------|
| Put tuple | `@ Tuplespace put <type> <k1> <v1> ...` |
| Get tuple | `@ Tuplespace get <type> [key] [value]` |
| Take tuple | `@ Tuplespace take <type> [key] [value]` |
| Wait for tuple | `@ Tuplespace wait <type> [key] [value] [timeout]` |
| Extract field | `echo "$tuple" \| @ Tuplespace field_name - <field>` |
| Put KV | `@ Tuplespace putKV <key> <value>` |
| Get KV | `@ Tuplespace getKV <key>` |
| Put event | `@ Tuplespace putEvent_data <name> <data>` |
| List all | `@ Tuplespace list` |
| Count | `@ Tuplespace count [type]` |
| Clear | `@ Tuplespace clear` |
| Help | `@ Tuplespace help` |
