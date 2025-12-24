# Bash Tuplespace

A simple tuplespace implementation for bash that enables event-driven scripting and coordination between shell scripts.

## What is a Tuplespace?

A tuplespace is a shared memory space where processes can:
- **Put** tuples (structured data) into the space
- **Get** tuples from the space (read without removing)
- **Take** tuples from the space (read and remove)
- **Wait** for tuples matching certain criteria

This enables decoupled, event-driven communication between scripts.

## Features

- **File-based storage** using recutils for structured data
- **Event system** with listeners using `entr` for file watching
- **Key-value convenience functions** for simple use cases
- **Pattern matching** queries on tuple fields
- **Timeout support** for waiting operations
- **Process coordination** for task queues and workflows

## Dependencies

- `recutils` (for structured tuple storage) - already in your Brewfile
- `entr` (for file watching) - already in your Brewfile
- `jq` (for JSON processing) - already in your Brewfile
- `sqlite3` (for listener process tracking via SQLite)
- Your existing `fun.sh` utilities

## Quick Start

```bash
# Source the tuplespace functions
source bash/tuplespace/tuplespace.bash

# Initialize (creates ~/.tuplespace/)
ts_init

# Put a tuple
ts_put "person" "name" "Alice" "age" "30" "city" "NYC"

# Get tuples
ts_get "person" "city" "NYC"

# Take (remove) tuples
ts_take "person" "name" "Alice"

# Key-value convenience
ts_kv_put "config" "debug=true"
ts_kv_get "config"

# Events
ts_event_put "user_login" "alice@example.com"

# Wait for tuples (with timeout)
ts_wait "event" "name" "user_login" 10
```

## Core Functions

### Basic Operations
- `ts_put <type> <key1> <value1> [key2] [value2] ...` - Put tuple
- `ts_get <type> [key] [value]` - Get matching tuples
- `ts_take <type> [key] [value]` - Take (remove) matching tuples
- `ts_wait <type> [key] [value] [timeout]` - Wait for matching tuple

### Convenience Functions
- `ts_kv_put <key> <value>` - Put key-value pair
- `ts_kv_get <key>` - Get value by key
- `ts_event_put <event_name> [data]` - Put event tuple

### Event System
- `ts_listen <type> <command>` - Listen for tuples of type
- `ts_stop_listener <type> <process_id>` - Stop listener

### Utility Functions
- `ts_list` - List all tuples
- `ts_clear` - Clear all tuples
- `ts_count [type]` - Count tuples
- `ts_field <tuple_record> <field_name>` - Extract field from tuple

## Examples

### 1. Simple Task Queue

Producer:
```bash
# Add tasks to queue
ts_put "task" "id" "1" "command" "backup_database" "status" "pending"
ts_put "task" "id" "2" "command" "send_emails" "status" "pending"
```

Worker:
```bash
# Process tasks
while true; do
    task=$(ts_take "task" "status" "pending" | head -1)
    if [[ -n "$task" ]]; then
        task_id=$(echo "$task" | ts_field - "id")
        command=$(echo "$task" | ts_field - "command")
        echo "Processing task $task_id: $command"
        eval "$command"
        ts_put "task" "id" "$task_id" "status" "completed"
    fi
    sleep 1
done
```

### 2. Event-Driven File Processing

File watcher:
```bash
# Watch for file changes and put events
find /path/to/watch -type f | entr -r bash -c "
    ts_event_put 'file_changed' \"\$(date)\"
"
```

Event handler:
```bash
# Listen for file change events
ts_listen "event" '
    event_data=$(ts_get "event" "ID" "$1")
    event_name=$(echo "$event_data" | ts_field - "name")
    if [[ "$event_name" == "file_changed" ]]; then
        echo "Processing files..."
        # Your processing logic here
    fi
'
```

### 3. Configuration Management

```bash
# Put configuration
ts_kv_put "app.debug" "true"
ts_kv_put "app.timeout" "30"

# Listen for config changes
ts_listen "kv" '
    tuple_data=$(ts_get "kv" "ID" "$1")
    key=$(echo "$tuple_data" | ts_field - "key")
    if [[ "$key" == app.* ]]; then
        echo "App config changed: $key"
        # Restart services, reload config, etc.
    fi
'
```

## Testing

Run the test suite:
```bash
bash bash/tuplespace/test-tuplespace.bash
```

## File Structure

- `~/.tuplespace/tuples.rec` - Main tuple database (recutils format)
- `~/.tuplespace/events/` - Event trigger files
- `~/.tuplespace/listeners/` - Listener scripts
- `~/.trashtalk/instances.db` - Listener process tracking (via SQLite)

## Integration with Your Existing Tools

The tuplespace integrates well with your existing bash utilities:
- Uses SQLite for process tracking (via `kv_set`/`kv_get`/`kv_del`)
- Compatible with your functional programming utilities in `fun.sh`
- Leverages `recutils` and `entr` from your Brewfile
- Follows your existing bash scripting patterns

## Advanced Usage

See `bash/tuplespace/tuplespace-examples.bash` for more complex examples including:
- System monitoring with metric collection
- Alert handling based on thresholds
- Multi-worker task processing
- Configuration change propagation
