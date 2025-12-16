#!/usr/bin/env bash
# Simple Tuplespace Implementation for Bash
# Uses recutils for structured storage and file watching for events

# Source dependencies (use prefixed vars to avoid colliding with SCRIPT_DIR)
_TUPLESPACE_DIR="$(dirname "${BASH_SOURCE[0]}")"
_TUPLESPACE_PARENT="$(dirname "$_TUPLESPACE_DIR")"
if [[ -f "$_TUPLESPACE_PARENT/kv-bash" ]]; then
    source "$_TUPLESPACE_PARENT/kv-bash"
fi
if [[ -f "$_TUPLESPACE_PARENT/fun.sh" ]]; then
    source "$_TUPLESPACE_PARENT/fun.sh"
fi

# Tuplespace configuration
TUPLESPACE_DIR="${HOME}/.tuplespace"
TUPLESPACE_DB="${TUPLESPACE_DIR}/tuples.rec"
TUPLESPACE_EVENTS_DIR="${TUPLESPACE_DIR}/events"
TUPLESPACE_LISTENERS_DIR="${TUPLESPACE_DIR}/listeners"

# Initialize tuplespace
ts_init() {
    mkdir -p "$TUPLESPACE_DIR" "$TUPLESPACE_EVENTS_DIR" "$TUPLESPACE_LISTENERS_DIR"
    touch "$TUPLESPACE_DB"
}

# Put a tuple into the space
# Usage: ts_put <type> <key1> <value1> [key2] [value2] ...
ts_put() {
    ts_init
    local tuple_type="$1"
    shift
    
    local timestamp=$(date +%s.%N)
    local tuple_id="${tuple_type}_${timestamp}_$$"
    
    # Build recutils record
    {
        echo "Type: $tuple_type"
        echo "ID: $tuple_id"
        echo "Timestamp: $timestamp"
        
        # Add key-value pairs
        while [[ $# -ge 2 ]]; do
            echo "$1: $2"
            shift 2
        done
        echo ""  # Empty line to separate records
    } >> "$TUPLESPACE_DB"
    
    # Trigger event
    ts_trigger_event "$tuple_type" "$tuple_id"
    echo "$tuple_id"
}

# Get tuples matching criteria
# Usage: ts_get <type> [key] [value]
ts_get() {
    ts_init
    local tuple_type="$1"
    local key="$2"
    local value="$3"

    if [[ -n "$key" && -n "$value" ]]; then
        recsel -e "Type = '$tuple_type' && $key = '$value'" "$TUPLESPACE_DB"
    else
        recsel -e "Type = '$tuple_type'" "$TUPLESPACE_DB"
    fi
}

# Remove tuples matching criteria
# Usage: ts_take <type> [key] [value]
ts_take() {
    ts_init
    local tuple_type="$1"
    local key="$2"
    local value="$3"

    # Get matching tuples first
    local matches
    if [[ -n "$key" && -n "$value" ]]; then
        matches=$(recsel -e "Type = '$tuple_type' && $key = '$value'" "$TUPLESPACE_DB")
    else
        matches=$(recsel -e "Type = '$tuple_type'" "$TUPLESPACE_DB")
    fi

    if [[ -n "$matches" ]]; then
        # Remove from database
        local temp_db=$(mktemp)
        if [[ -n "$key" && -n "$value" ]]; then
            recdel -e "Type = '$tuple_type' && $key = '$value'" "$TUPLESPACE_DB" > "$temp_db"
        else
            recdel -e "Type = '$tuple_type'" "$TUPLESPACE_DB" > "$temp_db"
        fi
        mv "$temp_db" "$TUPLESPACE_DB"

        echo "$matches"
    fi
}

# Wait for a tuple matching criteria
# Usage: ts_wait <type> [key] [value] [timeout_seconds]
ts_wait() {
    local tuple_type="$1"
    local key="$2"
    local value="$3"
    local timeout="${4:-0}"
    
    local start_time=$(date +%s)
    
    while true; do
        local result=$(ts_get "$tuple_type" "$key" "$value")
        if [[ -n "$result" ]]; then
            echo "$result"
            return 0
        fi
        
        # Check timeout
        if [[ $timeout -gt 0 ]]; then
            local current_time=$(date +%s)
            if [[ $((current_time - start_time)) -ge $timeout ]]; then
                return 1
            fi
        fi
        
        sleep 0.1
    done
}

# Trigger event for listeners
ts_trigger_event() {
    local event_type="$1"
    local tuple_id="$2"
    
    local event_file="${TUPLESPACE_EVENTS_DIR}/${event_type}_$(date +%s.%N)"
    echo "$tuple_id" > "$event_file"
}

# Register a listener for events
# Usage: ts_listen <type> <command>
ts_listen() {
    local tuple_type="$1"
    local command="$2"
    
    ts_init
    
    # Create listener script
    local listener_script="${TUPLESPACE_LISTENERS_DIR}/listener_${tuple_type}_$$"
    cat > "$listener_script" << EOF
#!/bin/bash
# Auto-generated listener for type: $tuple_type
while read -r event_file; do
    if [[ "\$(basename "\$event_file")" == ${tuple_type}_* ]]; then
        tuple_id=\$(cat "\$event_file")
        $command "\$tuple_id"
        rm -f "\$event_file"
    fi
done
EOF
    chmod +x "$listener_script"
    
    # Start watching for events
    echo "$TUPLESPACE_EVENTS_DIR" | entr -r "$listener_script" &
    local listener_pid=$!
    
    # Store listener info
    kvset "listener_${tuple_type}_$$" "$listener_pid"
    
    echo "Listener started with PID: $listener_pid"
    echo "To stop: ts_stop_listener $tuple_type $$"
}

# Stop a listener
# Usage: ts_stop_listener <type> <process_id>
ts_stop_listener() {
    local tuple_type="$1"
    local process_id="$2"

    local listener_pid=$(kvget "listener_${tuple_type}_${process_id}")
    if [[ -n "$listener_pid" ]]; then
        kill "$listener_pid" 2>/dev/null
        kvdel "listener_${tuple_type}_${process_id}"
        rm -f "${TUPLESPACE_LISTENERS_DIR}/listener_${tuple_type}_${process_id}"
        echo "Listener stopped"
    else
        echo "No listener found for type: $tuple_type, process: $process_id"
    fi
}

# List all tuples
ts_list() {
    ts_init
    if [[ -s "$TUPLESPACE_DB" ]]; then
        recsel "$TUPLESPACE_DB"
    else
        echo "No tuples in space"
    fi
}

# Clear all tuples
ts_clear() {
    ts_init
    > "$TUPLESPACE_DB"
    rm -f "${TUPLESPACE_EVENTS_DIR}"/*
    echo "Tuplespace cleared"
}

# Count tuples by type
# Usage: ts_count [type]
ts_count() {
    ts_init
    if [[ -n "$1" ]]; then
        local result=$(recsel -e "Type = '$1'" "$TUPLESPACE_DB" 2>/dev/null | grep -c "^Type:" 2>/dev/null)
        echo "${result:-0}"
    else
        local result=$(grep -c "^Type:" "$TUPLESPACE_DB" 2>/dev/null)
        echo "${result:-0}"
    fi
}

# Helper: Extract field from tuple
# Usage: ts_field <tuple_record> <field_name>
# Or: echo "<tuple_record>" | ts_field - <field_name>
ts_field() {
    local tuple_record="$1"
    local field_name="$2"

    if [[ "$tuple_record" == "-" ]]; then
        # Read from stdin
        grep "^$field_name:" | head -1 | sed "s/^$field_name: //"
    else
        # Use provided tuple record
        echo "$tuple_record" | grep "^$field_name:" | head -1 | sed "s/^$field_name: //"
    fi
}

# Convenience function: put simple key-value tuple
# Usage: ts_kv_put <key> <value>
ts_kv_put() {
    ts_put "kv" "key" "$1" "value" "$2"
}

# Convenience function: get value by key
# Usage: ts_kv_get <key>
ts_kv_get() {
    ts_get "kv" "key" "$1" | ts_field - "value"
}

# Convenience function: put event tuple
# Usage: ts_event_put <event_name> [data]
ts_event_put() {
    local event_name="$1"
    local data="${2:-}"
    ts_put "event" "name" "$event_name" "data" "$data"
}

# Example usage and tests
ts_demo() {
    echo "=== Tuplespace Demo ==="

    # Clear space
    ts_clear

    # Put some tuples
    echo "Putting tuples..."
    ts_put "person" "name" "Alice" "age" "30" "city" "NYC"
    ts_put "person" "name" "Bob" "age" "25" "city" "SF"
    ts_kv_put "config" "debug=true"
    ts_event_put "user_login" "alice@example.com"

    echo -e "\nAll tuples:"
    ts_list

    echo -e "\nPeople in NYC:"
    ts_get "person" "city" "NYC"

    echo -e "\nConfig value:"
    ts_kv_get "config"

    echo -e "\nTuple counts:"
    echo "Total: $(ts_count)"
    echo "People: $(ts_count person)"
    echo "Events: $(ts_count event)"
}
