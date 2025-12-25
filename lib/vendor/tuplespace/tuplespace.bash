#!/usr/bin/env bash
# Simple Tuplespace Implementation for Bash
# Uses SQLite for structured storage (atomic operations, no race conditions)

# Source dependencies (use prefixed vars to avoid colliding with SCRIPT_DIR)
_TUPLESPACE_DIR="$(dirname "${BASH_SOURCE[0]}")"
_TUPLESPACE_PARENT="$(dirname "$_TUPLESPACE_DIR")"
if [[ -f "$_TUPLESPACE_PARENT/fun.sh" ]]; then
    source "$_TUPLESPACE_PARENT/fun.sh"
fi

# Tuplespace configuration
TUPLESPACE_DIR="${HOME}/.tuplespace"
TUPLESPACE_DB="${TUPLESPACE_DIR}/tuples.db"

# Initialize tuplespace
ts_init() {
    mkdir -p "$TUPLESPACE_DIR"
    sqlite3 "$TUPLESPACE_DB" "CREATE TABLE IF NOT EXISTS tuples (
        id TEXT PRIMARY KEY,
        type TEXT NOT NULL,
        data JSON NOT NULL,
        created_at INTEGER DEFAULT (strftime('%s', 'now'))
    )"
    sqlite3 "$TUPLESPACE_DB" "CREATE INDEX IF NOT EXISTS idx_type ON tuples(type)"
}

# Put a tuple into the space
# Usage: ts_put <type> <key1> <value1> [key2] [value2] ...
ts_put() {
    ts_init
    local tuple_type="$1"
    shift

    # Use time + PID + random to ensure uniqueness
    local tuple_id="${tuple_type}_$(date +%s)_$$_$RANDOM"

    # Build JSON from key-value pairs
    local json="{"
    local first=true
    while [[ $# -ge 2 ]]; do
        $first || json+=","
        # Escape quotes in values
        local escaped_value="${2//\"/\\\"}"
        json+="\"$1\":\"$escaped_value\""
        first=false
        shift 2
    done
    json+="}"

    sqlite3 "$TUPLESPACE_DB" "INSERT INTO tuples (id, type, data) VALUES ('$tuple_id', '$tuple_type', json('$json'))"
    echo "$tuple_id"
}

# Get tuples matching criteria (non-destructive read)
# Usage: ts_get <type> [key] [value]
ts_get() {
    ts_init
    local tuple_type="$1"
    local key="$2"
    local value="$3"

    if [[ -n "$key" && -n "$value" ]]; then
        sqlite3 -json "$TUPLESPACE_DB" "SELECT id, type, data FROM tuples
            WHERE type = '$tuple_type'
            AND json_extract(data, '$.$key') = '$value'"
    else
        sqlite3 -json "$TUPLESPACE_DB" "SELECT id, type, data FROM tuples WHERE type = '$tuple_type'"
    fi
}

# Remove and return tuples matching criteria (atomic take)
# Usage: ts_take <type> [key] [value]
ts_take() {
    ts_init
    local tuple_type="$1"
    local key="$2"
    local value="$3"

    # Atomic select + delete using a transaction
    if [[ -n "$key" && -n "$value" ]]; then
        sqlite3 -json "$TUPLESPACE_DB" "
            BEGIN IMMEDIATE;
            CREATE TEMP TABLE taken AS
                SELECT id, type, data FROM tuples
                WHERE type = '$tuple_type'
                AND json_extract(data, '$.$key') = '$value'
                LIMIT 1;
            DELETE FROM tuples WHERE id IN (SELECT id FROM taken);
            SELECT * FROM taken;
            DROP TABLE taken;
            COMMIT;
        "
    else
        sqlite3 -json "$TUPLESPACE_DB" "
            BEGIN IMMEDIATE;
            CREATE TEMP TABLE taken AS
                SELECT id, type, data FROM tuples
                WHERE type = '$tuple_type'
                LIMIT 1;
            DELETE FROM tuples WHERE id IN (SELECT id FROM taken);
            SELECT * FROM taken;
            DROP TABLE taken;
            COMMIT;
        "
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
        if [[ -n "$result" && "$result" != "[]" ]]; then
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

# List all tuples
ts_list() {
    ts_init
    local count=$(sqlite3 "$TUPLESPACE_DB" "SELECT COUNT(*) FROM tuples")
    if [[ "$count" -gt 0 ]]; then
        sqlite3 -json "$TUPLESPACE_DB" "SELECT id, type, data, datetime(created_at, 'unixepoch') as created FROM tuples ORDER BY created_at"
    else
        echo "No tuples in space"
    fi
}

# Clear all tuples
ts_clear() {
    ts_init
    sqlite3 "$TUPLESPACE_DB" "DELETE FROM tuples"
    echo "Tuplespace cleared"
}

# Count tuples by type
# Usage: ts_count [type]
ts_count() {
    ts_init
    if [[ -n "$1" ]]; then
        sqlite3 "$TUPLESPACE_DB" "SELECT COUNT(*) FROM tuples WHERE type = '$1'"
    else
        sqlite3 "$TUPLESPACE_DB" "SELECT COUNT(*) FROM tuples"
    fi
}

# Helper: Extract field from JSON tuple result
# Usage: ts_field <json_result> <field_name>
ts_field() {
    local json_result="$1"
    local field_name="$2"

    if [[ "$json_result" == "-" ]]; then
        # Read from stdin - data is a JSON string, need to parse it
        jq -r ".[0].data | fromjson | .$field_name // empty" 2>/dev/null
    else
        # data is a JSON string, need to parse it
        echo "$json_result" | jq -r ".[0].data | fromjson | .$field_name // empty" 2>/dev/null
    fi
}

# Convenience function: put simple key-value tuple
# Usage: ts_kv_put <key> <value>
# Note: This is an upsert - removes existing key before putting new value
ts_kv_put() {
    # Remove existing entry with same key (if any)
    ts_take "kv" "key" "$1" >/dev/null 2>&1 || true
    ts_put "kv" "key" "$1" "value" "$2"
}

# Convenience function: get value by key
# Usage: ts_kv_get <key>
ts_kv_get() {
    local result=$(ts_get "kv" "key" "$1")
    # data is returned as a JSON string, need to parse it
    echo "$result" | jq -r '.[0].data | fromjson | .value // empty' 2>/dev/null
}

# Convenience function: put event tuple
# Usage: ts_event_put <event_name> [data]
ts_event_put() {
    local event_name="$1"
    local data="${2:-}"
    ts_put "event" "name" "$event_name" "data" "$data"
}

# Get tuple by ID
# Usage: ts_get_by_id <id>
ts_get_by_id() {
    ts_init
    local tuple_id="$1"
    sqlite3 -json "$TUPLESPACE_DB" "SELECT id, type, data FROM tuples WHERE id = '$tuple_id'"
}

# Delete tuple by ID
# Usage: ts_delete <id>
ts_delete() {
    ts_init
    local tuple_id="$1"
    sqlite3 "$TUPLESPACE_DB" "DELETE FROM tuples WHERE id = '$tuple_id'"
}

# Example usage and tests
ts_demo() {
    echo "=== Tuplespace Demo (SQLite Backend) ==="

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

    echo -e "\nTaking a person from NYC (atomic remove):"
    ts_take "person" "city" "NYC"

    echo -e "\nPeople remaining:"
    ts_get "person"
}
