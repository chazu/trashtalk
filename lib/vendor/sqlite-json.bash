#!/usr/bin/env bash
# sqlite-json.bash
# SQLite-based JSON document store with virtual column indexing
#
# Author: Trashtalk project
# Version: 0.1
# Requirements: sqlite3
#
# USAGE:
#    source ./sqlite-json.bash    # import functions
#    db_init                       # initialize database
#    db_put <id> <json>            # store document
#    db_get <id>                   # retrieve document
#    db_delete <id>                # delete document
#    db_find_by_class <class>      # find all instances of class
#    db_query <where_clause>       # general query
#    db_ensure_virtual_column <name> <json_path>
#    db_create_index <column>
#    db_list_indices

########################
# CONSTANTS
########################

SQLITE_JSON_DB="${SQLITE_JSON_DB:-$HOME/.trashtalk/instances.db}"

########################
# LOCAL FUNCTIONS
########################

# print to stderr, red color
_db_echo_err() {
    echo -e "\e[01;31m$@\e[0m" >&2
}

_db_echo_err_box() {
    _db_echo_err "  +-------------------------------+"
    _db_echo_err "  | ERROR: $1"
    _db_echo_err "  | function: $2"
    _db_echo_err "  +-------------------------------+"
}

# Validate ID format (alphanumeric, dots, underscores, hyphens, colons)
# This prevents SQL injection in ID fields
# Usage: _db_validate_id <id>
_db_validate_id() {
    [[ "$1" =~ ^[0-9a-zA-Z._:-]+$ ]]
}

# Validate column/index names (alphanumeric and underscores only)
# Usage: _db_validate_name <name>
_db_validate_name() {
    [[ "$1" =~ ^[0-9a-zA-Z_]+$ ]]
}

# Escape single quotes for SQL string literals
# Usage: escaped=$(_db_escape "$string")
_db_escape() {
    local str="$1"
    echo "${str//\'/\'\'}"
}

# Run SQL command
_db_sql() {
    sqlite3 "$SQLITE_JSON_DB" "$@"
}

# Run SQL and return JSON results
_db_sql_json() {
    sqlite3 -json "$SQLITE_JSON_DB" "$@"
}

########################
# ENSURE THIS-FILE IS CALLED BY 'source'
########################

[[ "${BASH_SOURCE[0]}" != "${0}" ]] || {
    _db_echo_err "  +------------------------------------------------+"
    _db_echo_err "  | FATAL ERROR: wrong usage                       |"
    _db_echo_err "  | You should use this via source                 |"
    _db_echo_err "  |     $ source ./sqlite-json.bash                |"
    _db_echo_err "  +------------------------------------------------+"
    exit 1
}

########################
# PUBLIC FUNCTIONS
########################

# Initialize database with instances table
# Creates virtual column and index for 'class' field
# Usage: db_init
db_init() {
    local db_dir
    db_dir="$(dirname "$SQLITE_JSON_DB")"

    # Ensure directory exists
    [[ -d "$db_dir" ]] || mkdir -p "$db_dir"

    # Create table with class virtual column
    _db_sql <<'EOF'
CREATE TABLE IF NOT EXISTS instances (
    id TEXT PRIMARY KEY,
    data JSON NOT NULL
);
EOF

    # Add class virtual column if it doesn't exist
    db_ensure_virtual_column "class" '$.class' 2>/dev/null || true

    # Add created_at virtual column if it doesn't exist
    db_ensure_virtual_column "created_at" '$.created_at' 2>/dev/null || true

    # Ensure class index exists
    db_create_index "class" 2>/dev/null || true
}

# Store or update a document
# Usage: db_put <id> <json_data>
db_put() {
    local id="$1"
    local data="$2"

    [[ -n "$id" ]] || {
        _db_echo_err_box 'missing param "id"' 'db_put()'
        return 1
    }

    _db_validate_id "$id" || {
        _db_echo_err_box 'invalid id format' 'db_put()'
        return 1
    }

    [[ -n "$data" ]] || {
        _db_echo_err_box 'missing param "data"' 'db_put()'
        return 1
    }

    # Escape single quotes in JSON data for SQL string literal
    # json() function then validates and rejects malformed JSON
    local escaped_data
    escaped_data=$(_db_escape "$data")
    _db_sql "INSERT OR REPLACE INTO instances (id, data) VALUES ('$id', json('$escaped_data'));"
}

# Retrieve a document by id
# Usage: db_get <id>
# Returns: JSON document or empty string
db_get() {
    local id="$1"

    [[ -n "$id" ]] || {
        _db_echo_err_box 'missing param "id"' 'db_get()'
        return 1
    }

    _db_validate_id "$id" || {
        _db_echo_err_box 'invalid id format' 'db_get()'
        return 1
    }

    local result
    result=$(_db_sql "SELECT data FROM instances WHERE id = '$id';")
    echo "$result"
    [[ -n "$result" ]]
}

# Delete a document by id
# Usage: db_delete <id>
db_delete() {
    local id="$1"

    [[ -n "$id" ]] || {
        _db_echo_err_box 'missing param "id"' 'db_delete()'
        return 1
    }

    _db_validate_id "$id" || {
        _db_echo_err_box 'invalid id format' 'db_delete()'
        return 1
    }

    _db_sql "DELETE FROM instances WHERE id = '$id';"
}

# Find all instances of a given class
# Usage: db_find_by_class <class_name>
# Returns: newline-separated list of instance IDs
db_find_by_class() {
    local class_name="$1"

    [[ -n "$class_name" ]] || {
        _db_echo_err_box 'missing param "class_name"' 'db_find_by_class()'
        return 1
    }

    class_name=$(_db_escape "$class_name")
    _db_sql "SELECT id FROM instances WHERE class = '$class_name';"
}

# General query with WHERE clause
# Usage: db_query <where_clause>
# Returns: newline-separated list of instance IDs
# Example: db_query "class = 'Counter' AND json_extract(data, '$.value') > 5"
# WARNING: where_clause is passed directly to SQL. Use only with trusted input
#          or sanitize via _db_escape for any user-provided string values.
db_query() {
    local where_clause="$1"

    [[ -n "$where_clause" ]] || {
        _db_echo_err_box 'missing param "where_clause"' 'db_query()'
        return 1
    }

    _db_sql "SELECT id FROM instances WHERE $where_clause;"
}

# Query returning full documents
# Usage: db_query_data <where_clause>
# Returns: JSON array of documents
# WARNING: where_clause is passed directly to SQL. Use only with trusted input
#          or sanitize via _db_escape for any user-provided string values.
db_query_data() {
    local where_clause="$1"

    [[ -n "$where_clause" ]] || {
        _db_echo_err_box 'missing param "where_clause"' 'db_query_data()'
        return 1
    }

    _db_sql "SELECT json_group_array(json(data)) FROM instances WHERE $where_clause;"
}

# Add a virtual column extracted from JSON
# Usage: db_ensure_virtual_column <column_name> <json_path>
# Example: db_ensure_virtual_column "status" '$.status'
db_ensure_virtual_column() {
    local column_name="$1"
    local json_path="$2"

    [[ -n "$column_name" ]] || {
        _db_echo_err_box 'missing param "column_name"' 'db_ensure_virtual_column()'
        return 1
    }

    _db_validate_name "$column_name" || {
        _db_echo_err_box 'invalid column name format' 'db_ensure_virtual_column()'
        return 1
    }

    [[ -n "$json_path" ]] || {
        _db_echo_err_box 'missing param "json_path"' 'db_ensure_virtual_column()'
        return 1
    }

    # Escape the json_path for safety
    json_path=$(_db_escape "$json_path")
    _db_sql "ALTER TABLE instances ADD COLUMN $column_name TEXT GENERATED ALWAYS AS (json_extract(data, '$json_path')) VIRTUAL;"
}

# Create an index on a column (typically a virtual column)
# Usage: db_create_index <column_name>
db_create_index() {
    local column_name="$1"

    [[ -n "$column_name" ]] || {
        _db_echo_err_box 'missing param "column_name"' 'db_create_index()'
        return 1
    }

    _db_validate_name "$column_name" || {
        _db_echo_err_box 'invalid column name format' 'db_create_index()'
        return 1
    }

    _db_sql "CREATE INDEX IF NOT EXISTS idx_instances_$column_name ON instances($column_name);"
}

# List all indices on the instances table
# Usage: db_list_indices
db_list_indices() {
    _db_sql "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'instances';"
}

# List all columns (including virtual) on instances table
# Usage: db_list_columns
db_list_columns() {
    _db_sql "PRAGMA table_xinfo(instances);" | while IFS='|' read -r cid name type notnull dflt pk hidden; do
        echo "$name"
    done
}

# Get count of instances by class
# Usage: db_count_by_class <class_name>
db_count_by_class() {
    local class_name="$1"

    [[ -n "$class_name" ]] || {
        _db_echo_err_box 'missing param "class_name"' 'db_count_by_class()'
        return 1
    }

    class_name=$(_db_escape "$class_name")
    _db_sql "SELECT COUNT(*) FROM instances WHERE class = '$class_name';"
}

# List all distinct classes in the database
# Usage: db_list_classes
db_list_classes() {
    _db_sql "SELECT DISTINCT class FROM instances WHERE class IS NOT NULL;"
}

# Clear all data (but keep schema)
# Usage: db_clear
db_clear() {
    _db_sql "DELETE FROM instances;"
}

# Drop database completely
# Usage: db_drop
db_drop() {
    [[ -f "$SQLITE_JSON_DB" ]] && rm -f "$SQLITE_JSON_DB"
}

########################
# SIMPLE KEY-VALUE STORE
########################
# These functions provide simple string key-value storage
# using the same SQLite database. Keys are prefixed with __kv__
# to distinguish from instance documents.

# Store a string value by key
# Usage: kv_set <key> <value>
kv_set() {
    local key="$1"
    local value="$2"

    [[ -n "$key" ]] || {
        _db_echo_err_box 'missing param "key"' 'kv_set()'
        return 1
    }

    # Build JSON with proper escaping via jq
    local json_value
    json_value=$(printf '%s' "$value" | jq -Rs '{v: .}')
    db_put "__kv__${key}" "$json_value"
}

# Retrieve a string value by key
# Usage: kv_get <key>
# Returns: value string or empty
kv_get() {
    local key="$1"

    [[ -n "$key" ]] || {
        _db_echo_err_box 'missing param "key"' 'kv_get()'
        return 1
    }

    local result
    result=$(db_get "__kv__${key}" 2>/dev/null)
    if [[ -n "$result" ]]; then
        echo "$result" | jq -r '.v // empty'
        return 0
    fi
    return 1
}

# Delete a key
# Usage: kv_del <key>
kv_del() {
    local key="$1"

    [[ -n "$key" ]] || {
        _db_echo_err_box 'missing param "key"' 'kv_del()'
        return 1
    }

    db_delete "__kv__${key}" 2>/dev/null
}
