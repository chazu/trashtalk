#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source dependencies quietly
source "$SCRIPT_DIR/vendor/bsfl.sh" 2>/dev/null || echo "Warning: bsfl.sh not found"
source "$SCRIPT_DIR/vendor/fun.sh" 2>/dev/null || echo "Warning: fun.sh not found"
source "$SCRIPT_DIR/vendor/kv-bash" 2>/dev/null || echo "Warning: kv-bash not found"
source "$SCRIPT_DIR/vendor/sqlite-json.bash" 2>/dev/null || echo "Warning: sqlite-json.bash not found"

# Export kv-bash functions so they're available in subshells (used for stack frames)
export -f kvset kvget kvdel kv_validate_key 2>/dev/null
export KV_USER_DIR

# Export sqlite-json functions so they're available in subshells
export -f db_init db_put db_get db_delete db_find_by_class db_query db_query_data 2>/dev/null
export -f db_ensure_virtual_column db_create_index db_list_indices db_list_columns 2>/dev/null
export -f db_count_by_class db_list_classes db_clear db_drop 2>/dev/null
export -f _db_validate_id _db_validate_name _db_escape _db_sql 2>/dev/null
export SQLITE_JSON_DB

# Override msg_debug to respect DEBUG mode and output to stderr
# This overrides BSFL's msg_debug which outputs to stdout regardless
msg_debug() {
    [[ "${DEBUG:-no}" == "yes" ]] && [[ "${TRASH_DEBUG:-1}" != "0" ]] && echo "[DEBUG] $*" >&2
}

# Override msg_info to output to stderr (BSFL outputs to stdout)
msg_info() { echo "[INFO] $*" >&2; }

# Set default TRASHDIR if not already set
TRASHDIR=${TRASHDIR:-~/.trashtalk/trash}
_SUPERCLASS=$TRASHDIR/Object
_RECEIVER=Object

# System constants
TRASH_VERSION="0.1.0"
TRASH_AUTHOR="Chaz Straney"
TRASH_DESCRIPTION="Smalltalk-inspired message-passing system for Bash"

# ============================================
# Context Stack System
# ============================================
# Lightweight in-memory stacks for context management.
# Uses indexed arrays with depth counters for Bash 3.2 compatibility.

# Call stack for debugging/introspection (opt-in via TRASH_DEBUG)
declare -a _CALL_STACK=()
_CALL_DEPTH=0

# Ensure stack for cleanup handlers (always active)
declare -a _ENSURE_STACK=()
_ENSURE_DEPTH=0

# Error handler stack for exception handling
declare -a _HANDLER_STACK=()
_HANDLER_DEPTH=0

# Current error state (set by _throw)
_ERROR_TYPE=""
_ERROR_MSG=""
_ERROR_ORIGIN=""

# Advice registries
declare -a _BEFORE_ADVICE=()
declare -a _AFTER_ADVICE=()

# Print stack trace
_print_stack_trace() {
  local i
  echo "Stack trace:" >&2
  for ((i = _CALL_DEPTH - 1; i >= 0; i--)); do
    echo "  at ${_CALL_STACK[i]}" >&2
  done
}

# Throw an error (sets error state and returns 1)
# Usage: _throw "ErrorType" "Error message"
_throw() {
  _ERROR_TYPE="$1"
  _ERROR_MSG="$2"
  _ERROR_ORIGIN="${_CALL_STACK[_CALL_DEPTH - 1]:-unknown}"
  return 1
}

# Clear error state
_clear_error() {
  _ERROR_TYPE=""
  _ERROR_MSG=""
  _ERROR_ORIGIN=""
}

# Register an ensure handler (cleanup that runs on frame exit)
# Usage: _ensure "cleanup_command"
_ensure() {
  _ENSURE_STACK[_ENSURE_DEPTH]="$1"
  ((_ENSURE_DEPTH++))
}

# Register an error handler for the current frame
# Usage: _on_error "ErrorType" "handler_function"
# Use "*" as ErrorType to catch all errors
_on_error() {
  local pattern="$1"
  local handler="$2"
  _HANDLER_STACK[_HANDLER_DEPTH]="$pattern|$handler"
  ((_HANDLER_DEPTH++))
}

# Pop an error handler (call when leaving protected region)
_pop_handler() {
  if ((_HANDLER_DEPTH > 0)); then
    ((_HANDLER_DEPTH--))
  fi
}

# Add before advice
# Usage: _add_before_advice "Class" "selector" "handler_function"
# Use "*" for class or selector to match all
_add_before_advice() {
  local class="$1" selector="$2" handler="$3"
  _BEFORE_ADVICE+=("$class:$selector:$handler")
}

# Add after advice
# Usage: _add_after_advice "Class" "selector" "handler_function"
_add_after_advice() {
  local class="$1" selector="$2" handler="$3"
  _AFTER_ADVICE+=("$class:$selector:$handler")
}

# Remove all advice for a class/selector
_remove_advice() {
  local class="$1" selector="$2"
  local new_before=() new_after=()
  local advice

  for advice in "${_BEFORE_ADVICE[@]}"; do
    IFS=: read -r adv_class adv_sel handler <<< "$advice"
    if [[ "$adv_class" != "$class" || "$adv_sel" != "$selector" ]]; then
      new_before+=("$advice")
    fi
  done
  _BEFORE_ADVICE=("${new_before[@]}")

  for advice in "${_AFTER_ADVICE[@]}"; do
    IFS=: read -r adv_class adv_sel handler <<< "$advice"
    if [[ "$adv_class" != "$class" || "$adv_sel" != "$selector" ]]; then
      new_after+=("$advice")
    fi
  done
  _AFTER_ADVICE=("${new_after[@]}")
}

# Run before advice handlers
_run_before_advice() {
  local class="$1" selector="$2"
  shift 2
  local advice
  for advice in "${_BEFORE_ADVICE[@]}"; do
    IFS=: read -r adv_class adv_sel handler <<< "$advice"
    if [[ ("$adv_class" == "$class" || "$adv_class" == "*") && \
          ("$adv_sel" == "$selector" || "$adv_sel" == "*") ]]; then
      "$handler" "$class" "$selector" "$@"
    fi
  done
}

# Run after advice handlers
_run_after_advice() {
  local class="$1" selector="$2" exit_code="$3"
  shift 3
  local advice
  for advice in "${_AFTER_ADVICE[@]}"; do
    IFS=: read -r adv_class adv_sel handler <<< "$advice"
    if [[ ("$adv_class" == "$class" || "$adv_class" == "*") && \
          ("$adv_sel" == "$selector" || "$adv_sel" == "*") ]]; then
      "$handler" "$class" "$selector" "$exit_code" "$@"
    fi
  done
}

# Export context functions for subshells
export -f _throw _clear_error _ensure _on_error _pop_handler
export -f _print_stack_trace
export -f _add_before_advice _add_after_advice _remove_advice
export -f _run_before_advice _run_after_advice

# ============================================

function wrapped_readlink {
  if command -v greadlink >/dev/null 2>&1; then
    greadlink "$@"
  else
    readlink "$@"
  fi
}

function current_unit {
  $BASH_SOURCE
}

# Return a value by pushing it onto the stack
function ^ {
  _set_return_value "$(jo value="$*")"
}

# Initialize the stack if it doesn't exist
function _ensure_stack {
  if ! kvget _stack >/dev/null 2>&1; then
    kvset _stack "[]"
  fi
}

# Push a new stack frame
function _push_stack_frame {
  local frame_id="frame_$(uuidgen 2>/dev/null || echo "$$_$(date +%s)")"
  local frame_data="$(jo _type="StackFrame" args="$(jo "$@")" return="")"

  _ensure_stack
  kvset "$frame_id" "$frame_data"
  kvset _stack "$(kvget _stack | jq ". + [\"$frame_id\"]")"

  echo "$frame_id"
}

# Pop the current stack frame
function _pop_stack_frame {
  _ensure_stack
  local frame_id=$(_current_stack_frame_id)
  if [[ -n "$frame_id" && "$frame_id" != "null" ]]; then
    local frame_data=$(kvget "$frame_id")
    kvdel "$frame_id"
    kvset _stack "$(kvget _stack | jq '.[0:-1]')"
    echo "$frame_data"
  fi
}

function _current_stack_frame_id {
  kvget _stack | jq -r '.[-1]'
}

function _current_stack_frame {
  kvget `_current_stack_frame_id`
}


# Set the return value of the current stack frame
function _set_return_value {
  local frame_id=$(_current_stack_frame_id)
  if [[ -n "$frame_id" && "$frame_id" != "null" ]]; then
    local current_frame=$(_current_stack_frame)
    local updated_frame=$(echo "$current_frame" | jq ".return = $1")
    kvset "$frame_id" "$updated_frame"
  fi
}

# ============================================
# Instance Variable Declaration
# ============================================

# Stores declared instance vars for current class being defined
_CURRENT_CLASS_VARS=""

# Associative array for instance variable defaults
# Key: var_name, Value: default value (or empty for null)
declare -gA _CURRENT_CLASS_DEFAULTS

# Ensure a class is sourced (for accessing its metadata)
# Usage: _ensure_class_sourced ClassName
function _ensure_class_sourced {
  local class_name="$1"

  # Check if already sourced by looking for superclass metadata
  local super_var="__${class_name}__superclass"
  if [[ -n "${!super_var+x}" ]]; then
    return 0
  fi

  # Try to source the compiled class
  local compiled_file="$TRASHDIR/.compiled/$class_name"
  if [[ -f "$compiled_file" ]]; then
    source "$compiled_file"
    return 0
  fi

  # Try the runtime copy
  local runtime_file="$TRASHDIR/$class_name"
  if [[ -f "$runtime_file" ]]; then
    source "$runtime_file"
    return 0
  fi

  return 1
}
export -f _ensure_class_sourced

# Get instance vars for a class from compiled metadata or legacy file format
# Usage: _get_class_instance_vars ClassName
# Returns: space-separated list of var specs (e.g., "count:0 step:5 name")
function _get_class_instance_vars {
  local class_name="$1"

  # Extract just the class name if a path was passed
  class_name="${class_name##*/}"

  # Ensure class is sourced so metadata is available
  _ensure_class_sourced "$class_name"

  # First check compiled metadata variable (preferred)
  local vars_var="__${class_name}__instanceVars"
  if [[ -n "${!vars_var+x}" ]]; then
    echo "${!vars_var}"
    return 0
  fi

  # Fallback: parse legacy file format
  local class_file="$TRASHDIR/$class_name"
  if [[ ! -f "$class_file" ]]; then
    return 0
  fi

  # Extract instance_vars line and get the arguments
  # Handles: instance_vars foo bar baz
  # Or:      instance_vars count:0 step:5
  grep -E "^instance_vars " "$class_file" 2>/dev/null | sed 's/^instance_vars //' || true
}

# Get the parent class of a given class from compiled metadata or legacy file format
# Usage: _get_parent_class ClassName
# Returns: parent class name or empty if none/Object
function _get_parent_class {
  local class_name="$1"
  local parent=""

  # Extract just the class name if a path was passed
  class_name="${class_name##*/}"

  # Ensure class is sourced so metadata is available
  _ensure_class_sourced "$class_name"

  # First check compiled metadata variable (preferred)
  local super_var="__${class_name}__superclass"
  if [[ -n "${!super_var+x}" ]]; then
    parent="${!super_var}"
  else
    # Fallback: parse legacy file format
    local class_file="$TRASHDIR/$class_name"
    if [[ -f "$class_file" ]]; then
      parent=$(grep -E "^is_a " "$class_file" 2>/dev/null | head -1 | sed 's/^is_a //' || true)
    fi
  fi

  # Don't return Object as a parent to traverse (it's the root)
  if [[ "$parent" != "Object" && -n "$parent" ]]; then
    echo "$parent"
  fi
}

# Collect all instance vars from a class and its ancestors
# Usage: _collect_inherited_vars ClassName
# Sets: _INHERITED_VARS (space-separated list)
#       _INHERITED_DEFAULTS (associative array)
declare -gA _INHERITED_DEFAULTS

function _collect_inherited_vars {
  local class_name="$1"
  local visited=""
  local all_vars=""
  _INHERITED_DEFAULTS=()

  # Walk up the inheritance chain
  local current="$class_name"
  while [[ -n "$current" ]]; do
    # Prevent infinite loops
    if [[ " $visited " == *" $current "* ]]; then
      break
    fi
    visited="$visited $current"

    # Get vars for this class
    local class_vars
    class_vars=$(_get_class_instance_vars "$current")

    # Parse and merge vars (child vars override parent vars)
    for spec in $class_vars; do
      local var default_val

      if [[ "$spec" == *:* ]]; then
        var="${spec%%:*}"
        default_val="${spec#*:}"
      else
        var="$spec"
        default_val=""
      fi

      # Only add if not already defined (child takes precedence)
      if [[ " $all_vars " != *" $var "* ]]; then
        all_vars="$all_vars $var"
        _INHERITED_DEFAULTS["$var"]="$default_val"
      fi
    done

    # Move to parent
    current=$(_get_parent_class "$current")
  done

  # Trim leading space
  _INHERITED_VARS="${all_vars# }"
}

# Declare instance variables for a class
# Usage: instance_vars foo bar baz           (all default to null)
# Usage: instance_vars count:0 step:1 name   (count=0, step=1, name=null)
# Generates: getFoo/setFoo, getBar/setBar, getBaz/setBaz
# Also generates accessors for inherited vars from parent classes
function instance_vars {
  _CURRENT_CLASS_VARS=""
  _CURRENT_CLASS_DEFAULTS=()

  # First, generate accessors for inherited vars from parent class
  # _SUPERCLASS is set by is_a before instance_vars is called
  if [[ -n "$_SUPERCLASS" && "$_SUPERCLASS" != "Object" ]]; then
    _collect_inherited_vars "$_SUPERCLASS"
    for var in $_INHERITED_VARS; do
      _generate_accessor "$var"
    done
  fi

  # Now process this class's declared vars
  for spec in $*; do
    local var default_val

    # Check for default value syntax: var:default
    if [[ "$spec" == *:* ]]; then
      var="${spec%%:*}"
      default_val="${spec#*:}"
    else
      var="$spec"
      default_val=""
    fi

    # Add to var list (space-separated)
    if [[ -z "$_CURRENT_CLASS_VARS" ]]; then
      _CURRENT_CLASS_VARS="$var"
    else
      _CURRENT_CLASS_VARS="$_CURRENT_CLASS_VARS $var"
    fi

    # Store default value
    _CURRENT_CLASS_DEFAULTS["$var"]="$default_val"

    # Generate accessor for this var
    _generate_accessor "$var"
  done
}

# ============================================
# Instance Management Functions
# ============================================

# Generate accessor methods (getter/setter) for a variable
# Usage: _generate_accessor <var_name> [class_name]
# If class_name provided, generates namespaced functions (__ClassName__getFoo)
# Otherwise generates plain functions (getFoo) - legacy mode
function _generate_accessor {
  local var="$1"
  local class_name="${2:-}"

  # Capitalize first letter for method names
  local capitalized
  capitalized="$(echo "${var:0:1}" | tr '[:lower:]' '[:upper:]')${var:1}"

  # Determine function name prefix
  local prefix=""
  if [[ -n "$class_name" ]]; then
    prefix="__${class_name}__"
  fi

  # Generate getter: getFoo() or __ClassName__getFoo()
  eval "${prefix}get${capitalized}() {
    local data=\$(db_get \"\$_RECEIVER\")
    [[ -n \"\$data\" ]] && echo \"\$data\" | jq -r \".$var // empty\"
  }"

  # Generate setter: setFoo() or __ClassName__setFoo()
  eval "${prefix}set${capitalized}() {
    local value=\"\$1\"
    local data=\$(db_get \"\$_RECEIVER\")

    if [[ -z \"\$data\" ]]; then
      echo \"Error: Instance \$_RECEIVER not found\" >&2
      return 1
    fi

    # Validate field is declared
    local vars=\$(echo \"\$data\" | jq -r '._vars // [] | .[]')
    if ! echo \"\$vars\" | grep -qx \"$var\"; then
      echo \"Error: Field '$var' not declared for this instance\" >&2
      return 1
    fi

    # Update the field (handle numeric, JSON, or string)
    local updated
    if [[ \"\$value\" =~ ^-?[0-9]+\$ ]]; then
      # Numeric value
      updated=\$(echo \"\$data\" | jq -c \".$var = \$value\")
    elif echo \"\$value\" | jq -e . >/dev/null 2>&1; then
      # Valid JSON - use as raw JSON value
      updated=\$(echo \"\$data\" | jq -c --argjson v \"\$value\" \".$var = \\\$v\")
    else
      # String value - use --arg for safe escaping
      updated=\$(echo \"\$data\" | jq -c --arg v \"\$value\" \".$var = \\\$v\")
    fi

    db_put \"\$_RECEIVER\" \"\$updated\"
  }"
}

# Create an instance with declared instance variables
# Usage: _create_instance <class_name> <instance_id>
function _create_instance {
  local class_name="$1"
  local instance_id="$2"
  local created_at
  created_at=$(date +%s)

  # Get instance vars from compiled class metadata (preferred)
  # This avoids using stale global _CURRENT_CLASS_VARS from previous classes
  local class_vars=""
  local vars_var="__${class_name}__instanceVars"
  if [[ -n "${!vars_var}" ]]; then
    class_vars="${!vars_var}"
  fi

  # Parse class vars into vars list and defaults
  local class_var_list=""
  declare -A class_var_defaults
  for spec in $class_vars; do
    local var default_val
    if [[ "$spec" == *:* ]]; then
      var="${spec%%:*}"
      default_val="${spec#*:}"
    else
      var="$spec"
      default_val=""
    fi
    class_var_list="$class_var_list $var"
    class_var_defaults["$var"]="$default_val"
    _generate_accessor "$var" "$class_name"
  done
  class_var_list="${class_var_list# }"

  # Collect inherited vars from parent classes
  _collect_inherited_vars "$class_name"

  # Merge: start with inherited vars, then add/override with class vars
  local all_vars=""
  declare -A all_defaults

  # First add inherited vars
  for var in $_INHERITED_VARS; do
    if [[ -z "$all_vars" ]]; then
      all_vars="$var"
    else
      all_vars="$all_vars $var"
    fi
    all_defaults["$var"]="${_INHERITED_DEFAULTS[$var]}"
    _generate_accessor "$var" "$class_name"
  done

  # Then add/override with current class vars
  for var in $class_var_list; do
    if [[ " $all_vars " != *" $var "* ]]; then
      if [[ -z "$all_vars" ]]; then
        all_vars="$var"
      else
        all_vars="$all_vars $var"
      fi
    fi
    all_defaults["$var"]="${class_var_defaults[$var]}"
  done

  # Build vars array from merged vars
  local vars_json="[]"
  if [[ -n "$all_vars" ]]; then
    vars_json=$(printf '%s\n' $all_vars | jq -R . | jq -s .)
  fi

  # Create initial JSON with class, created_at, _vars
  local data
  data=$(jq -n \
    --arg class "$class_name" \
    --arg created "$created_at" \
    --argjson vars "$vars_json" \
    '{class: $class, created_at: $created, _vars: $vars}')

  # Add value for each variable (using default or null)
  for var in $all_vars; do
    local default_val="${all_defaults[$var]}"

    if [[ -z "$default_val" ]]; then
      # No default - use null
      data=$(echo "$data" | jq -c ". + {\"$var\": null}")
    elif [[ "$default_val" =~ ^-?[0-9]+$ ]]; then
      # Numeric default
      data=$(echo "$data" | jq -c ". + {\"$var\": $default_val}")
    elif [[ "$default_val" =~ ^-?[0-9]+\.[0-9]+$ ]]; then
      # Float default
      data=$(echo "$data" | jq -c ". + {\"$var\": $default_val}")
    elif [[ "$default_val" == "true" || "$default_val" == "false" ]]; then
      # Boolean default
      data=$(echo "$data" | jq -c ". + {\"$var\": $default_val}")
    elif [[ "$default_val" == "[]" || "$default_val" == "{}" ]]; then
      # Empty array or object
      data=$(echo "$data" | jq -c ". + {\"$var\": $default_val}")
    else
      # String default (use --arg for safe escaping)
      data=$(echo "$data" | jq -c --arg v "$default_val" ". + {\"$var\": \$v}")
    fi
  done

  db_put "$instance_id" "$data"
}

# Get the class of an instance
function _get_instance_class {
  local instance_id="$1"
  local data
  data=$(db_get "$instance_id" 2>/dev/null)
  [[ -n "$data" ]] && echo "$data" | jq -r '.class // empty'
}

# Check if something is an instance ID
function _is_instance {
  local maybe_instance="$1"
  local type_val=$(_get_instance_class "$maybe_instance")
  [[ -n "$type_val" ]]
}

# Delete an instance
function _delete_instance {
  local instance_id="$1"
  db_delete "$instance_id" 2>/dev/null
}

# Find instances matching a predicate
# Usage: _find_with_predicate ClassName "field > value"
# Supports: =, !=, >, <, >=, <=
function _find_with_predicate {
  local class_name="$1"
  local predicate="$2"
  local field op value sql_predicate

  # Parse predicate: "field op value"
  if [[ "$predicate" =~ ^([a-zA-Z_][a-zA-Z0-9_]*)[[:space:]]*(!=|>=|<=|=|>|<)[[:space:]]*(.+)$ ]]; then
    field="${BASH_REMATCH[1]}"
    op="${BASH_REMATCH[2]}"
    value="${BASH_REMATCH[3]}"

    # Build SQL predicate with json_extract
    if [[ "$value" =~ ^-?[0-9]+$ ]]; then
      # Numeric value
      sql_predicate="json_extract(data, '\$.$field') $op $value"
    elif [[ "$value" =~ ^\'.*\'$ ]]; then
      # Already quoted string
      sql_predicate="json_extract(data, '\$.$field') $op $value"
    else
      # Unquoted string - add quotes
      sql_predicate="json_extract(data, '\$.$field') $op '$value'"
    fi

    db_query "class = '$class_name' AND $sql_predicate"
  else
    echo "Error: Invalid predicate format. Use: field op value (e.g., 'value > 5')" >&2
    return 1
  fi
}
export -f _find_with_predicate

# Generate a unique instance ID for a class
function _generate_instance_id {
  local class_name="$1"
  local prefix=$(echo "$class_name" | tr '[:upper:]' '[:lower:]')
  echo "${prefix}_$(uuidgen 2>/dev/null || echo "$$_$(date +%s)")"
}

# Get instance variable value directly (for Smalltalk-style syntax)
# Usage: _ivar <var_name>
# Returns the value of the instance variable for $_RECEIVER
function _ivar {
  local var="$1"
  local data
  data=$(db_get "$_RECEIVER" 2>/dev/null)
  [[ -n "$data" ]] && echo "$data" | jq -r ".$var // empty"
}

# Set instance variable value directly (for Smalltalk-style syntax)
# Usage: _ivar_set <var_name> <value>
function _ivar_set {
  local var="$1"
  local value="$2"
  local data
  data=$(db_get "$_RECEIVER" 2>/dev/null)

  if [[ -z "$data" ]]; then
    echo "Error: Instance $_RECEIVER not found" >&2
    return 1
  fi

  # Update the field (handle numeric, JSON, or string)
  local updated
  if [[ "$value" =~ ^-?[0-9]+$ ]]; then
    # Numeric value
    updated=$(echo "$data" | jq -c ".$var = $value")
  elif echo "$value" | jq -e . >/dev/null 2>&1; then
    # Valid JSON - use as raw JSON value
    updated=$(echo "$data" | jq -c --argjson v "$value" ".$var = \$v")
  else
    # String value - use --arg for safe escaping
    updated=$(echo "$data" | jq -c --arg v "$value" ".$var = \$v")
  fi

  db_put "$_RECEIVER" "$updated"
}

# Get instance variable as bash indexed array (for collection ivars stored as JSON)
# Usage: _ivar_array <var_name>
# Example: local -a arr; arr=($(_ivar_array items))
#          for item in "${arr[@]}"; do echo "$item"; done
function _ivar_array {
  local var="$1"
  local data json_arr
  data=$(db_get "$_RECEIVER" 2>/dev/null)
  if [[ -n "$data" ]]; then
    json_arr=$(echo "$data" | jq -r ".$var // empty")
    if [[ -n "$json_arr" && "$json_arr" != "null" ]]; then
      # Convert JSON array to space-separated quoted values for bash array assignment
      echo "$json_arr" | jq -r '.[] | @sh'
    fi
  fi
}

# Get instance variable as bash associative array declaration (for dict ivars stored as JSON)
# Usage: eval "$(_ivar_dict config)"
#        echo "${config[name]}"
# Returns: declare -A statements that can be eval'd
function _ivar_dict {
  local var="$1"
  local data json_obj
  data=$(db_get "$_RECEIVER" 2>/dev/null)
  if [[ -n "$data" ]]; then
    json_obj=$(echo "$data" | jq -r ".$var // empty")
    if [[ -n "$json_obj" && "$json_obj" != "null" ]]; then
      # Generate bash associative array assignment
      # Output: declare -A var; var=([key1]="val1" [key2]="val2")
      echo "declare -A $var"
      echo -n "$var=("
      echo "$json_obj" | jq -r 'to_entries | .[] | "[\(.key)]=\"\(.value)\""' | tr '\n' ' '
      echo ")"
    fi
  fi
}

# Get a single element from an array ivar by index
# Usage: _ivar_array_at <var_name> <index>
function _ivar_array_at {
  local var="$1"
  local idx="$2"
  local data
  data=$(db_get "$_RECEIVER" 2>/dev/null)
  if [[ -n "$data" ]]; then
    echo "$data" | jq -r ".$var[$idx] // empty"
  fi
}

# Get a single value from a dict ivar by key
# Usage: _ivar_dict_at <var_name> <key>
function _ivar_dict_at {
  local var="$1"
  local key="$2"
  local data
  data=$(db_get "$_RECEIVER" 2>/dev/null)
  if [[ -n "$data" ]]; then
    echo "$data" | jq -r ".$var[\"$key\"] // empty"
  fi
}

# Export instance functions so they're available in subshells (for command substitution)
export -f instance_vars
export -f _create_instance
export -f _get_instance_class
export -f _is_instance
export -f _delete_instance
export -f _generate_instance_id
export -f _get_class_instance_vars
export -f _get_parent_class
export -f _collect_inherited_vars
export -f _generate_accessor
export -f _ivar
export -f _ivar_set
export -f _ivar_array
export -f _ivar_dict
export -f _ivar_array_at
export -f _ivar_dict_at

# ============================================
# Object Reference Helpers
# ============================================

# Get an ivar that contains an object reference (instance ID)
# Usage: ref=$(_ivar_ref customerId)
#        @ $ref someMethod
# Same as _ivar but documents intent and could add validation later
function _ivar_ref {
  _ivar "$1"
}

# Check if an ivar contains a valid (existing) object reference
# Usage: if _ivar_ref_valid customerId; then ... fi
# Returns 0 if reference exists and points to valid instance, 1 otherwise
function _ivar_ref_valid {
  local var="$1"
  local ref
  ref=$(_ivar "$var")
  [[ -n "$ref" ]] && _is_instance "$ref"
}

# Get the class of an object stored in an ivar
# Usage: class=$(_ivar_ref_class customerId)
function _ivar_ref_class {
  local var="$1"
  local ref
  ref=$(_ivar "$var")
  [[ -n "$ref" ]] && _get_instance_class "$ref"
}

# Send a message to an object stored in an ivar
# Usage: result=$(_ivar_send customerId getName)
# Convenience for: @ $(_ivar customerId) getName
function _ivar_send {
  local var="$1"
  shift
  local ref
  ref=$(_ivar "$var")
  if [[ -n "$ref" ]]; then
    @ "$ref" "$@"
  fi
}

export -f _ivar_ref
export -f _ivar_ref_valid
export -f _ivar_ref_class
export -f _ivar_send

# ============================================
# Class Instance Variable Helpers
# ============================================
# Class instance variables are shared across all instances of a class.
# They are stored in kv-bash with keys like __ClassName__cvar__varname

# Get class instance variable value
# Usage: _cvar <var_name>
# Returns the value of the class instance variable for $_CLASS
function _cvar {
  local var="$1"
  kvget "__${_CLASS}__cvar__${var}"
}

# Set class instance variable value
# Usage: _cvar_set <var_name> <value>
function _cvar_set {
  local var="$1"
  local value="$2"
  kvset "__${_CLASS}__cvar__${var}" "$value"
}

export -f _cvar
export -f _cvar_set

# ============================================
# Protocol Helpers
# ============================================

# Check if a class has a method (including inherited methods)
# Usage: _class_has_method ClassName selector
# Returns 0 (true) if method exists, 1 (false) otherwise
function _class_has_method {
  local class_name="$1"
  local selector="$2"

  # Normalize selector: do: -> do, inject:into: -> inject_into
  local normalized="${selector%:}"
  normalized="${normalized//:/_}"

  # Walk the inheritance chain
  local current_class="$class_name"
  while [[ -n "$current_class" ]]; do
    # Ensure class is sourced
    local compiled_file="$TRASHDIR/.compiled/$current_class"
    if [[ -f "$compiled_file" && -z "${_SOURCED_COMPILED_CLASSES[$current_class]}" ]]; then
      source "$compiled_file"
      _SOURCED_COMPILED_CLASSES[$current_class]=1
    fi

    # Check for instance method
    local instance_func="__${current_class}__${normalized}"
    if declare -f "$instance_func" >/dev/null 2>&1; then
      return 0
    fi

    # Check for class method
    local class_func="__${current_class}__class__${normalized}"
    if declare -f "$class_func" >/dev/null 2>&1; then
      return 0
    fi

    # Move to parent class
    local super_var="__${current_class}__superclass"
    current_class="${!super_var:-}"
  done

  return 1
}

# Check if an object/class conforms to a protocol
# Usage: _conforms_to className protocolName
# Returns "true" or "false"
function _conforms_to {
  local class_name="$1"
  local protocol_name="$2"

  # Ensure protocol is sourced
  local proto_file="$TRASHDIR/.compiled/$protocol_name"
  if [[ -f "$proto_file" && -z "${_SOURCED_COMPILED_CLASSES[$protocol_name]}" ]]; then
    source "$proto_file"
    _SOURCED_COMPILED_CLASSES[$protocol_name]=1
  fi

  # Get required methods from protocol metadata
  local req_var="__${protocol_name}__requires"
  local required="${!req_var:-}"

  # If no requirements, trivially satisfied
  [[ -z "$required" ]] && echo "true" && return 0

  # Check each required method
  for selector in $required; do
    if ! _class_has_method "$class_name" "$selector"; then
      echo "false"
      return 0
    fi
  done

  echo "true"
}

export -f _class_has_method
export -f _conforms_to

# ============================================

# Traverse inheritance chain looking for method
# Prefers compiled classes to prevent namespace pollution
method_missing() {
  msg_debug "In method_missing with args: $*"
  msg_debug "Current superclass is $_SUPERCLASS"

  local original_receiver="$_RECEIVER"
  local current_class="$_SUPERCLASS"

  # Traverse up the inheritance chain
  while [[ -n "$current_class" ]]; do
    msg_debug "Checking class: $current_class"

    # Check for compiled version first (prevents namespace pollution)
    local compiled_file="$TRASHDIR/.compiled/$current_class"
    if [[ -f "$compiled_file" ]]; then
      # Source compiled file only once
      if [[ -z "${_SOURCED_COMPILED_CLASSES[$current_class]}" ]]; then
        source "$compiled_file"
        _SOURCED_COMPILED_CLASSES[$current_class]=1
        msg_debug "Sourced compiled class $current_class in method_missing"

        # Set up superclass from compiled class metadata (always)
        local super_var="__${current_class}__superclass"
        if [[ -n "${!super_var}" ]]; then
          _SUPERCLASS="${!super_var}"
        fi

        # Set up instance variables if present
        local vars_var="__${current_class}__instanceVars"
        if [[ -n "${!vars_var}" ]]; then
          instance_vars ${!vars_var}
        fi
      fi

      # Check for namespaced method
      local namespaced_func="__${current_class}__${_SELECTOR}"
      if declare -F "$namespaced_func" >/dev/null 2>&1; then
        msg_info "Found $_SELECTOR in compiled $current_class"
        "$namespaced_func" "$@"
        return $?
      fi

      # Check for class method
      local class_method_func="__${current_class}__class__${_SELECTOR}"
      if declare -F "$class_method_func" >/dev/null 2>&1; then
        msg_info "Found $_SELECTOR (class method) in compiled $current_class"
        "$class_method_func" "$@"
        return $?
      fi

      # Get superclass from compiled metadata and continue
      local super_var="__${current_class}__superclass"
      if [[ -n "${!super_var}" && "${!super_var}" != "$current_class" ]]; then
        current_class="${!super_var}"
        continue
      fi
    fi

    # Fall back to legacy class file if no compiled version
    if [[ -f "$TRASHDIR/$current_class" ]]; then
      source "$TRASHDIR/$current_class"

      # Check if method is defined in this class
      if file_defines_function "$TRASHDIR/$current_class" "$_SELECTOR"; then
        msg_info "Found $_SELECTOR in $current_class"
        "$_SELECTOR" "$@"
        return $?
      fi

      # Stop if we've checked Object (root class)
      if [[ "$current_class" == "Object" ]]; then
        break
      fi

      # Move up the inheritance chain
      current_class="$_SUPERCLASS"
    else
      msg_debug "Class file not found: $TRASHDIR/$current_class"
      break
    fi
  done

  # Method not found anywhere
  echo "Error: Method '$_SELECTOR' not found in $original_receiver or its superclasses"
  return 1
}

# Track which compiled classes have been sourced to avoid re-sourcing
declare -gA _SOURCED_COMPILED_CLASSES

function send {
  msg_debug "Send: $*"

  # ============================================
  # Context variables - LOCAL for proper nesting
  # ============================================
  # Using local instead of export means each send() call has its own
  # context. Bash's dynamic scoping ensures called methods see these
  # values, and when nested send() calls return, the caller's values
  # are automatically restored.

  # IMPORTANT: Capture calling_class BEFORE declaring local _CLASS,
  # because local declaration shadows the parent's variable immediately.
  local calling_class="${_CLASS:-}"

  local _RECEIVER="$1"; shift
  local _SELECTOR="$1"; shift
  # Normalize selector: remove trailing colon (keyword message syntax)
  _SELECTOR="${_SELECTOR%:}"
  local _CLASS=""
  local _INSTANCE=""

  local class_file
  local class_name
  local compiled_file
  local exit_code=0

  # ============================================
  # Stack tracking for debugging
  # ============================================
  # Record frame boundaries for ensures and handlers
  local frame_ensure_start=$_ENSURE_DEPTH
  local frame_handler_start=$_HANDLER_DEPTH

  # Check if receiver is an instance or a class
  if _is_instance "$_RECEIVER"; then
    class_name=$(_get_instance_class "$_RECEIVER")
    if [[ -z "$class_name" ]]; then
      echo "Error: Cannot determine class for instance $_RECEIVER" >&2
      return 1
    fi
    class_file=$(receiver_path "$class_name")
    msg_debug "Instance $_RECEIVER is of class $class_name"
    _CLASS="$class_name"
    _INSTANCE="$_RECEIVER"
  else
    class_name="$_RECEIVER"
    class_file=$(receiver_path "$_RECEIVER")
    _CLASS="$_RECEIVER"
    _INSTANCE=""
  fi

  # Push to call stack for debugging (lightweight, always on)
  _CALL_STACK[_CALL_DEPTH]="$_CLASS.$_SELECTOR"
  ((_CALL_DEPTH++))

  # ============================================
  # Before advice
  # ============================================
  if [[ ${#_BEFORE_ADVICE[@]} -gt 0 ]]; then
    _run_before_advice "$_CLASS" "$_SELECTOR" "$@"
  fi

  # ============================================
  # Private method enforcement
  # ============================================
  if [[ "$_SELECTOR" == _* ]]; then
    if [[ -z "$calling_class" || "$calling_class" != "$class_name" ]]; then
      echo "Error: Cannot call private method '$_SELECTOR' on $class_name from outside the class" >&2
      _send_cleanup $frame_ensure_start $frame_handler_start 1
      return 1
    fi
    msg_debug "Private method $_SELECTOR allowed (called from same class: $calling_class)"
  fi

  # ============================================
  # Method dispatch
  # ============================================

  # Check for compiled version first (prevents namespace pollution)
  compiled_file="$TRASHDIR/.compiled/$class_name"
  if [[ -f "$compiled_file" ]]; then
    msg_debug "Found compiled class: $compiled_file"

    # Source compiled file only once
    if [[ -z "${_SOURCED_COMPILED_CLASSES[$class_name]}" ]]; then
      source "$compiled_file"
      _SOURCED_COMPILED_CLASSES[$class_name]=1
      msg_debug "Sourced compiled class $class_name"

      # Set up _SUPERCLASS from compiled metadata for inheritance support
      local super_var="__${class_name}__superclass"
      if [[ -n "${!super_var}" ]]; then
        _SUPERCLASS="${!super_var}"
      fi

      # Set up instance variables if present (only once)
      # This will also generate accessors for inherited vars via _SUPERCLASS
      local vars_var="__${class_name}__instanceVars"
      if [[ -n "${!vars_var}" ]]; then
        instance_vars ${!vars_var}
        msg_debug "Generated accessors for compiled class $class_name: ${!vars_var}"
      fi

      # Initialize class instance variables if present (only once)
      local init_func="__${class_name}__initClassVars"
      if declare -F "$init_func" >/dev/null 2>&1; then
        "$init_func"
        msg_debug "Initialized class vars for $class_name"
      fi
    fi

    # Set up superclass from compiled class metadata (EVERY call, not just first)
    local super_var="__${class_name}__superclass"
    if [[ -n "${!super_var}" ]]; then
      _SUPERCLASS="${!super_var}"
      msg_debug "Set superclass for $class_name: $_SUPERCLASS"
    fi

    # Try namespaced instance method
    local namespaced_func="__${class_name}__${_SELECTOR}"
    if declare -F "$namespaced_func" >/dev/null 2>&1; then
      msg_debug "Calling namespaced function: $namespaced_func"
      "$namespaced_func" "$@"
      exit_code=$?
      _send_cleanup $frame_ensure_start $frame_handler_start $exit_code
      return $exit_code
    fi

    # Try class method
    local class_method_func="__${class_name}__class__${_SELECTOR}"
    if declare -F "$class_method_func" >/dev/null 2>&1; then
      msg_debug "Calling class method: $class_method_func"
      "$class_method_func" "$@"
      exit_code=$?
      _send_cleanup $frame_ensure_start $frame_handler_start $exit_code
      return $exit_code
    fi

    # Try trait methods
    local traits_var="__${class_name}__traits"
    if [[ -n "${!traits_var}" ]]; then
      local trait_name
      for trait_name in ${!traits_var}; do
        # Source trait if not already sourced
        if [[ -z "${_SOURCED_COMPILED_CLASSES[$trait_name]}" ]]; then
          local trait_file="$TRASHDIR/traits/$trait_name"
          if [[ -f "$trait_file" ]]; then
            source "$trait_file"
            _SOURCED_COMPILED_CLASSES[$trait_name]=1
            msg_debug "Sourced trait $trait_name"
          fi
        fi
        # Try trait method
        local trait_func="__${trait_name}__${_SELECTOR}"
        if declare -F "$trait_func" >/dev/null 2>&1; then
          msg_debug "Calling trait method: $trait_func"
          "$trait_func" "$@"
          exit_code=$?
          _send_cleanup $frame_ensure_start $frame_handler_start $exit_code
          return $exit_code
        fi
      done
    fi

    # Try generated accessor
    if declare -F "$_SELECTOR" >/dev/null 2>&1; then
      shopt -s extdebug
      local defined_in=$(declare -F "$_SELECTOR" | awk '{print $NF}')
      shopt -u extdebug
      if [[ "$defined_in" == *"trash.bash" ]]; then
        msg_debug "Calling generated accessor: $_SELECTOR"
        "$_SELECTOR" "$@"
        exit_code=$?
        _send_cleanup $frame_ensure_start $frame_handler_start $exit_code
        return $exit_code
      fi
    fi

    msg_debug "Method $_SELECTOR not in compiled $class_name, checking inheritance"
  fi

  # Legacy mode: source class file directly
  if [[ ! -f "$class_file" ]]; then
    echo "Error: Class file not found: $class_file" >&2
    _send_cleanup $frame_ensure_start $frame_handler_start 1
    return 1
  fi

  source "$class_file"

  if file_defines_function "$class_file" "$_SELECTOR"; then
    msg_debug "Function $_SELECTOR found in $class_file, calling it"
    "$_SELECTOR" "$@"
    exit_code=$?
  elif declare -F "$_SELECTOR" >/dev/null 2>&1; then
    shopt -s extdebug
    local defined_in=$(declare -F "$_SELECTOR" | awk '{print $NF}')
    shopt -u extdebug
    if [[ "$defined_in" == *"trash.bash" ]]; then
      msg_debug "Function $_SELECTOR is generated accessor, calling it"
      "$_SELECTOR" "$@"
      exit_code=$?
    else
      msg_debug "Function $_SELECTOR from other class, trying method_missing"
      method_missing "$@"
      exit_code=$?
    fi
  else
    msg_debug "Function $_SELECTOR not found, trying method_missing"
    method_missing "$@"
    exit_code=$?
  fi

  _send_cleanup $frame_ensure_start $frame_handler_start $exit_code
  return $exit_code
}

# Internal cleanup function for send()
# Handles: ensures, error handlers, after advice, call stack
_send_cleanup() {
  local frame_ensure_start=$1
  local frame_handler_start=$2
  local exit_code=$3

  # ============================================
  # Error handling
  # ============================================
  if [[ $exit_code -ne 0 && -n "$_ERROR_TYPE" ]]; then
    # Error was thrown - look for a handler in this frame
    local i handled=0
    for ((i = _HANDLER_DEPTH - 1; i >= frame_handler_start; i--)); do
      IFS='|' read -r pattern handler <<< "${_HANDLER_STACK[i]}"
      if [[ "$_ERROR_TYPE" == $pattern || "$pattern" == "*" ]]; then
        msg_debug "Error '$_ERROR_TYPE' caught by handler at depth $i"
        "$handler" "$_ERROR_TYPE" "$_ERROR_MSG"
        handled=1
        _clear_error
        exit_code=0
        break
      fi
    done

    # If not handled and we're at depth 1, print stack trace
    if [[ $handled -eq 0 && $_CALL_DEPTH -eq 1 ]]; then
      echo "Unhandled error: $_ERROR_TYPE: $_ERROR_MSG" >&2
      _print_stack_trace
    fi
  fi

  # ============================================
  # Run ensure handlers (cleanup) for this frame
  # ============================================
  while ((_ENSURE_DEPTH > frame_ensure_start)); do
    ((_ENSURE_DEPTH--))
    msg_debug "Running ensure: ${_ENSURE_STACK[_ENSURE_DEPTH]}"
    eval "${_ENSURE_STACK[_ENSURE_DEPTH]}"
  done

  # Reset handler stack for this frame
  _HANDLER_DEPTH=$frame_handler_start

  # ============================================
  # After advice
  # ============================================
  if [[ ${#_AFTER_ADVICE[@]} -gt 0 ]]; then
    _run_after_advice "$_CLASS" "$_SELECTOR" "$exit_code" "$@"
  fi

  # Pop call stack
  ((_CALL_DEPTH--))
}

# Export send helper for subshells
export -f _send_cleanup

function receiver_path {
  local receiver="$1"
  # Validate: no path separators or traversal
  if [[ "$receiver" =~ [/\\] ]] || [[ "$receiver" == ".." ]] || [[ "$receiver" == "." ]]; then
    echo "Error: Invalid receiver name: $receiver" >&2
    return 1
  fi
  echo "${TRASHDIR}/${receiver}"
}

# Last result variable - stores output of most recent @ command
# Access via $__ in REPL context (double underscore, since $_ is bash special)
declare -g __=""

# Invoke trash - Send a message
# Captures output in $__ for REPL chaining: @ Counter new → @ $__ increment
function @ {
  if [ $# == 1 ]; then
    is_a Object
  fi

  msg_debug "Entrypoint: $*"

  # Capture output and store in $__
  local ___result
  ___result=$(send "$@")
  local ___exit_code=$?

  # Store in $__ for next command (only if there's output)
  if [[ -n "$___result" ]]; then
    __="$___result"
  fi

  # Still echo the result so it's visible
  [[ -n "$___result" ]] && echo "$___result"

  return $___exit_code
}

# Get list of functions defined in $1
get_fcn_list () {
  env -i bash --noprofile --norc -c '
    source "'"$1"'"
    typeset -f |
    grep '\''^[^{} ].* () $'\'' |
    awk "{print \$1}" |
    while read -r fcn_name; do
        type "$fcn_name" | head -n 1 | grep -q "is a function$" || continue
        echo "$fcn_name"
    done
'
}

# TODO Check whether this is available in a utility library already
# if IFS is set to $1 we can make this join_by and specify separator
function join { local IFS=""; shift; echo "$*"; }

function file_defines_function() {
  msg_debug "file_defines_function $@"
  shopt -s extdebug

  EXPANDED_FILEPATH=$(wrapped_readlink -f "$1")
  DEFINED_IN=$(declare -F "$2" | awk '{print $NF}')
  shopt -u extdebug
  msg_debug "Expanded Filepath: $EXPANDED_FILEPATH"
  msg_debug "Defined in: $DEFINED_IN"

  if [[ "$EXPANDED_FILEPATH" == "$DEFINED_IN" ]]; then
    msg_debug "Function $2 found in $1"
    return 0
  else
    msg_debug "Function $2 not found in $1"
    return 1
  fi
}

function is_a() {
  export _SUPERCLASS=$1
}

# Include a trait (mixin)
function include() {
  local trait_name="$1"
  local trait_file="$TRASHDIR/traits/$trait_name"

  if [[ -f "$trait_file" ]]; then
    msg_debug "Including trait: $trait_name"
    source "$trait_file"
  else
    echo "Warning: Trait $trait_name not found at $trait_file" >&2
  fi
}

function create_object_stub() {
  object_name=$1; shift
  alias $object_name='@ $object_name $@'
}

function initialize_trash() {
  # Ensure trash directory exists
  if [[ ! -d "$TRASHDIR" ]]; then
    mkdir -p "$TRASHDIR"
    msg_info "Created trash directory: $TRASHDIR"
  fi

  # Ensure traits directory exists
  if [[ ! -d "$TRASHDIR/traits" ]]; then
    mkdir -p "$TRASHDIR/traits"
    msg_info "Created traits directory: $TRASHDIR/traits"
  fi

  # Initialize SQLite database for instance storage
  db_init 2>/dev/null || msg_debug "Database already initialized"

  # Create object stubs for all objects
  for file in $TRASHDIR/*; do
    if [[ -f "$file" ]]; then  # Only process files, not directories
      object_name=`basename $file`
      msg_debug "Creating object stub for $object_name"
      create_object_stub $object_name
    fi
  done

  msg_info "Trash system initialized with $(find $TRASHDIR -maxdepth 1 -type f | wc -l) objects"
}

# Auto-initialize when sourced
initialize_trash
