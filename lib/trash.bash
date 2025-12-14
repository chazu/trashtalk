#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source dependencies quietly
source "$SCRIPT_DIR/vendor/bsfl.sh" 2>/dev/null || echo "Warning: bsfl.sh not found"
source "$SCRIPT_DIR/vendor/fun.sh" 2>/dev/null || echo "Warning: fun.sh not found"
source "$SCRIPT_DIR/vendor/kv-bash" 2>/dev/null || echo "Warning: kv-bash not found"

# Export kv-bash functions so they're available in subshells
export -f kvset kvget kvdel kv_validate_key 2>/dev/null
export KV_USER_DIR

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

function wrapped_readlink {
  local greadlink_exists
  if command -v greadlink; then
    greadlink $@
  else
    readlink $@
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
# Instance Management Functions
# ============================================

# Create an instance with type tracking
function _create_instance {
  local class_name="$1"
  local instance_id="$2"
  local initial_value="$3"

  kvset "$instance_id" "$initial_value"
  kvset "${instance_id}._type" "$class_name"
  kvset "${instance_id}._created" "$(date +%s)"
}

# Get the class of an instance
function _get_instance_class {
  local instance_id="$1"
  kvget "${instance_id}._type" 2>/dev/null
}

# Check if something is an instance ID
function _is_instance {
  local maybe_instance="$1"
  local type_val=$(_get_instance_class "$maybe_instance")
  [[ -n "$type_val" ]]
}

# Delete an instance and its metadata
function _delete_instance {
  local instance_id="$1"
  kvdel "$instance_id" 2>/dev/null
  kvdel "${instance_id}._type" 2>/dev/null
  kvdel "${instance_id}._created" 2>/dev/null
}

# Generate a unique instance ID for a class
function _generate_instance_id {
  local class_name="$1"
  local prefix=$(echo "$class_name" | tr '[:upper:]' '[:lower:]')
  echo "${prefix}_$(uuidgen 2>/dev/null || echo "$$_$(date +%s)")"
}

# Export instance functions so they're available in subshells (for command substitution)
export -f _create_instance
export -f _get_instance_class
export -f _is_instance
export -f _delete_instance
export -f _generate_instance_id

# ============================================

# Traverse inheritance chain looking for method
method_missing() {
  msg_debug "In method_missing with args: $*"
  msg_debug "Current superclass is $_SUPERCLASS"

  local original_receiver="$_RECEIVER"
  local current_class="$_SUPERCLASS"

  # Traverse up the inheritance chain
  while [[ -n "$current_class" && "$current_class" != "Object" ]]; do
    msg_debug "Checking class: $current_class"

    # Source the superclass
    if [[ -f "$TRASHDIR/$current_class" ]]; then
      source "$TRASHDIR/$current_class"

      # Check if method is defined in this class
      if file_defines_function "$TRASHDIR/$current_class" "$_SELECTOR"; then
        msg_info "Found $_SELECTOR in $current_class"
        "$_SELECTOR" "$@"
        return $?
      fi

      # Move up the inheritance chain
      current_class="$_SUPERCLASS"
    else
      msg_debug "Class file not found: $TRASHDIR/$current_class"
      break
    fi
  done

  # If we reach Object and still haven't found it, try Object
  if [[ "$current_class" == "Object" && -f "$TRASHDIR/Object" ]]; then
    source "$TRASHDIR/Object"
    if file_defines_function "$TRASHDIR/Object" "$_SELECTOR"; then
      msg_info "Found $_SELECTOR in Object"
      "$_SELECTOR" "$@"
      return $?
    fi
  fi

  # Method not found anywhere
  echo "Error: Method '$_SELECTOR' not found in $original_receiver or its superclasses"
  return 1
}

function send {
  msg_debug "Send: $*"
  local _INVOCATION=$*
  export _RECEIVER=$1; shift
  export _SELECTOR=$1; shift

  local class_file
  local class_name
  local frame_id=""
  local result
  local exit_code

  # Push stack frame if stack tracing is enabled
  if [[ "${TRASH_STACK_FRAMES:-0}" == "1" ]]; then
    frame_id=$(_push_stack_frame receiver="$_RECEIVER" selector="$_SELECTOR" args="$*")
  fi

  # Check if receiver is an instance or a class
  if _is_instance "$_RECEIVER"; then
    # Instance: look up its class
    class_name=$(_get_instance_class "$_RECEIVER")
    if [[ -z "$class_name" ]]; then
      echo "Error: Cannot determine class for instance $_RECEIVER" >&2
      [[ -n "$frame_id" ]] && _pop_stack_frame >/dev/null
      return 1
    fi
    class_file=$(receiver_path "$class_name")
    msg_debug "Instance $_RECEIVER is of class $class_name"
  else
    # Assume it's a class name
    class_file=$(receiver_path "$_RECEIVER")
  fi

  # Validate class file exists
  if [[ ! -f "$class_file" ]]; then
    echo "Error: Class file not found: $class_file" >&2
    [[ -n "$frame_id" ]] && _pop_stack_frame >/dev/null
    return 1
  fi

  # Load the function context of receiver unit
  source "$class_file"

  # Call the function if it exists after sourcing
  if declare -F "$_SELECTOR" >/dev/null 2>&1; then
    msg_debug "Function $_SELECTOR found, calling it"
    "$_SELECTOR" "$@"
    exit_code=$?
  else
    msg_debug "Function $_SELECTOR not found, trying method_missing"
    method_missing $_INVOCATION
    exit_code=$?
  fi

  # Pop stack frame if we pushed one
  [[ -n "$frame_id" ]] && _pop_stack_frame >/dev/null

  return $exit_code
}

function receiver_path {
  local receiver="$1"
  # Validate: no path separators or traversal
  if [[ "$receiver" =~ [/\\] ]] || [[ "$receiver" == ".." ]] || [[ "$receiver" == "." ]]; then
    echo "Error: Invalid receiver name: $receiver" >&2
    return 1
  fi
  echo "${TRASHDIR}/${receiver}"
}

# Invoke trash - Send a message
function @ {
  if [ $# == 1 ]; then
    is_a Object
  fi

  msg_debug "Entrypoint: $*"
  send $*
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
