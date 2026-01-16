#!/usr/bin/env bash
# native-daemon.bash
#
# Bash integration for native Trashtalk binaries with --serve mode.
# Each .native binary can run as its own daemon using --serve.
#
# Usage:
#   source lib/native-daemon.bash
#
#   # One-shot call (spawns binary, exits after one call):
#   _native_call "Counter" '{"class":"Counter","value":5}' "increment"
#
#   # Daemon mode (keeps binary running for multiple calls):
#   _native_start "Counter"
#   _native_send "Counter" '{"class":"Counter","value":5}' "increment"
#   _native_send "Counter" '{"class":"Counter","value":8}' "increment"
#   _native_stop "Counter"
#

# Configuration
TRASHTALK_NATIVE_DIR="${TRASHTALK_NATIVE_DIR:-$HOME/.trashtalk/trash/.compiled}"

# Per-class daemon state (associative arrays)
declare -A _NATIVE_PIDS      # class -> PID
declare -A _NATIVE_FD_IN     # class -> write FD
declare -A _NATIVE_FD_OUT    # class -> read FD
declare -A _NATIVE_FIFO_IN   # class -> input fifo path
declare -A _NATIVE_FIFO_OUT  # class -> output fifo path

# Result from last call
_NATIVE_RESULT=""
_NATIVE_INSTANCE=""
_NATIVE_EXIT_CODE=0

# Check if a class has a native binary
_native_exists() {
    local class="$1"
    [[ -x "$TRASHTALK_NATIVE_DIR/${class}.native" ]]
}

# Check if daemon is running for a class
_native_running() {
    local class="$1"
    local pid="${_NATIVE_PIDS[$class]:-}"
    [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null
}

# Start daemon for a class
_native_start() {
    local class="$1"

    if _native_running "$class"; then
        return 0  # Already running
    fi

    local binary="$TRASHTALK_NATIVE_DIR/${class}.native"
    if [[ ! -x "$binary" ]]; then
        return 1  # No binary
    fi

    # Create named pipes
    local fifo_in=$(mktemp -u "${TMPDIR:-/tmp}/native-${class}-in.XXXXXX")
    local fifo_out=$(mktemp -u "${TMPDIR:-/tmp}/native-${class}-out.XXXXXX")
    mkfifo "$fifo_in" || return 1
    mkfifo "$fifo_out" || return 1

    # Start binary in --serve mode
    "$binary" --serve < "$fifo_in" > "$fifo_out" 2>/dev/null &
    local pid=$!

    # Open file descriptors
    local fd_in fd_out
    exec {fd_in}>"$fifo_in"
    exec {fd_out}<"$fifo_out"

    # Store state
    _NATIVE_PIDS[$class]=$pid
    _NATIVE_FD_IN[$class]=$fd_in
    _NATIVE_FD_OUT[$class]=$fd_out
    _NATIVE_FIFO_IN[$class]=$fifo_in
    _NATIVE_FIFO_OUT[$class]=$fifo_out

    return 0
}

# Stop daemon for a class
_native_stop() {
    local class="$1"
    local pid="${_NATIVE_PIDS[$class]:-}"

    if [[ -n "$pid" ]]; then
        kill "$pid" 2>/dev/null
        wait "$pid" 2>/dev/null
    fi

    # Close FDs
    local fd_in="${_NATIVE_FD_IN[$class]:-}"
    local fd_out="${_NATIVE_FD_OUT[$class]:-}"
    [[ -n "$fd_in" ]] && exec {fd_in}>&- 2>/dev/null
    [[ -n "$fd_out" ]] && exec {fd_out}<&- 2>/dev/null

    # Remove FIFOs
    local fifo_in="${_NATIVE_FIFO_IN[$class]:-}"
    local fifo_out="${_NATIVE_FIFO_OUT[$class]:-}"
    [[ -n "$fifo_in" ]] && rm -f "$fifo_in"
    [[ -n "$fifo_out" ]] && rm -f "$fifo_out"

    # Clear state
    unset "_NATIVE_PIDS[$class]"
    unset "_NATIVE_FD_IN[$class]"
    unset "_NATIVE_FD_OUT[$class]"
    unset "_NATIVE_FIFO_IN[$class]"
    unset "_NATIVE_FIFO_OUT[$class]"
}

# Stop all daemons
_native_stop_all() {
    for class in "${!_NATIVE_PIDS[@]}"; do
        _native_stop "$class"
    done
}

# Send a request to a running daemon
# Usage: _native_send <class> <instance_json> <selector> [args...]
_native_send() {
    local class="$1"
    local instance_json="$2"
    local selector="$3"
    shift 3

    if ! _native_running "$class"; then
        return 200  # Not running
    fi

    local fd_in="${_NATIVE_FD_IN[$class]}"
    local fd_out="${_NATIVE_FD_OUT[$class]}"

    # Build args JSON
    local args_json="["
    local first=true
    for arg in "$@"; do
        $first || args_json+=","
        first=false
        arg="${arg//\\/\\\\}"
        arg="${arg//\"/\\\"}"
        args_json+="\"$arg\""
    done
    args_json+="]"

    # Build request (instance is already JSON, needs to be a string field)
    local escaped_instance="${instance_json//\\/\\\\}"
    escaped_instance="${escaped_instance//\"/\\\"}"
    local request="{\"instance\":\"$escaped_instance\",\"selector\":\"$selector\",\"args\":$args_json}"

    # Send request
    echo "$request" >&"$fd_in"

    # Read response
    local response
    if ! IFS= read -r response <&"$fd_out"; then
        _native_stop "$class"
        return 200
    fi

    # Parse response
    _NATIVE_EXIT_CODE=$(echo "$response" | jq -r '.exit_code // 1')
    _NATIVE_RESULT=$(echo "$response" | jq -r '.result // ""')
    _NATIVE_INSTANCE=$(echo "$response" | jq -r '.instance // ""')

    return "$_NATIVE_EXIT_CODE"
}

# One-shot call (spawns binary, runs one command, exits)
# Usage: _native_call <class> <instance_json> <selector> [args...]
# This is simpler but has process spawn overhead per call
_native_call() {
    local class="$1"
    local instance_json="$2"
    local selector="$3"
    shift 3

    local binary="$TRASHTALK_NATIVE_DIR/${class}.native"
    if [[ ! -x "$binary" ]]; then
        return 200  # No binary, fall back
    fi

    # Build args JSON
    local args_json="["
    local first=true
    for arg in "$@"; do
        $first || args_json+=","
        first=false
        arg="${arg//\\/\\\\}"
        arg="${arg//\"/\\\"}"
        args_json+="\"$arg\""
    done
    args_json+="]"

    # Build request
    local escaped_instance="${instance_json//\\/\\\\}"
    escaped_instance="${escaped_instance//\"/\\\"}"
    local request="{\"instance\":\"$escaped_instance\",\"selector\":\"$selector\",\"args\":$args_json}"

    # Call binary in serve mode with single request
    local response
    response=$(echo "$request" | "$binary" --serve 2>/dev/null)

    if [[ -z "$response" ]]; then
        return 200  # No response, fall back
    fi

    # Parse response
    _NATIVE_EXIT_CODE=$(echo "$response" | jq -r '.exit_code // 1')
    _NATIVE_RESULT=$(echo "$response" | jq -r '.result // ""')
    _NATIVE_INSTANCE=$(echo "$response" | jq -r '.instance // ""')

    return "$_NATIVE_EXIT_CODE"
}

# Cleanup on shell exit
trap '_native_stop_all' EXIT
