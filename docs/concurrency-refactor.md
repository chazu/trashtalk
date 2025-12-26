# Concurrency Refactor: Process, Future, and Tuplespace

## Overview

This document outlines a refactor of Trashtalk's concurrency model, moving from a complex Actor-based system to a simpler, more Unix-native approach.

### Design Principles

1. **Work with bash, not against it** - Use native primitives (`&`, `wait`, FIFOs, files) rather than fighting them
2. **Simple over clever** - Prefer 30 lines that work over 450 lines that almost work
3. **Decoupled abstractions** - Each class solves one problem well
4. **Pragmatic concurrency** - Trashtalk is for scripting, not distributed systems

### Summary of Changes

| Action | Class | Rationale |
|--------|-------|-----------|
| **Keep** | Process | Already works for external command management |
| **Add** | Future | Simple async computation with result retrieval |
| **Remove** | Actor | Over-engineered, fundamentally broken by race conditions |
| **Decouple & Fix Later** | Tuplespace | Useful for coordination, but needs SQLite backend; not needed by Process/Future |

---

## Part 1: Remove Actor

### Why Remove Actor?

The Actor class attempts to implement Erlang-style message-passing concurrency using Tuplespace as a message bus. This design has fundamental problems:

1. **Race conditions in Tuplespace** - The recutils-based storage uses file replacement (`recdel > temp && mv temp db`) which corrupts data under concurrent access

2. **No true blocking** - Linda's `in`/`rd` primitives are meant to suspend the process until a match exists. Our implementation polls every 100ms, wasting CPU and adding latency

3. **Complexity** - 450 lines of code for an abstraction that doesn't reliably work

4. **Wrong abstraction for bash** - Erlang actors work because Erlang has lightweight processes, immutable data, and a supervision runtime. Bash has none of these.

### Files to Remove

```
trash/Actor.trash           # The Actor class
tests/test_actor.bash        # Actor tests
```

### Migration Path

Users with Actor code should migrate to:
- **Future** for async computation with results
- **Process** for managing external commands
- Direct use of `&` and `wait` for simple backgrounding

---

## Part 2: Add Future Class

### Design

A Future represents an async computation that will produce a result. It's the minimal abstraction for "do this in the background, get the result later."

```
┌─────────────────────────────────────────────────────┐
│                      Future                          │
├─────────────────────────────────────────────────────┤
│ run: command        → Start async, return future ID │
│ await               → Block until done, return result│
│ poll                → Check if done (non-blocking)  │
│ cancel              → Kill the background process   │
│ status              → Show state (pending/done/failed)│
└─────────────────────────────────────────────────────┘
```

### Implementation

```smalltalk
# Future - Simple async computation with result retrieval
Future subclass: Object
  instanceVars: pid result_file status exit_code

  # Start an async computation
  # Usage: future=$(@ Future run: '@ Counter expensiveCalculation')
  rawMethod: run: command [
    local future_id
    future_id=$(_generate_instance_id Future)

    # Create result storage
    local result_dir="/tmp/trashtalk/futures"
    mkdir -p "$result_dir"
    local result_file="$result_dir/$future_id"
    local exit_file="$result_dir/${future_id}.exit"

    # Run command in background subshell
    (
      eval "$command" > "$result_file" 2>&1
      echo $? > "$exit_file"
    ) &
    local bg_pid=$!

    # Create instance with metadata
    _create_instance Future "$future_id"
    _ivar_set pid "$bg_pid"
    _ivar_set result_file "$result_file"
    _ivar_set status "pending"
    _ivar_set exit_code ""

    echo "$future_id"
  ]

  # Block until the computation completes, return result
  method: await [
    | pid result_file exit_file |
    pid := $(_ivar pid)
    result_file := $(_ivar result_file)
    exit_file := "${result_file}.exit"

    # Wait for process to complete
    wait "$pid" 2>/dev/null

    # Read exit code and update status
    local exit_code
    exit_code=$(cat "$exit_file" 2>/dev/null || echo "1")
    _ivar_set exit_code "$exit_code"

    if [[ "$exit_code" == "0" ]]; then
      _ivar_set status "completed"
    else
      _ivar_set status "failed"
    fi

    # Return result
    cat "$result_file"
  ]

  # Check if computation is done without blocking
  method: poll [
    | pid |
    pid := $(_ivar pid)

    if kill -0 "$pid" 2>/dev/null; then
      echo "pending"
    else
      # Process finished, update status
      @ self await >/dev/null
      echo "$(_ivar status)"
    fi
  ]

  # Check if done (returns 0 if done, 1 if pending)
  method: isDone [
    | pid |
    pid := $(_ivar pid)
    ! kill -0 "$pid" 2>/dev/null
  ]

  # Cancel the computation
  method: cancel [
    | pid |
    pid := $(_ivar pid)

    if kill -0 "$pid" 2>/dev/null; then
      kill -TERM "$pid" 2>/dev/null
      sleep 0.1
      kill -KILL "$pid" 2>/dev/null
      _ivar_set status "cancelled"
      echo "Cancelled"
    else
      echo "Already completed"
    fi
  ]

  # Get current status
  method: status [
    | pid status |
    status := $(_ivar status)
    pid := $(_ivar pid)

    # Update status if process finished
    if [[ "$status" == "pending" ]] && ! kill -0 "$pid" 2>/dev/null; then
      @ self await >/dev/null
      status := $(_ivar status)
    fi

    echo "$status"
  ]

  # Get exit code (empty if still pending)
  method: exitCode [
    ^ $(_ivar exit_code)
  ]

  # Clean up resources
  method: cleanup [
    | result_file |
    result_file := $(_ivar result_file)
    rm -f "$result_file" "${result_file}.exit" 2>/dev/null
    @ self delete
  ]

  # Show help
  classMethod: help [
    echo "=== Future - Async Computation ==="
    echo ""
    echo "Usage:"
    echo "  future=\$(@ Future run: 'command')"
    echo "  result=\$(@ \$future await)"
    echo ""
    echo "Methods:"
    echo "  run: <command>  - Start async computation, return future ID"
    echo "  await           - Block until done, return result"
    echo "  poll            - Check status without blocking"
    echo "  isDone          - Returns 0 if done, 1 if pending"
    echo "  cancel          - Kill the computation"
    echo "  status          - Show status (pending/completed/failed/cancelled)"
    echo "  exitCode        - Get exit code (empty if pending)"
    echo "  cleanup         - Remove result files and delete future"
    echo ""
    echo "Example:"
    echo "  # Start expensive computation"
    echo "  f=\$(@ Future run: 'sleep 2 && echo done')"
    echo "  "
    echo "  # Do other work..."
    echo "  @ SomeClass doOtherStuff"
    echo "  "
    echo "  # Get result (blocks if not ready)"
    echo "  result=\$(@ \$f await)"
    echo "  echo \"Result: \$result\""
  ]
```

### Usage Examples

```bash
# Basic usage
future=$(@ Future run: '@ DataProcessor crunchNumbers')
# ... do other work ...
result=$(@ $future await)

# Non-blocking check
if @ $future isDone; then
  result=$(@ $future await)
else
  echo "Still computing..."
fi

# Multiple futures in parallel
f1=$(@ Future run: '@ Report generateSales')
f2=$(@ Future run: '@ Report generateInventory')
f3=$(@ Future run: '@ Report generateCustomers')

# Wait for all
sales=$(@ $f1 await)
inventory=$(@ $f2 await)
customers=$(@ $f3 await)

# Cleanup
@ $f1 cleanup
@ $f2 cleanup
@ $f3 cleanup
```

### Test File

Create `tests/test_future.bash`:

```bash
#!/usr/bin/env bash
# Tests for Future class

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/trash.bash" 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((++PASSED)); }
fail() { echo "  ✗ $1 (expected: $2, got: $3)"; ((++FAILED)); }

assert_eq() { [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"; }
assert_contains() { [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "contains '$2'" "$3"; }
assert_not_empty() { [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" "(empty)"; }

echo "=== Future Tests ==="

# ------------------------------------------------------------------------------
echo ""
echo "--- 1. Future run: creates future ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'echo hello')
assert_not_empty "run: returns future ID" "$future"
assert_contains "future ID has future_ prefix" "future_" "$future"

# Wait for it to complete
sleep 0.2

# ------------------------------------------------------------------------------
echo ""
echo "--- 2. Future await returns result ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'echo "test result"')
result=$(@ $future await)
assert_eq "await returns command output" "test result" "$result"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 3. Future status lifecycle ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'sleep 1 && echo done')
status=$(@ $future status)
assert_eq "status is pending while running" "pending" "$status"

# Wait for completion
@ $future await >/dev/null
status=$(@ $future status)
assert_eq "status is completed after await" "completed" "$status"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 4. Future isDone ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'echo quick')
sleep 0.2
@ $future isDone && pass "isDone returns true when done" || fail "isDone returns true" "0" "1"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 5. Future poll ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'sleep 0.5 && echo done')
poll1=$(@ $future poll)
assert_eq "poll shows pending" "pending" "$poll1"

sleep 0.7
poll2=$(@ $future poll)
assert_eq "poll shows completed" "completed" "$poll2"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 6. Future cancel ---"
# ------------------------------------------------------------------------------

future=$(@ Future run: 'sleep 10')
sleep 0.1
cancel_result=$(@ $future cancel)
assert_eq "cancel returns Cancelled" "Cancelled" "$cancel_result"

status=$(@ $future status)
assert_eq "status is cancelled after cancel" "cancelled" "$status"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 7. Future exitCode ---"
# ------------------------------------------------------------------------------

# Successful command
future=$(@ Future run: 'true')
@ $future await >/dev/null
exit_code=$(@ $future exitCode)
assert_eq "exitCode is 0 for success" "0" "$exit_code"
@ $future cleanup 2>/dev/null

# Failed command
future=$(@ Future run: 'false')
@ $future await >/dev/null
exit_code=$(@ $future exitCode)
assert_eq "exitCode is 1 for failure" "1" "$exit_code"

status=$(@ $future status)
assert_eq "status is failed for non-zero exit" "failed" "$status"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 8. Multiple futures in parallel ---"
# ------------------------------------------------------------------------------

f1=$(@ Future run: 'echo one')
f2=$(@ Future run: 'echo two')
f3=$(@ Future run: 'echo three')

r1=$(@ $f1 await)
r2=$(@ $f2 await)
r3=$(@ $f3 await)

assert_eq "first future result" "one" "$r1"
assert_eq "second future result" "two" "$r2"
assert_eq "third future result" "three" "$r3"

@ $f1 cleanup 2>/dev/null
@ $f2 cleanup 2>/dev/null
@ $f3 cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 9. Future with Trashtalk object ---"
# ------------------------------------------------------------------------------

counter=$(@ Counter new)
@ $counter setValue: 10

future=$(@ Future run: "@ $counter getValue")
result=$(@ $future await)
assert_eq "future can call object methods" "10" "$result"

@ $future cleanup 2>/dev/null
@ $counter delete 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 10. Help ---"
# ------------------------------------------------------------------------------

help_output=$(@ Future help)
assert_contains "help shows run:" "run:" "$help_output"
assert_contains "help shows await" "await" "$help_output"

# ------------------------------------------------------------------------------
# Cleanup
rm -rf /tmp/trashtalk/futures 2>/dev/null

echo ""
echo "================================"
echo "Tests: $((PASSED + FAILED)) | Passed: $PASSED | Failed: $FAILED"

[[ $FAILED -gt 0 ]] && exit 1 || exit 0
```

---

## Part 3: Process Class (Keep As-Is)

The Process class handles external command execution. Recent fixes addressed:

1. Wrapping `eval` in subshell to prevent `exit` from killing the method
2. Using `kill -0` polling instead of `wait` (which can't work across subshells)

No changes needed for this refactor. Process and Future are complementary:

- **Process**: Run external shell commands, capture output/errors, manage lifecycle
- **Future**: Run Trashtalk computations asynchronously, get result later

---

## Part 4: Tuplespace (Decouple and Fix Later)

### Current Problems

1. **Race condition**: File replacement with `recdel > temp && mv temp db` corrupts data under concurrent access
2. **No true blocking**: Polls every 100ms instead of suspending
3. **Coupled to Actor**: Designed primarily as Actor's message bus

### Recommended Fix (Future Work)

Replace recutils storage with SQLite:

```bash
# In tuplespace.bash

TUPLESPACE_DB="${TUPLESPACE_DIR}/tuples.db"

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

ts_put() {
  local tuple_type="$1"
  shift
  local tuple_id="${tuple_type}_$(date +%s.%N)_$$"

  # Build JSON from key-value pairs
  local json="{"
  local first=true
  while [[ $# -ge 2 ]]; do
    $first || json+=","
    json+="\"$1\":\"$2\""
    first=false
    shift 2
  done
  json+="}"

  sqlite3 "$TUPLESPACE_DB" "INSERT INTO tuples (id, type, data) VALUES ('$tuple_id', '$tuple_type', json('$json'))"
  echo "$tuple_id"
}

ts_take() {
  local tuple_type="$1"
  local key="$2"
  local value="$3"

  # Atomic select + delete with RETURNING
  if [[ -n "$key" && -n "$value" ]]; then
    sqlite3 "$TUPLESPACE_DB" "DELETE FROM tuples WHERE id IN (
      SELECT id FROM tuples
      WHERE type = '$tuple_type'
      AND json_extract(data, '$.$key') = '$value'
      LIMIT 1
    ) RETURNING id, type, data"
  else
    sqlite3 "$TUPLESPACE_DB" "DELETE FROM tuples WHERE id IN (
      SELECT id FROM tuples WHERE type = '$tuple_type' LIMIT 1
    ) RETURNING id, type, data"
  fi
}

ts_get() {
  local tuple_type="$1"
  local key="$2"
  local value="$3"

  if [[ -n "$key" && -n "$value" ]]; then
    sqlite3 "$TUPLESPACE_DB" "SELECT id, type, data FROM tuples
      WHERE type = '$tuple_type'
      AND json_extract(data, '$.$key') = '$value'"
  else
    sqlite3 "$TUPLESPACE_DB" "SELECT id, type, data FROM tuples WHERE type = '$tuple_type'"
  fi
}

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

    if [[ $timeout -gt 0 ]]; then
      local elapsed=$(($(date +%s) - start_time))
      if [[ $elapsed -ge $timeout ]]; then
        return 1
      fi
    fi

    sleep 0.1
  done
}
```

### Priority

This is **not blocking** for the Process/Future refactor. Tuplespace can be fixed independently when cross-script coordination is needed.

---

## Implementation Plan

### Phase 1: Add Future (Immediate)

1. Create `trash/Future.trash` with implementation above
2. Create `tests/test_future.bash`
3. Run `make` to compile
4. Run tests to verify

### Phase 2: Remove Actor (Immediate)

1. Delete `trash/Actor.trash`
2. Delete `tests/test_actor.bash`
3. Run `make clean && make`
4. Update any documentation referencing Actor

### Phase 3: Fix Tuplespace (Later, Optional)

1. Replace recutils backend with SQLite in `lib/vendor/tuplespace/tuplespace.bash`
2. Update `trash/Tuplespace.trash` if needed
3. Create/update tests
4. Document as standalone coordination primitive

---

## API Summary After Refactor

### Process (External Commands)

```bash
proc=$(@ Process for: "long_running_command")
@ $proc start                    # Run in background
@ $proc isRunning                # Check if alive
@ $proc wait                     # Block until done
output=$(@ $proc output)         # Get stdout
exit_code=$(@ $proc exitCode)    # Get exit code
```

### Future (Async Trashtalk)

```bash
future=$(@ Future run: '@ MyClass compute')
# ... do other work ...
result=$(@ $future await)        # Block until done
status=$(@ $future status)       # pending/completed/failed/cancelled
@ $future cancel                 # Kill if still running
@ $future cleanup                # Remove temp files
```

### Tuplespace (Cross-Script Coordination) - Optional

```bash
@ Tuplespace put "task" id "123" status "pending"
task=$(@ Tuplespace take "task" status "pending")  # Atomic remove
@ Tuplespace get "config" key "debug"              # Non-destructive read
```

---

## Migration Guide

### From Actor to Future

Before (Actor):
```bash
actor=$(@ Actor spawn: "$counter")
@ Actor sendTo: "$actor" message: "increment"
result=$(@ Actor getFrom: "$actor" timeout: 10)
@ Actor terminate: "$actor"
```

After (Future):
```bash
future=$(@ Future run: "@ $counter increment")
result=$(@ $future await)
@ $future cleanup
```

### From Actor to Process (for external commands)

Before:
```bash
actor=$(@ Actor spawn: SomeWrapper)
@ Actor sendTo: "$actor" message: "runExternal ls -la"
```

After:
```bash
proc=$(@ Process for: "ls -la")
@ $proc run
output=$(@ $proc output)
```

---

## Rationale Summary

| Old | New | Why |
|-----|-----|-----|
| Actor (450 lines) | Future (50 lines) | Simpler, actually works, uses native bash primitives |
| Tuplespace as message bus | Tuplespace as optional coordination | Decoupling, each tool does one thing |
| Polling-based async | `&` and `wait` | Native bash, reliable, efficient |
| Complex message protocol | Direct method calls in background | Less moving parts, easier to debug |

The refactor embraces bash's strengths (process management, files, simple IPC) rather than fighting them to emulate Erlang's actor model.
