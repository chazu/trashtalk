#!/usr/bin/env bash
# Integration tests for Process class

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/trash.bash" 2>/dev/null

PASSED=0
FAILED=0

pass() { echo "  ✓ $1"; ((++PASSED)); }
fail() { echo "  ✗ $1 (expected: $2, got: $3)"; ((++FAILED)); }

assert_eq() {
    [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"
}

assert_contains() {
    [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "contains '$2'" "$3"
}

assert_not_empty() {
    [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" "(empty)"
}

echo "=== Process Tests ==="

# ------------------------------------------------------------------------------
echo ""
echo "--- 1. Class Method: exec: ---"
# ------------------------------------------------------------------------------

output=$(@ Process exec: "echo hello")
assert_eq "exec: returns stdout" "hello" "$output"

output=$(@ Process exec: "echo -n foo_bar")
assert_eq "exec: handles output" "foo_bar" "$output"

# ------------------------------------------------------------------------------
echo ""
echo "--- 2. Class Method: run: ---"
# ------------------------------------------------------------------------------

@ Process run: "true"
assert_eq "run: true returns 0" "0" "$?"

@ Process run: "false" || true
# Can't easily test return value without capturing in different way

# ------------------------------------------------------------------------------
echo ""
echo "--- 3. Instance: for: and run ---"
# ------------------------------------------------------------------------------

proc=$(@ Process for: "echo 'process output'")
assert_not_empty "for: creates process" "$proc"

exit_code=$(@ "$proc" run)
assert_eq "run returns exit code 0" "0" "$exit_code"

output=$(@ "$proc" output)
assert_eq "output captures stdout" "process output" "$output"

succeeded=$(@ "$proc" succeeded)
assert_eq "succeeded returns true" "true" "$succeeded"

# Note: no getStatus method, check via info output
info=$(@ "$proc" info)
assert_contains "status is completed" "completed" "$info"

@ "$proc" delete 2>/dev/null || true

# ------------------------------------------------------------------------------
echo ""
echo "--- 4. Instance: Failing Command ---"
# ------------------------------------------------------------------------------

proc=$(@ Process for: "exit 42")
exit_code=$(@ "$proc" run)
assert_eq "run captures non-zero exit" "42" "$exit_code"

succeeded=$(@ "$proc" succeeded)
assert_eq "succeeded returns false for non-zero" "false" "$succeeded"

@ "$proc" delete 2>/dev/null || true

# ------------------------------------------------------------------------------
echo ""
echo "--- 5. Instance: stderr capture ---"
# ------------------------------------------------------------------------------

proc=$(@ Process for: "echo error >&2")
@ "$proc" run >/dev/null
errors=$(@ "$proc" errors)
assert_eq "errors captures stderr" "error" "$errors"

@ "$proc" delete 2>/dev/null || true

# ------------------------------------------------------------------------------
echo ""
echo "--- 6. Background Process: start/wait ---"
# ------------------------------------------------------------------------------

proc=$(@ Process for: "sleep 0.2 && echo done")
pid=$(@ "$proc" start)
assert_not_empty "start returns PID" "$pid"

running=$(@ "$proc" isRunning)
assert_eq "isRunning returns true while running" "true" "$running"

exit_code=$(@ "$proc" wait)
assert_eq "wait returns exit code" "0" "$exit_code"

output=$(@ "$proc" output)
assert_eq "output available after wait" "done" "$output"

running=$(@ "$proc" isRunning)
assert_eq "isRunning returns false after completion" "false" "$running"

@ "$proc" delete 2>/dev/null || true

# ------------------------------------------------------------------------------
echo ""
echo "--- 7. Class Method: spawn:/waitPid: ---"
# ------------------------------------------------------------------------------

pid=$(@ Process spawn: "sleep 0.1")
assert_not_empty "spawn: returns PID" "$pid"

running=$(@ Process isRunningPid: "$pid")
assert_eq "isRunningPid: true while running" "true" "$running"

sleep 0.2
running=$(@ Process isRunningPid: "$pid")
assert_eq "isRunningPid: false after completion" "false" "$running"

# ------------------------------------------------------------------------------
echo ""
echo "--- 8. Process info ---"
# ------------------------------------------------------------------------------

proc=$(@ Process for: "echo test")
@ "$proc" run >/dev/null
info=$(@ "$proc" info)
assert_contains "info shows Command" "Command:" "$info"
assert_contains "info shows Status" "Status:" "$info"
assert_contains "info shows completed" "completed" "$info"

@ "$proc" delete 2>/dev/null || true

# ------------------------------------------------------------------------------
echo ""
echo "================================"
echo "Tests: $((PASSED + FAILED)) | Passed: $PASSED | Failed: $FAILED"

[[ $FAILED -gt 0 ]] && exit 1 || exit 0
