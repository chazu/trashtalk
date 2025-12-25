#!/usr/bin/env bash
# Tests for Future class
#
# Uses the two-call pattern to avoid subshell issues:
#   future=$(@ Future for: 'command')  # Create instance (in subshell - fine)
#   @ $future start                     # Start in background (outside subshell)

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
echo "--- 1. Future for: creates future (without starting) ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo hello')
assert_not_empty "for: returns future ID" "$future"
assert_contains "future ID has future_ prefix" "future_" "$future"

status=$(@ $future status)
assert_eq "initial status is created" "created" "$status"

@ $future cleanup 2>/dev/null
sleep 0.1

# ------------------------------------------------------------------------------
echo ""
echo "--- 2. Two-call pattern: for: then start ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo "test result"')
@ $future start  # Start OUTSIDE subshell
result=$(@ $future await)
assert_eq "await returns command output" "test result" "$result"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 3. Future status lifecycle ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo lifecycle_test')
@ $future start
@ $future await >/dev/null
status=$(@ $future status)
assert_eq "status is completed after await" "completed" "$status"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 4. Future isDone ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo quick')
@ $future start
@ $future await >/dev/null  # Ensure completion
@ $future isDone && pass "isDone returns true when done" || fail "isDone returns true" "0" "1"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 5. Future poll after completion ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo poll_test')
@ $future start
@ $future await >/dev/null  # Ensure completion
poll=$(@ $future poll)
assert_eq "poll shows completed after await" "completed" "$poll"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 6. Future cancel on completed ---"
# ------------------------------------------------------------------------------

future=$(@ Future for: 'echo done')
@ $future start
@ $future await >/dev/null
cancel_result=$(@ $future cancel)
assert_eq "cancel on completed returns Already completed" "Already completed" "$cancel_result"
@ $future cleanup 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 7. Future exitCode ---"
# ------------------------------------------------------------------------------

# Successful command
future=$(@ Future for: 'true')
@ $future start
@ $future await >/dev/null
exit_code=$(@ $future exitCode)
assert_eq "exitCode is 0 for success" "0" "$exit_code"
@ $future cleanup 2>/dev/null

# Failed command
future=$(@ Future for: 'false')
@ $future start
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

# Create all futures first (with delays to avoid db locking)
f1=$(@ Future for: 'echo one')
sleep 0.2
f2=$(@ Future for: 'echo two')
sleep 0.2
f3=$(@ Future for: 'echo three')
sleep 0.2

# Start all of them (outside subshell, with delays to avoid db locking)
@ $f1 start
sleep 0.2
@ $f2 start
sleep 0.2
@ $f3 start

# Await results
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

future=$(@ Future for: "@ $counter getValue")
@ $future start
result=$(@ $future await)
assert_eq "future can call object methods" "10" "$result"

@ $future cleanup 2>/dev/null
@ $counter delete 2>/dev/null

# ------------------------------------------------------------------------------
echo ""
echo "--- 10. Help ---"
# ------------------------------------------------------------------------------

help_output=$(@ Future help)
assert_contains "help shows for:" "for:" "$help_output"
assert_contains "help shows await" "await" "$help_output"

# ------------------------------------------------------------------------------
# Cleanup
rm -rf /tmp/trashtalk/futures 2>/dev/null

echo ""
echo "================================"
echo "Tests: $((PASSED + FAILED)) | Passed: $PASSED | Failed: $FAILED"

[[ $FAILED -gt 0 ]] && exit 1 || exit 0
