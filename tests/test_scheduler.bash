#!/usr/bin/env bash
# Test suite for Scheduler class

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

if ! honker_available; then
    echo "SKIP: honker extension not installed"
    exit 0
fi

export SQLITE_JSON_DB="/tmp/test_scheduler_$$.db"
db_init
honker_bootstrap

PASSED=0
FAILED=0

pass() {
    echo "  PASS: $1"
    ((PASSED++)) || true
}

fail() {
    echo "  FAIL: $1 (expected: $2, got: $3)"
    ((FAILED++)) || true
}

assert_eq() {
    [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"
}

assert_not_empty() {
    [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" "(empty)"
}

cleanup() {
    @ Scheduler stop 2>/dev/null
    rm -f "$SQLITE_JSON_DB" /tmp/sched_test_* 2>/dev/null
}
trap cleanup EXIT

echo "=== Scheduler Tests ==="
echo ""

# ==========================================
echo "1. Register Task"
# ==========================================

_test_sched_callback() {
    echo "fired" >> "/tmp/sched_test_$$"
}
export -f _test_sched_callback

@ Scheduler every: '* * * * *' call: '_test_sched_callback' named: 'test-every-minute'
pass "scheduler task registered"

# ==========================================
echo ""
echo "2. Manual Tick"
# ==========================================

tick_result=$(@ Scheduler tick)
assert_not_empty "tick returned result" "$tick_result"

# ==========================================
echo ""
echo "3. Soonest"
# ==========================================

soonest=$(@ Scheduler soonest)
assert_not_empty "soonest returned a value" "$soonest"

# ==========================================
echo ""
echo "4. Unregister Task"
# ==========================================

@ Scheduler unschedule: 'test-every-minute'
pass "scheduler task unregistered"

# ==========================================
echo ""
echo "5. IsRunning (before start)"
# ==========================================

running=$(@ Scheduler isRunning)
assert_eq "not running before start" "false" "$running"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
