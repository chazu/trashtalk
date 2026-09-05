#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for EventBus and Observable trait

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

if ! honker_available; then
    echo "SKIP: honker extension not installed"
    exit 0
fi

export SQLITE_JSON_DB="/tmp/test_eventbus_$$.db"
db_init
honker_bootstrap

PASSED=0
FAILED=0
BUS_PIDS=()

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

assert_contains() {
    [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"
}

cleanup() {
    for pid in "${BUS_PIDS[@]}"; do
        kill "$pid" 2>/dev/null
    done
    rm -f "$SQLITE_JSON_DB" /tmp/eventbus_test_* 2>/dev/null
}
trap cleanup EXIT

echo "=== EventBus Tests ==="
echo ""

# ==========================================
echo "1. Create EventBus"
# ==========================================

bus=$(@ EventBus named: 'test')
[[ -n "$bus" ]] && pass "EventBus created: $bus" || fail "EventBus created" "non-empty" ""

# ==========================================
echo ""
echo "2. Subscribe and Emit"
# ==========================================

RESULT_FILE="/tmp/eventbus_test_$$"
rm -f "$RESULT_FILE"

_test_bus_handler() {
    echo "$1" >> "$RESULT_FILE"
}
export -f _test_bus_handler

# Subscribe using a raw callback (bypass block for simpler testing)
topic="test.ping"
listener_pid=$(honker_listen "$topic" "_test_bus_handler" 10)
BUS_PIDS+=("$listener_pid")

sleep 0.05
honker_notify "$topic" '{"msg":"pong"}'
sleep 0.15

if [[ -f "$RESULT_FILE" ]]; then
    content=$(cat "$RESULT_FILE")
    assert_contains "subscriber received event" "pong" "$content"
else
    fail "subscriber received event" "file exists" "no output"
fi

honker_unlisten "$listener_pid"
rm -f "$RESULT_FILE"

# ==========================================
echo ""
echo "3. Multiple Subscribers"
# ==========================================

RESULT_A="/tmp/eventbus_test_a_$$"
RESULT_B="/tmp/eventbus_test_b_$$"
rm -f "$RESULT_A" "$RESULT_B"

_test_handler_a() { echo "$1" >> "$RESULT_A"; }
_test_handler_b() { echo "$1" >> "$RESULT_B"; }
export -f _test_handler_a _test_handler_b

pid_a=$(honker_listen "test.multi" "_test_handler_a" 10)
pid_b=$(honker_listen "test.multi" "_test_handler_b" 10)
BUS_PIDS+=("$pid_a" "$pid_b")

sleep 0.05
honker_notify "test.multi" '{"multi":true}'
sleep 0.15

[[ -f "$RESULT_A" ]] && assert_contains "subscriber A received" "multi" "$(cat "$RESULT_A")" || fail "subscriber A received" "file" "missing"
[[ -f "$RESULT_B" ]] && assert_contains "subscriber B received" "multi" "$(cat "$RESULT_B")" || fail "subscriber B received" "file" "missing"

honker_unlisten "$pid_a"
honker_unlisten "$pid_b"
rm -f "$RESULT_A" "$RESULT_B"

# ==========================================
echo ""
echo "4. EventBus Shutdown"
# ==========================================

bus2=$(@ EventBus named: 'shutdown-test')
# We can't easily test block-based subscription without compiled Block class,
# so test shutdown on a bus with no listeners
@ "$bus2" shutdown
count=$(@ "$bus2" listenerCount)
assert_eq "listener count after shutdown is 0" "0" "$count"

# ==========================================
echo ""
echo "5. Emit with No Payload"
# ==========================================

RESULT_EMPTY="/tmp/eventbus_test_empty_$$"
rm -f "$RESULT_EMPTY"

_test_empty_handler() { echo "received" >> "$RESULT_EMPTY"; }
export -f _test_empty_handler

pid_e=$(honker_listen "test.empty" "_test_empty_handler" 10)
BUS_PIDS+=("$pid_e")

sleep 0.05
honker_notify "test.empty" ""
sleep 0.15

if [[ -f "$RESULT_EMPTY" ]]; then
    pass "empty-payload event received"
else
    fail "empty-payload event received" "file exists" "no output"
fi

honker_unlisten "$pid_e"
rm -f "$RESULT_EMPTY"

# ==========================================
echo ""
echo "=== Observable Trait Tests ==="
echo ""
echo "6. Transactional Persist + Notify"
# ==========================================

RESULT_TXN="/tmp/eventbus_test_txn_$$"
rm -f "$RESULT_TXN"

_test_txn_handler() { echo "$1" >> "$RESULT_TXN"; }
export -f _test_txn_handler

pid_t=$(honker_listen "test_inst.saved" "_test_txn_handler" 10)
BUS_PIDS+=("$pid_t")

sleep 0.05
honker_persist_and_notify "test_inst" '{"class":"TestObs","value":7}' "test_inst.saved" '{"v":7}'
sleep 0.15

stored=$(db_get "test_inst")
assert_contains "instance persisted atomically" "7" "$stored"

if [[ -f "$RESULT_TXN" ]]; then
    assert_contains "event emitted atomically" "7" "$(cat "$RESULT_TXN")"
else
    fail "event emitted atomically" "file exists" "no output"
fi

honker_unlisten "$pid_t"
rm -f "$RESULT_TXN"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
