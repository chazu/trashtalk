#!/usr/bin/env bash
# Test suite for honker.bash - the bash-level honker wrapper

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

# Skip if honker is not installed
if ! honker_available; then
    echo "SKIP: honker extension not installed"
    echo "  Install libhonker_ext and set HONKER_EXT to enable these tests"
    exit 0
fi

# Use a dedicated test database
export SQLITE_JSON_DB="/tmp/test_honker_$$.db"
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
    local desc="$1" expected="$2" actual="$3"
    [[ "$expected" == "$actual" ]] && pass "$desc" || fail "$desc" "$expected" "$actual"
}

assert_contains() {
    local desc="$1" needle="$2" haystack="$3"
    [[ "$haystack" == *"$needle"* ]] && pass "$desc" || fail "$desc" "*$needle*" "$haystack"
}

assert_not_empty() {
    local desc="$1" value="$2"
    [[ -n "$value" ]] && pass "$desc" || fail "$desc" "non-empty" "(empty)"
}

cleanup() {
    rm -f "$SQLITE_JSON_DB" 2>/dev/null
    # Kill any leftover listener processes
    for pid in "${LISTENER_PIDS[@]}"; do
        kill "$pid" 2>/dev/null
    done
}
trap cleanup EXIT

LISTENER_PIDS=()

echo "=== Honker Bash Wrapper Tests ==="
echo ""

# ==========================================
echo "1. Availability"
# ==========================================

honker_available && pass "honker_available returns true" || fail "honker_available" "true" "false"

# ==========================================
echo ""
echo "2. Ephemeral Pub/Sub (notify + listen)"
# ==========================================

NOTIFY_RESULT="/tmp/honker_test_notify_$$"
rm -f "$NOTIFY_RESULT"

_test_notify_handler() {
    echo "$1" >> "$NOTIFY_RESULT"
}
export -f _test_notify_handler

listener_pid=$(honker_listen "test.channel" "_test_notify_handler" 10)
LISTENER_PIDS+=("$listener_pid")

sleep 0.05
honker_notify "test.channel" '{"msg":"hello"}'
sleep 0.15

if [[ -f "$NOTIFY_RESULT" ]]; then
    content=$(cat "$NOTIFY_RESULT")
    assert_contains "listener received notification" "hello" "$content"
else
    fail "listener received notification" "file exists" "no output file"
fi

honker_unlisten "$listener_pid"
! kill -0 "$listener_pid" 2>/dev/null && pass "unlisten killed listener" || fail "unlisten killed listener" "dead" "alive"
rm -f "$NOTIFY_RESULT"

# ==========================================
echo ""
echo "3. Work Queue (enqueue + claim + ack)"
# ==========================================

honker_enqueue "test.queue" '{"task":"process_order","id":42}'
claimed=$(honker_claim "test.queue")
assert_not_empty "claim returned a job" "$claimed"

if [[ -n "$claimed" ]]; then
    job_id=$(echo "$claimed" | jq -r '.[0].id // empty' 2>/dev/null)
    assert_not_empty "job has an id" "$job_id"

    if [[ -n "$job_id" ]]; then
        honker_ack "[$job_id]"
        pass "ack succeeded"

        # Claim again should be empty
        second_claim=$(honker_claim "test.queue")
        if [[ -z "$second_claim" || "$second_claim" == "[]" ]]; then
            pass "queue empty after ack"
        else
            fail "queue empty after ack" "empty" "$second_claim"
        fi
    fi
fi

# ==========================================
echo ""
echo "4. Work Queue - Delayed Enqueue"
# ==========================================

honker_enqueue "test.delayed" '{"delayed":true}' 0 3 2
immediate_claim=$(honker_claim "test.delayed")
if [[ -z "$immediate_claim" || "$immediate_claim" == "[]" ]]; then
    pass "delayed job not claimable immediately"
else
    fail "delayed job not claimable immediately" "empty" "$immediate_claim"
fi

# ==========================================
echo ""
echo "5. Durable Streams (publish + read + offset)"
# ==========================================

honker_stream_publish "test.stream" "key1" '{"event":"created","id":1}'
honker_stream_publish "test.stream" "key2" '{"event":"updated","id":2}'

messages=$(honker_stream_read "test.stream" 0 10)
assert_not_empty "stream read returned messages" "$messages"

if [[ -n "$messages" ]]; then
    count=$(echo "$messages" | jq 'length' 2>/dev/null)
    [[ "$count" -ge 2 ]] && pass "stream has >= 2 messages" || fail "stream has >= 2 messages" ">=2" "$count"
fi

# Test consumer offset tracking
honker_stream_save_offset "test-consumer" "test.stream" 1
offset=$(honker_stream_get_offset "test-consumer" "test.stream")
assert_eq "consumer offset saved and retrieved" "1" "$offset"

# ==========================================
echo ""
echo "6. Distributed Locks"
# ==========================================

lock_result=$(honker_lock "test.lock" "owner1" 30)
assert_not_empty "lock acquired" "$lock_result"

honker_unlock "test.lock" "owner1"
pass "lock released"

# ==========================================
echo ""
echo "7. Rate Limiting"
# ==========================================

result1=$(honker_rate_limit "test.action" 2 10)
assert_eq "first rate limit try allowed" "1" "$result1"

result2=$(honker_rate_limit "test.action" 2 10)
assert_eq "second rate limit try allowed" "1" "$result2"

result3=$(honker_rate_limit "test.action" 2 10)
assert_eq "third rate limit try blocked" "0" "$result3"

# ==========================================
echo ""
echo "8. Scheduler (register + tick)"
# ==========================================

honker_schedule "test-sched" "test-task" "* * * * *" '{"action":"cleanup"}'
pass "scheduler registered"

tick_result=$(honker_scheduler_tick)
assert_not_empty "scheduler tick returned result" "$tick_result"

honker_unschedule "test-sched"
pass "scheduler unregistered"

# ==========================================
echo ""
echo "9. Result Storage"
# ==========================================

honker_result_save 9999 '{"output":"done"}' 60
result=$(honker_result_get 9999)
assert_contains "result retrieved" "done" "$result"

honker_result_sweep
pass "result sweep ran"

# ==========================================
echo ""
echo "10. Transactional Persist + Notify"
# ==========================================

# First ensure instances table exists
db_init

honker_persist_and_notify "test_instance_1" '{"class":"TestClass","value":42}' "test.persist" '{"id":"test_instance_1"}'

# Verify instance was persisted
stored=$(db_get "test_instance_1")
assert_contains "instance persisted in transaction" "42" "$stored"

# ==========================================
echo ""
echo "11. Transactional Persist + Enqueue"
# ==========================================

honker_persist_and_enqueue "test_instance_2" '{"class":"TestClass","value":99}' "test.persist.queue" '{"process":"test_instance_2"}'

stored2=$(db_get "test_instance_2")
assert_contains "instance persisted in enqueue transaction" "99" "$stored2"

queued=$(honker_claim "test.persist.queue")
assert_not_empty "job was enqueued in transaction" "$queued"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
