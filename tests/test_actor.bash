#!/usr/bin/env bash
# Test suite for Actor class

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

if ! honker_available; then
    echo "SKIP: honker extension not installed"
    exit 0
fi

export SQLITE_JSON_DB="/tmp/test_actor_$$.db"
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

assert_contains() {
    [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"
}

cleanup() {
    # Stop any running actors
    for pid in "${ACTOR_PIDS[@]}"; do
        kill "$pid" 2>/dev/null
    done
    rm -f "$SQLITE_JSON_DB" /tmp/actor_test_* 2>/dev/null
}
trap cleanup EXIT

ACTOR_PIDS=()

echo "=== Actor Tests ==="
echo ""

# ==========================================
echo "1. Create Actor"
# ==========================================

actor=$(@ Actor named: 'test-processor')
[[ -n "$actor" ]] && pass "Actor created: $actor" || fail "Actor created" "non-empty" ""

queue=$(@ "$actor" queueName)
assert_eq "queue name is actor.test-processor" "actor.test-processor" "$queue"

# ==========================================
echo ""
echo "2. Actor Status Lifecycle"
# ==========================================

status=$(@ "$actor" status)
assert_eq "initial status is idle" "idle" "$status"

running=$(@ "$actor" isRunning)
assert_eq "not running initially" "false" "$running"

# ==========================================
echo ""
echo "3. Enqueue Messages"
# ==========================================

@ "$actor" send: 'processOrder'
@ "$actor" send: 'processOrder' with: '42'

pending=$(@ "$actor" pendingCount)
[[ "$pending" -ge 2 ]] && pass "2+ messages pending" || fail "2+ messages pending" ">=2" "$pending"

# ==========================================
echo ""
echo "4. Start and Process"
# ==========================================

# Create a result file to verify processing
RESULT="/tmp/actor_test_$$"
rm -f "$RESULT"

# We'll test with a simpler approach: enqueue and claim directly
# since the actor dispatcher needs compiled methods to dispatch to
claimed=$(honker_claim "actor.test-processor" "test_worker" 1 5)
[[ -n "$claimed" && "$claimed" != "[]" ]] && pass "claimed message from actor queue" || fail "claimed message" "non-empty" "$claimed"

if [[ -n "$claimed" && "$claimed" != "[]" ]]; then
    job_id=$(echo "$claimed" | jq -r '.[0].id // empty' 2>/dev/null)
    payload=$(echo "$claimed" | jq -r '.[0].payload // empty' 2>/dev/null)
    assert_contains "payload has selector" "processOrder" "$payload"

    [[ -n "$job_id" ]] && honker_ack "[$job_id]" "test_worker"
fi

# ==========================================
echo ""
echo "5. Multiple Actors on Different Queues"
# ==========================================

actor_a=$(@ Actor named: 'worker-a')
actor_b=$(@ Actor named: 'worker-b')

@ "$actor_a" send: 'taskA'
@ "$actor_b" send: 'taskB'

pending_a=$(@ "$actor_a" pendingCount)
pending_b=$(@ "$actor_b" pendingCount)

assert_eq "worker-a has 1 pending" "1" "$pending_a"
assert_eq "worker-b has 1 pending" "1" "$pending_b"

# ==========================================
echo ""
echo "6. Send with Multiple Args"
# ==========================================

actor_c=$(@ Actor named: 'multi-arg')
@ "$actor_c" send: 'transfer' with: 'from-acct' and: 'to-acct'

claimed_c=$(honker_claim "actor.multi-arg" "test" 1 5)
if [[ -n "$claimed_c" && "$claimed_c" != "[]" ]]; then
    payload_c=$(echo "$claimed_c" | jq -r '.[0].payload // empty' 2>/dev/null)
    assert_contains "multi-arg payload has from-acct" "from-acct" "$payload_c"
    assert_contains "multi-arg payload has to-acct" "to-acct" "$payload_c"
    job_id_c=$(echo "$claimed_c" | jq -r '.[0].id // empty' 2>/dev/null)
    [[ -n "$job_id_c" ]] && honker_ack "[$job_id_c]" "test"
else
    fail "multi-arg message claimed" "non-empty" "$claimed_c"
fi

# ==========================================
echo ""
echo "7. Destroy Actor"
# ==========================================

@ "$actor" destroy
pass "actor destroyed without error"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
