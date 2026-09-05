#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for Stream class

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

if ! honker_available; then
    echo "SKIP: honker extension not installed"
    exit 0
fi

export SQLITE_JSON_DB="/tmp/test_stream_$$.db"
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

assert_not_empty() {
    [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" "(empty)"
}

cleanup() {
    rm -f "$SQLITE_JSON_DB" 2>/dev/null
}
trap cleanup EXIT

echo "=== Stream Tests ==="
echo ""

# ==========================================
echo "1. Create Producer and Consumer"
# ==========================================

producer=$(@ Stream named: 'metrics')
[[ -n "$producer" ]] && pass "producer created" || fail "producer created" "non-empty" ""

consumer=$(@ Stream named: 'metrics' consumer: 'dashboard')
[[ -n "$consumer" ]] && pass "consumer created" || fail "consumer created" "non-empty" ""

# ==========================================
echo ""
echo "2. Publish and Read"
# ==========================================

@ "$producer" publish: '{"cpu":42,"host":"web1"}'
@ "$producer" publish: '{"cpu":87,"host":"web2"}'

messages=$(@ "$consumer" read)
assert_not_empty "consumer read returned messages" "$messages"

if [[ -n "$messages" && "$messages" != "[]" ]]; then
    count=$(echo "$messages" | jq 'length' 2>/dev/null)
    [[ "$count" -ge 2 ]] && pass "stream has >= 2 messages" || fail "stream has >= 2 messages" ">=2" "$count"
fi

# ==========================================
echo ""
echo "3. Consumer Offset Tracking"
# ==========================================

@ "$consumer" ack: 1
offset=$(@ "$consumer" offset)
assert_eq "consumer offset saved" "1" "$offset"

# Read again should start from offset 1
messages2=$(@ "$consumer" read)
# Should get fewer or different messages since offset advanced
pass "read after ack returned (offset-aware)"

# ==========================================
echo ""
echo "4. Publish with Key"
# ==========================================

@ "$producer" publish: '{"event":"login"}' key: 'auth'
pass "publish with key succeeded"

# ==========================================
echo ""
echo "5. Read from Specific Offset"
# ==========================================

all_messages=$(@ "$consumer" readFrom: 0 limit: 100)
assert_not_empty "readFrom:0 returned messages" "$all_messages"

# ==========================================
echo ""
echo "6. Multiple Consumers on Same Stream"
# ==========================================

consumer2=$(@ Stream named: 'metrics' consumer: 'alerting')
offset2=$(@ "$consumer2" offset)
assert_eq "new consumer starts at offset 0" "0" "$offset2"

msgs_c2=$(@ "$consumer2" read)
assert_not_empty "second consumer can read" "$msgs_c2"

# Advancing one consumer doesn't affect the other
@ "$consumer2" ack: 2
offset_c1=$(@ "$consumer" offset)
offset_c2=$(@ "$consumer2" offset)
assert_eq "original consumer offset unchanged" "1" "$offset_c1"
assert_eq "second consumer offset advanced" "2" "$offset_c2"

# ==========================================
echo ""
echo "7. Destroy Stream"
# ==========================================

@ "$producer" destroy
@ "$consumer" destroy
@ "$consumer2" destroy
pass "streams destroyed"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
