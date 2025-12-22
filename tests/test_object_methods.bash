#!/opt/homebrew/bin/bash

# Tests for Object class methods: inspect, findAll, count, find

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Clean slate
rm -f "$TRASHTALK_DIR/instances.db"
rm -f ~/.trashtalk/instances.db

source "$TRASHTALK_DIR/lib/trash.bash"

PASSED=0
FAILED=0

pass() {
    echo "  PASS: $1"
    ((PASSED++)) || true
}

fail() {
    echo "  FAIL: $1"
    ((FAILED++)) || true
}

echo "=== Test: Object Class Methods ==="
echo ""

# ==========================================
echo "1. count Class Method"
# ==========================================

echo "  Initial count should be 0..."
count=$(@ Counter count)
[[ "$count" == "0" ]] && pass "Initial Counter count is 0" || fail "Initial count is $count, expected 0"

echo "  Creating instances..."
c1=$(@ Counter new)
c2=$(@ Counter new)
c3=$(@ Counter new)

count=$(@ Counter count)
[[ "$count" == "3" ]] && pass "Counter count is 3 after creating 3" || fail "Count is $count, expected 3"

echo ""

# ==========================================
echo "2. findAll Class Method"
# ==========================================

echo "  Finding all counters..."
all=$(@ Counter findAll)
[[ "$all" == *"$c1"* ]] && pass "findAll includes c1" || fail "findAll missing c1"
[[ "$all" == *"$c2"* ]] && pass "findAll includes c2" || fail "findAll missing c2"
[[ "$all" == *"$c3"* ]] && pass "findAll includes c3" || fail "findAll missing c3"

line_count=$(echo "$all" | wc -l | tr -d ' ')
[[ "$line_count" == "3" ]] && pass "findAll returns 3 lines" || fail "findAll returns $line_count lines, expected 3"

echo ""

# ==========================================
echo "3. find Class Method with Predicates"
# ==========================================

echo "  Setting up counters with different values..."
@ $c1 incrementBy: 3
@ $c2 incrementBy: 7
@ $c3 incrementBy: 5

echo "  Testing find with 'value > 4'..."
found=$(@ Counter find 'value > 4')
[[ "$found" == *"$c2"* ]] && pass "find 'value > 4' includes c2 (value=7)" || fail "find missing c2"
[[ "$found" == *"$c3"* ]] && pass "find 'value > 4' includes c3 (value=5)" || fail "find missing c3"
[[ "$found" != *"$c1"* ]] && pass "find 'value > 4' excludes c1 (value=3)" || fail "find should not include c1"

echo "  Testing find with 'value = 7'..."
found=$(@ Counter find 'value = 7')
[[ "$found" == *"$c2"* ]] && pass "find 'value = 7' includes c2" || fail "find missing c2"
found_count=$(echo "$found" | grep -c . || echo 0)
[[ "$found_count" == "1" ]] && pass "find 'value = 7' returns exactly 1 result" || fail "Expected 1 result, got $found_count"

echo "  Testing find with 'value >= 5'..."
found=$(@ Counter find 'value >= 5')
[[ "$found" == *"$c2"* ]] && pass "find 'value >= 5' includes c2 (value=7)" || fail "find missing c2"
[[ "$found" == *"$c3"* ]] && pass "find 'value >= 5' includes c3 (value=5)" || fail "find missing c3"

echo "  Testing find with no predicate (same as findAll)..."
found=$(@ Counter find '')
all=$(@ Counter findAll)
[[ "$found" == "$all" ]] && pass "find with empty predicate equals findAll" || fail "find empty != findAll"

echo ""

# ==========================================
echo "4. inspect Instance Method"
# ==========================================

echo "  Inspecting counter (c1 has value=3 from earlier)..."
inspection=$(@ $c1 inspect)
[[ "$inspection" == *"a Counter"* ]] && pass "inspect shows class name" || fail "inspect missing class name"
[[ "$inspection" == *"id: $c1"* ]] && pass "inspect shows instance ID" || fail "inspect missing instance ID"
[[ "$inspection" == *"value: 3"* ]] && pass "inspect shows value: 3" || fail "inspect missing or wrong value"

echo ""

# ==========================================
echo "5. Cleanup Affects count"
# ==========================================

echo "  Deleting one instance..."
@ $c1 delete
count=$(@ Counter count)
[[ "$count" == "2" ]] && pass "Count is 2 after delete" || fail "Count is $count, expected 2"

echo "  Deleting remaining instances..."
@ $c2 delete
@ $c3 delete
count=$(@ Counter count)
[[ "$count" == "0" ]] && pass "Count is 0 after all deleted" || fail "Count is $count, expected 0"

echo ""

# ==========================================
echo "=== Summary ==="
# ==========================================
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo ""

if [[ $FAILED -eq 0 ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
