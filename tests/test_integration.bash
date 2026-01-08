#!/usr/bin/env bash

# Integration test for new instance_vars model

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Clean slate
rm -f "$TRASHTALK_DIR/instances.db"
rm -f ~/.trashtalk/instances.db

source "$TRASHTALK_DIR/lib/trash.bash"

# Reset native daemon to ensure clean state
@ NativeDaemon reset 2>/dev/null

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

echo "=== Integration Test: instance_vars Model ==="
echo ""

# ==========================================
echo "1. Counter Class"
# ==========================================

echo "  Creating counter (with persistence for later Store tests)..."
counter=$(@ Counter create)
[[ -n "$counter" ]] && pass "Counter created: $counter" || fail "Counter creation failed"

echo "  Getting initial value..."
val=$(@ $counter getValue)
[[ "$val" == "0" ]] && pass "Initial value is 0" || fail "Initial value is $val, expected 0"

echo "  Incrementing..."
@ $counter increment
val=$(@ $counter getValue)
[[ "$val" == "1" ]] && pass "After increment: 1" || fail "After increment: $val, expected 1"

echo "  Incrementing by 5..."
@ $counter incrementBy: 5
val=$(@ $counter getValue)
[[ "$val" == "6" ]] && pass "After incrementBy: 5: 6" || fail "After incrementBy: 5: $val, expected 6"

echo "  Decrementing..."
@ $counter decrement
val=$(@ $counter getValue)
[[ "$val" == "5" ]] && pass "After decrement: 5" || fail "After decrement: $val, expected 5"

echo "  Resetting..."
@ $counter reset
val=$(@ $counter getValue)
[[ "$val" == "0" ]] && pass "After reset: 0" || fail "After reset: $val, expected 0"

echo ""

# ==========================================
echo "2. Array Class"
# ==========================================

echo "  Creating array (with persistence for later Store tests)..."
arr=$(@ Array create)
[[ -n "$arr" ]] && pass "Array created: $arr" || fail "Array creation failed"

echo "  Checking initial size..."
size=$(@ $arr size)
[[ "$size" == "0" ]] && pass "Initial size is 0" || fail "Initial size is $size, expected 0"

echo "  Pushing values..."
@ $arr push: hello
@ $arr push: world
@ $arr push: test

size=$(@ $arr size)
[[ "$size" == "3" ]] && pass "Size after 3 pushes: 3" || fail "Size is $size, expected 3"

echo "  Getting element at index 1..."
val=$(@ $arr at: 1)
[[ "$val" == "world" ]] && pass "Element at 1: world" || fail "Element at 1: $val, expected world"

echo "  Setting element at index 1..."
@ $arr at: 1 put: "replaced"
val=$(@ $arr at: 1)
[[ "$val" == "replaced" ]] && pass "After at_put: replaced" || fail "After at_put: $val, expected replaced"

echo ""

# ==========================================
echo "3. Field Validation"
# ==========================================

echo "  Trying to set undeclared field..."
result=$(@ $counter setUndeclared "test" 2>&1)
if [[ "$result" == *"not declared"* ]] || [[ "$result" == *"not found"* ]]; then
    pass "Undeclared field rejected"
else
    fail "Undeclared field was accepted: $result"
fi

echo ""

# ==========================================
echo "4. Store Class"
# ==========================================

echo "  Finding counters by class..."
counters=$(@ Store findByClass: Counter)
[[ "$counters" == *"$counter"* ]] && pass "findByClass: found our counter" || fail "findByClass: didn't find counter"

echo "  Counting counters..."
count=$(@ Store countByClass: Counter)
[[ "$count" -ge 1 ]] && pass "countByClass: $count" || fail "countByClass: returned $count"

echo "  Listing classes..."
classes=$(@ Store listClasses)
[[ "$classes" == *"Counter"* ]] && pass "listClasses includes Counter" || fail "listClasses missing Counter"
[[ "$classes" == *"Array"* ]] && pass "listClasses includes Array" || fail "listClasses missing Array"

echo ""

# ==========================================
echo "5. Data Persistence Check"
# ==========================================

echo "  Checking raw database..."
raw=$(sqlite3 ~/.trashtalk/instances.db "SELECT data FROM instances WHERE id='$counter';")
[[ "$raw" == *'"class":"Counter"'* ]] && pass "Raw data has class" || fail "Raw data missing class"
[[ "$raw" == *'"_vars"'* ]] && pass "Raw data has _vars" || fail "Raw data missing _vars"
[[ "$raw" == *'"value"'* ]] && pass "Raw data has value field" || fail "Raw data missing value"

echo ""

# ==========================================
echo "6. Utility Methods"
# ==========================================

echo "  Testing @ Trash version..."
result=$(@ Trash version 2>&1)
[[ "$result" == *"Trash System"* ]] && pass "@ Trash version works" || fail "@ Trash version failed: $result"

echo "  Testing @ Trash listObjects..."
objects=$(@ Trash listObjects 2>&1)
[[ -n "$objects" ]] && pass "@ Trash listObjects returns results" || fail "@ Trash listObjects returned empty"

echo ""

# ==========================================
echo "7. Error Handling"
# ==========================================

echo "  Testing path traversal rejection..."
result=$(@ "../etc/passwd" foo 2>&1) || true
[[ "$result" == *"Invalid receiver"* ]] && pass "Path traversal rejected" || fail "Path traversal was not rejected: $result"

echo "  Testing non-existent class error..."
result=$(@ NonExistentClass foo 2>&1) || true
[[ "$result" == *"not found"* ]] && pass "Non-existent class reports error" || fail "Non-existent class did not report error: $result"

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
