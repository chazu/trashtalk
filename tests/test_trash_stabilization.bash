#!/bin/bash
# Test suite for Trash system stabilization
# Run with: bash test_trash_stabilization.bash

# Don't exit on error - we want to see all test results
# set -e

# Get script directory and trashtalk root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Disable debug output for tests
export DEBUG=no
export TRASH_DEBUG=0

# Source trash system (suppress initialization output)
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null

# Test counters
TESTS_PASSED=0
TESTS_FAILED=0

# Test helper functions
pass() {
    echo "  PASS: $1"
    ((TESTS_PASSED++))
}

fail() {
    echo "  FAIL: $1"
    ((TESTS_FAILED++))
}

test_eq() {
    local expected="$1"
    local actual="$2"
    local description="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass "$description"
    else
        fail "$description (expected '$expected', got '$actual')"
    fi
}

test_not_empty() {
    local actual="$1"
    local description="$2"
    if [[ -n "$actual" ]]; then
        pass "$description"
    else
        fail "$description (expected non-empty)"
    fi
}

test_contains() {
    local haystack="$1"
    local needle="$2"
    local description="$3"
    if [[ "$haystack" == *"$needle"* ]]; then
        pass "$description"
    else
        fail "$description (expected to contain '$needle')"
    fi
}

# Cleanup function
cleanup() {
    # Clean up any test instances
    for id in $TEST_INSTANCES; do
        _delete_instance "$id" 2>/dev/null || true
    done
}
trap cleanup EXIT

TEST_INSTANCES=""

echo "=========================================="
echo "Trash System Stabilization Tests"
echo "=========================================="
echo ""

# ===========================================
echo "1. Instance Helper Functions"
echo "-------------------------------------------"

# Test _generate_instance_id
id=$(_generate_instance_id "TestClass")
test_contains "$id" "testclass_" "generate_instance_id creates prefixed ID"

# Test _create_instance (current API stores JSON with class field)
test_id="test_instance_$$"
# First need to set up instance vars for TestClass (simulate legacy class setup)
_CURRENT_CLASS_VARS=""
declare -gA _CURRENT_CLASS_DEFAULTS
_create_instance "TestClass" "$test_id"
TEST_INSTANCES="$TEST_INSTANCES $test_id"

# Current implementation stores JSON, so verify via db_get
stored_data=$(db_get "$test_id" 2>/dev/null)
if [[ "$stored_data" == *'"class":"TestClass"'* ]]; then
    pass "create_instance stores class in JSON"
else
    fail "create_instance stores class in JSON (expected class field, got '$stored_data')"
fi

stored_class=$(_get_instance_class "$test_id")
test_eq "TestClass" "$stored_class" "create_instance stores type (via _get_instance_class)"

# Test _is_instance
if _is_instance "$test_id"; then
    pass "_is_instance returns true for instance"
else
    fail "_is_instance returns true for instance"
fi

if ! _is_instance "NotAnInstance"; then
    pass "_is_instance returns false for non-instance"
else
    fail "_is_instance returns false for non-instance"
fi

# Test _get_instance_class
class=$(_get_instance_class "$test_id")
test_eq "TestClass" "$class" "get_instance_class returns correct class"

# Test _delete_instance
_delete_instance "$test_id"
if ! _is_instance "$test_id"; then
    pass "_delete_instance removes instance"
else
    fail "_delete_instance removes instance"
fi

echo ""

# ===========================================
echo "2. Instance-as-Receiver Model"
echo "-------------------------------------------"

# Create a Counter instance
counter=$(@ Counter new)
TEST_INSTANCES="$TEST_INSTANCES $counter"
test_not_empty "$counter" "Counter new returns instance ID"

# Test that instance has type metadata
counter_type=$(_get_instance_class "$counter")
test_eq "Counter" "$counter_type" "Counter instance has correct type"

# Test instance-as-receiver: call method on instance
@ $counter setValue 42
value=$(@ $counter getValue)
test_eq "42" "$value" "Instance-as-receiver: setValue/getValue works"

# Test increment
@ $counter increment
value=$(@ $counter getValue)
test_eq "43" "$value" "Instance-as-receiver: increment works"

# Create an Array instance
arr=$(@ Array new)
TEST_INSTANCES="$TEST_INSTANCES $arr"
test_not_empty "$arr" "Array new returns instance ID"

# Test Array instance-as-receiver
@ $arr push "hello"
@ $arr push "world"
size=$(@ $arr size)
test_eq "2" "$size" "Instance-as-receiver: Array push/size works"

first=$(@ $arr at 0)
test_eq "hello" "$first" "Instance-as-receiver: Array at works"

echo ""

# ===========================================
echo "3. Query API"
echo "-------------------------------------------"

# Create some counters for query tests
c1=$(@ Counter new)
c2=$(@ Counter new)
c3=$(@ Counter new)
TEST_INSTANCES="$TEST_INSTANCES $c1 $c2 $c3"

@ $c1 setValue 10
@ $c2 setValue 3
@ $c3 setValue 7

# Test findAll
all_counters=$(@ Trash findAll Counter)
test_contains "$all_counters" "$c1" "findAll finds counter 1"
test_contains "$all_counters" "$c2" "findAll finds counter 2"
test_contains "$all_counters" "$c3" "findAll finds counter 3"

# Test countInstances
count=$(@ Trash countInstances Counter)
# Should be at least 4 (our test counter + c1, c2, c3)
if [[ "$count" -ge 4 ]]; then
    pass "countInstances returns correct count (at least 4)"
else
    fail "countInstances returns correct count (expected >= 4, got $count)"
fi

# Test find with predicate
high_counters=$(@ Trash find Counter 'value > 5')
test_contains "$high_counters" "$c1" "find predicate: c1 (10) > 5"
test_contains "$high_counters" "$c3" "find predicate: c3 (7) > 5"
if [[ "$high_counters" != *"$c2"* ]]; then
    pass "find predicate: c2 (3) not > 5"
else
    fail "find predicate: c2 (3) not > 5"
fi

# Test listInstanceTypes
types=$(@ Trash listInstanceTypes)
test_contains "$types" "Counter" "listInstanceTypes includes Counter"
test_contains "$types" "Array" "listInstanceTypes includes Array"

echo ""

# ===========================================
echo "4. Stack Frame System"
echo "-------------------------------------------"

# Clear any existing stack
@ Trash clearStack >/dev/null 2>&1

# Test stack is empty initially
depth=$(@ Trash stackDepth)
test_eq "0" "$depth" "Stack is empty initially"

# Enable stack frames
export TRASH_STACK_FRAMES=1

# Make a call - this should create a stack frame during execution
# Note: frames are popped after call completes, so stack should still be empty
@ $counter getValue >/dev/null

# Stack should still be empty (frames are popped)
depth=$(@ Trash stackDepth)
test_eq "0" "$depth" "Stack is empty after completed call"

# Test showStack output
output=$(@ Trash showStack)
test_contains "$output" "empty" "showStack reports empty stack"

# Disable stack frames
export TRASH_STACK_FRAMES=0

echo ""

# ===========================================
echo "5. Backward Compatibility"
echo "-------------------------------------------"

# Test class-based calls still work
result=$(@ Trash version)
test_contains "$result" "Trash System" "Class-based calls still work"

# Test inheritance still works
objects=$(@ Trash listObjects)
test_not_empty "$objects" "listObjects returns results"

echo ""

# ===========================================
echo "6. Error Handling"
echo "-------------------------------------------"

# Test invalid receiver path
result=$(@ "../etc/passwd" foo 2>&1) || true
test_contains "$result" "Invalid receiver" "Path traversal is rejected"

# Test non-existent class
result=$(@ NonExistentClass foo 2>&1) || true
test_contains "$result" "not found" "Non-existent class reports error"

echo ""

# ===========================================
# Summary
# ===========================================
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo ""

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
