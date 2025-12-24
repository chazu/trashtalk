#!/usr/bin/env bash
# Test suite for trait functionality

set -uo pipefail

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="$(dirname "$SCRIPT_DIR")"

# Source the runtime
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null

# Test counters
PASSED=0
FAILED=0

# Test helper functions
pass() {
    echo "  ✓ $1"
    ((++PASSED))
}

fail() {
    echo "  ✗ $1"
    echo "    Expected: $2"
    echo "    Got: $3"
    ((++FAILED))
}

assert_eq() {
    local desc="$1"
    local expected="$2"
    local actual="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass "$desc"
    else
        fail "$desc" "$expected" "$actual"
    fi
}

assert_contains() {
    local desc="$1"
    local needle="$2"
    local haystack="$3"
    if [[ "$haystack" == *"$needle"* ]]; then
        pass "$desc"
    else
        fail "$desc" "contains '$needle'" "$haystack"
    fi
}

assert_not_empty() {
    local desc="$1"
    local value="$2"
    if [[ -n "$value" ]]; then
        pass "$desc"
    else
        fail "$desc" "non-empty" "(empty)"
    fi
}

echo "=== Trait Tests ==="
echo ""

# ------------------------------------------------------------------------------
echo "--- 1. Debuggable Trait Inclusion ---"
# ------------------------------------------------------------------------------

# Array includes Debuggable (Counter does not)
arr=$(@ Array new)
assert_not_empty "Array instance created" "$arr"

# Check that Array has the trait declared (need to source first)
_ensure_class_sourced "Array"
array_traits="${__Array__traits:-}"
assert_contains "Array declares Debuggable trait" "Debuggable" "$array_traits"

# ------------------------------------------------------------------------------
echo ""
echo "--- 2. Trait Method: debug: ---"
# ------------------------------------------------------------------------------

# debug: should output to stderr and respect TRASH_DEBUG
debug_output=$(TRASH_DEBUG=1 @ "$arr" debug: "test message" 2>&1)
assert_contains "debug: outputs message" "test message" "$debug_output"
assert_contains "debug: includes DEBUG label" "DEBUG" "$debug_output"
assert_contains "debug: includes receiver" "$arr" "$debug_output"

# debug: should be silent when TRASH_DEBUG=0
silent_output=$(TRASH_DEBUG=0 @ "$arr" debug: "should not appear" 2>&1)
if [[ -z "$silent_output" ]]; then
    pass "debug: silent when TRASH_DEBUG=0"
else
    fail "debug: silent when TRASH_DEBUG=0" "(empty)" "$silent_output"
fi

# ------------------------------------------------------------------------------
echo ""
echo "--- 3. Trait Method: trace:args: ---"
# ------------------------------------------------------------------------------

trace_output=$(TRASH_DEBUG=1 @ "$arr" trace: "push" args: "item" 2>&1)
assert_contains "trace:args: outputs method name" "push" "$trace_output"
assert_contains "trace:args: outputs args" "item" "$trace_output"

# ------------------------------------------------------------------------------
echo ""
echo "--- 4. Trait Method: inspect ---"
# ------------------------------------------------------------------------------

# Note: Object has its own inspect that may override Debuggable's
# Test that inspect works and returns useful info
inspect_output=$(@ "$arr" inspect 2>&1)
assert_not_empty "inspect returns output" "$inspect_output"
# Should contain the instance ID or class info
if [[ "$inspect_output" == *"$arr"* ]] || [[ "$inspect_output" == *"Array"* ]]; then
    pass "inspect shows instance/class info"
else
    fail "inspect shows instance/class info" "contains '$arr' or 'Array'" "$inspect_output"
fi

# ------------------------------------------------------------------------------
echo ""
echo "--- 5. Multiple Classes with Same Trait ---"
# ------------------------------------------------------------------------------

# Dictionary also includes Debuggable
dict=$(@ Dictionary new)
assert_not_empty "Dictionary instance created" "$dict"

dict_debug=$(TRASH_DEBUG=1 @ "$dict" debug: "dict debug" 2>&1)
assert_contains "Dictionary can use debug:" "dict debug" "$dict_debug"
assert_contains "Dictionary debug: shows dict receiver" "$dict" "$dict_debug"

# Create second dictionary to verify trait works across instances of same class
dict2=$(@ Dictionary new)
assert_not_empty "Second Dictionary instance created" "$dict2"

dict2_debug=$(TRASH_DEBUG=1 @ "$dict2" debug: "dict2 debug" 2>&1)
assert_contains "Second Dictionary can use debug:" "dict2 debug" "$dict2_debug"

# ------------------------------------------------------------------------------
echo ""
echo "--- 6. Trait Methods Don't Interfere with Class Methods ---"
# ------------------------------------------------------------------------------

# Array should still have its own methods working
@ "$arr" push: "item1"
size=$(@ "$arr" size)
assert_eq "Array push: still works with trait" "1" "$size"

# Dictionary should still work
@ "$dict" at: "key1" put: "value1"
dict_val=$(@ "$dict" at: "key1")
assert_eq "Dictionary at:put: still works with trait" "value1" "$dict_val"

# ------------------------------------------------------------------------------
echo ""
echo "--- 7. Trait on Multiple Instances of Same Class ---"
# ------------------------------------------------------------------------------

# Create multiple arrays, each should have trait methods
a1=$(@ Array new)
a2=$(@ Array new)

d1=$(TRASH_DEBUG=1 @ "$a1" debug: "from a1" 2>&1)
d2=$(TRASH_DEBUG=1 @ "$a2" debug: "from a2" 2>&1)

assert_contains "First array debug works" "from a1" "$d1"
assert_contains "Second array debug works" "from a2" "$d2"
assert_contains "First array shows its ID" "$a1" "$d1"
assert_contains "Second array shows its ID" "$a2" "$d2"

# ------------------------------------------------------------------------------
echo ""
echo "--- 8. Cleanup ---"
# ------------------------------------------------------------------------------

# Clean up test instances
@ "$arr" delete 2>/dev/null || true
@ "$dict" delete 2>/dev/null || true
@ "$dict2" delete 2>/dev/null || true
@ "$a1" delete 2>/dev/null || true
@ "$a2" delete 2>/dev/null || true
pass "Test instances cleaned up"

# ------------------------------------------------------------------------------
echo ""
echo "================================"
echo "Tests: $((PASSED + FAILED)) | Passed: $PASSED | Failed: $FAILED"

if [[ $FAILED -gt 0 ]]; then
    exit 1
fi
