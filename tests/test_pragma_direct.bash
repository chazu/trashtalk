#!/opt/homebrew/bin/bash

# Test pragma: direct feature
# Usage: bash tests/test_pragma_direct.bash
#
# The pragma: direct feature allows methods to opt out of subshell capture,
# enabling them to modify variables in the calling shell.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source trash system (suppress noisy output)
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

# Test counter for pass/fail
TESTS_PASSED=0
TESTS_FAILED=0

# Colors for output
_RED='\033[0;31m'
_GREEN='\033[0;32m'
_NC='\033[0m' # No Color

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${_GREEN}PASS${_NC}: $1"
}

fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${_RED}FAIL${_NC}: $1"
    echo "  Expected: $2"
    echo "  Got: $3"
}

run_test() {
    "$@"
}

# Helper to create, compile, and load a test class
create_test_class() {
    local class_name="$1"
    local class_content="$2"
    local trash_file="$PROJECT_DIR/trash/${class_name}.trash"
    local compiled_file="$PROJECT_DIR/trash/.compiled/$class_name"

    # Ensure .compiled directory exists
    mkdir -p "$PROJECT_DIR/trash/.compiled"

    # Write .trash file
    echo "$class_content" > "$trash_file"

    # Compile it
    "$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$trash_file" 2>/dev/null > "$compiled_file"

    # Source it in the current shell
    source "$compiled_file"
}

# Helper to clean up a test class
cleanup_test_class() {
    local class_name="$1"
    rm -f "$PROJECT_DIR/trash/${class_name}.trash"
    rm -f "$PROJECT_DIR/trash/.compiled/$class_name"
}

# ==============================================================================
# Tests
# ==============================================================================

echo "=== pragma: direct Tests ==="

# ------------------------------------------------------------------------------
# Test 1: Marker generation and visibility
# ------------------------------------------------------------------------------

test_marker_visibility() {
    create_test_class "PragmaTestMarker" 'PragmaTestMarker subclass: Object
  rawMethod: directMethod [
    pragma: direct
    echo "ran"
  ]
  rawMethod: normalMethod [
    echo "ran"
  ]'

    # Check that marker is set for direct method
    if [[ "${__PragmaTestMarker__directMethod__direct:-}" == "1" ]]; then
        pass "direct method marker is set"
    else
        fail "direct method marker is set" "1" "${__PragmaTestMarker__directMethod__direct:-NOT SET}"
    fi

    # Check that marker is NOT set for normal method
    if [[ -z "${__PragmaTestMarker__normalMethod__direct:-}" ]]; then
        pass "normal method has no marker"
    else
        fail "normal method has no marker" "NOT SET" "${__PragmaTestMarker__normalMethod__direct}"
    fi

    cleanup_test_class "PragmaTestMarker"
}

# ------------------------------------------------------------------------------
# Test 2: Normal method runs in subshell (baseline)
# ------------------------------------------------------------------------------

test_normal_method_subshell() {
    create_test_class "PragmaTestNormal" 'PragmaTestNormal subclass: Object
  rawMethod: setVar [
    PRAGMA_TEST_NORMAL="modified_by_normal"
    echo "method ran"
  ]'

    # Set initial value
    PRAGMA_TEST_NORMAL="original"

    # Call normal method (should run in subshell)
    @ PragmaTestNormal setVar >/dev/null

    # Variable should NOT be modified (ran in subshell)
    if [[ "$PRAGMA_TEST_NORMAL" == "original" ]]; then
        pass "normal method cannot modify parent shell variable"
    else
        fail "normal method cannot modify parent shell variable" "original" "$PRAGMA_TEST_NORMAL"
    fi

    unset PRAGMA_TEST_NORMAL
    cleanup_test_class "PragmaTestNormal"
}

# ------------------------------------------------------------------------------
# Test 3: Direct method bypasses subshell
# ------------------------------------------------------------------------------

test_direct_method_bypass() {
    create_test_class "PragmaTestDirect" 'PragmaTestDirect subclass: Object
  rawMethod: setVar [
    pragma: direct
    PRAGMA_TEST_DIRECT="modified_by_direct"
    echo "method ran"
  ]'

    # Set initial value
    PRAGMA_TEST_DIRECT="original"

    # Call direct method (should bypass subshell)
    @ PragmaTestDirect setVar >/dev/null

    # Variable SHOULD be modified (ran directly)
    if [[ "$PRAGMA_TEST_DIRECT" == "modified_by_direct" ]]; then
        pass "direct method can modify parent shell variable"
    else
        fail "direct method can modify parent shell variable" "modified_by_direct" "$PRAGMA_TEST_DIRECT"
    fi

    unset PRAGMA_TEST_DIRECT
    cleanup_test_class "PragmaTestDirect"
}

# ------------------------------------------------------------------------------
# Test 4: Direct method with keyword selector
# ------------------------------------------------------------------------------

test_direct_keyword_method() {
    create_test_class "PragmaTestKeyword" 'PragmaTestKeyword subclass: Object
  rawMethod: setValue: val [
    pragma: direct
    PRAGMA_TEST_KEYWORD="$val"
    echo "set to $val"
  ]'

    # Set initial value
    PRAGMA_TEST_KEYWORD="original"

    # Call keyword method with pragma: direct
    @ PragmaTestKeyword setValue: "new_value" >/dev/null

    # Variable SHOULD be modified
    if [[ "$PRAGMA_TEST_KEYWORD" == "new_value" ]]; then
        pass "direct keyword method can modify parent shell variable"
    else
        fail "direct keyword method can modify parent shell variable" "new_value" "$PRAGMA_TEST_KEYWORD"
    fi

    unset PRAGMA_TEST_KEYWORD
    cleanup_test_class "PragmaTestKeyword"
}

# ------------------------------------------------------------------------------
# Test 5: Direct method still returns output
# ------------------------------------------------------------------------------

test_direct_method_output() {
    create_test_class "PragmaTestOutput" 'PragmaTestOutput subclass: Object
  rawMethod: getMessage [
    pragma: direct
    echo "hello from direct"
  ]'

    # Call direct method and capture output
    local output
    output=$(@ PragmaTestOutput getMessage)

    # Should still capture output (though via different mechanism)
    if [[ "$output" == "hello from direct" ]]; then
        pass "direct method output can be captured"
    else
        fail "direct method output can be captured" "hello from direct" "$output"
    fi

    cleanup_test_class "PragmaTestOutput"
}

# ------------------------------------------------------------------------------
# Test 6: Mixed methods in same class
# ------------------------------------------------------------------------------

test_mixed_methods() {
    create_test_class "PragmaTestMixed" 'PragmaTestMixed subclass: Object
  rawMethod: directSet [
    pragma: direct
    PRAGMA_MIXED_DIRECT="direct_value"
  ]
  rawMethod: normalSet [
    PRAGMA_MIXED_NORMAL="normal_value"
  ]'

    PRAGMA_MIXED_DIRECT="original"
    PRAGMA_MIXED_NORMAL="original"

    @ PragmaTestMixed directSet >/dev/null
    @ PragmaTestMixed normalSet >/dev/null

    if [[ "$PRAGMA_MIXED_DIRECT" == "direct_value" ]]; then
        pass "direct method in mixed class modifies variable"
    else
        fail "direct method in mixed class modifies variable" "direct_value" "$PRAGMA_MIXED_DIRECT"
    fi

    if [[ "$PRAGMA_MIXED_NORMAL" == "original" ]]; then
        pass "normal method in mixed class cannot modify variable"
    else
        fail "normal method in mixed class cannot modify variable" "original" "$PRAGMA_MIXED_NORMAL"
    fi

    unset PRAGMA_MIXED_DIRECT PRAGMA_MIXED_NORMAL
    cleanup_test_class "PragmaTestMixed"
}

# ==============================================================================
# Run all tests
# ==============================================================================

run_test test_marker_visibility
run_test test_normal_method_subshell
run_test test_direct_method_bypass
run_test test_direct_keyword_method
run_test test_direct_method_output
run_test test_mixed_methods

# ==============================================================================
# Summary
# ==============================================================================

echo ""
TESTS_TOTAL=$((TESTS_PASSED + TESTS_FAILED))
echo "Results: $TESTS_PASSED/$TESTS_TOTAL tests passed"

if [[ $TESTS_FAILED -eq 0 ]]; then
    exit 0
else
    exit 1
fi
