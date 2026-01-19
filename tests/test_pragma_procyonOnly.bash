#!/opt/homebrew/bin/bash

# Test pragma: procyonOnly and pragma: bashOnly features
# Usage: bash tests/test_pragma_procyonOnly.bash
#
# The pragma: procyonOnly feature marks methods that require native Procyon runtime.
# The pragma: bashOnly feature marks methods that should skip native dispatch.

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

echo "=== pragma: procyonOnly Tests ==="

# ------------------------------------------------------------------------------
# Test 1: procyonOnly marker generation
# ------------------------------------------------------------------------------

test_procyonOnly_marker() {
    create_test_class "PragmaTestProcyon" 'PragmaTestProcyon subclass: Object
  method: nativeOnly [
    pragma: procyonOnly
    ^ 42
  ]
  method: normalMethod [
    ^ 1
  ]'

    # Check that marker is set for procyonOnly method
    if [[ "${__PragmaTestProcyon__nativeOnly__procyonOnly:-}" == "1" ]]; then
        pass "procyonOnly method marker is set"
    else
        fail "procyonOnly method marker is set" "1" "${__PragmaTestProcyon__nativeOnly__procyonOnly:-NOT SET}"
    fi

    # Check that marker is NOT set for normal method
    if [[ -z "${__PragmaTestProcyon__normalMethod__procyonOnly:-}" ]]; then
        pass "normal method has no procyonOnly marker"
    else
        fail "normal method has no procyonOnly marker" "NOT SET" "${__PragmaTestProcyon__normalMethod__procyonOnly}"
    fi

    cleanup_test_class "PragmaTestProcyon"
}

run_test test_procyonOnly_marker

# ------------------------------------------------------------------------------
# Test 2: procyonOnly throws error without native plugin
# ------------------------------------------------------------------------------

test_procyonOnly_throws_without_native() {
    create_test_class "PragmaTestProcyon2" 'PragmaTestProcyon2 subclass: Object
  method: nativeOnly [
    pragma: procyonOnly
    ^ 42
  ]'

    # Should fail because no native plugin exists
    local output
    output=$(@ PragmaTestProcyon2 nativeOnly 2>&1)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]] && [[ "$output" == *"requires native Procyon plugin"* ]]; then
        pass "procyonOnly method throws error without native plugin"
    else
        fail "procyonOnly method throws error without native plugin" "Error about native plugin" "$output (exit: $exit_code)"
    fi

    cleanup_test_class "PragmaTestProcyon2"
}

run_test test_procyonOnly_throws_without_native

# ------------------------------------------------------------------------------
# Test 3: normal methods still work
# ------------------------------------------------------------------------------

test_normal_methods_work() {
    create_test_class "PragmaTestNormal" 'PragmaTestNormal subclass: Object
  method: regularMethod [
    ^ 123
  ]'

    local output
    output=$(@ PragmaTestNormal regularMethod 2>&1)
    local exit_code=$?

    if [[ $exit_code -eq 0 ]] && [[ "$output" == "123" ]]; then
        pass "normal methods still work"
    else
        fail "normal methods still work" "123" "$output (exit: $exit_code)"
    fi

    cleanup_test_class "PragmaTestNormal"
}

run_test test_normal_methods_work

echo ""
echo "=== pragma: bashOnly Tests ==="

# ------------------------------------------------------------------------------
# Test 4: bashOnly marker generation
# ------------------------------------------------------------------------------

test_bashOnly_marker() {
    create_test_class "PragmaTestBash" 'PragmaTestBash subclass: Object
  method: bashOnlyMethod [
    pragma: bashOnly
    ^ 999
  ]
  method: normalMethod [
    ^ 1
  ]'

    # Check that marker is set for bashOnly method
    if [[ "${__PragmaTestBash__bashOnlyMethod__bashOnly:-}" == "1" ]]; then
        pass "bashOnly method marker is set"
    else
        fail "bashOnly method marker is set" "1" "${__PragmaTestBash__bashOnlyMethod__bashOnly:-NOT SET}"
    fi

    # Check that marker is NOT set for normal method
    if [[ -z "${__PragmaTestBash__normalMethod__bashOnly:-}" ]]; then
        pass "normal method has no bashOnly marker"
    else
        fail "normal method has no bashOnly marker" "NOT SET" "${__PragmaTestBash__normalMethod__bashOnly}"
    fi

    cleanup_test_class "PragmaTestBash"
}

run_test test_bashOnly_marker

# ------------------------------------------------------------------------------
# Test 5: bashOnly methods execute in Bash (not native)
# ------------------------------------------------------------------------------

test_bashOnly_executes_in_bash() {
    create_test_class "PragmaTestBash2" 'PragmaTestBash2 subclass: Object
  method: bashOnlyMethod [
    pragma: bashOnly
    ^ 456
  ]'

    local output
    output=$(@ PragmaTestBash2 bashOnlyMethod 2>&1)
    local exit_code=$?

    if [[ $exit_code -eq 0 ]] && [[ "$output" == "456" ]]; then
        pass "bashOnly method executes correctly"
    else
        fail "bashOnly method executes correctly" "456" "$output (exit: $exit_code)"
    fi

    cleanup_test_class "PragmaTestBash2"
}

run_test test_bashOnly_executes_in_bash

# ------------------------------------------------------------------------------
# Test 6: Multiple pragmas on different methods
# ------------------------------------------------------------------------------

test_multiple_pragmas() {
    create_test_class "PragmaTestMulti" 'PragmaTestMulti subclass: Object
  method: nativeOnly [
    pragma: procyonOnly
    ^ 1
  ]
  method: bashOnlyMethod [
    pragma: bashOnly
    ^ 2
  ]
  method: regularMethod [
    ^ 3
  ]'

    # Check procyonOnly marker
    if [[ "${__PragmaTestMulti__nativeOnly__procyonOnly:-}" == "1" ]]; then
        pass "multiple pragmas: procyonOnly marker set"
    else
        fail "multiple pragmas: procyonOnly marker set" "1" "${__PragmaTestMulti__nativeOnly__procyonOnly:-NOT SET}"
    fi

    # Check bashOnly marker
    if [[ "${__PragmaTestMulti__bashOnlyMethod__bashOnly:-}" == "1" ]]; then
        pass "multiple pragmas: bashOnly marker set"
    else
        fail "multiple pragmas: bashOnly marker set" "1" "${__PragmaTestMulti__bashOnlyMethod__bashOnly:-NOT SET}"
    fi

    # Check regular method works
    local output
    output=$(@ PragmaTestMulti regularMethod 2>&1)
    if [[ "$output" == "3" ]]; then
        pass "multiple pragmas: regular method works"
    else
        fail "multiple pragmas: regular method works" "3" "$output"
    fi

    # Check bashOnly method works
    output=$(@ PragmaTestMulti bashOnlyMethod 2>&1)
    if [[ "$output" == "2" ]]; then
        pass "multiple pragmas: bashOnly method works"
    else
        fail "multiple pragmas: bashOnly method works" "2" "$output"
    fi

    cleanup_test_class "PragmaTestMulti"
}

run_test test_multiple_pragmas

# ==============================================================================
# Summary
# ==============================================================================

echo ""
echo "================================"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "================================"

# Clean up test file we created earlier
rm -f "$PROJECT_DIR/trash/.compiled/TestPragma"

if [[ $TESTS_FAILED -gt 0 ]]; then
    exit 1
fi
exit 0
