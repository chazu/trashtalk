#!/usr/bin/env bash
# ==============================================================================
# Namespace Runtime Tests
# ==============================================================================
# Tests for namespace support in the Trashtalk runtime
# ==============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="$(dirname "$SCRIPT_DIR")"

# Source the runtime (suppress init messages)
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null

# Colors (use different variable names to avoid conflicts)
_RED='\033[0;31m'
_GREEN='\033[0;32m'
_NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local name="$1"
    local expected="$2"
    local actual="$3"
    ((TESTS_RUN++))
    if [[ "$expected" == "$actual" ]]; then
        echo -e "  ${_GREEN}✓${_NC} $name"
        ((TESTS_PASSED++))
    else
        echo -e "  ${_RED}✗${_NC} $name"
        echo "    Expected: $expected"
        echo "    Actual:   $actual"
        ((TESTS_FAILED++))
    fi
}

echo "=== Namespace Helper Functions ==="

run_test "_is_qualified MyApp::Counter" "true" \
    "$(_is_qualified "MyApp::Counter" && echo true || echo false)"

run_test "_is_qualified Counter" "false" \
    "$(_is_qualified "Counter" && echo true || echo false)"

run_test "_get_package MyApp::Counter" "MyApp" \
    "$(_get_package "MyApp::Counter")"

run_test "_get_package Counter" "" \
    "$(_get_package "Counter")"

run_test "_get_class_name MyApp::Counter" "Counter" \
    "$(_get_class_name "MyApp::Counter")"

run_test "_get_class_name Counter" "Counter" \
    "$(_get_class_name "Counter")"

run_test "_to_func_prefix MyApp::Counter" "__MyApp__Counter" \
    "$(_to_func_prefix "MyApp::Counter")"

run_test "_to_func_prefix Counter" "__Counter" \
    "$(_to_func_prefix "Counter")"

run_test "_to_instance_prefix MyApp::Counter" "myapp_counter" \
    "$(_to_instance_prefix "MyApp::Counter")"

run_test "_to_instance_prefix Counter" "counter" \
    "$(_to_instance_prefix "Counter")"

run_test "_to_compiled_name MyApp::Counter" "MyApp__Counter" \
    "$(_to_compiled_name "MyApp::Counter")"

run_test "_to_compiled_name Counter" "Counter" \
    "$(_to_compiled_name "Counter")"

echo ""
echo "=== Backward Compatibility ==="

# Test non-namespaced Counter still works
counter=$(@ Counter new)
run_test "Counter new returns instance ID" "counter_" \
    "${counter%%_*}_"

@ $counter increment >/dev/null
run_test "Counter increment works" "1" \
    "$(@ $counter getValue)"

# Clean up
@ $counter delete 2>/dev/null || true

echo ""
echo "=== Summary ==="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo -e "${_GREEN}All tests passed!${_NC}"
    exit 0
else
    echo -e "${_RED}Some tests failed.${_NC}"
    exit 1
fi
