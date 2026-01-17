#!/usr/bin/env bash
# Test Yutani::Widget class functionality
# Regression tests for native compilation refactoring

# Determine script location and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Source test utilities if available
source "$SCRIPT_DIR/test_utils.bash" 2>/dev/null || {
    # Minimal test harness
    TESTS_RUN=0
    TESTS_PASSED=0
    TESTS_FAILED=0

    run_test() {
        local name="$1"
        local expected="$2"
        local actual="$3"
        ((TESTS_RUN++))
        if [[ "$expected" == "$actual" ]]; then
            echo "  $name"
            ((TESTS_PASSED++))
        else
            echo "  $name"
            echo "  expected: '$expected'"
            echo "  actual:   '$actual'"
            ((TESTS_FAILED++))
        fi
    }

    print_summary() {
        echo ""
        echo "================================"
        echo "Tests: $TESTS_RUN | Passed: $TESTS_PASSED | Failed: $TESTS_FAILED"
        if [[ $TESTS_FAILED -gt 0 ]]; then
            exit 1
        fi
    }
}

# Source the runtime
source "$PROJECT_ROOT/lib/trash.bash" 2>/dev/null

echo "=== Yutani::Widget Tests ==="
echo ""

# ============================================================================
# Basic Instance Creation
# ============================================================================
echo "--- Instance Creation ---"

widget=$(@ Yutani::Widget new)
run_test "Widget new creates instance" "yutani_widget_" "${widget:0:14}"

# ============================================================================
# Session Accessor
# ============================================================================
echo ""
echo "--- Session Accessor ---"

@ $widget setSession: "test_session_123"
result=$(@ $widget getSession)
run_test "setSession:/getSession: round-trip" "test_session_123" "$result"

# Test empty session
widget2=$(@ Yutani::Widget new)
result=$(@ $widget2 getSession)
run_test "getSession: returns empty by default" "" "$result"

# ============================================================================
# Widget ID Accessor
# ============================================================================
echo ""
echo "--- Widget ID Accessor ---"

@ $widget setWidgetId: "wid_abc123"
result=$(@ $widget getWidgetId)
run_test "setWidgetId:/getWidgetId: round-trip" "wid_abc123" "$result"

# ============================================================================
# Title Accessor
# ============================================================================
echo ""
echo "--- Title Accessor ---"

@ $widget setTitle: "My Widget Title"
result=$(@ $widget getTitle)
run_test "setTitle:/getTitle: round-trip" "My Widget Title" "$result"

# ============================================================================
# Dispatcher Accessor
# ============================================================================
echo ""
echo "--- Dispatcher Accessor ---"

@ $widget setDispatcher: "dispatcher_xyz"
result=$(@ $widget getDispatcher)
run_test "setDispatcher:/getDispatcher: round-trip" "dispatcher_xyz" "$result"

# ============================================================================
# Multiple Widgets Independence
# ============================================================================
echo ""
echo "--- Multiple Widgets Independence ---"

widgetA=$(@ Yutani::Widget new)
widgetB=$(@ Yutani::Widget new)

@ $widgetA setTitle: "Widget A"
@ $widgetB setTitle: "Widget B"

run_test "Widget A title" "Widget A" "$(@ $widgetA getTitle)"
run_test "Widget B title" "Widget B" "$(@ $widgetB getTitle)"

# Verify they're different instances
run_test "Widgets are different instances" "false" "$( [[ "$widgetA" == "$widgetB" ]] && echo true || echo false )"

# Print summary
print_summary
