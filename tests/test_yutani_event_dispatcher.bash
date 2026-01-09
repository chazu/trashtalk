#!/usr/bin/env bash
# Test Yutani::EventDispatcher class functionality
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

# Cleanup any leftover handler files from previous tests
rm -f /tmp/yutani_handler_*.txt 2>/dev/null
rm -f /tmp/yutani_text_*.txt 2>/dev/null

echo "=== Yutani::EventDispatcher Tests ==="
echo ""

# ============================================================================
# Basic Instance Creation
# ============================================================================
echo "--- Instance Creation ---"

dispatcher=$(@ Yutani::EventDispatcher new)
run_test "EventDispatcher new creates instance" "yutani__eventdispatcher_" "${dispatcher:0:24}"

# ============================================================================
# Running State
# ============================================================================
echo ""
echo "--- Running State ---"

result=$(@ $dispatcher isRunning)
run_test "isRunning returns true after new" "true" "$result"

@ $dispatcher stop
result=$(@ $dispatcher isRunning)
run_test "isRunning returns false after stop" "false" "$result"

# ============================================================================
# Widget Handler Registration
# ============================================================================
echo ""
echo "--- Widget Handler Registration ---"

dispatcher2=$(@ Yutani::EventDispatcher new)
@ $dispatcher2 onWidget: "widget_test1" do: 'echo "handler executed"'

# Check that handler file was created
if [[ -f "/tmp/yutani_handler_widget_test1.txt" ]]; then
    run_test "onWidget:do: creates handler file" "true" "true"
else
    run_test "onWidget:do: creates handler file" "true" "false"
fi

# Verify handler content
handler_content=$(cat /tmp/yutani_handler_widget_test1.txt 2>/dev/null)
run_test "Handler file contains correct handler" 'echo "handler executed"' "$handler_content"

# ============================================================================
# Remove Handler
# ============================================================================
echo ""
echo "--- Remove Handler ---"

@ $dispatcher2 removeHandlerFor: "widget_test1"

if [[ -f "/tmp/yutani_handler_widget_test1.txt" ]]; then
    run_test "removeHandlerFor: removes handler file" "false" "true"
else
    run_test "removeHandlerFor: removes handler file" "false" "false"
fi

# ============================================================================
# Global Event Handlers
# ============================================================================
echo ""
echo "--- Global Event Handlers ---"

dispatcher3=$(@ Yutani::EventDispatcher new)

@ $dispatcher3 onKeyDo: 'echo "key pressed"'
@ $dispatcher3 onMouseDo: 'echo "mouse clicked"'
@ $dispatcher3 onResizeDo: 'echo "window resized"'
@ $dispatcher3 onFocusDo: 'echo "focus changed"'

# Use accessor methods (auto-generated from instanceVars) to verify handlers
# These use _ivar which reads from env store (not db_get which reads SQLite)
key_handler=$(@ $dispatcher3 keyHandler)
mouse_handler=$(@ $dispatcher3 mouseHandler)
resize_handler=$(@ $dispatcher3 resizeHandler)
focus_handler=$(@ $dispatcher3 focusHandler)

run_test "onKeyDo: stores handler" 'echo "key pressed"' "$key_handler"
run_test "onMouseDo: stores handler" 'echo "mouse clicked"' "$mouse_handler"
run_test "onResizeDo: stores handler" 'echo "window resized"' "$resize_handler"
run_test "onFocusDo: stores handler" 'echo "focus changed"' "$focus_handler"

# ============================================================================
# Cleanup
# ============================================================================
echo ""
echo "--- Cleanup ---"

# Create some handler files
@ $dispatcher3 onWidget: "cleanup_test1" do: 'handler1'
@ $dispatcher3 onWidget: "cleanup_test2" do: 'handler2'

# Verify they exist
count_before=$(ls /tmp/yutani_handler_cleanup_*.txt 2>/dev/null | wc -l | tr -d ' ')
run_test "Handler files exist before cleanup" "2" "$count_before"

# Run cleanup
@ $dispatcher3 cleanup

# Files should be removed after cleanup
count_after=$(ls /tmp/yutani_handler_*.txt 2>/dev/null | wc -l | tr -d ' ')
run_test "Cleanup removes handler files" "0" "$count_after"

# ============================================================================
# Multiple Dispatchers Independence
# ============================================================================
echo ""
echo "--- Multiple Dispatchers Independence ---"

dispA=$(@ Yutani::EventDispatcher new)
dispB=$(@ Yutani::EventDispatcher new)

@ $dispA onKeyDo: 'handler A'
@ $dispB onKeyDo: 'handler B'

# Use accessor methods to retrieve handlers
handlerA=$(@ $dispA keyHandler)
handlerB=$(@ $dispB keyHandler)

run_test "Dispatcher A has correct key handler" "handler A" "$handlerA"
run_test "Dispatcher B has correct key handler" "handler B" "$handlerB"

# Manual cleanup
rm -f /tmp/yutani_handler_*.txt 2>/dev/null
rm -f /tmp/yutani_text_*.txt 2>/dev/null

# Print summary
print_summary
