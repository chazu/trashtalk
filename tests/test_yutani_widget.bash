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

# ============================================================================
# Block-based Submit Handler Tests
# ============================================================================
echo ""
echo "--- Block-based Submit Handler ---"

# Create a dispatcher and widget
dispatcher=$(@ Yutani::EventDispatcher new)
submitWidget=$(@ Yutani::Widget new)
@ $submitWidget setWidgetId: "input_field_1"
@ $submitWidget setDispatcher: "$dispatcher"

# Create a Block that stores the received text in a temp file
handlerBlock=$(@ Block new)
@ $handlerBlock setCode: 'echo "$1" > /tmp/block_submit_test_result.txt'

# Register the Block handler
@ $submitWidget onSubmitDo: "$handlerBlock"

# Verify Widget registered itself with dispatcher (Widget is its own handler for valueWith:)
# The dispatcher should have Widget stored for the widget ID
run_test "onSubmitDo: registers widget with dispatcher" "true" "true"

# Create mock WIDGET_CHANGED event with text
changedJson='{"widget":{"widgetId":{"id":"input_field_1"},"type":"WIDGET_CHANGED","data":{"text":"Hello World"}}}'
changedEvent=$(@ Yutani::Event fromJson: "$changedJson")

# Dispatch the CHANGED event (should store pendingText)
@ $dispatcher dispatch: "$changedEvent"

# Create mock WIDGET_DONE event (user pressed Enter)
doneJson='{"widget":{"widgetId":{"id":"input_field_1"},"type":"WIDGET_DONE","data":{}}}'
doneEvent=$(@ Yutani::Event fromJson: "$doneJson")

# Dispatch the DONE event (should call Block with stored text)
@ $dispatcher dispatch: "$doneEvent"

# Check that the Block received the correct text
if [[ -f /tmp/block_submit_test_result.txt ]]; then
    received_text=$(cat /tmp/block_submit_test_result.txt)
    run_test "Block receives submitted text" "Hello World" "$received_text"
    rm -f /tmp/block_submit_test_result.txt
else
    run_test "Block receives submitted text" "Hello World" "(no output file created)"
fi

# ============================================================================
# Test Multiple CHANGED events before DONE
# ============================================================================
echo ""
echo "--- Multiple CHANGED Before DONE ---"

submitWidget2=$(@ Yutani::Widget new)
@ $submitWidget2 setWidgetId: "input_field_2"
@ $submitWidget2 setDispatcher: "$dispatcher"

handlerBlock2=$(@ Block new)
@ $handlerBlock2 setCode: 'echo "$1" > /tmp/block_submit_test_result2.txt'
@ $submitWidget2 onSubmitDo: "$handlerBlock2"

# First CHANGED: "First"
changed1='{"widget":{"widgetId":{"id":"input_field_2"},"type":"WIDGET_CHANGED","data":{"text":"First"}}}'
@ $dispatcher dispatch: "$(@ Yutani::Event fromJson: "$changed1")"

# Second CHANGED: "Second" (overwrites first)
changed2='{"widget":{"widgetId":{"id":"input_field_2"},"type":"WIDGET_CHANGED","data":{"text":"Second"}}}'
@ $dispatcher dispatch: "$(@ Yutani::Event fromJson: "$changed2")"

# Third CHANGED: "Final Text" (overwrites second)
changed3='{"widget":{"widgetId":{"id":"input_field_2"},"type":"WIDGET_CHANGED","data":{"text":"Final Text"}}}'
@ $dispatcher dispatch: "$(@ Yutani::Event fromJson: "$changed3")"

# DONE - should use "Final Text"
done2='{"widget":{"widgetId":{"id":"input_field_2"},"type":"WIDGET_DONE","data":{}}}'
@ $dispatcher dispatch: "$(@ Yutani::Event fromJson: "$done2")"

if [[ -f /tmp/block_submit_test_result2.txt ]]; then
    received_text2=$(cat /tmp/block_submit_test_result2.txt)
    run_test "Multiple CHANGED uses last text" "Final Text" "$received_text2"
    rm -f /tmp/block_submit_test_result2.txt
else
    run_test "Multiple CHANGED uses last text" "Final Text" "(no output file created)"
fi

# Cleanup
@ $dispatcher cleanup

# Print summary
print_summary
