#!/usr/bin/env bash
# Test Yutani::Event class functionality
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

echo "=== Yutani::Event Tests ==="
echo ""

# ============================================================================
# Key Event Parsing
# ============================================================================
echo "--- Key Event Parsing ---"

KEY_JSON='{"key":{"key":"KEY_ENTER","modifiers":"CTRL"},"widget":{"widgetId":{"id":"wid_123"}}}'
keyEvent=$(@ Yutani::Event fromJson: "$KEY_JSON")

run_test "fromJson: creates key event instance" "yutani__event_" "${keyEvent:0:14}"

result=$(@ $keyEvent eventType)
run_test "Key event type is 'key'" "key" "$result"

result=$(@ $keyEvent isKeyEvent)
run_test "isKeyEvent returns true for key event" "true" "$result"

result=$(@ $keyEvent keyName)
run_test "keyName returns correct key" "KEY_ENTER" "$result"

result=$(@ $keyEvent keyMods)
run_test "keyMods returns correct modifiers" "CTRL" "$result"

result=$(@ $keyEvent widgetId)
run_test "widgetId returns widget ID from key event" "wid_123" "$result"

# ============================================================================
# Key Helper Methods
# ============================================================================
echo ""
echo "--- Key Helper Methods ---"

ENTER_JSON='{"key":{"key":"KEY_ENTER"}}'
enterEvent=$(@ Yutani::Event fromJson: "$ENTER_JSON")

result=$(@ $enterEvent isEnterKey)
run_test "isEnterKey returns true for KEY_ENTER" "true" "$result"

result=$(@ $enterEvent isEscapeKey)
run_test "isEscapeKey returns false for KEY_ENTER" "false" "$result"

result=$(@ $enterEvent isKey: KEY_ENTER)
run_test "isKey: KEY_ENTER returns true" "true" "$result"

# ============================================================================
# Widget Event Parsing
# ============================================================================
echo ""
echo "--- Widget Event Parsing ---"

WIDGET_JSON='{"widget":{"widgetId":{"id":"input_456"},"type":"WIDGET_CHANGED","data":{"text":"hello world"}}}'
widgetEvent=$(@ Yutani::Event fromJson: "$WIDGET_JSON")

result=$(@ $widgetEvent eventType)
run_test "Widget event type is 'widget'" "widget" "$result"

result=$(@ $widgetEvent isWidgetEvent)
run_test "isWidgetEvent returns true for widget event" "true" "$result"

result=$(@ $widgetEvent widgetId)
run_test "widgetId returns correct ID" "input_456" "$result"

result=$(@ $widgetEvent widgetEventType)
run_test "widgetEventType returns WIDGET_CHANGED" "WIDGET_CHANGED" "$result"

# ============================================================================
# Widget Submission Detection
# ============================================================================
echo ""
echo "--- Widget Submission Detection ---"

DONE_JSON='{"widget":{"widgetId":{"id":"input_789"},"type":"WIDGET_DONE","data":{"text":"submitted"}}}'
doneEvent=$(@ Yutani::Event fromJson: "$DONE_JSON")

result=$(@ $doneEvent isSubmission)
run_test "isSubmission returns true for WIDGET_DONE" "true" "$result"

# ============================================================================
# Mouse Event Parsing (known bug: field misalignment in jq template)
# ============================================================================
echo ""
echo "--- Mouse Event Parsing ---"

MOUSE_JSON='{"mouse":{"x":100,"y":200,"button":"LEFT"}}'
mouseEvent=$(@ Yutani::Event fromJson: "$MOUSE_JSON")

result=$(@ $mouseEvent eventType)
run_test "Mouse event type is 'mouse'" "mouse" "$result"

result=$(@ $mouseEvent keyMods)
run_test "keyMods is empty for mouse event" "" "$result"

result=$(@ $mouseEvent mouseX)
run_test "mouseX returns correct X coordinate" "100" "$result"

result=$(@ $mouseEvent mouseY)
run_test "mouseY returns correct Y coordinate" "200" "$result"

result=$(@ $mouseEvent mouseButton)
run_test "mouseButton returns correct button" "LEFT" "$result"

# ============================================================================
# Resize Event Parsing
# ============================================================================
echo ""
echo "--- Resize Event Parsing ---"

RESIZE_JSON='{"resize":{"width":1920,"height":1080}}'
resizeEvent=$(@ Yutani::Event fromJson: "$RESIZE_JSON")

result=$(@ $resizeEvent eventType)
run_test "Resize event type is 'resize'" "resize" "$result"

result=$(@ $resizeEvent isResizeEvent)
run_test "isResizeEvent returns true for resize event" "true" "$result"

# ============================================================================
# Focus Event Parsing
# ============================================================================
echo ""
echo "--- Focus Event Parsing ---"

FOCUS_JSON='{"focus":{"widgetId":"focused_widget"}}'
focusEvent=$(@ Yutani::Event fromJson: "$FOCUS_JSON")

result=$(@ $focusEvent eventType)
run_test "Focus event type is 'focus'" "focus" "$result"

result=$(@ $focusEvent isFocusEvent)
run_test "isFocusEvent returns true for focus event" "true" "$result"

# Print summary
print_summary
