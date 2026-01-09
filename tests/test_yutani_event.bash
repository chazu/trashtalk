#!/usr/bin/env bash
# Test Yutani::Event class functionality
# Regression tests for native compilation refactoring
#
# NOTE: Some tests document KNOWN BUGS in current behavior:
# - Comparison methods (isKeyEvent, etc.) return expression strings instead of true/false
# - Mouse event parsing has field misalignment due to jq template bug

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

# Note: isKeyEvent has known bug - returns expression string instead of true/false
result=$(@ $keyEvent isKeyEvent)
run_test "isKeyEvent (known bug: returns expression)" "eventType == key" "$result"

result=$(@ $keyEvent keyName)
run_test "keyName returns correct key" "KEY_ENTER" "$result"

result=$(@ $keyEvent keyMods)
run_test "keyMods returns correct modifiers" "CTRL" "$result"

result=$(@ $keyEvent widgetId)
run_test "widgetId returns widget ID from key event" "wid_123" "$result"

# ============================================================================
# Key Helper Methods (document current buggy behavior)
# ============================================================================
echo ""
echo "--- Key Helper Methods ---"

ENTER_JSON='{"key":{"key":"KEY_ENTER"}}'
enterEvent=$(@ Yutani::Event fromJson: "$ENTER_JSON")

# These methods have bugs - they return expression strings or shell comparison exit codes
result=$(@ $enterEvent isEnterKey)
run_test "isEnterKey (known bug: returns expression)" "keyName == KEY_ENTER" "$result"

result=$(@ $enterEvent isEscapeKey)
run_test "isEscapeKey for KEY_ENTER (known bug: returns expression)" "keyName == KEY_ESC" "$result"

# isKey: returns bash comparison exit code (1 for false, 0 for true) but captured as string
result=$(@ $enterEvent isKey: KEY_ENTER)
run_test "isKey: KEY_ENTER (known bug: returns exit code)" "1" "$result"

# ============================================================================
# Widget Event Parsing
# ============================================================================
echo ""
echo "--- Widget Event Parsing ---"

WIDGET_JSON='{"widget":{"widgetId":{"id":"input_456"},"type":"WIDGET_CHANGED","data":{"text":"hello world"}}}'
widgetEvent=$(@ Yutani::Event fromJson: "$WIDGET_JSON")

result=$(@ $widgetEvent eventType)
run_test "Widget event type is 'widget'" "widget" "$result"

# Known bug: returns expression string
result=$(@ $widgetEvent isWidgetEvent)
run_test "isWidgetEvent (known bug: returns expression)" "eventType == widget" "$result"

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

# isSubmission has known bug - returns bash comparison exit code
result=$(@ $doneEvent isSubmission)
run_test "isSubmission (known bug: returns exit code)" "1" "$result"

# ============================================================================
# Mouse Event Parsing (known bug: field misalignment in jq template)
# ============================================================================
echo ""
echo "--- Mouse Event Parsing (known bug: fields misaligned) ---"

MOUSE_JSON='{"mouse":{"x":100,"y":200,"button":"LEFT"}}'
mouseEvent=$(@ Yutani::Event fromJson: "$MOUSE_JSON")

result=$(@ $mouseEvent eventType)
run_test "Mouse event type is 'mouse'" "mouse" "$result"

# Known bug: fields are shifted due to missing pipe in jq template
# mouseX ends up in keyMods slot, etc.
result=$(@ $mouseEvent keyMods)
run_test "keyMods contains mouseX (known bug: misalignment)" "100" "$result"

result=$(@ $mouseEvent mouseX)
run_test "mouseX contains mouseY (known bug: misalignment)" "200" "$result"

result=$(@ $mouseEvent mouseY)
run_test "mouseY contains button (known bug: misalignment)" "LEFT" "$result"

result=$(@ $mouseEvent mouseButton)
run_test "mouseButton is empty (known bug: misalignment)" "" "$result"

# ============================================================================
# Resize Event Parsing
# ============================================================================
echo ""
echo "--- Resize Event Parsing ---"

RESIZE_JSON='{"resize":{"width":1920,"height":1080}}'
resizeEvent=$(@ Yutani::Event fromJson: "$RESIZE_JSON")

result=$(@ $resizeEvent eventType)
run_test "Resize event type is 'resize'" "resize" "$result"

# Known bug: returns expression string
result=$(@ $resizeEvent isResizeEvent)
run_test "isResizeEvent (known bug: returns expression)" "eventType == resize" "$result"

# ============================================================================
# Focus Event Parsing
# ============================================================================
echo ""
echo "--- Focus Event Parsing ---"

FOCUS_JSON='{"focus":{"widgetId":"focused_widget"}}'
focusEvent=$(@ Yutani::Event fromJson: "$FOCUS_JSON")

result=$(@ $focusEvent eventType)
run_test "Focus event type is 'focus'" "focus" "$result"

# Known bug: returns expression string
result=$(@ $focusEvent isFocusEvent)
run_test "isFocusEvent (known bug: returns expression)" "eventType == focus" "$result"

# Print summary
print_summary
