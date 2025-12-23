#!/usr/bin/env bash
# Test Dictionary class functionality

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
            echo "✓ $name"
            ((TESTS_PASSED++))
        else
            echo "✗ $name"
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

echo "=== Dictionary Tests ==="
echo ""

# Test: Create a dictionary
echo "--- Basic Operations ---"
dict=$(@ Dictionary new)
run_test "Dictionary new creates instance" "dictionary_" "${dict:0:11}"

# Test: at:put: and at:
@ $dict at: 'name' put: 'Alice'
result=$(@ $dict at: 'name')
run_test "at:put: stores value, at: retrieves it" "Alice" "$result"

# Test: Multiple values
@ $dict at: 'age' put: '30'
@ $dict at: 'city' put: 'NYC'
run_test "at: age" "30" "$(@ $dict at: 'age')"
run_test "at: city" "NYC" "$(@ $dict at: 'city')"

# Test: size
run_test "size returns 3" "3" "$(@ $dict size)"

# Test: isEmpty
run_test "isEmpty returns false when populated" "false" "$(@ $dict isEmpty)"

# Test: includesKey:
run_test "includesKey: existing key" "true" "$(@ $dict includesKey: 'name')"
run_test "includesKey: missing key" "false" "$(@ $dict includesKey: 'missing')"

# Test: at:ifAbsent:
run_test "at:ifAbsent: existing key" "Alice" "$(@ $dict at: 'name' ifAbsent: 'default')"
run_test "at:ifAbsent: missing key" "default" "$(@ $dict at: 'missing' ifAbsent: 'default')"

# Test: keys
echo ""
echo "--- Keys and Values ---"
keys=$(@ $dict keys | sort | tr '\n' ',')
run_test "keys returns all keys" "age,city,name," "$keys"

# Test: values
values=$(@ $dict values | sort | tr '\n' ',')
run_test "values returns all values" "30,Alice,NYC," "$values"

# Test: removeAt:
echo ""
echo "--- Modification ---"
removed=$(@ $dict removeAt: 'city')
run_test "removeAt: returns old value" "NYC" "$removed"
run_test "size after remove" "2" "$(@ $dict size)"
run_test "removed key no longer exists" "false" "$(@ $dict includesKey: 'city')"

# Test: clear
@ $dict clear >/dev/null
run_test "isEmpty after clear" "true" "$(@ $dict isEmpty)"
run_test "size after clear" "0" "$(@ $dict size)"

# Test: withPairs:
echo ""
echo "--- Bulk Operations ---"
dict2=$(@ Dictionary new)
@ $dict2 withPairs: 'x:1 y:2 z:3' >/dev/null
run_test "withPairs: sets values" "1" "$(@ $dict2 at: 'x')"
run_test "withPairs: size" "3" "$(@ $dict2 size)"

# Test: merge:
echo ""
echo "--- Merge ---"
dictA=$(@ Dictionary new)
dictB=$(@ Dictionary new)
@ $dictA at: 'a' put: '1'
@ $dictA at: 'b' put: '2'
@ $dictB at: 'c' put: '3'
@ $dictB at: 'd' put: '4'
@ $dictA merge: $dictB >/dev/null
run_test "merge: size" "4" "$(@ $dictA size)"
run_test "merge: has 'c'" "true" "$(@ $dictA includesKey: 'c')"
run_test "merge: value of 'd'" "4" "$(@ $dictA at: 'd')"

# Test: asJson and fromJson:
echo ""
echo "--- JSON Serialization ---"
dictJ=$(@ Dictionary new)
@ $dictJ at: 'foo' put: 'bar'
json=$(@ $dictJ asJson)
# asJson produces compact JSON
run_test "asJson produces valid JSON" '{"foo":"bar"}' "$(echo "$json" | tr -d '\n')"

dictFromJson=$(@ Dictionary fromJson: '{"hello":"world"}')
run_test "fromJson: creates dict" "world" "$(@ $dictFromJson at: 'hello')"

# Print summary
print_summary
