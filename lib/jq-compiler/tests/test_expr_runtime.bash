#!/usr/bin/env bash
# Runtime tests for the Smalltalk-style expression parser
# These tests verify that compiled code actually executes correctly

_TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_COMPILER_DIR="$(dirname "$_TEST_DIR")"
_TRASH_ROOT="$(dirname "$(dirname "$_COMPILER_DIR")")"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

PASSED=0
FAILED=0

pass() {
    echo -e "  ${GREEN}✓${NC} $1"
    PASSED=$((PASSED + 1))
}

fail() {
    echo -e "  ${RED}✗${NC} $1"
    echo -e "    Expected: $2"
    echo -e "    Got:      $3"
    FAILED=$((FAILED + 1))
}

# Load trash runtime
if ! source "$_TRASH_ROOT/lib/trash.bash" 2>/dev/null; then
    echo "Failed to load trash.bash from $_TRASH_ROOT/lib/trash.bash"
    exit 1
fi

# Compile the test class
"$_COMPILER_DIR/driver.bash" compile "$_TEST_DIR/test_expr.trash" > /tmp/ExprTest.bash 2>/dev/null

# Source the compiled class
if ! source /tmp/ExprTest.bash; then
    echo "Failed to source /tmp/ExprTest.bash"
    exit 1
fi

# Verify functions loaded
if ! declare -F __ExprTest__testArithmetic > /dev/null; then
    echo "ERROR: ExprTest methods not loaded"
    exit 1
fi

# Helper to create fresh instance for each test
new_instance() {
    local id
    id=$(_generate_instance_id ExprTest)
    _create_instance ExprTest "$id"
    echo "$id"
}

# Helper to run a method and capture output
run_method() {
    local instance="$1"
    local method="$2"
    _RECEIVER="$instance"
    __ExprTest__${method} 2>/dev/null
}

echo "Expression Parser Runtime Tests"
echo "================================"
echo ""

# === ARITHMETIC TESTS ===
echo "Arithmetic Tests:"

# Test 1: Basic arithmetic (value=10, step=2 -> 10+2=12)
id=$(new_instance)
result=$(run_method "$id" testArithmetic)
[[ "$result" == "12" ]] && pass "testArithmetic: value + step = 12" || fail "testArithmetic" "12" "$result"

# Test 2: Precedence (10 + 2*2 = 14, not 24)
id=$(new_instance)
result=$(run_method "$id" testPrecedence)
[[ "$result" == "14" ]] && pass "testPrecedence: value + step * 2 = 14" || fail "testPrecedence" "14" "$result"

# Test 3: All operators
id=$(new_instance)
result=$(run_method "$id" testAllOperators)
[[ "$result" == "25" ]] && pass "testAllOperators: 20 + 5 = 25" || fail "testAllOperators" "25" "$result"

# Test 4: Parentheses override precedence ((10+2)*2 = 24)
id=$(new_instance)
result=$(run_method "$id" testParentheses)
[[ "$result" == "24" ]] && pass "testParentheses: (value + step) * 2 = 24" || fail "testParentheses" "24" "$result"

# Test 5: Unary minus
id=$(new_instance)
result=$(run_method "$id" testUnaryMinus)
[[ "$result" == "-5" ]] && pass "testUnaryMinus: x := -5" || fail "testUnaryMinus" "-5" "$result"

# Test 6: Chained arithmetic
id=$(new_instance)
result=$(run_method "$id" testChainedArithmetic)
[[ "$result" == "10" ]] && pass "testChainedArithmetic: 1 + 2 + 3 + 4 = 10" || fail "testChainedArithmetic" "10" "$result"

echo ""
echo "Assignment Tests:"

# Test 7: Local assignment chain (10, 10+2=12, 12*2=24)
id=$(new_instance)
result=$(run_method "$id" testLocalAssignment)
[[ "$result" == "24" ]] && pass "testLocalAssignment: (value + step) * 2 = 24" || fail "testLocalAssignment" "24" "$result"

# Test 8: Ivar assignment (10 + 5 = 15)
id=$(new_instance)
result=$(run_method "$id" testIvarAssignment)
[[ "$result" == "15" ]] && pass "testIvarAssignment: value := value + 5 -> 15" || fail "testIvarAssignment" "15" "$result"

# Test 9: Multiple ivar assignments (step=3, value=10+3=13)
id=$(new_instance)
result=$(run_method "$id" testMultipleIvarAssignment)
[[ "$result" == "13" ]] && pass "testMultipleIvarAssignment: step++, value += step -> 13" || fail "testMultipleIvarAssignment" "13" "$result"

# Test 10: Mixed local and ivar (temp=20, value=20*2=40)
id=$(new_instance)
result=$(run_method "$id" testMixedAssignment)
[[ "$result" == "40" ]] && pass "testMixedAssignment: temp := value + 10; value := temp * step -> 40" || fail "testMixedAssignment" "40" "$result"

echo ""
echo "Message Send Tests:"

# Test 11: Unary message (returns old value before reset)
id=$(new_instance)
result=$(run_method "$id" testUnaryMessage)
[[ "$result" == "10" ]] && pass "testUnaryMessage: result := value; @ self reset -> returns 10" || fail "testUnaryMessage" "10" "$result"

# Test 12: Keyword message (returns v before setValue)
id=$(new_instance)
result=$(run_method "$id" testKeywordMessage)
[[ "$result" == "10" ]] && pass "testKeywordMessage: @ self setValue: v + 1 -> returns 10" || fail "testKeywordMessage" "10" "$result"

# Test 13: Multi-keyword message
id=$(new_instance)
result=$(run_method "$id" testMultiKeywordMessage)
[[ "$result" == "5" ]] && pass "testMultiKeywordMessage: @ self at: x + 1 put: y * 2 -> returns 5" || fail "testMultiKeywordMessage" "5" "$result"

echo ""
echo "Cascade Tests:"

# Test 14: Cascade-style local increment (x := 0; x := x + 1 three times)
id=$(new_instance)
result=$(run_method "$id" testCascade)
[[ "$result" == "3" ]] && pass "testCascade: x := x + 1 (3 times) -> 3" || fail "testCascade" "3" "$result"

# Test 15: Cascade-style arithmetic (5 + 10 + 15 = 30)
id=$(new_instance)
result=$(run_method "$id" testCascadeKeyword)
[[ "$result" == "30" ]] && pass "testCascadeKeyword: 5 + 10 + 15 -> 30" || fail "testCascadeKeyword" "30" "$result"

echo ""
echo "Return Tests:"

# Test 16: Return local
id=$(new_instance)
result=$(run_method "$id" testReturnLocal)
[[ "$result" == "42" ]] && pass "testReturnLocal: ^ 42" || fail "testReturnLocal" "42" "$result"

# Test 17: Return ivar
id=$(new_instance)
result=$(run_method "$id" testReturnIvar)
[[ "$result" == "10" ]] && pass "testReturnIvar: ^ value" || fail "testReturnIvar" "10" "$result"

# Test 18: Return self (should return instance ID)
id=$(new_instance)
result=$(run_method "$id" testReturnSelf)
[[ "$result" == "$id" ]] && pass "testReturnSelf: ^ self" || fail "testReturnSelf" "$id" "$result"

# Test 19: Return expression
id=$(new_instance)
result=$(run_method "$id" testReturnExpression)
[[ "$result" == "12" ]] && pass "testReturnExpression: ^ value + step" || fail "testReturnExpression" "12" "$result"

echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed"

[[ $FAILED -eq 0 ]] || exit 1
