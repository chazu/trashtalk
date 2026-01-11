#!/usr/bin/env bash
# Test detect: and inject:into: collection methods

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="$SCRIPT_DIR/.."

export TRASHTALK_DISABLE_NATIVE=1
source "$TRASHTALK_ROOT/lib/trash.bash"

echo "=== Collection Methods Tests ==="
echo ""

PASSED=0
FAILED=0

pass() {
    echo -e "  \e[32m✓\e[0m $1"
    ((PASSED++)) || true
}

fail() {
    echo -e "  \e[31m✗\e[0m $1"
    ((FAILED++)) || true
}

# Create test array with numbers
arr=$(@ Array new)
@ "$arr" push: "1" >/dev/null
@ "$arr" push: "2" >/dev/null
@ "$arr" push: "3" >/dev/null
@ "$arr" push: "4" >/dev/null
@ "$arr" push: "5" >/dev/null

echo "Array contents: $(@ "$arr" asJson | jq -c .items)"
echo ""

echo "--- detect: tests ---"

# Test detect: - find first element > 3
block=$(@ Block params: '["x"]' code: '(( $x > 3 )) && echo "true"' captured: '{}')
found=$(@ "$arr" detect: "$block")
if [[ "$found" == "4" ]]; then
    pass "detect: finds first element > 3"
else
    fail "detect: expected '4', got '$found'"
fi

# Test detect: - no match
block=$(@ Block params: '["x"]' code: '(( $x > 10 )) && echo "true"' captured: '{}')
found=$(@ "$arr" detect: "$block")
if [[ "$found" == "" ]]; then
    pass "detect: returns empty when no match"
else
    fail "detect: no match expected '', got '$found'"
fi

# Test detect:ifNone: - with match
block=$(@ Block params: '["x"]' code: '(( $x > 3 )) && echo "true"' captured: '{}')
defaultBlock=$(@ Block params: '[]' code: 'echo "none"' captured: '{}')
found=$(@ "$arr" detect: "$block" ifNone: "$defaultBlock")
if [[ "$found" == "4" ]]; then
    pass "detect:ifNone: finds match, ignores default"
else
    fail "detect:ifNone: with match expected '4', got '$found'"
fi

# Test detect:ifNone: - no match
block=$(@ Block params: '["x"]' code: '(( $x > 10 )) && echo "true"' captured: '{}')
defaultBlock=$(@ Block params: '[]' code: 'echo "not found"' captured: '{}')
found=$(@ "$arr" detect: "$block" ifNone: "$defaultBlock")
if [[ "$found" == "not found" ]]; then
    pass "detect:ifNone: uses default when no match"
else
    fail "detect:ifNone: no match expected 'not found', got '$found'"
fi

echo ""
echo "--- inject:into: tests ---"

# Test inject:into: - sum
block=$(@ Block params: '["acc","x"]' code: 'echo $(( $acc + $x ))' captured: '{}')
sum=$(@ "$arr" inject: "0" into: "$block")
if [[ "$sum" == "15" ]]; then
    pass "inject:into: sum of 1-5 = 15"
else
    fail "inject:into: sum expected '15', got '$sum'"
fi

# Test inject:into: - product
block=$(@ Block params: '["acc","x"]' code: 'echo $(( $acc * $x ))' captured: '{}')
product=$(@ "$arr" inject: "1" into: "$block")
if [[ "$product" == "120" ]]; then
    pass "inject:into: product of 1-5 = 120"
else
    fail "inject:into: product expected '120', got '$product'"
fi

# Test inject:into: - max (trim whitespace from bash subprocess output)
block=$(@ Block params: '["acc","x"]' code: '(( $x > $acc )) && echo "$x" || echo "$acc"' captured: '{}')
max=$(@ "$arr" inject: "0" into: "$block")
max="${max// /}"  # Remove whitespace artifact from bash subprocess capture
if [[ "$max" == "5" ]]; then
    pass "inject:into: max of 1-5 = 5"
else
    fail "inject:into: max expected '5', got '$max'"
fi

echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed"

[[ $FAILED -eq 0 ]] || exit 1
