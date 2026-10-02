#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test String splitToArray:on: and the Array constructors built on it

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

PASSED=0
FAILED=0

assert_eq() {
    if [[ "$2" == "$3" ]]; then
        echo "  PASS: $1"
        ((PASSED++)) || true
    else
        echo "  FAIL: $1 (expected: $2, got: $3)"
        ((FAILED++)) || true
    fi
}

newline=$'\n'

echo "=== String split Tests ==="
echo ""
echo "1. splitToArray:on: returns strings"

assert_eq "words split on space" '["a","b","c"]' "$(@ String splitToArray: 'a b c' on: ' ')"
assert_eq "numbers and booleans stay strings" '["1","true","null","2.5"]' \
    "$(@ String splitToArray: '1 true null 2.5' on: ' ')"
assert_eq "element starting with @ is not read as a file" '["@/etc/hosts"]' \
    "$(@ String splitToArray: '@/etc/hosts' on: ' ')"
assert_eq "empty input gives empty array" '[]' "$(@ String splitToArray: '' on: ' ')"
assert_eq "newline split keeps every line" '["x y","42"]' \
    "$(@ String splitToArray: "x y${newline}42" on: "$newline")"
assert_eq "custom delimiter" '["a b","c"]' "$(@ String splitToArray: 'a b,c' on: ',')"

echo ""
echo "2. Array constructors"

arr=$(@ Array fromLines: "1${newline}two")
assert_eq "fromLines: element reads back as text" "1" "$(@ "$arr" at: 0)"
assert_eq "fromLines: size" "2" "$(@ "$arr" size)"

echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
