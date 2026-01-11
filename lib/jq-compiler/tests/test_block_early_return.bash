#!/usr/bin/env bash
# Test: Block early return (^ value inside ifTrue: blocks)
#
# Tests that ^ (caret/return) works correctly inside inline control flow blocks.
# Note: Early return from blocks passed to methods like do: is a known limitation
# due to bash's inability to do non-local returns from eval'd code.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="${SCRIPT_DIR}/../../.."
DRIVER="$SCRIPT_DIR/../driver.bash"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

pass() { echo -e "  ${GREEN}✓${NC} $1"; }
fail() { echo -e "  ${RED}✗${NC} $1"; echo "    Expected: $2"; echo "    Got: $3"; }

echo "Block Early Return Tests"
echo "========================"

# Source runtime for runtime tests
cd "$TRASHTALK_ROOT"
source lib/trash.bash >/dev/null 2>&1

# Test 1: Simple ifTrue: with early return
echo ""
echo "Test 1: ifTrue: with early return"

cat > /tmp/test_early_return.trash << 'EOF'
TestEarlyReturn subclass: Object
  method: testPositive: n [
    (n > 0) ifTrue: [^ 'positive'].
    ^ 'not positive'
  ]

  method: testNegative: n [
    (n < 0) ifTrue: [^ 'negative'].
    ^ 'not negative'
  ]

  method: testEqual: n to: m [
    (n = m) ifTrue: [^ 'equal'].
    ^ 'not equal'
  ]
EOF

"$DRIVER" compile /tmp/test_early_return.trash > "$TRASHTALK_ROOT/trash/.compiled/TestEarlyReturn" 2>/dev/null

obj=$(@ TestEarlyReturn new 2>/dev/null)
result=$(@ $obj testPositive: 5 2>/dev/null)
if [[ "$result" == "positive" ]]; then
  pass "ifTrue: early return works (positive case)"
else
  fail "ifTrue: early return (positive case)" "positive" "$result"
fi

result=$(@ $obj testPositive: -3 2>/dev/null)
if [[ "$result" == "not positive" ]]; then
  pass "ifTrue: fallthrough works (negative case)"
else
  fail "ifTrue: fallthrough (negative case)" "not positive" "$result"
fi

# Test 2: ifTrue:ifFalse: with early returns in both branches
echo ""
echo "Test 2: ifTrue:ifFalse: with early returns"

cat > /tmp/test_dual_branch.trash << 'EOF'
TestDualBranch subclass: Object
  method: sign: n [
    (n > 0) ifTrue: [^ 'pos'] ifFalse: [^ 'non-pos']
  ]

  method: classify: n [
    (n > 0) ifTrue: [^ 'positive'].
    (n < 0) ifTrue: [^ 'negative'].
    ^ 'zero'
  ]
EOF

"$DRIVER" compile /tmp/test_dual_branch.trash > "$TRASHTALK_ROOT/trash/.compiled/TestDualBranch" 2>/dev/null

obj=$(@ TestDualBranch new 2>/dev/null)

result=$(@ $obj sign: 10 2>/dev/null)
if [[ "$result" == "pos" ]]; then
  pass "ifTrue:ifFalse: true branch early return"
else
  fail "ifTrue:ifFalse: true branch" "pos" "$result"
fi

result=$(@ $obj sign: -5 2>/dev/null)
if [[ "$result" == "non-pos" ]]; then
  pass "ifTrue:ifFalse: false branch early return"
else
  fail "ifTrue:ifFalse: false branch" "non-pos" "$result"
fi

# Test 3: Multiple chained conditionals
echo ""
echo "Test 3: Chained conditionals with early returns"

result=$(@ $obj classify: 5 2>/dev/null)
if [[ "$result" == "positive" ]]; then
  pass "Chained: positive case"
else
  fail "Chained: positive case" "positive" "$result"
fi

result=$(@ $obj classify: -5 2>/dev/null)
if [[ "$result" == "negative" ]]; then
  pass "Chained: negative case"
else
  fail "Chained: negative case" "negative" "$result"
fi

result=$(@ $obj classify: 0 2>/dev/null)
if [[ "$result" == "zero" ]]; then
  pass "Chained: zero case"
else
  fail "Chained: zero case" "zero" "$result"
fi

# Cleanup
rm -f /tmp/test_early_return.trash /tmp/test_dual_branch.trash
rm -f "$TRASHTALK_ROOT/trash/.compiled/TestEarlyReturn" "$TRASHTALK_ROOT/trash/.compiled/TestDualBranch"

echo ""
echo "========================"
echo "Block early return tests complete"
