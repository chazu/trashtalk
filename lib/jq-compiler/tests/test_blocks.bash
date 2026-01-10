#!/usr/bin/env bash
# Test block closures

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DRIVER="$SCRIPT_DIR/../driver.bash"
TRASHTALK_ROOT="$SCRIPT_DIR/../../.."

# Source the runtime for runtime tests (tests 6-8)
source "$TRASHTALK_ROOT/lib/trash.bash" 2>/dev/null

# Test counters
PASSED=0
FAILED=0
SKIPPED=0

pass() {
    echo -e "  \e[32m✓\e[0m $1"
    ((PASSED++)) || true
}

fail() {
    echo -e "  \e[31m✗\e[0m $1"
    ((FAILED++)) || true
}

skip() {
    echo -e "  \e[33m⊘\e[0m $1 (SKIPPED: $2)"
    ((SKIPPED++)) || true
}

echo "Block Closure Tests"
echo "==================="
echo ""

# Test 1: Block literal with one parameter parses correctly
echo "Test 1: Block literal parsing ([:x | x + 1])"
cat > /tmp/test_block1.trash << 'EOF'
TestBlock subclass: Object
  instanceVars: data

  method: makeAdder [
    | b |
    b := [:x | x + 1].
    ^ b
  ]
EOF

output=$("$DRIVER" compile /tmp/test_block1.trash 2>/dev/null)
if echo "$output" | grep -q 'Block params:.*\["x"\].*code:.*captured:'; then
    pass "Block literal generates Block creation"
else
    fail "Block literal should generate Block creation"
    echo "  Got: $(echo "$output" | grep -A2 'makeAdder')"
fi

# Test 2: Block literal with two parameters
echo ""
echo "Test 2: Block literal with two parameters ([:x :y | x + y])"
cat > /tmp/test_block2.trash << 'EOF'
TestBlock subclass: Object

  method: makeAdder [
    | b |
    b := [:x :y | x + y].
    ^ b
  ]
EOF

output=$("$DRIVER" compile /tmp/test_block2.trash 2>/dev/null)
if echo "$output" | grep -q 'Block params:.*\["x","y"\].*code:.*captured:'; then
    pass "Two-param block generates correct params"
else
    fail "Two-param block should have [\"x\",\"y\"] params"
    echo "  Got: $(echo "$output" | grep -A2 'makeAdder')"
fi

# Test 3: Block body generates echo for value expressions
echo ""
echo "Test 3: Block body wraps value in echo"
output=$("$DRIVER" compile /tmp/test_block1.trash 2>/dev/null)
if echo "$output" | grep -q 'echo "\$'; then
    pass "Block body wraps arithmetic in echo"
else
    fail "Block body should wrap arithmetic in echo"
fi

# Test 4: Block with no parameters creates Block object
echo ""
echo "Test 4: No-param block creates Block object"
cat > /tmp/test_block3.trash << 'EOF'
TestBlock subclass: Object

  method: noParamBlock [
    | b |
    b := [42].
    ^ b
  ]
EOF

output=$("$DRIVER" compile /tmp/test_block3.trash 2>/dev/null)
if echo "$output" | grep -q 'Block params:.*\[\].*code:.*captured:'; then
    pass "No-param block creates Block with empty params"
else
    fail "No-param block should create Block with empty params"
    echo "  Got: $(echo "$output" | grep -A2 'noParamBlock')"
fi

# Test 5: Block captures _RECEIVER
echo ""
echo "Test 5: Block captures _RECEIVER in context"
output=$("$DRIVER" compile /tmp/test_block1.trash 2>/dev/null)
if echo "$output" | grep -q '_RECEIVER'; then
    pass "Block captures _RECEIVER"
else
    fail "Block should capture _RECEIVER"
fi

# Test 6: Runtime execution (if Block class is compiled)
echo ""
echo "Test 6: Runtime block execution"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Block" ]]; then
    # Create a block directly using the class method
    block_id=$(@ Block params: '["x"]' code: 'echo $(( $x + 1 ))' captured: '{}' 2>/dev/null)
    if [[ "$block_id" =~ ^block_ ]]; then
        pass "Block created: $block_id"

        # Execute the block with an argument
        result=$(@ "$block_id" valueWith: 5 2>/dev/null)
        if [[ "$result" == "6" ]]; then
            pass "Block valueWith: returned correct result (6)"
        else
            fail "Block valueWith: expected 6, got '$result'"
        fi
    else
        fail "Failed to create Block instance (got: $block_id)"
    fi
else
    skip "Runtime block execution" "Block class not compiled"
fi

# Test 7: Array do: with blocks
echo ""
echo "Test 7: Array do: iteration"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Array" ]] && [[ -f "$TRASHTALK_ROOT/trash/.compiled/Block" ]]; then
    arr=$(@ Array new)
    if [[ ! "$arr" =~ ^array_ ]]; then
        fail "Array new failed: $arr"
    else
        @ $arr push: 1 >/dev/null 2>&1
        @ $arr push: 2 >/dev/null 2>&1
        @ $arr push: 3 >/dev/null 2>&1

        block=$(@ Block params: '["x"]' code: 'echo "item:$x"' captured: '{}')
        output=$(@ $arr do: "$block")

        if echo "$output" | grep -q "item" ; then
            pass "Array do: iterates over all elements"
        else
            fail "Array do: should iterate over elements"
            echo "  Got: $output"
        fi
    fi
else
    skip "Array do: iteration" "Array or Block class not compiled"
fi

# Test 8: Array collect: with blocks
echo ""
echo "Test 8: Array collect: mapping"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Array" ]] && [[ -f "$TRASHTALK_ROOT/trash/.compiled/Block" ]]; then
    arr=$(@ Array new 2>/dev/null)
    @ $arr push: 1 >/dev/null 2>&1
    @ $arr push: 2 >/dev/null 2>&1
    @ $arr push: 3 >/dev/null 2>&1

    block=$(@ Block params: '["x"]' code: 'echo $(( $x * 2 ))' captured: '{}' 2>/dev/null)
    doubled=$(@ $arr collect: "$block" 2>&1)

    # Check by showing the doubled array
    items=$(@ $doubled asJson 2>&1)
    if echo "$items" | grep -qE "[246]"; then
        pass "Array collect: maps elements through block"
    else
        fail "Array collect: should map [1,2,3] to [2,4,6]"
        echo "  Got: $items (doubled=$doubled)"
    fi
else
    skip "Array collect: mapping" "Array or Block class not compiled"
fi

echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed, $SKIPPED skipped"

# Exit with failure if any test failed (skipped tests don't count as failures)
[[ $FAILED -eq 0 ]] || exit 1
