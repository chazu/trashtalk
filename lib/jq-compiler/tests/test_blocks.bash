#!/bin/bash
# Test block closures

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DRIVER="$SCRIPT_DIR/../driver.bash"
TRASHTALK_ROOT="$SCRIPT_DIR/../../.."

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
if echo "$output" | grep -q 'Block params_code_captured.*\["x"\]'; then
    echo "  [32m✓[0m Block literal generates Block creation"
else
    echo "  [31m✗[0m Block literal should generate Block creation"
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
if echo "$output" | grep -q 'Block params_code_captured.*\["x","y"\]'; then
    echo "  [32m✓[0m Two-param block generates correct params"
else
    echo "  [31m✗[0m Two-param block should have [\"x\",\"y\"] params"
    echo "  Got: $(echo "$output" | grep -A2 'makeAdder')"
fi

# Test 3: Block body generates echo for value expressions
echo ""
echo "Test 3: Block body wraps value in echo"
output=$("$DRIVER" compile /tmp/test_block1.trash 2>/dev/null)
if echo "$output" | grep -q 'echo "\$'; then
    echo "  [32m✓[0m Block body wraps arithmetic in echo"
else
    echo "  [31m✗[0m Block body should wrap arithmetic in echo"
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
if echo "$output" | grep -q 'Block params_code_captured.*\[\]'; then
    echo "  [32m✓[0m No-param block creates Block with empty params"
else
    echo "  [31m✗[0m No-param block should create Block with empty params"
    echo "  Got: $(echo "$output" | grep -A2 'noParamBlock')"
fi

# Test 5: Block captures _RECEIVER
echo ""
echo "Test 5: Block captures _RECEIVER in context"
output=$("$DRIVER" compile /tmp/test_block1.trash 2>/dev/null)
if echo "$output" | grep -q '_RECEIVER'; then
    echo "  [32m✓[0m Block captures _RECEIVER"
else
    echo "  [31m✗[0m Block should capture _RECEIVER"
fi

# Test 6: Runtime execution (if Block class is compiled)
echo ""
echo "Test 6: Runtime block execution"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Block" ]]; then
    # Create a test that creates and executes a block
    source "$TRASHTALK_ROOT/lib/trash.bash" 2>/dev/null

    # Create a block directly using the class method
    block_id=$(@ Block params_code_captured '["x"]' 'echo $(( $x + 1 ))' '{}' 2>/dev/null)
    if [[ "$block_id" =~ ^block_ ]]; then
        echo "  [32m✓[0m Block created: $block_id"

        # Execute the block with an argument
        result=$(@ "$block_id" valueWith 5 2>/dev/null)
        if [[ "$result" == "6" ]]; then
            echo "  [32m✓[0m Block valueWith: returned correct result (6)"
        else
            echo "  [31m✗[0m Block valueWith: expected 6, got '$result'"
        fi
    else
        echo "  [31m✗[0m Failed to create Block instance (got: $block_id)"
    fi
else
    echo "  [33m![0m Block class not compiled, skipping runtime test"
fi

# Test 7: Array do: with blocks
echo ""
echo "Test 7: Array do: iteration"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Array" ]]; then
    arr=$(@ Array new 2>/dev/null)
    @ $arr push 1 2>/dev/null
    @ $arr push 2 2>/dev/null
    @ $arr push 3 2>/dev/null

    block=$(@ Block params_code_captured '["x"]' 'echo "item:$x"' '{}' 2>/dev/null)
    output=$(@ $arr do $block 2>/dev/null)

    if echo "$output" | grep -q "item.*1" && echo "$output" | grep -q "item.*2" && echo "$output" | grep -q "item.*3"; then
        echo "  [32m✓[0m Array do: iterates over all elements"
    else
        echo "  [31m✗[0m Array do: should iterate over elements"
        echo "  Got: $output"
    fi
else
    echo "  [33m![0m Array class not compiled, skipping test"
fi

# Test 8: Array collect: with blocks
echo ""
echo "Test 8: Array collect: mapping"
if [[ -f "$TRASHTALK_ROOT/trash/.compiled/Array" ]]; then
    arr=$(@ Array new 2>/dev/null)
    @ $arr push 1 2>/dev/null
    @ $arr push 2 2>/dev/null
    @ $arr push 3 2>/dev/null

    block=$(@ Block params_code_captured '["x"]' 'echo $(( $x * 2 ))' '{}' 2>/dev/null)
    doubled=$(@ $arr collect $block 2>/dev/null)

    # Check by showing the doubled array
    items=$(@ $doubled show 2>/dev/null)
    if echo "$items" | grep -q "2" && echo "$items" | grep -q "4" && echo "$items" | grep -q "6"; then
        echo "  [32m✓[0m Array collect: maps elements through block"
    else
        echo "  [31m✗[0m Array collect: should map [1,2,3] to [2,4,6]"
        echo "  Got: $items (doubled=$doubled)"
    fi
else
    echo "  [33m![0m Array class not compiled, skipping test"
fi

echo ""
echo "Done!"
