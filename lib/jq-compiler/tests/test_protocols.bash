#!/usr/bin/env bash
# Tests for Protocol implementation

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_DIR="$(dirname "$(dirname "$COMPILER_DIR")")"
TRASHDIR="$PROJECT_DIR/trash"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Test counter
TESTS_RUN=0
TESTS_PASSED=0

pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    echo "  Expected: $2"
    echo "  Got: $3"
    ((TESTS_RUN++))
}

# Load runtime
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

echo "=== Protocol Tests ==="
echo ""

# Test 1: Parse requires: with single keyword selector
echo "Test 1: Parse requires: with single keyword selector"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Protocol
  requires: do:' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methodRequirements[0]')
if [[ "$result" == "do:" ]]; then
    pass "Single keyword selector parsed correctly"
else
    fail "Single keyword selector parsing" "do:" "$result"
fi

# Test 2: Parse requires: with multi-keyword selector
echo "Test 2: Parse requires: with multi-keyword selector"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Protocol
  requires: inject: into:' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methodRequirements[0]')
if [[ "$result" == "inject:into:" ]]; then
    pass "Multi-keyword selector parsed correctly"
else
    fail "Multi-keyword selector parsing" "inject:into:" "$result"
fi

# Test 3: Parse multiple requires
echo "Test 3: Parse multiple requires"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Protocol
  requires: do:
  requires: collect:
  requires: inject: into:' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methodRequirements | length')
if [[ "$result" == "3" ]]; then
    pass "Multiple requires parsed correctly"
else
    fail "Multiple requires count" "3" "$result"
fi

# Test 4: Codegen generates __requires metadata
echo "Test 4: Codegen generates __requires metadata"
cat > /tmp/TestMeta.trash << 'EOF'
Test subclass: Protocol
  requires: do:
  requires: collect:
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestMeta.trash 2>&1 | grep '__Test__requires')
rm -f /tmp/TestMeta.trash
expected='__Test__requires="do: collect:"'
if [[ "$result" == "$expected" ]]; then
    pass "__requires metadata generated correctly"
else
    fail "__requires metadata" "$expected" "$result"
fi

# Test 5: Protocol.trash compiles correctly
echo "Test 5: Protocol.trash compiles correctly"
if [[ -f "$TRASHDIR/.compiled/Protocol" ]]; then
    if grep -q '__Protocol__class__isSatisfiedBy' "$TRASHDIR/.compiled/Protocol"; then
        pass "Protocol.trash compiled with isSatisfiedBy: method"
    else
        fail "Protocol.trash compilation" "isSatisfiedBy: method" "not found"
    fi
else
    fail "Protocol.trash compilation" "compiled file" "not found"
fi

# Test 6: _class_has_method finds instance method
echo "Test 6: _class_has_method finds instance method"
# Counter has 'increment' method
if _class_has_method "Counter" "increment"; then
    pass "_class_has_method finds instance method"
else
    fail "_class_has_method instance method" "found" "not found"
fi

# Test 7: _class_has_method finds keyword method
echo "Test 7: _class_has_method finds keyword method"
# Counter has 'incrementBy:' method
if _class_has_method "Counter" "incrementBy:"; then
    pass "_class_has_method finds keyword method"
else
    fail "_class_has_method keyword method" "found" "not found"
fi

# Test 8: _class_has_method returns false for missing method
echo "Test 8: _class_has_method returns false for missing method"
if ! _class_has_method "Counter" "nonexistentMethod"; then
    pass "_class_has_method returns false for missing method"
else
    fail "_class_has_method missing method" "not found" "found"
fi

# Test 9: Create and test a protocol
echo "Test 9: Create and test a protocol end-to-end"

# Create a test protocol
cat > /tmp/TestProtocol.trash << 'EOF'
Incrementable subclass: Protocol
  requires: increment
  requires: incrementBy:
EOF

# Create a test class that conforms
cat > /tmp/TestConforming.trash << 'EOF'
TestConforming subclass: Object
  instanceVars: value:0

  method: increment [
    | newValue |
    newValue := $(( $(_ivar value) + 1 )).
    _ivar_set value "$newValue"
  ]

  method: incrementBy: amount [
    | newValue |
    newValue := $(( $(_ivar value) + amount )).
    _ivar_set value "$newValue"
  ]
EOF

# Compile them
"$COMPILER_DIR/driver.bash" compile /tmp/TestProtocol.trash > "$TRASHDIR/.compiled/Incrementable" 2>/dev/null
"$COMPILER_DIR/driver.bash" compile /tmp/TestConforming.trash > "$TRASHDIR/.compiled/TestConforming" 2>/dev/null

# Source the compiled files
source "$TRASHDIR/.compiled/Incrementable"
source "$TRASHDIR/.compiled/TestConforming"

# Test conformance
result=$(_conforms_to "TestConforming" "Incrementable")
if [[ "$result" == "true" ]]; then
    pass "TestConforming conforms to Incrementable"
else
    fail "Protocol conformance" "true" "$result"
fi

# Test 10: Non-conforming class
echo "Test 10: Non-conforming class"

# Create a class that doesn't have all methods
cat > /tmp/TestNonConforming.trash << 'EOF'
TestNonConforming subclass: Object
  instanceVars: value:0

  method: increment [
    | newValue |
    newValue := $(( $(_ivar value) + 1 )).
    _ivar_set value "$newValue"
  ]
EOF

# Compile it
"$COMPILER_DIR/driver.bash" compile /tmp/TestNonConforming.trash > "$TRASHDIR/.compiled/TestNonConforming" 2>/dev/null
source "$TRASHDIR/.compiled/TestNonConforming"

# Test non-conformance (missing incrementBy:)
result=$(_conforms_to "TestNonConforming" "Incrementable")
if [[ "$result" == "false" ]]; then
    pass "TestNonConforming does not conform (missing incrementBy:)"
else
    fail "Non-conformance check" "false" "$result"
fi

# Cleanup
rm -f /tmp/TestProtocol.trash /tmp/TestConforming.trash /tmp/TestNonConforming.trash
rm -f "$TRASHDIR/.compiled/Incrementable" "$TRASHDIR/.compiled/TestConforming" "$TRASHDIR/.compiled/TestNonConforming"

echo ""
echo "=== Results ==="
echo "Passed: $TESTS_PASSED / $TESTS_RUN"

if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
    exit 0
else
    exit 1
fi
