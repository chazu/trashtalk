#!/usr/bin/env bash
# Test inherited instance variable handling in the jq-compiler
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER="$SCRIPT_DIR/../driver.bash"
TRASHTALK_DIR="${TRASHTALK_DIR:-$HOME/.trashtalk}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

pass() { echo -e "${GREEN}PASS${NC}: $1"; }
fail() { echo -e "${RED}FAIL${NC}: $1"; exit 1; }

# Create a temporary parent class
PARENT_SRC=$(mktemp)
PARENT_COMPILED=$(mktemp)
cat > "$PARENT_SRC" << 'EOF'
TestParent subclass: Object
  instanceVars: parentVar:100 parentName:''

  method: getParentVar [
    ^ parentVar
  ]

  method: setParentVar: v [
    parentVar := v
  ]
EOF

# Compile the parent class to the standard location
"$COMPILER" compile "$PARENT_SRC" -o "$TRASHTALK_DIR/trash/.compiled/TestParent" 2>/dev/null

# Create a child class that uses inherited ivars
CHILD_SRC=$(mktemp)
cat > "$CHILD_SRC" << 'EOF'
TestChild subclass: TestParent
  instanceVars: childVar:200

  # This method uses the parent's ivar
  method: useParentVar [
    | result |
    result := parentVar + childVar
    ^ result
  ]

  # This method modifies parent's ivar
  method: doubleParentVar [
    parentVar := parentVar * 2
    ^ parentVar
  ]

  method: getChildVar [
    ^ childVar
  ]
EOF

# Compile the child class
CHILD_OUTPUT=$("$COMPILER" compile "$CHILD_SRC" 2>&1)

# Test 1: Check that parentVar is recognized as an ivar in useParentVar
if echo "$CHILD_OUTPUT" | grep -q '\$(_ivar parentVar)'; then
    pass "parentVar recognized as inherited ivar (uses \$(_ivar parentVar))"
else
    echo "Output:"
    echo "$CHILD_OUTPUT" | grep -A5 '__TestChild__useParentVar' || true
    fail "parentVar NOT recognized as inherited ivar"
fi

# Test 2: Check that parentVar assignment uses _ivar_set
if echo "$CHILD_OUTPUT" | grep -q '_ivar_set parentVar'; then
    pass "parentVar assignment uses _ivar_set"
else
    echo "Output:"
    echo "$CHILD_OUTPUT" | grep -A5 '__TestChild__doubleParentVar' || true
    fail "parentVar assignment doesn't use _ivar_set"
fi

# Test 3: Check own ivar still works
if echo "$CHILD_OUTPUT" | grep -q '\$(_ivar childVar)'; then
    pass "childVar (own ivar) recognized correctly"
else
    fail "childVar (own ivar) NOT recognized"
fi

# Clean up
rm -f "$PARENT_SRC" "$CHILD_SRC" "$PARENT_COMPILED"
rm -f "$TRASHTALK_DIR/trash/.compiled/TestParent"

echo ""
echo "All inherited ivar tests passed!"
