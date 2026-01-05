#!/usr/bin/env bash
# Test that rawMethod assignments compile with correct spacing
# Regression test for: keys =false -> keys=false bug

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
DRIVER="$COMPILER_DIR/driver.bash"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

PASSED=0
FAILED=0

pass() {
    echo -e "${GREEN}✓${NC} $1"
    PASSED=$((PASSED + 1))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    FAILED=$((FAILED + 1))
}

echo "=== rawMethod Assignment Spacing Tests ==="
echo ""

# Create temp file for test class
TMPFILE=$(mktemp /tmp/test_rawmethod_XXXXXX.trash)
trap "rm -f $TMPFILE" EXIT

# Test 1: Simple variable assignment in rawMethod
echo "Test 1: Simple assignment (var=value)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local x=5
    x=10
    echo "$x"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local x=5'; then
    pass "local x=5 compiled correctly"
else
    fail "local x=5 has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local x')"
fi

# Test 2: Multiple assignments on one line
echo ""
echo "Test 2: Multiple assignments (local a=1 b=2 c=3)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local a=1 b=2 c=3
    echo "$a $b $c"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local a=1 b=2 c=3'; then
    pass "Multiple assignments compiled correctly"
else
    fail "Multiple assignments have incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local a')"
fi

# Test 3: Boolean assignments (the original bug case)
echo ""
echo "Test 3: Boolean assignments (keys=false keys=true)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local keys=false mouse=false
    keys=true
    mouse=true
    echo "$keys $mouse"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local keys=false mouse=false'; then
    pass "Boolean local declarations compiled correctly"
else
    fail "Boolean local declarations have incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local keys')"
fi

if echo "$OUTPUT" | grep -q 'keys=true'; then
    pass "Boolean reassignment compiled correctly"
else
    fail "Boolean reassignment has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'keys=true\|keys =true')"
fi

# Test 4: Assignment with string value
echo ""
echo "Test 4: String assignment (name=\"value\")"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local name="hello"
    name="world"
    echo "$name"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local name="hello"'; then
    pass "String local declaration compiled correctly"
else
    fail "String local declaration has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local name')"
fi

if echo "$OUTPUT" | grep -q 'name="world"'; then
    pass "String reassignment compiled correctly"
else
    fail "String reassignment has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'name="world"\|name ="world"')"
fi

# Test 5: Assignment with command substitution
echo ""
echo "Test 5: Command substitution assignment (result=\$(cmd))"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local result=$(echo "test")
    result=$(echo "updated")
    echo "$result"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local result=\$(echo'; then
    pass "Command substitution local compiled correctly"
else
    fail "Command substitution local has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local result')"
fi

if echo "$OUTPUT" | grep -q 'result=\$(echo "updated")'; then
    pass "Command substitution reassignment compiled correctly"
else
    fail "Command substitution reassignment has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'result=\$(echo "updated")\|result =\$(echo')"
fi

# Test 6: Assignment in conditional (the exact bug pattern)
echo ""
echo "Test 6: Assignment after && (condition && var=value)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local enabled=false
    [[ "$1" == "yes" ]] && enabled=true
    echo "$enabled"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q '\&\& enabled=true'; then
    pass "Conditional assignment compiled correctly"
else
    fail "Conditional assignment has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'enabled=true\|enabled =true')"
fi

# Test 7: Assignment with variable reference
echo ""
echo "Test 7: Variable reference assignment (x=\$y)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local y=5
    local x=$y
    x=$y
    echo "$x"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if echo "$OUTPUT" | grep -q 'local x=\$y'; then
    pass "Variable reference local compiled correctly"
else
    fail "Variable reference local has incorrect spacing"
    echo "  Got: $(echo "$OUTPUT" | grep 'local x=\|local x =')"
fi

# Test 8: Glob pattern in conditional (original bug trigger)
echo ""
echo "Test 8: Glob pattern spacing in [[ ]] (the original bug)"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: test [
    local eventTypes="$1"
    local keys=false
    [[ "$eventTypes" == *"keys"* ]] && keys=true
    echo "$keys"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
# Check for the BROKEN pattern: *"keys" * (space before closing *)
if echo "$OUTPUT" | grep -q '\*"keys" \*'; then
    fail "Glob pattern has broken spacing (*\"keys\" *)"
    echo "  Got: $(echo "$OUTPUT" | grep 'keys')"
else
    pass "Glob pattern spacing is correct"
fi

# Also verify bash -n passes
SYNTAX_CHECK=$(echo "$OUTPUT" | bash -n 2>&1) || true
if [[ -z "$SYNTAX_CHECK" ]]; then
    pass "Glob pattern compiles to valid bash"
else
    fail "Glob pattern produces syntax error"
    echo "  Error: $SYNTAX_CHECK"
fi

# Test 9: Syntax validation of compiled output
echo ""
echo "Test 9: Compiled output has valid bash syntax"
cat > "$TMPFILE" << 'EOF'
TestAssign subclass: Object
  rawMethod: complexTest [
    local a=1 b=2 c=3
    local flag=false enabled=true
    local name="test" value="$1"
    [[ -n "$value" ]] && flag=true
    a=$((b + c))
    echo "$a $flag $name"
  ]
EOF

OUTPUT=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
SYNTAX_CHECK=$(echo "$OUTPUT" | bash -n 2>&1) || true
if [[ -z "$SYNTAX_CHECK" ]]; then
    pass "Compiled output has valid bash syntax"
else
    fail "Compiled output has syntax errors"
    echo "  Error: $SYNTAX_CHECK"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [[ $FAILED -gt 0 ]]; then
    exit 1
fi
