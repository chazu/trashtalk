#!/usr/bin/env bash
# Tests for Method Aliasing

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

echo "=== Method Aliasing Tests ==="
echo ""

# Test 1: Parse alias directive
echo "Test 1: Parse alias directive"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: count [^ 42]
  alias: size for: count' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.aliases[0].aliasName')
if [[ "$result" == "size" ]]; then
    pass "Alias name parsed correctly"
else
    fail "Alias name parsing" "size" "$result"
fi

# Test 2: Parse alias original method
echo "Test 2: Parse alias original method"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: count [^ 42]
  alias: size for: count' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.aliases[0].originalMethod')
if [[ "$result" == "count" ]]; then
    pass "Original method parsed correctly"
else
    fail "Original method parsing" "count" "$result"
fi

# Test 3: Multiple aliases
echo "Test 3: Multiple aliases"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: count [^ 42]
  alias: size for: count
  alias: length for: count' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '[.aliases[].aliasName] | join(",")')
if [[ "$result" == "size,length" ]]; then
    pass "Multiple aliases parsed correctly"
else
    fail "Multiple aliases" "size,length" "$result"
fi

# Test 4: Codegen generates alias wrapper function
echo "Test 4: Codegen generates alias wrapper function"
cat > /tmp/TestAlias.trash << 'EOF'
TestAlias subclass: Object
  method: count [^ 42]
  alias: size for: count
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestAlias.trash 2>&1 | grep -A2 '__TestAlias__size()')
expected='__TestAlias__size() {
  __TestAlias__count "$@"
}'
if [[ "$result" == "$expected" ]]; then
    pass "Alias wrapper function generated correctly"
else
    fail "Alias wrapper" "$expected" "$result"
fi
rm -f /tmp/TestAlias.trash

# Test 5: Runtime - alias works
echo "Test 5: Runtime - alias works"
cat > /tmp/TestAliasRuntime.trash << 'EOF'
TestAliasRuntime subclass: Object

  classMethod: answer [
    ^ 42
  ]

  alias: theAnswer for: answer
  alias: ultimateAnswer for: answer
EOF

# Compile the test class
"$COMPILER_DIR/driver.bash" compile /tmp/TestAliasRuntime.trash > "$TRASHDIR/.compiled/TestAliasRuntime" 2>/dev/null
source "$TRASHDIR/.compiled/TestAliasRuntime" 2>/dev/null

if [[ -f "$TRASHDIR/.compiled/TestAliasRuntime" ]]; then
    # Call through alias
    result=$(@ TestAliasRuntime theAnswer)
    if [[ "$result" == "42" ]]; then
        pass "Alias method call works"
    else
        fail "Alias method call" "42" "$result"
    fi
else
    fail "Alias runtime" "test class to compile" "compilation failed"
fi

# Test 6: Both aliases work
echo "Test 6: Both aliases work"
if [[ -f "$TRASHDIR/.compiled/TestAliasRuntime" ]]; then
    result_answer=$(@ TestAliasRuntime answer)
    result_theAnswer=$(@ TestAliasRuntime theAnswer)
    result_ultimateAnswer=$(@ TestAliasRuntime ultimateAnswer)

    if [[ "$result_answer" == "$result_theAnswer" && "$result_theAnswer" == "$result_ultimateAnswer" ]]; then
        pass "Multiple aliases all return same value"
    else
        fail "Multiple aliases" "all equal" "answer=$result_answer, theAnswer=$result_theAnswer, ultimateAnswer=$result_ultimateAnswer"
    fi
fi

# Cleanup
rm -f /tmp/TestAliasRuntime.trash "$TRASHDIR/.compiled/TestAliasRuntime"

echo ""
echo "=== Results ==="
echo "Passed: $TESTS_PASSED / $TESTS_RUN"

if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
    exit 0
else
    exit 1
fi
