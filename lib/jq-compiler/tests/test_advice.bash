#!/usr/bin/env bash
# Tests for Before/After Hooks (Method Advice)

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

echo "=== Before/After Hooks Tests ==="
echo ""

# Test 1: Parse before: directive
echo "Test 1: Parse before: directive"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: save [^ 1]
  before: save do: [@ self validate]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.advice[0].adviceType')
if [[ "$result" == "before" ]]; then
    pass "before: advice type parsed correctly"
else
    fail "before: advice type" "before" "$result"
fi

# Test 2: Parse after: directive
echo "Test 2: Parse after: directive"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: delete [^ 1]
  after: delete do: [@ self notify]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.advice[0].adviceType')
if [[ "$result" == "after" ]]; then
    pass "after: advice type parsed correctly"
else
    fail "after: advice type" "after" "$result"
fi

# Test 3: Parse advice selector
echo "Test 3: Parse advice selector"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: save [^ 1]
  before: save do: [@ self validate]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.advice[0].selector')
if [[ "$result" == "save" ]]; then
    pass "Advice selector parsed correctly"
else
    fail "Advice selector" "save" "$result"
fi

# Test 4: Parse advice block
echo "Test 4: Parse advice block"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: save [^ 1]
  before: save do: [@ self validate]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.advice[0].block.type')
if [[ "$result" == "block" ]]; then
    pass "Advice block parsed correctly"
else
    fail "Advice block type" "block" "$result"
fi

# Test 5: Multiple advice declarations
echo "Test 5: Multiple advice declarations"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: save [^ 1]
  method: delete [^ 2]
  before: save do: [@ self validate]
  after: delete do: [@ self notify]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '[.advice[].adviceType] | join(",")')
if [[ "$result" == "before,after" ]]; then
    pass "Multiple advice declarations parsed correctly"
else
    fail "Multiple advice" "before,after" "$result"
fi

# Test 6: Codegen generates handler function
echo "Test 6: Codegen generates handler function"
cat > /tmp/TestAdvice.trash << 'EOF'
TestAdvice subclass: Object
  method: save [^ 1]
  before: save do: [echo "validating"]
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestAdvice.trash 2>&1 | grep -c '__TestAdvice__before__save()')
if [[ "$result" == "1" ]]; then
    pass "Handler function generated"
else
    fail "Handler function" "1 occurrence" "$result occurrences"
fi

# Test 7: Codegen generates registration call
echo "Test 7: Codegen generates registration call"
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestAdvice.trash 2>&1 | grep '_add_before_advice "TestAdvice" "save"')
if [[ -n "$result" ]]; then
    pass "Registration call generated"
else
    fail "Registration call" "_add_before_advice call" "not found"
fi
rm -f /tmp/TestAdvice.trash

# Test 8: Runtime - before advice executes
echo "Test 8: Runtime - before advice executes"
cat > /tmp/TestAdviceRuntime.trash << 'EOF'
TestAdviceRuntime subclass: Object
  classInstanceVars: counter:0

  classMethod: getCounter [
    ^ counter + 0
  ]

  classMethod: reset [
    counter := 0 + 0
  ]

  classMethod: doWork [
    counter := counter + 10
  ]

  before: doWork do: [
    counter := counter + 1
  ]

  after: doWork do: [
    counter := counter + 100
  ]
EOF

# Compile the test class (let dispatcher source it, don't source manually)
"$COMPILER_DIR/driver.bash" compile /tmp/TestAdviceRuntime.trash > "$TRASHDIR/.compiled/TestAdviceRuntime" 2>/dev/null

if [[ -f "$TRASHDIR/.compiled/TestAdviceRuntime" ]]; then
    # Reset and run doWork - should be: 0 + 1 (before) + 10 (work) + 100 (after) = 111
    @ TestAdviceRuntime reset
    @ TestAdviceRuntime doWork
    result=$(@ TestAdviceRuntime getCounter)
    expected="111"
    if [[ "$result" == "$expected" ]]; then
        pass "Before and after advice execute in correct order"
    else
        fail "Advice execution order" "$expected" "$result"
    fi
else
    fail "Advice runtime" "test class to compile" "compilation failed"
fi

# Cleanup
rm -f /tmp/TestAdviceRuntime.trash "$TRASHDIR/.compiled/TestAdviceRuntime"

echo ""
echo "=== Results ==="
echo "Passed: $TESTS_PASSED / $TESTS_RUN"

if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
    exit 0
else
    exit 1
fi
