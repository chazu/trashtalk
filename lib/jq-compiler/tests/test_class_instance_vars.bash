#!/usr/bin/env bash
# Test class instance variables

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_DIR="$(dirname "$(dirname "$COMPILER_DIR")")"

# Source the runtime
source "$PROJECT_DIR/lib/trash.bash"

PASSED=0
FAILED=0

pass() {
  echo "✓ $1"
  ((PASSED++))
}

fail() {
  echo "✗ $1"
  echo "  Expected: $2"
  echo "  Got: $3"
  ((FAILED++))
}

# Test 1: Parser recognizes classInstanceVars
echo "=== Test 1: Parser recognizes classInstanceVars ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: count:0 name' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.classInstanceVars | length')
if [[ "$result" == "2" ]]; then
  pass "Parser recognizes classInstanceVars (found 2 vars)"
else
  fail "Parser recognizes classInstanceVars" "2" "$result"
fi

# Test 2: Parser extracts var names correctly
echo "=== Test 2: Parser extracts var names ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: instanceCount:0' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.classInstanceVars[0].name')
if [[ "$result" == "instanceCount" ]]; then
  pass "Parser extracts cvar name correctly"
else
  fail "Parser extracts cvar name" "instanceCount" "$result"
fi

# Test 3: Parser extracts default values
echo "=== Test 3: Parser extracts default values ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: counter:42' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.classInstanceVars[0].default.value')
if [[ "$result" == "42" ]]; then
  pass "Parser extracts cvar default value"
else
  fail "Parser extracts cvar default" "42" "$result"
fi

# Test 4: Codegen generates metadata
echo "=== Test 4: Codegen generates metadata ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: counter:0' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -f "$COMPILER_DIR/codegen.jq" 2>/dev/null | grep -c 'classInstanceVars.*counter:0')
if [[ "$result" -ge "1" ]]; then
  pass "Codegen generates classInstanceVars metadata"
else
  fail "Codegen generates metadata" "contains counter:0" "not found"
fi

# Test 5: Codegen generates initClassVars function
echo "=== Test 5: Codegen generates initClassVars function ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: counter:0' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -f "$COMPILER_DIR/codegen.jq" 2>/dev/null | grep -c '__TestClass__initClassVars')
if [[ "$result" -ge "1" ]]; then
  pass "Codegen generates initClassVars function"
else
  fail "Codegen generates initClassVars" "function definition" "not found"
fi

# Test 6: Cvar inference in expression parser
echo "=== Test 6: Cvar inference in expression parser ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: counter:0

  classMethod: count [
    ^ counter
  ]' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -f "$COMPILER_DIR/codegen.jq" 2>/dev/null | grep -c '_cvar counter')
if [[ "$result" -ge "1" ]]; then
  pass "Cvar inference generates _cvar call"
else
  fail "Cvar inference" "_cvar counter" "not found"
fi

# Test 7: Cvar assignment generates _cvar_set
echo "=== Test 7: Cvar assignment ==="
result=$(echo 'TestClass subclass: Object
  classInstanceVars: counter:0

  classMethod: increment [
    counter := counter + 1.
  ]' | "$COMPILER_DIR/tokenizer.bash" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -f "$COMPILER_DIR/codegen.jq" 2>/dev/null | grep -c '_cvar_set counter')
if [[ "$result" -ge "1" ]]; then
  pass "Cvar assignment generates _cvar_set call"
else
  fail "Cvar assignment" "_cvar_set counter" "not found"
fi

# Test 8: Runtime _cvar and _cvar_set work
echo "=== Test 8: Runtime _cvar and _cvar_set ==="
_CLASS="TestRuntime"
_cvar_set testVar "hello"
result=$(_cvar testVar)
if [[ "$result" == "hello" ]]; then
  pass "Runtime _cvar and _cvar_set work"
else
  fail "Runtime cvar functions" "hello" "$result"
fi

# Cleanup
kvdel "__TestRuntime__cvar__testVar" 2>/dev/null

# Test 9: Full integration - compile and run a class with cvars
echo "=== Test 9: Full integration test ==="

# Create test class in the proper location
cat > "$PROJECT_DIR/trash/TestCvarClass.trash" << 'EOF'
TestCvarClass subclass: Object
  classInstanceVars: instanceCount:0
  instanceVars: id:0

  rawClassMethod: new [
    local instance
    instance=$(_generate_instance_id TestCvarClass)
    _create_instance TestCvarClass $instance
    @ $instance initialize
    echo $instance
  ]

  classMethod: count [
    ^ instanceCount
  ]

  classMethod: resetCount [
    instanceCount := 0.
  ]

  method: initialize [
    instanceCount := instanceCount + 1.
    id := instanceCount.
  ]

  method: getId [
    ^ id
  ]
EOF

# Compile to proper location
"$COMPILER_DIR/driver.bash" compile "$PROJECT_DIR/trash/TestCvarClass.trash" > "$PROJECT_DIR/trash/.compiled/TestCvarClass" 2>/dev/null

# Clean any stale kv entries
kvdel "__TestCvarClass__cvar__instanceCount" 2>/dev/null

# Reset count first (this also sources the class)
@ TestCvarClass resetCount 2>/dev/null

# Create first instance
obj1=$(@ TestCvarClass new 2>/dev/null)
count1=$(@ TestCvarClass count 2>/dev/null)

# Create second instance
obj2=$(@ TestCvarClass new 2>/dev/null)
count2=$(@ TestCvarClass count 2>/dev/null)

# Check results
if [[ "$count1" == "1" && "$count2" == "2" ]]; then
  pass "Full integration: class vars shared across instances"
else
  fail "Full integration" "count1=1, count2=2" "count1=$count1, count2=$count2"
fi

# Cleanup
kvdel "__TestCvarClass__cvar__instanceCount" 2>/dev/null
rm -f "$PROJECT_DIR/trash/TestCvarClass.trash" "$PROJECT_DIR/trash/.compiled/TestCvarClass"

echo ""
echo "================================"
echo "Passed: $PASSED, Failed: $FAILED"
echo "================================"

[[ $FAILED -eq 0 ]]
