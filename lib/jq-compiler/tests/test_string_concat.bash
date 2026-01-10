#!/usr/bin/env bash
# Tests for string concatenation with comma operator (,)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"

cd "$COMPILER_DIR"

PASS=0
FAIL=0

pass() {
  echo "  PASS: $1"
  ((PASS++))
}

fail() {
  echo "  FAIL: $1"
  echo "    Expected: $2"
  echo "    Got:      $3"
  ((FAIL++))
}

# Helper to compile and extract method body
compile_method() {
  local source="$1"
  local method="$2"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  local output
  output=$(timeout 30 ./driver.bash compile "$tmpfile" 2>/dev/null)
  rm -f "$tmpfile"
  echo "$output" | awk "/^__.*__${method}\(\)/,/^\}/" | tail -n +2 | sed '$d' | sed 's/^  //'
}

echo "=== String Concatenation Tests ==="
echo ""

# Test 1: Simple literal concatenation
echo "Test 1: Simple literal concatenation"
source='TestConcat subclass: Object
  method: test [
    | result |
    result := "Hello" , "World".
    ^ result
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'result="HelloWorld"'* ]]; then
  pass "Simple literal concatenation"
else
  fail "Simple literal concatenation" 'result="HelloWorld"' "$result"
fi

# Test 2: Concatenation with space literal
echo "Test 2: Concatenation with space"
source='TestConcat subclass: Object
  method: test [
    | result |
    result := "Hello" , " " , "World".
    ^ result
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'result="Hello World"'* ]]; then
  pass "Concatenation with space"
else
  fail "Concatenation with space" 'result="Hello World"' "$result"
fi

# Test 3: Variable concatenation
echo "Test 3: Variable concatenation"
source='TestConcat subclass: Object
  method: test [
    | a b result |
    a := "Hello".
    b := "World".
    result := a , b.
    ^ result
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'result="${a}${b}"'* ]]; then
  pass "Variable concatenation"
else
  fail "Variable concatenation" 'result="${a}${b}"' "$result"
fi

# Test 4: Mixed variable and literal
echo "Test 4: Mixed variable and literal"
source='TestConcat subclass: Object
  method: test [
    | name result |
    name := "World".
    result := "Hello " , name.
    ^ result
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'result="Hello ${name}"'* ]]; then
  pass "Mixed variable and literal"
else
  fail "Mixed variable and literal" 'result="Hello ${name}"' "$result"
fi

# Test 5: Three-part concatenation with variables
echo "Test 5: Three-part with space variable"
source='TestConcat subclass: Object
  method: test [
    | a b result |
    a := "Hello".
    b := "World".
    result := a , " " , b.
    ^ result
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'result="${a} ${b}"'* ]]; then
  pass "Three-part with variables and space"
else
  fail "Three-part with variables and space" 'result="${a} ${b}"' "$result"
fi

echo ""
echo "=== Runtime Tests ==="
echo ""

# Test 6: Runtime literal concatenation
echo "Test 6: Runtime literal concatenation"
test_result="Hello""World"
if [[ "$test_result" == "HelloWorld" ]]; then
  pass "Runtime literal concatenation"
else
  fail "Runtime literal concatenation" "HelloWorld" "$test_result"
fi

# Test 7: Runtime variable concatenation
echo "Test 7: Runtime variable concatenation"
a="Hello"
b="World"
test_result="${a}${b}"
if [[ "$test_result" == "HelloWorld" ]]; then
  pass "Runtime variable concatenation"
else
  fail "Runtime variable concatenation" "HelloWorld" "$test_result"
fi

# Test 8: Runtime with space
echo "Test 8: Runtime with space"
a="Hello"
b="World"
test_result="${a} ${b}"
if [[ "$test_result" == "Hello World" ]]; then
  pass "Runtime with space"
else
  fail "Runtime with space" "Hello World" "$test_result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
