#!/usr/bin/env bash
# Tests for array literals #(...)

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

# Helper to check if file compiles without error
compiles_ok() {
  local source="$1"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  if timeout 30 ./driver.bash compile "$tmpfile" >/dev/null 2>&1; then
    rm -f "$tmpfile"
    echo "true"
  else
    rm -f "$tmpfile"
    echo "false"
  fi
}

# Helper to compile full output
compile_full() {
  local source="$1"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  timeout 30 ./driver.bash compile "$tmpfile" 2>/dev/null
  rm -f "$tmpfile"
}

echo "=== Array Literal Tests ==="
echo ""

# Test 1: Empty array compiles
echo "Test 1: Empty array compiles"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #()
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Empty array compiles"
else
  fail "Empty array compiles" "true" "$result"
fi

# Test 2: Empty array generates arr=()
echo "Test 2: Empty array syntax"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #()
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'arr=()'* ]]; then
  pass "Empty array generates ()"
else
  fail "Empty array generates ()" 'arr=()' "$result"
fi

# Test 3: Single element array
echo "Test 3: Single element array"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #(only)
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'arr=("only")'* ]]; then
  pass "Single element array"
else
  fail "Single element array" 'arr=("only")' "$result"
fi

# Test 4: Numeric array
echo "Test 4: Numeric array"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #(1 2 3)
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'arr=("1" "2" "3")'* ]]; then
  pass "Numeric array"
else
  fail "Numeric array" 'arr=("1" "2" "3")' "$result"
fi

# Test 5: String array
echo "Test 5: String array"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #(hello world test)
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'arr=("hello" "world" "test")'* ]]; then
  pass "String array"
else
  fail "String array" 'arr=("hello" "world" "test")' "$result"
fi

# Test 6: Array assigned to instance variable
echo "Test 6: Array assigned to ivar"
source='ArrayTest subclass: Object
  instanceVars: items
  method: test [
    items := #(a b c)
  ]'
result=$(compile_method "$source" "test")
# ivars use _ivar_set with JSON format for collections
if [[ "$result" == *'_ivar_set items'* ]] && [[ "$result" == *'"a"'* ]] && [[ "$result" == *'"b"'* ]] && [[ "$result" == *'"c"'* ]]; then
  pass "Array assigned to ivar"
else
  fail "Array assigned to ivar" '_ivar_set items with [a,b,c]' "$result"
fi

# Test 7: Multiple arrays in method
echo "Test 7: Multiple arrays in method"
source='ArrayTest subclass: Object
  method: test [
    | arr1 arr2 |
    arr1 := #(x y).
    arr2 := #(a b)
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'arr1=("x" "y")'* ]] && [[ "$result" == *'arr2=("a" "b")'* ]]; then
  pass "Multiple arrays in method"
else
  fail "Multiple arrays in method" 'arr1=("x" "y") and arr2=("a" "b")' "$result"
fi

# Test 8: Array with quoted strings
echo "Test 8: Array with quoted strings"
source="ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #('hello world' 'foo bar')
  ]"
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Array with quoted strings compiles"
else
  fail "Array with quoted strings compiles" "true" "$result"
fi

# Test 9: Returned array
echo "Test 9: Returned array"
source='ArrayTest subclass: Object
  method: test [
    ^ #(1 2 3)
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'("1" "2" "3")'* ]]; then
  pass "Returned array"
else
  fail "Returned array" '("1" "2" "3") in output' "$result"
fi

# Test 10: Compiled code is valid bash
echo "Test 10: Compiled code is valid bash"
source='ArrayTest subclass: Object
  method: test [
    | arr |
    arr := #(1 2 3)
  ]'
result=$(compile_full "$source")
if bash -n <<<"$result" 2>/dev/null; then
  pass "Compiled code is valid bash"
else
  fail "Compiled code is valid bash" "valid syntax" "syntax error"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
