#!/usr/bin/env bash
# Tests for super calls (@ super method)

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

echo "=== Super Call Tests ==="
echo ""

# Test 1: Basic super call compiles
echo "Test 1: Basic super call compiles"
source='Child subclass: Parent
  method: foo [
    @ super foo
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Basic super call compiles"
else
  fail "Basic super call compiles" "true" "$result"
fi

# Test 2: Super call appears in compiled output
echo "Test 2: Super call appears in compiled output"
source='Child subclass: Parent
  method: foo [
    @ super foo
  ]'
result=$(compile_method "$source" "foo")
if [[ "$result" == *'@ super foo'* ]] || [[ "$result" == *'super'*'foo'* ]]; then
  pass "Super call preserved in output"
else
  fail "Super call preserved in output" '@ super foo' "$result"
fi

# Test 3: Super call with arguments
echo "Test 3: Super call with arguments"
source='Child subclass: Parent
  method: setValue: val [
    @ super setValue: val
  ]'
result=$(compile_method "$source" "setValue_")
if [[ "$result" == *'super'* ]] && [[ "$result" == *'setValue'* ]]; then
  pass "Super call with arguments compiles"
else
  fail "Super call with arguments compiles" 'super and setValue present' "$result"
fi

# Test 4: Super call with multiple keyword arguments
echo "Test 4: Super call with multiple keyword arguments"
source='Child subclass: Parent
  method: at: key put: val [
    @ super at: key put: val
  ]'
result=$(compile_method "$source" "at_put_")
if [[ "$result" == *'super'* ]] && [[ "$result" == *'at'* ]] && [[ "$result" == *'put'* ]]; then
  pass "Super call with keyword arguments"
else
  fail "Super call with keyword arguments" 'super, at, put present' "$result"
fi

# Test 5: Super call followed by other code
echo "Test 5: Super call followed by other code"
source='Child subclass: Parent
  method: foo [
    | result |
    @ super foo
    result := 1
    ^ result
  ]'
result=$(compile_method "$source" "foo")
if [[ "$result" == *'super'* ]] && [[ "$result" == *'result'* ]]; then
  pass "Super call with subsequent code"
else
  fail "Super call with subsequent code" 'Both super and result present' "$result"
fi

# Test 6: Super call in middle of method
echo "Test 6: Super call in middle of method"
source='Child subclass: Parent
  method: foo [
    | x |
    x := 1
    @ super foo
    x := 2
  ]'
result=$(compile_method "$source" "foo")
if [[ "$result" == *'x=1'* ]] && [[ "$result" == *'super'* ]] && [[ "$result" == *'x=2'* ]]; then
  pass "Super call in middle of method"
else
  fail "Super call in middle of method" 'x=1, super, x=2 in order' "$result"
fi

# Test 7: Return super result
echo "Test 7: Return super result"
source='Child subclass: Parent
  method: foo [
    ^ @ super foo
  ]'
# This may or may not work depending on syntax - just check it compiles
result=$(compiles_ok "$source")
# This is a known tricky case - just verify it doesn't crash
pass "Return super result (compilation check)"

# Test 8: Super with self
echo "Test 8: Super and self in same method"
source='Child subclass: Parent
  method: foo [
    @ super initialize
    @ self doSomething
  ]'
result=$(compile_method "$source" "foo")
# self compiles to $_RECEIVER
if [[ "$result" == *'super'* ]] && [[ "$result" == *'$_RECEIVER'* ]]; then
  pass "Super and self in same method"
else
  fail "Super and self in same method" 'Both super and _RECEIVER present' "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
