#!/usr/bin/env bash
# Tests for cascade message sends (@ obj foo; bar; baz)

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

echo "=== Cascade Tests ==="
echo ""

# Test 1: Basic cascade with two messages
echo "Test 1: Basic cascade with two messages"
source='TestCascade subclass: Object
  method: test [
    @ self foo; bar
  ]'
result=$(compile_method "$source" "test")
# Cascade generates multiple @ calls (may be on separate lines or with semicolons)
if [[ "$result" == *'@ "$_RECEIVER" foo'* ]] && [[ "$result" == *'@ "$_RECEIVER" bar'* ]]; then
  pass "Two-message cascade generates separate calls"
else
  fail "Two-message cascade" 'Both @ calls present' "$result"
fi

# Test 2: Cascade with three messages
echo "Test 2: Cascade with three messages"
source='TestCascade subclass: Object
  method: test [
    @ self foo; bar; baz
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'@ "$_RECEIVER" foo'* ]] && [[ "$result" == *'@ "$_RECEIVER" bar'* ]] && [[ "$result" == *'@ "$_RECEIVER" baz'* ]]; then
  pass "Three-message cascade"
else
  fail "Three-message cascade" 'All three @ calls present' "$result"
fi

# Test 3: Cascade with arguments
echo "Test 3: Cascade with arguments"
source='TestCascade subclass: Object
  method: test [
    | obj |
    @ obj setValue: 10; getValue
  ]'
result=$(compile_method "$source" "test")
# Check that both messages appear (with or without semicolons between)
if [[ "$result" == *'setValue'* ]] && [[ "$result" == *'getValue'* ]]; then
  pass "Cascade with keyword argument"
else
  fail "Cascade with keyword argument" 'Both setValue and getValue present' "$result"
fi

# Test 4: Cascade with variable receiver
echo "Test 4: Cascade with variable receiver"
source='TestCascade subclass: Object
  method: test [
    | counter |
    @ counter increment; increment; getValue
  ]'
result=$(compile_method "$source" "test")
# Check that increment and getValue both appear
if [[ "$result" == *'increment'* ]] && [[ "$result" == *'getValue'* ]]; then
  pass "Cascade with variable receiver"
else
  fail "Cascade with variable receiver" 'Both increment and getValue present' "$result"
fi

# Test 5: Cascade preserves receiver reference
echo "Test 5: Cascade preserves receiver reference"
source='TestCascade subclass: Object
  method: test [
    | obj |
    @ obj foo; bar; baz
  ]'
result=$(compile_method "$source" "test")
# All three calls should use obj as receiver (either $obj or "$obj")
count=$(echo "$result" | grep -cE '@ (\$obj|"\$obj")' || true)
if [[ $count -ge 3 ]]; then
  pass "All cascade messages use same receiver"
else
  fail "All cascade messages use same receiver" "3+ occurrences of obj receiver" "$count occurrences in: $result"
fi

# Test 6: Multiple cascades in method
echo "Test 6: Multiple cascades in method"
source='TestCascade subclass: Object
  method: test [
    | a b |
    @ a foo; bar.
    @ b baz; qux
  ]'
result=$(compile_method "$source" "test")
# Check both variables appear in @ calls
if [[ "$result" == *'@ $a'* ]] && [[ "$result" == *'@ $b'* ]]; then
  pass "Multiple cascades in same method"
else
  fail "Multiple cascades in same method" 'Both @ $a and @ $b present' "$result"
fi

# Test 7: Cascade with multiple keyword arguments
echo "Test 7: Cascade with multiple keyword arguments"
source='TestCascade subclass: Object
  method: test [
    | dict |
    @ dict at: '\''key1'\'' put: '\''val1'\''; at: '\''key2'\'' put: '\''val2'\''
  ]'
result=$(compile_method "$source" "test")
# Check that at: put: appears (keyword methods get compiled)
if [[ "$result" == *'at'* ]] && [[ "$result" == *'put'* ]]; then
  pass "Cascade with multi-keyword messages"
else
  fail "Cascade with multi-keyword messages" 'at and put present' "$result"
fi

# Test 8: Single message (no cascade) still works
echo "Test 8: Single message (no cascade) still works"
source='TestCascade subclass: Object
  method: test [
    @ self foo
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'@ "$_RECEIVER" foo'* ]] && [[ "$result" != *'; @'* ]]; then
  pass "Single message without cascade"
else
  fail "Single message without cascade" 'Single @ call, no semicolons' "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
