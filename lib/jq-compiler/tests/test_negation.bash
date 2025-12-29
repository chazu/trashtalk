#!/usr/bin/env bash
# Tests for boolean negation (not)

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

echo "=== Boolean Negation Tests ==="
echo ""

# Test 1: Basic negation of arithmetic comparison
echo "Test 1: Basic negation of arithmetic comparison"
source='TestNot subclass: Object
  method: test [
    | x |
    x := 5
    (x > 10) not ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! (( $x > 10 ))'*'then result="1"'* ]]; then
  pass "Arithmetic negation generates ! (( ))"
else
  fail "Arithmetic negation generates ! (( ))" 'if ! (( ... )); then' "$result"
fi

# Test 2: Negation of test predicate
echo "Test 2: Negation of test predicate"
source='TestNot subclass: Object
  method: test [
    | path |
    path := '\''/nonexistent'\''
    (path fileExists) not ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! [[ -e "$path" ]]'*'then result="1"'* ]]; then
  pass "Test predicate negation generates ! [[ ]]"
else
  fail "Test predicate negation generates ! [[ ]]" 'if ! [[ -e ... ]]' "$result"
fi

# Test 3: Double negation
echo "Test 3: Double negation"
source='TestNot subclass: Object
  method: test [
    | x |
    x := 5
    (x > 10) not not ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! ! (( $x > 10 ))'*'then result="1"'* ]]; then
  pass "Double negation generates ! ! ..."
else
  fail "Double negation generates ! ! ..." 'if ! ! (( ... ))' "$result"
fi

# Test 4: Negation with ifFalse:
echo "Test 4: Negation with ifFalse:"
source='TestNot subclass: Object
  method: test [
    | x |
    x := 5
    (x > 10) not ifFalse: [result := 1]
  ]'
result=$(compile_method "$source" "test")
# (x > 10) not ifFalse: means if NOT (NOT (x > 10)), which is if (x > 10)
if [[ "$result" == *'! ! (( $x > 10 ))'* ]]; then
  pass "Negation with ifFalse: generates double negation"
else
  fail "Negation with ifFalse: generates double negation" 'if (( ! ... )) not' "$result"
fi

# Test 5: Negation combined with boolean operators
echo "Test 5: Negation then and:"
source='TestNot subclass: Object
  method: test [
    | x y |
    x := 5
    y := 3
    (x > 10) not and: [y < 5] ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
# (x > 10) not produces a "not" node, then and: combines with [y < 5]
if [[ "$result" == *'! (( $x > 10 ))'*'&&'*'(( $y < 5 ))'* ]]; then
  pass "Negation then and: works"
else
  fail "Negation then and: works" '! (( ... )) && (( ... ))' "$result"
fi

# Test 6: Negation of isEmpty
echo "Test 6: Negation of isEmpty"
source='TestNot subclass: Object
  method: test [
    | str |
    str := '\''hello'\''
    (str isEmpty) not ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! [[ -z "$str" ]]'* ]]; then
  pass "Negation of isEmpty generates ! [[ -z ]]"
else
  fail "Negation of isEmpty generates ! [[ -z ]]" 'if ! [[ -z ... ]]' "$result"
fi

echo ""
echo "=== Runtime Tests ==="
echo ""

# Test 7: Runtime - negated true condition
echo "Test 7: Runtime - negated true condition"
x=15
if ! (( $x > 10 )); then
  result="not_executed"
else
  result="executed"
fi
if [[ "$result" == "executed" ]]; then
  pass "Runtime - negated true condition does not fire"
else
  fail "Runtime - negated true condition" "executed" "$result"
fi

# Test 8: Runtime - negated false condition
echo "Test 8: Runtime - negated false condition"
x=5
result="not_executed"
if ! (( $x > 10 )); then
  result="executed"
fi
if [[ "$result" == "executed" ]]; then
  pass "Runtime - negated false condition fires"
else
  fail "Runtime - negated false condition" "executed" "$result"
fi

# Test 9: Runtime - double negation
echo "Test 9: Runtime - double negation"
x=15
if ! ! (( $x > 10 )); then
  result="executed"
else
  result="not_executed"
fi
if [[ "$result" == "executed" ]]; then
  pass "Runtime - double negation cancels out"
else
  fail "Runtime - double negation" "executed" "$result"
fi

# Test 10: Runtime - negated file test
echo "Test 10: Runtime - negated file test"
if ! [[ -e "/nonexistent/file" ]]; then
  result="executed"
else
  result="not_executed"
fi
if [[ "$result" == "executed" ]]; then
  pass "Runtime - negated file test works"
else
  fail "Runtime - negated file test" "executed" "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
