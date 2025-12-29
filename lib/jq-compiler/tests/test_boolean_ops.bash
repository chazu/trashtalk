#!/usr/bin/env bash
# Tests for boolean operators (and: / or:)
# Tests short-circuit boolean operators in conditionals

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

echo "=== Boolean Operator Tests ==="
echo ""

# Test 1: Basic and: with arithmetic conditions
echo "Test 1: Basic and: with arithmetic conditions"
source='TestBool subclass: Object
  method: test [
    | x y |
    x := 5
    y := 10
    (x > 3) and: [y < 15] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'(( $x > 3 )) && (( $y < 15 ))'* ]]; then
  pass "and: generates &&"
else
  fail "and: generates &&" "(( ... )) && (( ... ))" "$result"
fi

# Test 2: Basic or: with arithmetic conditions
echo "Test 2: Basic or: with arithmetic conditions"
source='TestBool subclass: Object
  method: test [
    | x y |
    x := 5
    y := 10
    (x > 100) or: [y < 15] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'(( $x > 100 )) || (( $y < 15 ))'* ]]; then
  pass "or: generates ||"
else
  fail "or: generates ||" "(( ... )) || (( ... ))" "$result"
fi

# Test 3: Mixed test predicate and arithmetic with and:
echo "Test 3: Mixed test predicate and arithmetic with and:"
source='TestBool subclass: Object
  method: test [
    | path x |
    path := '\''/etc/passwd'\''
    x := 5
    (path fileExists) and: [x > 3] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -e "$path" ]] && (( $x > 3 ))'* ]]; then
  pass "Mixed test_expr and arithmetic with and:"
else
  fail "Mixed test_expr and arithmetic with and:" "[[ ... ]] && (( ... ))" "$result"
fi

# Test 4: Mixed test predicates with or:
echo "Test 4: Mixed test predicates with or:"
source='TestBool subclass: Object
  method: test [
    | file1 file2 |
    file1 := '\''/etc/passwd'\''
    file2 := '\''/etc/shadow'\''
    (file1 fileExists) or: [file2 fileExists] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -e "$file1" ]] || [[ -e "$file2" ]]'* ]]; then
  pass "Test predicates with or:"
else
  fail "Test predicates with or:" "[[ ... ]] || [[ ... ]]" "$result"
fi

# Test 5: Chained and: operators
echo "Test 5: Chained and: operators"
source='TestBool subclass: Object
  method: test [
    | x y z |
    x := 1
    y := 2
    z := 3
    (x > 0) and: [y > 0] and: [z > 0] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'&& (( $y > 0 )) && (( $z > 0 ))'* ]]; then
  pass "Chained and: operators"
else
  fail "Chained and: operators" "... && ... && ..." "$result"
fi

# Test 6: and: with ifFalse:
echo "Test 6: and: with ifFalse:"
source='TestBool subclass: Object
  method: test [
    | x y |
    x := 5
    y := 10
    (x > 3) and: [y < 5] ifFalse: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! (( $x > 3 )) && (( $y < 5 ))'* ]]; then
  pass "and: with ifFalse: negates combined condition"
else
  fail "and: with ifFalse: negates combined condition" "if ! ... && ..." "$result"
fi

# Test 7: or: with ifTrue:ifFalse:
echo "Test 7: or: with ifTrue:ifFalse:"
source='TestBool subclass: Object
  method: test [
    | x y |
    x := 5
    y := 10
    (x > 100) or: [y > 100] ifTrue: [a := 1] ifFalse: [b := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'(( $x > 100 )) || (( $y > 100 ))'*'then a="1"; else b="1"'* ]]; then
  pass "or: with ifTrue:ifFalse:"
else
  fail "or: with ifTrue:ifFalse:" "if ... || ...; then ... else ..." "$result"
fi

# Test 8: and: with instance variables
echo "Test 8: and: with instance variables"
source='TestBool subclass: Object
  instanceVars: minValue maxValue

  method: inRange: val [
    | result |
    result := 0
    (val >= minValue) and: [val <= maxValue] ifTrue: [result := 1]
    ^ result
  ]'
result=$(compile_method "$source" "inRange_")
if [[ "$result" == *'$(_ivar minValue)'*'&&'*'$(_ivar maxValue)'* ]]; then
  pass "and: with instance variables"
else
  fail "and: with instance variables" "... _ivar ... && ... _ivar ..." "$result"
fi

# Test 9: Multiple conditions with mixed operators
echo "Test 9: isEmpty and notEmpty with or:"
source='TestBool subclass: Object
  method: test [
    | name default |
    name := '\'''\''
    default := '\''unknown'\''
    (name isEmpty) or: [name notEmpty] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -z "$name" ]] || [[ -n "$name" ]]'* ]]; then
  pass "isEmpty and notEmpty with or:"
else
  fail "isEmpty and notEmpty with or:" "[[ -z ... ]] || [[ -n ... ]]" "$result"
fi

# Test 10: Verify basic arithmetic still works (regression)
echo "Test 10: Verify basic arithmetic still works (regression)"
source='TestBool subclass: Object
  method: test [
    | x |
    x := 5
    (x > 3) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if (( $x > 3 )); then'* ]]; then
  pass "Basic arithmetic condition still works"
else
  fail "Basic arithmetic condition still works" "if (( ... )); then" "$result"
fi

echo ""
echo "=== Runtime Tests ==="
echo ""

# Test 11: Runtime - and: both true
echo "Test 11: Runtime - and: both true"
if (( 5 > 3 )) && (( 10 < 15 )); then
  pass "Runtime - and: with both true evaluates to true"
else
  fail "Runtime - and: with both true" "true" "false"
fi

# Test 12: Runtime - and: one false
echo "Test 12: Runtime - and: one false"
result="passed"
if (( 5 > 3 )) && (( 10 > 15 )); then
  result="failed"
fi
if [[ "$result" == "passed" ]]; then
  pass "Runtime - and: with one false evaluates to false"
else
  fail "Runtime - and: with one false" "false" "true"
fi

# Test 13: Runtime - or: one true
echo "Test 13: Runtime - or: one true"
if (( 5 > 100 )) || (( 10 < 15 )); then
  pass "Runtime - or: with one true evaluates to true"
else
  fail "Runtime - or: with one true" "true" "false"
fi

# Test 14: Runtime - mixed test and arithmetic
echo "Test 14: Runtime - mixed test and arithmetic"
if [[ -e "/etc/passwd" ]] && (( 5 > 3 )); then
  pass "Runtime - mixed [[ ]] && (( )) works"
else
  fail "Runtime - mixed test and arithmetic" "true" "false"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
