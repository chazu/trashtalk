#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Tests for string comparisons (=, ~=, =~, matches:)

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

echo "=== String Comparison Tests ==="
echo ""

# Test 1: String equality with =
echo "Test 1: String equality with ="
source='TestStr subclass: Object
  method: test [
    | name |
    name := '\''hello'\''
    (name = '\''hello'\'') ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$name" == '* ]]; then
  pass "String equality generates [[ == ]]"
else
  fail "String equality generates [[ == ]]" '[[ "$name" == ... ]]' "$result"
fi

# Test 2: String inequality with ~=
echo "Test 2: String inequality with ~="
source='TestStr subclass: Object
  method: test [
    | name |
    name := '\''hello'\''
    (name ~= '\''world'\'') ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$name" != '* ]]; then
  pass "String inequality generates [[ != ]]"
else
  fail "String inequality generates [[ != ]]" '[[ "$name" != ... ]]' "$result"
fi

# Test 3: Regex match with =~
echo "Test 3: Regex match with =~"
source='TestStr subclass: Object
  method: test [
    | str |
    str := '\''hello world'\''
    (str =~ '\''hel.*'\'') ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$str" =~'* ]]; then
  pass "Regex match generates [[ =~ ]]"
else
  fail "Regex match generates [[ =~ ]]" '[[ "$str" =~ ... ]]' "$result"
fi

# Test 4: matches: keyword
echo "Test 4: matches: keyword"
source='TestStr subclass: Object
  method: test [
    | email |
    email := '\''test@example.com'\''
    (email matches: '\''^[^@]+@[^@]+$'\'') ifTrue: [valid := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$email" =~'* ]]; then
  pass "matches: keyword generates [[ =~ ]]"
else
  fail "matches: keyword generates [[ =~ ]]" '[[ "$email" =~ ... ]]' "$result"
fi

# Test 5: String comparison with variables
echo "Test 5: String comparison with variables"
source='TestStr subclass: Object
  method: test [
    | a b |
    a := '\''foo'\''
    b := '\''foo'\''
    (a = b) ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$a" == "$b" ]]'* ]]; then
  pass "String comparison with variables"
else
  fail "String comparison with variables" '[[ "$a" == "$b" ]]' "$result"
fi

# Test 6: String comparison with ifFalse:
echo "Test 6: String comparison with ifFalse:"
source='TestStr subclass: Object
  method: test [
    | str |
    str := '\''hello'\''
    (str = '\''world'\'') ifFalse: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! [[ "$str" == '* ]]; then
  pass "String comparison with ifFalse:"
else
  fail "String comparison with ifFalse:" 'if ! [[ ... ]]' "$result"
fi

# Test 7: Numeric vs string comparison
echo "Test 7: Numeric comparison still uses (( ))"
source='TestStr subclass: Object
  method: test [
    | x |
    x := 5
    (x == 5) ifTrue: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if (( $x == 5 ))'* ]]; then
  pass "Numeric comparison uses (( ))"
else
  fail "Numeric comparison uses (( ))" 'if (( $x == 5 ))' "$result"
fi

# Test 8: Combined string and numeric
echo "Test 8: String and numeric comparison combined"
source='TestStr subclass: Object
  method: test [
    | name age |
    name := '\''Alice'\''
    age := 30
    (name = '\''Alice'\'') and: [age > 25] ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ "$name" == '* ]] && [[ "$result" == *'&& (( $age > 25 ))'* ]]; then
  pass "Combined string and numeric comparison"
else
  fail "Combined string and numeric comparison" '[[ ... ]] && (( ... ))' "$result"
fi

echo ""
# Public runtime coverage: test_expression_behavior.bash.

echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
