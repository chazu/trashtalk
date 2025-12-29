#!/usr/bin/env bash
# Tests for nil handling (ifNil: / ifNotNil:)
# Tests nil check conditionals

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

echo "=== Nil Handling Tests ==="
echo ""

# Test 1: Basic ifNil:
echo "Test 1: Basic ifNil:"
source='TestNil subclass: Object
  method: test [
    | x |
    x := '\'''\''
    x ifNil: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -z "$x" ]]; then result="1"; fi'* ]]; then
  pass "ifNil: generates [[ -z ]]"
else
  fail "ifNil: generates [[ -z ]]" 'if [[ -z "$x" ]]; then ...; fi' "$result"
fi

# Test 2: Basic ifNotNil:
echo "Test 2: Basic ifNotNil:"
source='TestNil subclass: Object
  method: test [
    | x |
    x := '\''hello'\''
    x ifNotNil: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -n "$x" ]]; then result="1"; fi'* ]]; then
  pass "ifNotNil: generates [[ -n ]]"
else
  fail "ifNotNil: generates [[ -n ]]" 'if [[ -n "$x" ]]; then ...; fi' "$result"
fi

# Test 3: ifNil:ifNotNil: combined
echo "Test 3: ifNil:ifNotNil: combined"
source='TestNil subclass: Object
  method: test [
    | x |
    x := '\'''\''
    x ifNil: [result := 0] ifNotNil: [result := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -z "$x" ]]; then result="0"; else result="1"; fi'* ]]; then
  pass "ifNil:ifNotNil: generates if-else"
else
  fail "ifNil:ifNotNil: generates if-else" 'if [[ -z ...]]; then ...; else ...; fi' "$result"
fi

# Test 4: ifNotNil: with block parameter
echo "Test 4: ifNotNil: with block parameter"
source='TestNil subclass: Object
  method: test [
    | x |
    x := '\''hello'\''
    x ifNotNil: [:v | result := v]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -n "$x" ]]; then local v="$x"; result="$v"; fi'* ]]; then
  pass "ifNotNil: with block parameter binds value"
else
  fail "ifNotNil: with block parameter binds value" 'if [[ -n ...]]; then local v=...; ...; fi' "$result"
fi

# Test 5: ifNil:ifNotNil: with parameter in notnil block
echo "Test 5: ifNil:ifNotNil: with parameter in notnil block"
source='TestNil subclass: Object
  method: test [
    | name |
    name := '\'''\''
    name ifNil: [result := '\''unknown'\''] ifNotNil: [:n | result := n]
  ]'
result=$(compile_method "$source" "test")
# Check for key parts: -z check, else with local binding
if [[ "$result" == *'if [[ -z "$name" ]]'*'else local n="$name"'* ]]; then
  pass "ifNil:ifNotNil: with parameter in else block"
else
  fail "ifNil:ifNotNil: with parameter in else block" 'if [[ -z ... ]]; then ...; else local n=...; ...; fi' "$result"
fi

# Test 6: ifNil: with instance variable
echo "Test 6: ifNil: with instance variable"
source='TestNil subclass: Object
  instanceVars: value

  method: getOrDefault: default [
    value ifNil: [^ default]
    ^ value
  ]'
result=$(compile_method "$source" "getOrDefault_")
if [[ "$result" == *'if [[ -z "$(_ivar value)" ]]; then'* ]]; then
  pass "ifNil: with instance variable"
else
  fail "ifNil: with instance variable" 'if [[ -z "$(_ivar ...)" ]]' "$result"
fi

# Test 7: ifNotNil: with method call result (identifier)
echo "Test 7: ifNotNil: with identifier"
source='TestNil subclass: Object
  method: test [
    | result |
    result ifNotNil: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -n "$result" ]]; then ok="1"; fi'* ]]; then
  pass "ifNotNil: with identifier"
else
  fail "ifNotNil: with identifier" 'if [[ -n "$result" ]]' "$result"
fi

# Test 8: Multiple ifNil: in sequence
echo "Test 8: Multiple ifNil: in sequence"
source='TestNil subclass: Object
  method: test [
    | a b |
    a ifNil: [a := '\''default1'\'']
    b ifNil: [b := '\''default2'\'']
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -z "$a" ]]; then'* ]] && [[ "$result" == *'if [[ -z "$b" ]]; then'* ]]; then
  pass "Multiple ifNil: in sequence"
else
  fail "Multiple ifNil: in sequence" 'Two separate if [[ -z ... ]] statements' "$result"
fi

echo ""
echo "=== Runtime Tests ==="
echo ""

# Test 9: Runtime - ifNil: with empty string
echo "Test 9: Runtime - ifNil: with empty string"
x=""
result="unchanged"
if [[ -z "$x" ]]; then result="was_nil"; fi
if [[ "$result" == "was_nil" ]]; then
  pass "Runtime - empty string is nil"
else
  fail "Runtime - empty string is nil" "was_nil" "$result"
fi

# Test 10: Runtime - ifNil: with non-empty string
echo "Test 10: Runtime - ifNil: with non-empty string"
x="hello"
result="unchanged"
if [[ -z "$x" ]]; then result="was_nil"; fi
if [[ "$result" == "unchanged" ]]; then
  pass "Runtime - non-empty string is not nil"
else
  fail "Runtime - non-empty string is not nil" "unchanged" "$result"
fi

# Test 11: Runtime - ifNotNil: with value
echo "Test 11: Runtime - ifNotNil: with value"
x="hello"
result="unchanged"
if [[ -n "$x" ]]; then result="had_value"; fi
if [[ "$result" == "had_value" ]]; then
  pass "Runtime - ifNotNil: fires for value"
else
  fail "Runtime - ifNotNil: fires for value" "had_value" "$result"
fi

# Test 12: Runtime - ifNotNil: binding
echo "Test 12: Runtime - ifNotNil: binding"
test_binding() {
  local x="hello"
  local captured=""
  if [[ -n "$x" ]]; then local v="$x"; captured="$v"; fi
  echo "$captured"
}
result=$(test_binding)
if [[ "$result" == "hello" ]]; then
  pass "Runtime - ifNotNil: binds value correctly"
else
  fail "Runtime - ifNotNil: binds value correctly" "hello" "$result"
fi

# Test 13: Runtime - ifNil:ifNotNil: with nil
echo "Test 13: Runtime - ifNil:ifNotNil: with nil"
x=""
if [[ -z "$x" ]]; then result="nil_branch"; else result="notnil_branch"; fi
if [[ "$result" == "nil_branch" ]]; then
  pass "Runtime - ifNil:ifNotNil: takes nil branch"
else
  fail "Runtime - ifNil:ifNotNil: takes nil branch" "nil_branch" "$result"
fi

# Test 14: Runtime - ifNil:ifNotNil: with value
echo "Test 14: Runtime - ifNil:ifNotNil: with value"
x="test"
if [[ -z "$x" ]]; then result="nil_branch"; else result="notnil_branch"; fi
if [[ "$result" == "notnil_branch" ]]; then
  pass "Runtime - ifNil:ifNotNil: takes notnil branch"
else
  fail "Runtime - ifNil:ifNotNil: takes notnil branch" "notnil_branch" "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
