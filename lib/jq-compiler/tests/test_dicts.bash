#!/usr/bin/env bash
# Tests for dictionary literals #{key: value ...}

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

echo "=== Dictionary Literal Tests ==="
echo ""

# Test 1: Single key-value pair compiles
echo "Test 1: Single key-value pair compiles"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{key: value}
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Single key-value pair compiles"
else
  fail "Single key-value pair compiles" "true" "$result"
fi

# Test 2: Single pair generates bash associative array syntax
echo "Test 2: Single pair syntax"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{key: value}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[key]="value"'* ]]; then
  pass "Single pair generates [key]=value"
else
  fail "Single pair generates [key]=value" '[key]="value"' "$result"
fi

# Test 3: Multiple key-value pairs
echo "Test 3: Multiple key-value pairs"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{name: app version: 1}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[name]="app"'* ]] && [[ "$result" == *'[version]="1"'* ]]; then
  pass "Multiple key-value pairs"
else
  fail "Multiple key-value pairs" '[name]="app" and [version]="1"' "$result"
fi

# Test 4: Numeric values
echo "Test 4: Numeric values"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{count: 42 size: 100}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[count]="42"'* ]] && [[ "$result" == *'[size]="100"'* ]]; then
  pass "Numeric values"
else
  fail "Numeric values" '[count]="42" and [size]="100"' "$result"
fi

# Test 5: Dict assigned to instance variable
echo "Test 5: Dict assigned to ivar"
source='DictTest subclass: Object
  instanceVars: config
  method: test [
    config := #{host: localhost port: 8080}
  ]'
result=$(compile_method "$source" "test")
# ivars use _ivar_set with JSON format for collections
if [[ "$result" == *'_ivar_set config'* ]] && [[ "$result" == *'host'* ]] && [[ "$result" == *'port'* ]]; then
  pass "Dict assigned to ivar"
else
  fail "Dict assigned to ivar" '_ivar_set config with host,port' "$result"
fi

# Test 6: Multiple dicts in method
echo "Test 6: Multiple dicts in method"
source='DictTest subclass: Object
  method: test [
    | d1 d2 |
    d1 := #{a: 1}.
    d2 := #{b: 2}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[a]="1"'* ]] && [[ "$result" == *'[b]="2"'* ]]; then
  pass "Multiple dicts in method"
else
  fail "Multiple dicts in method" '[a]="1" and [b]="2"' "$result"
fi

# Test 7: Dict with string values
echo "Test 7: Dict with string values"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{greeting: hello farewell: goodbye}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[greeting]="hello"'* ]] && [[ "$result" == *'[farewell]="goodbye"'* ]]; then
  pass "Dict with string values"
else
  fail "Dict with string values" '[greeting]="hello" and [farewell]="goodbye"' "$result"
fi

# Test 8: Returned dict
echo "Test 8: Returned dict"
source='DictTest subclass: Object
  method: test [
    ^ #{status: ok}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[status]="ok"'* ]]; then
  pass "Returned dict"
else
  fail "Returned dict" '[status]="ok" in output' "$result"
fi

# Test 9: Compiled code is valid bash
echo "Test 9: Compiled code is valid bash"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{x: 1 y: 2}
  ]'
result=$(compile_full "$source")
if bash -n <<<"$result" 2>/dev/null; then
  pass "Compiled code is valid bash"
else
  fail "Compiled code is valid bash" "valid syntax" "syntax error"
fi

# Test 10: Dict with three or more pairs
echo "Test 10: Dict with three or more pairs"
source='DictTest subclass: Object
  method: test [
    | d |
    d := #{a: 1 b: 2 c: 3}
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[a]="1"'* ]] && [[ "$result" == *'[b]="2"'* ]] && [[ "$result" == *'[c]="3"'* ]]; then
  pass "Dict with three pairs"
else
  fail "Dict with three pairs" '[a]="1", [b]="2", [c]="3"' "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
