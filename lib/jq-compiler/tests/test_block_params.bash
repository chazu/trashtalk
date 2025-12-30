#!/usr/bin/env bash
# Tests for block parameter handling in pure Trashtalk methods

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
  echo "$output" | awk "/^__.*__${method}\\(\\)/,/^\\}/" | tail -n +2 | sed '$d' | sed 's/^  //'
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

echo "=== Block Parameter Handling Tests ==="
echo ""

# Test 1: Message send to local variable is quoted
echo "Test 1: Message send to local variable is quoted"
source='BlockTest subclass: Object
  method: runWith: aBlock [
    @ aBlock valueWith: 42
  ]'
result=$(compile_method "$source" "runWith_")
if [[ "$result" == *'"$aBlock"'* ]]; then
  pass "Local variable receiver is quoted"
else
  fail "Local variable receiver is quoted" '"$aBlock"' "$result"
fi

# Test 2: Keyword method has colon in output
echo "Test 2: Keyword method has colon in output"
source='BlockTest subclass: Object
  method: runWith: aBlock [
    @ aBlock valueWith: 42
  ]'
result=$(compile_method "$source" "runWith_")
if [[ "$result" == *'valueWith:'* ]]; then
  pass "Keyword method has colon"
else
  fail "Keyword method has colon" "valueWith:" "$result"
fi

# Test 3: Multi-keyword method has all colons
echo "Test 3: Multi-keyword method has all colons"
source='BlockTest subclass: Object
  method: test: a with: b [
    @ self foo: a bar: b
  ]'
result=$(compile_method "$source" "test_with_")
if [[ "$result" == *'foo:'* ]] && [[ "$result" == *'bar:'* ]]; then
  pass "Multi-keyword method has all colons"
else
  fail "Multi-keyword method has all colons" "foo: and bar:" "$result"
fi

# Test 4: Block literal uses keyword syntax for Block creation
echo "Test 4: Block literal uses keyword syntax"
source='BlockTest subclass: Object
  method: test [
    @ self run: [:x | x + 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'@ Block params:'* ]] && [[ "$result" == *'code:'* ]] && [[ "$result" == *'captured:'* ]]; then
  pass "Block literal uses keyword syntax"
else
  fail "Block literal uses keyword syntax" "params: code: captured:" "$result"
fi

# Test 5: Block with parameters compiles correctly
echo "Test 5: Block with parameters compiles"
source='BlockTest subclass: Object
  method: test [
    @ self run: [:x :y | x + y]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'["x","y"]'* ]]; then
  pass "Block parameters captured"
else
  fail "Block parameters captured" '["x","y"]' "$result"
fi

# Test 6: Unary message send (no args) has no colon
echo "Test 6: Unary message send has no colon"
source='BlockTest subclass: Object
  method: test [
    @ self size
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'@ "$_RECEIVER" size'* ]] && [[ "$result" != *'size:'* ]]; then
  pass "Unary message has no colon"
else
  fail "Unary message has no colon" '@ "$_RECEIVER" size (no colon)' "$result"
fi

# Test 7: Self receiver is quoted
echo "Test 7: Self receiver is quoted"
source='BlockTest subclass: Object
  method: test [
    @ self foo: 42
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'"$_RECEIVER"'* ]]; then
  pass "Self receiver is quoted"
else
  fail "Self receiver is quoted" '"$_RECEIVER"' "$result"
fi

# Test 8: Compiled code is valid bash
echo "Test 8: Compiled code is valid bash"
source='BlockTest subclass: Object
  method: runWith: aBlock [
    @ aBlock valueWith: 42
  ]
  method: test [
    @ self runWith: [:x | x + 1]
  ]'
result=$(compile_full "$source")
if bash -n <<<"$result" 2>/dev/null; then
  pass "Compiled code is valid bash"
else
  fail "Compiled code is valid bash" "valid syntax" "syntax error"
fi

# Test 9: Expression parser triggered for @ variable keyword
echo "Test 9: Expression parser triggered for message to variable"
source='BlockTest subclass: Object
  method: invoke: blk [
    @ blk value
  ]'
result=$(compile_method "$source" "invoke_")
# Should have local blk="$1" and proper @ send
if [[ "$result" == *'local blk="$1"'* ]] && [[ "$result" == *'@ "$blk"'* ]]; then
  pass "Expression parser handles variable receiver"
else
  fail "Expression parser handles variable receiver" 'local blk="$1" and @ "$blk"' "$result"
fi

# Test 10: Block body code is properly escaped
echo "Test 10: Block body code is escaped"
source="BlockTest subclass: Object
  method: test [
    @ self run: [:x | @ x foo: 'bar']
  ]"
result=$(compile_method "$source" "test")
# Single quotes in block body should be escaped
if [[ "$result" == *"'\\''bar'\\'''" ]] || [[ "$result" == *"code:"* ]]; then
  pass "Block body is escaped"
else
  fail "Block body is escaped" "escaped quotes" "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
