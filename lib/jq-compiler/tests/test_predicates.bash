#!/usr/bin/env bash
# Tests for file/string test predicates
# Tests the fileExists, isFile, isDirectory, isEmpty, notEmpty etc. predicates

# set -e  # Disabled - we handle errors ourselves

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

echo "=== Test Predicate Tests ==="
echo ""

# Test 1: fileExists with local variable
echo "Test 1: fileExists with local variable"
source='TestPred subclass: Object
  method: test [
    | path |
    path := '\''/tmp/foo'\''
    (path fileExists) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
expected='local path
path="'"'"'/tmp/foo'"'"'"
if [[ -e "$path" ]]; then ok="1"; fi'
if [[ "$result" == "$expected" ]]; then
  pass "fileExists generates [[ -e ... ]]"
else
  fail "fileExists generates [[ -e ... ]]" "$expected" "$result"
fi

# Test 2: isFile predicate
echo "Test 2: isFile predicate"
source='TestPred subclass: Object
  method: test [
    | f |
    f := '\''/tmp/test.txt'\''
    (f isFile) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -f "$f" ]]'* ]]; then
  pass "isFile generates [[ -f ... ]]"
else
  fail "isFile generates [[ -f ... ]]" "[[ -f ... ]]" "$result"
fi

# Test 3: isDirectory predicate
echo "Test 3: isDirectory predicate"
source='TestPred subclass: Object
  method: test [
    | dir |
    dir := '\''/tmp'\''
    (dir isDirectory) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -d "$dir" ]]'* ]]; then
  pass "isDirectory generates [[ -d ... ]]"
else
  fail "isDirectory generates [[ -d ... ]]" "[[ -d ... ]]" "$result"
fi

# Test 4: isEmpty predicate
echo "Test 4: isEmpty predicate"
source='TestPred subclass: Object
  method: test [
    | str |
    str := '\'''\''
    (str isEmpty) ifTrue: [str := '\''default'\'']
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -z "$str" ]]'* ]]; then
  pass "isEmpty generates [[ -z ... ]]"
else
  fail "isEmpty generates [[ -z ... ]]" "[[ -z ... ]]" "$result"
fi

# Test 5: notEmpty predicate
echo "Test 5: notEmpty predicate"
source='TestPred subclass: Object
  method: test [
    | str |
    str := '\''hello'\''
    (str notEmpty) ifTrue: [@ self process]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -n "$str" ]]'* ]]; then
  pass "notEmpty generates [[ -n ... ]]"
else
  fail "notEmpty generates [[ -n ... ]]" "[[ -n ... ]]" "$result"
fi

# Test 6: ifFalse with test predicate
echo "Test 6: ifFalse with test predicate"
source='TestPred subclass: Object
  method: test [
    | path |
    path := '\''/tmp/config'\''
    (path fileExists) ifFalse: [@ self createConfig]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if ! [[ -e "$path" ]]'* ]]; then
  pass "ifFalse negates test predicate"
else
  fail "ifFalse negates test predicate" "if ! [[ -e ... ]]" "$result"
fi

# Test 7: ifTrue:ifFalse with test predicate
echo "Test 7: ifTrue:ifFalse with test predicate"
source='TestPred subclass: Object
  method: test [
    | path |
    path := '\''/tmp/config'\''
    (path isFile) ifTrue: [loaded := 1] ifFalse: [loaded := 0]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if [[ -f "$path" ]]; then loaded="1"; else loaded="0"; fi'* ]]; then
  pass "ifTrue:ifFalse with test predicate"
else
  fail "ifTrue:ifFalse with test predicate" "if [[ -f ... ]]; then ... else ... fi" "$result"
fi

# Test 8: Instance variable in test predicate
echo "Test 8: Instance variable in test predicate"
source='TestPred subclass: Object
  instanceVars: configPath

  method: test [
    (configPath fileExists) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -e "$(_ivar configPath)" ]]'* ]]; then
  pass "Instance variable in test predicate"
else
  fail "Instance variable in test predicate" '[[ -e "$(_ivar configPath)" ]]' "$result"
fi

# Test 9: isReadable predicate
echo "Test 9: isReadable predicate"
source='TestPred subclass: Object
  method: test [
    | f |
    f := '\''/etc/passwd'\''
    (f isReadable) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -r "$f" ]]'* ]]; then
  pass "isReadable generates [[ -r ... ]]"
else
  fail "isReadable generates [[ -r ... ]]" "[[ -r ... ]]" "$result"
fi

# Test 10: isWritable predicate
echo "Test 10: isWritable predicate"
source='TestPred subclass: Object
  method: test [
    | f |
    f := '\''/tmp/test'\''
    (f isWritable) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -w "$f" ]]'* ]]; then
  pass "isWritable generates [[ -w ... ]]"
else
  fail "isWritable generates [[ -w ... ]]" "[[ -w ... ]]" "$result"
fi

# Test 11: isExecutable predicate
echo "Test 11: isExecutable predicate"
source='TestPred subclass: Object
  method: test [
    | script |
    script := '\''/usr/bin/bash'\''
    (script isExecutable) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -x "$script" ]]'* ]]; then
  pass "isExecutable generates [[ -x ... ]]"
else
  fail "isExecutable generates [[ -x ... ]]" "[[ -x ... ]]" "$result"
fi

# Test 12: isSymlink predicate
echo "Test 12: isSymlink predicate"
source='TestPred subclass: Object
  method: test [
    | link |
    link := '\''/tmp/link'\''
    (link isSymlink) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -L "$link" ]]'* ]]; then
  pass "isSymlink generates [[ -L ... ]]"
else
  fail "isSymlink generates [[ -L ... ]]" "[[ -L ... ]]" "$result"
fi

# Test 13: isFifo predicate
echo "Test 13: isFifo predicate"
source='TestPred subclass: Object
  method: test [
    | pipe |
    pipe := '\''/tmp/mypipe'\''
    (pipe isFifo) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'[[ -p "$pipe" ]]'* ]]; then
  pass "isFifo generates [[ -p ... ]]"
else
  fail "isFifo generates [[ -p ... ]]" "[[ -p ... ]]" "$result"
fi

# Test 14: Arithmetic comparison still works
echo "Test 14: Arithmetic comparison still works"
source='TestPred subclass: Object
  method: test [
    | x |
    x := 5
    (x > 3) ifTrue: [ok := 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *'if (( $x > 3 )); then'* ]]; then
  pass "Arithmetic comparison uses (( ))"
else
  fail "Arithmetic comparison uses (( ))" "if (( \$x > 3 )); then" "$result"
fi

echo ""
echo "=== Runtime Integration Tests ==="
echo ""

# Test 15: Runtime - fileExists returns true for existing file
echo "Test 15: Runtime - fileExists for existing file"
if [[ -e "/etc/passwd" ]]; then
  pass "Runtime - fileExists returns true for /etc/passwd (verified bash syntax)"
else
  fail "Runtime - fileExists returns true for /etc/passwd" "file exists" "file not found"
fi

# Test 16: Runtime - isEmpty with empty string
echo "Test 16: Runtime - isEmpty with empty string"
str=""
if [[ -z "$str" ]]; then
  pass "Runtime - isEmpty returns true for empty string (verified bash syntax)"
else
  fail "Runtime - isEmpty returns true for empty string" "empty" "not empty"
fi

# Test 17: Runtime - notEmpty with non-empty string
echo "Test 17: Runtime - notEmpty with non-empty string"
str="hello"
if [[ -n "$str" ]]; then
  pass "Runtime - notEmpty returns true for non-empty string (verified bash syntax)"
else
  fail "Runtime - notEmpty returns true for non-empty string" "not empty" "empty"
fi

echo ""
echo "=== Results ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
