#!/usr/bin/env bash
# Tests for JSON primitive operations
# Tests arrayPush:, arrayAt:, arrayLength, objectAt:, objectHasKey:, etc.

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

echo "=== JSON Primitives Tests ==="
echo ""

# ==========================================
# Array Primitives
# ==========================================

# Test 1: arrayPush:
echo "Test 1: arrayPush:"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: addItem [
    | val |
    val := '\''hello'\''
    items := items arrayPush: val
  ]'
result=$(compile_method "$source" "addItem")
if [[ "$result" == *'jq -c --arg v'* && "$result" == *'+ [$v]'* ]]; then
  pass "arrayPush: generates jq pipeline"
else
  fail "arrayPush: generates jq pipeline" "jq with '. + [\$v]'" "$result"
fi

# Test 2: arrayAt:
echo "Test 2: arrayAt:"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: getFirst [
    | idx |
    idx := 0
    ^ items arrayAt: idx
  ]'
result=$(compile_method "$source" "getFirst")
if [[ "$result" == *'jq -r --argjson i'* && "$result" == *'.['\$'i]'* ]]; then
  pass "arrayAt: generates jq pipeline"
else
  fail "arrayAt: generates jq pipeline" "jq with '.[\$i]'" "$result"
fi

# Test 3: arrayAt:put:
echo "Test 3: arrayAt:put:"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: setFirst [
    | idx val |
    idx := 0
    val := '\''new'\''
    items := items arrayAt: idx put: val
  ]'
result=$(compile_method "$source" "setFirst")
if [[ "$result" == *'jq -c --argjson i'* && "$result" == *'--arg v'* && "$result" == *'.[$i] = $v'* ]]; then
  pass "arrayAt:put: generates jq pipeline"
else
  fail "arrayAt:put: generates jq pipeline" "jq with '.[\$i] = \$v'" "$result"
fi

# Test 4: arrayLength
echo "Test 4: arrayLength"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: count [
    ^ items arrayLength
  ]'
result=$(compile_method "$source" "count")
if [[ "$result" == *"jq 'length'"* ]]; then
  pass "arrayLength generates jq length"
else
  fail "arrayLength generates jq length" "jq 'length'" "$result"
fi

# Test 5: arrayFirst
echo "Test 5: arrayFirst"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: first [
    ^ items arrayFirst
  ]'
result=$(compile_method "$source" "first")
if [[ "$result" == *"jq -r '.[0]"* ]]; then
  pass "arrayFirst generates jq .[0]"
else
  fail "arrayFirst generates jq .[0]" "jq '.[0]'" "$result"
fi

# Test 6: arrayLast
echo "Test 6: arrayLast"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: last [
    ^ items arrayLast
  ]'
result=$(compile_method "$source" "last")
if [[ "$result" == *"jq -r '.[-1]"* ]]; then
  pass "arrayLast generates jq .[-1]"
else
  fail "arrayLast generates jq .[-1]" "jq '.[-1]'" "$result"
fi

# Test 7: arrayIsEmpty
echo "Test 7: arrayIsEmpty"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: checkEmpty [
    ^ items arrayIsEmpty
  ]'
result=$(compile_method "$source" "checkEmpty")
if [[ "$result" == *"jq 'length == 0'"* ]]; then
  pass "arrayIsEmpty generates jq length == 0"
else
  fail "arrayIsEmpty generates jq length == 0" "jq 'length == 0'" "$result"
fi

# Test 8: arrayRemoveAt:
echo "Test 8: arrayRemoveAt:"
source='TestJSON subclass: Object
  instanceVars: items:'\''[]'\''

  method: removeFirst [
    | idx |
    idx := 0
    items := items arrayRemoveAt: idx
  ]'
result=$(compile_method "$source" "removeFirst")
if [[ "$result" == *'jq -c --argjson i'* && "$result" == *'del(.[$i])'* ]]; then
  pass "arrayRemoveAt: generates jq del"
else
  fail "arrayRemoveAt: generates jq del" "jq 'del(.[\$i])'" "$result"
fi

echo ""

# ==========================================
# Object Primitives
# ==========================================

# Test 9: objectAt:
echo "Test 9: objectAt:"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: getValue [
    | key |
    key := '\''name'\''
    ^ data objectAt: key
  ]'
result=$(compile_method "$source" "getValue")
if [[ "$result" == *'jq -r --arg k'* && "$result" == *'.[$k]'* ]]; then
  pass "objectAt: generates jq pipeline"
else
  fail "objectAt: generates jq pipeline" "jq with '.[\$k]'" "$result"
fi

# Test 10: objectAt:put:
echo "Test 10: objectAt:put:"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: setValue [
    | key val |
    key := '\''name'\''
    val := '\''Alice'\''
    data := data objectAt: key put: val
  ]'
result=$(compile_method "$source" "setValue")
if [[ "$result" == *'jq -c --arg k'* && "$result" == *'--arg v'* && "$result" == *'.[$k] = $v'* ]]; then
  pass "objectAt:put: generates jq pipeline"
else
  fail "objectAt:put: generates jq pipeline" "jq with '.[\$k] = \$v'" "$result"
fi

# Test 11: objectHasKey:
echo "Test 11: objectHasKey:"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: hasName [
    | key |
    key := '\''name'\''
    ^ data objectHasKey: key
  ]'
result=$(compile_method "$source" "hasName")
if [[ "$result" == *'jq --arg k'* && "$result" == *'has($k)'* ]]; then
  pass "objectHasKey: generates jq has"
else
  fail "objectHasKey: generates jq has" "jq 'has(\$k)'" "$result"
fi

# Test 12: objectRemoveKey:
echo "Test 12: objectRemoveKey:"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: removeName [
    | key |
    key := '\''name'\''
    data := data objectRemoveKey: key
  ]'
result=$(compile_method "$source" "removeName")
if [[ "$result" == *'jq -c --arg k'* && "$result" == *'del(.[$k])'* ]]; then
  pass "objectRemoveKey: generates jq del"
else
  fail "objectRemoveKey: generates jq del" "jq 'del(.[\$k])'" "$result"
fi

# Test 13: objectKeys
echo "Test 13: objectKeys"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: allKeys [
    ^ data objectKeys
  ]'
result=$(compile_method "$source" "allKeys")
if [[ "$result" == *"jq -c 'keys'"* ]]; then
  pass "objectKeys generates jq keys"
else
  fail "objectKeys generates jq keys" "jq 'keys'" "$result"
fi

# Test 14: objectValues
echo "Test 14: objectValues"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: allValues [
    ^ data objectValues
  ]'
result=$(compile_method "$source" "allValues")
if [[ "$result" == *"jq -c '[.[]]'"* ]]; then
  pass "objectValues generates jq [.[]]"
else
  fail "objectValues generates jq [.[]]" "jq '[.[]]'" "$result"
fi

# Test 15: objectLength
echo "Test 15: objectLength"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: count [
    ^ data objectLength
  ]'
result=$(compile_method "$source" "count")
if [[ "$result" == *"jq 'length'"* ]]; then
  pass "objectLength generates jq length"
else
  fail "objectLength generates jq length" "jq 'length'" "$result"
fi

# Test 16: objectIsEmpty
echo "Test 16: objectIsEmpty"
source='TestJSON subclass: Object
  instanceVars: data:'\''{}'\''

  method: checkEmpty [
    ^ data objectIsEmpty
  ]'
result=$(compile_method "$source" "checkEmpty")
if [[ "$result" == *"jq 'length == 0'"* ]]; then
  pass "objectIsEmpty generates jq length == 0"
else
  fail "objectIsEmpty generates jq length == 0" "jq 'length == 0'" "$result"
fi

echo ""

# ==========================================
# Summary
# ==========================================

echo "=== Summary ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"

if ((FAIL > 0)); then
  exit 1
fi
