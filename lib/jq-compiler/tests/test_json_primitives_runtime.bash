#!/usr/bin/env bash
# Runtime tests for JSON primitive operations
# Tests that the generated jq code actually works correctly

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
TRASHTALK_HOME="$(dirname "$(dirname "$COMPILER_DIR")")"

cd "$COMPILER_DIR"

# Load the trash runtime
source "$TRASHTALK_HOME/lib/trash.bash"

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

# Helper to compile and source a test class
setup_class() {
  local source="$1"
  local tmpfile
  tmpfile=$(mktemp)
  echo "$source" > "$tmpfile"
  local output
  output=$(timeout 30 ./driver.bash compile "$tmpfile" 2>/dev/null)
  rm -f "$tmpfile"
  # Source the generated code
  eval "$output"
}

echo "=== JSON Primitives Runtime Tests ==="
echo ""

# Test 1: arrayPush: and arrayLength
echo "Test 1: arrayPush and arrayLength"
setup_class 'Test1 subclass: Object
  instanceVars: items:'\''[]'\''

  method: testPush [
    items := items arrayPush: '\''a'\''
    items := items arrayPush: '\''b'\''
    ^ items arrayLength
  ]'

ID1="test1_$$_$(date +%s)"
_CLASS="Test1" _INSTANCE="$ID1" _RECEIVER="$ID1"
db_put "$ID1" '{"class":"Test1","items":"[]"}'
result=$(__Test1__testPush)
db_delete "$ID1" 2>/dev/null
if [[ "$result" == "2" ]]; then
  pass "arrayPush and arrayLength work correctly"
else
  fail "arrayPush and arrayLength" "2" "$result"
fi

# Test 2: arrayAt: with variable index
echo "Test 2: arrayAt: with variable"
setup_class 'Test2 subclass: Object
  instanceVars: items:'\''[]'\''

  method: setup [
    items := items arrayPush: '\''first'\''
    items := items arrayPush: '\''second'\''
    items := items arrayPush: '\''third'\''
  ]

  method: getAt: idx [
    ^ items arrayAt: idx
  ]'

ID2="test2_$$_$(date +%s)"
_CLASS="Test2" _INSTANCE="$ID2" _RECEIVER="$ID2"
db_put "$ID2" '{"class":"Test2","items":"[]"}'
__Test2__setup
# Need to reload from db to get updated items
result=$(__Test2__getAt_ 1)
db_delete "$ID2" 2>/dev/null
if [[ "$result" == "second" ]]; then
  pass "arrayAt: with variable works correctly"
else
  fail "arrayAt: with variable" "second" "$result"
fi

# Test 3: arrayFirst and arrayLast
echo "Test 3: arrayFirst and arrayLast"
setup_class 'Test3 subclass: Object
  instanceVars: items:'\''["x","y","z"]'\''

  method: getFirst [
    ^ items arrayFirst
  ]

  method: getLast [
    ^ items arrayLast
  ]'

ID3="test3_$$_$(date +%s)"
_CLASS="Test3" _INSTANCE="$ID3" _RECEIVER="$ID3"
# Note: ivars are stored as JSON-encoded strings
db_put "$ID3" '{"class":"Test3","items":"[\"x\",\"y\",\"z\"]"}'
first=$(__Test3__getFirst)
last=$(__Test3__getLast)
db_delete "$ID3" 2>/dev/null
if [[ "$first" == "x" && "$last" == "z" ]]; then
  pass "arrayFirst and arrayLast work correctly"
else
  fail "arrayFirst/arrayLast" "x and z" "$first and $last"
fi

# Test 4: objectAt: put: and objectAt:
echo "Test 4: objectAtPut and objectAt"
setup_class 'Test4 subclass: Object
  instanceVars: data:'\''{}'\''

  method: setName: val [
    data := data objectAt: '\''name'\'' put: val
  ]

  method: getName [
    ^ data objectAt: '\''name'\''
  ]'

ID4="test4_$$_$(date +%s)"
_CLASS="Test4" _INSTANCE="$ID4" _RECEIVER="$ID4"
db_put "$ID4" '{"class":"Test4","data":"{}"}'
__Test4__setName_ "Alice"
result=$(__Test4__getName)
db_delete "$ID4" 2>/dev/null
if [[ "$result" == "Alice" ]]; then
  pass "objectAtPut and objectAt work correctly"
else
  fail "objectAtPut/objectAt" "Alice" "$result"
fi

# Test 5: objectHasKey:
echo "Test 5: objectHasKey:"
setup_class 'Test5 subclass: Object
  instanceVars: data:'\''{"name":"Bob"}'\''

  method: hasName [
    ^ data objectHasKey: '\''name'\''
  ]

  method: hasAge [
    ^ data objectHasKey: '\''age'\''
  ]'

ID5="test5_$$_$(date +%s)"
_CLASS="Test5" _INSTANCE="$ID5" _RECEIVER="$ID5"
# Note: ivars are stored as JSON-encoded strings
db_put "$ID5" '{"class":"Test5","data":"{\"name\":\"Bob\"}"}'
hasName=$(__Test5__hasName)
hasAge=$(__Test5__hasAge)
db_delete "$ID5" 2>/dev/null
if [[ "$hasName" == "true" && "$hasAge" == "false" ]]; then
  pass "objectHasKey: works correctly"
else
  fail "objectHasKey:" "true and false" "$hasName and $hasAge"
fi

# Test 6: arrayIsEmpty
echo "Test 6: arrayIsEmpty"
setup_class 'Test6 subclass: Object
  instanceVars: empty:'\''[]'\'' full:'\''[1,2,3]'\''

  method: checkEmpty [
    ^ empty arrayIsEmpty
  ]

  method: checkFull [
    ^ full arrayIsEmpty
  ]'

ID6="test6_$$_$(date +%s)"
_CLASS="Test6" _INSTANCE="$ID6" _RECEIVER="$ID6"
db_put "$ID6" '{"class":"Test6","empty":"[]","full":"[1,2,3]"}'
isEmpty=$(__Test6__checkEmpty)
isFull=$(__Test6__checkFull)
db_delete "$ID6" 2>/dev/null
if [[ "$isEmpty" == "true" && "$isFull" == "false" ]]; then
  pass "arrayIsEmpty works correctly"
else
  fail "arrayIsEmpty" "true and false" "$isEmpty and $isFull"
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
