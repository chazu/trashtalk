#!/usr/bin/env bash
# Test control flow constructs

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

echo "Control Flow Tests"
echo "=================="

# Test helper
assert_contains() {
    local output="$1"
    local expected="$2"
    local desc="$3"
    if [[ "$output" == *"$expected"* ]]; then
        echo -e "  \e[32m✓\e[0m $desc"
        return 0
    else
        echo -e "  \e[31m✗\e[0m $desc"
        echo "    Expected to contain: $expected"
        echo "    Got: $output"
        return 1
    fi
}

# Test 1: Simple ifTrue:
echo -e "\nTest 1: Simple ifTrue:"
cat > /tmp/test_cf1.trash << 'TRASH'
CfTest1 subclass: Object
  method: test [
    (5 > 3) ifTrue: [x := 1].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf1.trash 2>/dev/null)
assert_contains "$output" 'if (( 5 > 3 )); then x="1"; fi' "ifTrue: generates if-then"

# Test 2: Simple ifFalse:
echo -e "\nTest 2: Simple ifFalse:"
cat > /tmp/test_cf2.trash << 'TRASH'
CfTest2 subclass: Object
  method: test [
    (5 > 3) ifFalse: [x := 0].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf2.trash 2>/dev/null)
assert_contains "$output" 'if (( !(5 > 3) )); then x="0"; fi' "ifFalse: generates negated if"

# Test 3: ifTrue:ifFalse:
echo -e "\nTest 3: ifTrue:ifFalse:"
cat > /tmp/test_cf3.trash << 'TRASH'
CfTest3 subclass: Object
  method: test [
    (10 > 5) ifTrue: [result := 1] ifFalse: [result := 0].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf3.trash 2>/dev/null)
assert_contains "$output" 'if (( 10 > 5 )); then result="1"; else result="0"; fi' "ifTrue:ifFalse: generates if-else"

# Test 4: timesRepeat:
echo -e "\nTest 4: timesRepeat:"
cat > /tmp/test_cf4.trash << 'TRASH'
CfTest4 subclass: Object
  method: test [
    5 timesRepeat: [x := x + 1].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf4.trash 2>/dev/null)
assert_contains "$output" 'for ((_i=0; _i<5; _i++)); do' "timesRepeat: generates for loop"

# Test 5: Comparison operators
echo -e "\nTest 5: Comparison operators:"
cat > /tmp/test_cf5.trash << 'TRASH'
CfTest5 subclass: Object
  method: testGt [
    (a > b) ifTrue: [x := 1].
  ]
  method: testLt [
    (a < b) ifTrue: [x := 1].
  ]
  method: testGe [
    (a >= b) ifTrue: [x := 1].
  ]
  method: testLe [
    (a <= b) ifTrue: [x := 1].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf5.trash 2>/dev/null)
assert_contains "$output" '$a > $b' "greater than operator"
assert_contains "$output" '$a < $b' "less than operator"
assert_contains "$output" '$a >= $b' "greater than or equal operator"
assert_contains "$output" '$a <= $b' "less than or equal operator"

# Test 6: Local vars with control flow
echo -e "\nTest 6: Local variables with control flow:"
cat > /tmp/test_cf6.trash << 'TRASH'
CfTest6 subclass: Object
  method: test [
    | result |
    (10 > 5) ifTrue: [result := 1] ifFalse: [result := 0].
    ^ result
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf6.trash 2>/dev/null)
assert_contains "$output" 'local result' "local var declaration"
assert_contains "$output" 'result="1"' "local var in true block"
assert_contains "$output" 'result="0"' "local var in false block"
assert_contains "$output" 'echo "$result"' "return statement"

# Test 7: whileTrue:
echo -e "\nTest 7: whileTrue:"
cat > /tmp/test_cf7.trash << 'TRASH'
CfTest7 subclass: Object
  method: test [
    (x < 10) whileTrue: [x := x + 1].
  ]
TRASH
output=$(./driver.bash compile /tmp/test_cf7.trash 2>/dev/null)
assert_contains "$output" 'while (( $x < 10 )); do' "whileTrue: generates while loop"

# Cleanup
rm -f /tmp/test_cf*.trash

echo -e "\nDone!"
