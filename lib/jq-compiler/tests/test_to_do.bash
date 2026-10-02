#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Tests for to:do: range iteration

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"

cd "$COMPILER_DIR"

source "$SCRIPT_DIR/compile_helpers.bash"

echo "=== to:do: Range Iteration Tests ==="
echo ""

# Test 1: Basic to:do: compiles
echo "Test 1: Basic to:do: compiles"
source='ToDoTest subclass: Object
  method: test [
    0 to: 5 do: [:i | @ Console log: i]
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Basic to:do: compiles"
else
  fail "Basic to:do: compiles" "true" "$result"
fi

# Test 2: to:do: generates for loop
echo "Test 2: to:do: generates for loop"
source='ToDoTest subclass: Object
  method: test [
    0 to: 5 do: [:i | @ Console log: i]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *"for (("* ]] && [[ "$result" == *"i=0"* ]] && [[ "$result" == *"i<5"* ]] && [[ "$result" == *"i++))"* ]]; then
  pass "to:do: generates for loop"
else
  fail "to:do: generates for loop" "for ((i=0; i<5; i++))" "$result"
fi

# Test 3: to:do: with variable bounds
echo "Test 3: to:do: with variable bounds"
source='ToDoTest subclass: Object
  method: test [
    | count |
    count := 10.
    0 to: count do: [:i | @ Console log: i]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *"for (("* ]] && [[ "$result" == *"i<"*"count"* || "$result" == *'i<$count'* ]]; then
  pass "to:do: with variable bounds"
else
  fail "to:do: with variable bounds" "for loop with count variable" "$result"
fi

# Test 4: to:do: with non-zero start
echo "Test 4: to:do: with non-zero start"
source='ToDoTest subclass: Object
  method: test [
    5 to: 10 do: [:i | @ Console log: i]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *"i=5"* ]] && [[ "$result" == *"i<10"* ]]; then
  pass "to:do: with non-zero start"
else
  fail "to:do: with non-zero start" "i=5 and i<10" "$result"
fi

# Test 5: to:do: block uses loop variable
echo "Test 5: to:do: block uses loop variable"
source='ToDoTest subclass: Object
  method: test [
    0 to: 3 do: [:idx | idx + 1]
  ]'
result=$(compile_method "$source" "test")
if [[ "$result" == *"idx"* ]] && [[ "$result" == *"for (("* ]]; then
  pass "to:do: block uses loop variable"
else
  fail "to:do: block uses loop variable" "idx in loop" "$result"
fi

# Test 6: Compiled code is valid bash
echo "Test 6: Compiled code is valid bash"
source='ToDoTest subclass: Object
  method: test [
    0 to: 5 do: [:i | @ Console log: i]
  ]'
result=$(compile_full "$source")
if bash -n <<<"$result" 2>/dev/null; then
  pass "Compiled code is valid bash"
else
  fail "Compiled code is valid bash" "valid syntax" "syntax error"
fi

# Test 7: Multiple to:do: in one method
echo "Test 7: Multiple to:do: in one method"
source='ToDoTest subclass: Object
  method: test [
    0 to: 3 do: [:i | @ Console log: i].
    5 to: 8 do: [:j | @ Console log: j]
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "Multiple to:do: in one method"
else
  fail "Multiple to:do: in one method" "true" "$result"
fi

# Test 8: to:do: as statement (not expression)
echo "Test 8: to:do: as statement"
source='ToDoTest subclass: Object
  method: test [
    | sum |
    sum := 0.
    0 to: 10 do: [:i | sum := sum + i].
    ^ sum
  ]'
result=$(compiles_ok "$source")
if [[ "$result" == "true" ]]; then
  pass "to:do: as statement with side effects"
else
  fail "to:do: as statement with side effects" "true" "$result"
fi

print_results
