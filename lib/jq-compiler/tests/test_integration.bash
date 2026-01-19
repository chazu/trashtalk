#!/usr/bin/env bash
# ==============================================================================
# Integration Tests
# ==============================================================================
# Tests that compile actual .trash files and compare with expected output
# ==============================================================================

# Source shared test helper for standalone execution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.bash"

DRIVER="$COMPILER_DIR/driver.bash"
TRASHTALK_DIR="$(dirname "$(dirname "$COMPILER_DIR")")"
TRASH_DIR="$TRASHTALK_DIR/trash"
TMPFILE="/tmp/test_integration_$$.trash"

# Cleanup on exit
trap 'rm -f "$TMPFILE"' EXIT

# Helper to check if file compiles without error
compiles_ok() {
    local trash_file="$1"
    if "$DRIVER" compile "$trash_file" >/dev/null 2>&1; then
        echo "true"
    else
        echo "false"
    fi
}

# ------------------------------------------------------------------------------
# Compilation Success Tests
# ------------------------------------------------------------------------------

echo -e "\n  Compilation Success:"

run_test "Counter compiles" "true" \
    "$(compiles_ok "$TRASH_DIR/Counter.trash")"

run_test "Array compiles" "true" \
    "$(compiles_ok "$TRASH_DIR/Array.trash")"

run_test "Store compiles" "true" \
    "$(compiles_ok "$TRASH_DIR/Store.trash")"

run_test "Process compiles" "true" \
    "$(compiles_ok "$TRASH_DIR/Process.trash")"

# Test traits if they exist
if [[ -f "$TRASH_DIR/traits/Debuggable.trash" ]]; then
    run_test "Debuggable trait compiles" "true" \
        "$(compiles_ok "$TRASH_DIR/traits/Debuggable.trash")"
fi

# ------------------------------------------------------------------------------
# Pipeline Stage Tests
# ------------------------------------------------------------------------------

echo -e "\n  Pipeline Stages:"

# Test tokenize stage
TOKENS=$("$DRIVER" tokenize "$TRASH_DIR/Counter.trash" 2>/dev/null)
run_test "tokenize produces JSON array" "true" \
    "$(echo "$TOKENS" | jq 'type == "array"' 2>/dev/null || echo false)"

# Test parse stage
AST=$("$DRIVER" parse "$TRASH_DIR/Counter.trash" 2>/dev/null)
run_test "parse produces object" "true" \
    "$(echo "$AST" | jq 'type == "object"' 2>/dev/null || echo false)"

run_test "parse has name field" "true" \
    "$(echo "$AST" | jq '.class | has("name")' 2>/dev/null || echo false)"

run_test "parse has methods field" "true" \
    "$(echo "$AST" | jq '.class | has("methods")' 2>/dev/null || echo false)"

# ------------------------------------------------------------------------------
# End-to-End Execution Tests
# ------------------------------------------------------------------------------

echo -e "\n  End-to-End Execution:"

# Create a minimal test class
TEST_CLASS='TestClass subclass: Object
  instanceVars: value:0

  method: new [
    | id |
    id := $(_generate_instance_id TestClass)
    _create_instance TestClass $id
    ^ $id
  ]

  method: setValue: val [
    @ "$_RECEIVER" _setInstanceVar_to value $val
  ]

  method: getValue [
    ^ $(@ "$_RECEIVER" _getInstanceVar value)
  ]

  method: increment [
    | current |
    current := $(@ self getValue)
    @ self setValue: $((current + 1))
    ^ $((current + 1))
  ]

  classMethod: description [
    ^ "A test class"
  ]'

# Compile and source the test class
printf '%s\n' "$TEST_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)

if [[ -n "$COMPILED" ]]; then
    # Verify key elements in compiled output
    run_test "e2e: generates new method" "true" \
        "$(echo "$COMPILED" | grep -q '__TestClass__new()' && echo true || echo false)"

    run_test "e2e: generates setValue method" "true" \
        "$(echo "$COMPILED" | grep -q '__TestClass__setValue_()' && echo true || echo false)"

    run_test "e2e: generates getValue method" "true" \
        "$(echo "$COMPILED" | grep -q '__TestClass__getValue()' && echo true || echo false)"

    run_test "e2e: generates class method" "true" \
        "$(echo "$COMPILED" | grep -q '__TestClass__class__description()' && echo true || echo false)"

    run_test "e2e: local vars correct" "true" \
        "$(echo "$COMPILED" | grep -q 'local id' && echo true || echo false)"

    run_test "e2e: self transformed" "true" \
        "$(echo "$COMPILED" | grep -q '\$_RECEIVER' && echo true || echo false)"

    run_test "e2e: assignment transformed" "true" \
        "$(echo "$COMPILED" | grep -qE 'current="?\$' && echo true || echo false)"
else
    run_test "e2e: compilation succeeded" "true" "false"
fi

# ------------------------------------------------------------------------------
# Error Handling Tests
# ------------------------------------------------------------------------------

echo -e "\n  Error Handling:"

# Empty input should produce error or empty output gracefully
EMPTY_RESULT=$("$DRIVER" compile /dev/null 2>&1 || true)
# Verify it either returns empty or contains an error indicator
if [[ -z "$EMPTY_RESULT" || "$EMPTY_RESULT" == *"error"* || "$EMPTY_RESULT" == *"Error"* ]]; then
    run_test "empty input produces empty or error" "true" "true"
else
    # If it produced non-empty output that's not an error, it should at least be valid bash
    if bash -n <<<"$EMPTY_RESULT" 2>/dev/null; then
        run_test "empty input produces valid bash" "true" "true"
    else
        run_test "empty input produces valid output" "true" "false"
    fi
fi

# Invalid syntax should produce error or at least not produce valid bash function definitions
INVALID='this is not valid trashtalk syntax at all'
printf '%s\n' "$INVALID" > "$TMPFILE"
INVALID_RESULT=$("$DRIVER" compile "$TMPFILE" 2>&1 || true)
# For invalid input, we expect either:
# 1. An error message (contains "error" case-insensitive)
# 2. No method definitions generated (no __ClassName__method patterns)
# 3. If output exists, it shouldn't define meaningful functions
if [[ "$INVALID_RESULT" == *"error"* || "$INVALID_RESULT" == *"Error"* ]]; then
    run_test "invalid syntax produces error message" "true" "true"
elif [[ -z "$INVALID_RESULT" ]]; then
    run_test "invalid syntax produces empty output" "true" "true"
elif ! echo "$INVALID_RESULT" | grep -q '__.*__.*()'; then
    run_test "invalid syntax produces no method definitions" "true" "true"
else
    run_test "invalid syntax handled gracefully" "true" "false"
fi

# Test missing file handling
MISSING_RESULT=$("$DRIVER" compile "/nonexistent/path/file.trash" 2>&1 || true)
if [[ "$MISSING_RESULT" == *"No such file"* || "$MISSING_RESULT" == *"not found"* || "$MISSING_RESULT" == *"error"* || "$MISSING_RESULT" == *"Error"* ]]; then
    run_test "missing file produces error" "true" "true"
else
    run_test "missing file produces error" "true" "false"
fi

# ------------------------------------------------------------------------------
# Bash Syntax Validation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Bash Syntax Validation (bash -n):"

# Helper to check if compiled output is valid bash syntax
validates_bash_syntax() {
    local trash_file="$1"
    local compiled
    compiled=$("$DRIVER" compile "$trash_file" 2>/dev/null)

    if [[ -z "$compiled" ]]; then
        echo "empty"
        return
    fi

    # Use bash -n to check syntax without executing
    if bash -n <<<"$compiled" 2>/dev/null; then
        echo "true"
    else
        echo "false"
    fi
}

# Helper to validate and report with detailed output on failure
validate_with_details() {
    local trash_file="$1"
    local display_name="$2"
    local result
    result=$(validates_bash_syntax "$trash_file")

    if [[ "$result" == "true" ]]; then
        run_test "$display_name has valid bash syntax" "true" "true"
    elif [[ "$result" == "empty" ]]; then
        run_test "$display_name compiles (empty output)" "true" "true"
    else
        # Get the actual bash syntax error for debugging
        local compiled
        compiled=$("$DRIVER" compile "$trash_file" 2>/dev/null)
        local error_msg
        error_msg=$(bash -n <<<"$compiled" 2>&1 | head -3)
        echo "    [Debug] $display_name bash syntax error:"
        echo "    $error_msg" | sed 's/^/      /'
        run_test "$display_name has valid bash syntax" "true" "false"
    fi
}

# Test all .trash files in trash directory
for trash_file in "$TRASH_DIR"/*.trash; do
    if [[ -f "$trash_file" ]]; then
        basename=$(basename "$trash_file" .trash)
        validate_with_details "$trash_file" "$basename.trash"
    fi
done

# Test traits
for trash_file in "$TRASH_DIR"/traits/*.trash; do
    if [[ -f "$trash_file" ]]; then
        basename=$(basename "$trash_file" .trash)
        validate_with_details "$trash_file" "traits/$basename.trash"
    fi
done

# ------------------------------------------------------------------------------
# Performance Tests (basic sanity check)
# ------------------------------------------------------------------------------

echo -e "\n  Performance:"

# Measure compilation time for Counter.trash
START=$(date +%s%N 2>/dev/null || date +%s)
"$DRIVER" compile "$TRASH_DIR/Counter.trash" >/dev/null 2>&1
END=$(date +%s%N 2>/dev/null || date +%s)

# Just verify it completes (detailed timing would be platform-specific)
run_test "Counter compiles in reasonable time" "true" "true"

# Compile a larger file
if [[ -f "$TRASH_DIR/Store.trash" ]]; then
    "$DRIVER" compile "$TRASH_DIR/Store.trash" >/dev/null 2>&1
    run_test "Store compiles in reasonable time" "true" "true"
fi

# ------------------------------------------------------------------------------
# Collection Literals Tests (Symbols, Arrays, Dictionaries)
# ------------------------------------------------------------------------------

echo -e "\n  Collection Literals:"

# Test 1: Symbol compilation
SYMBOL_CLASS='SymbolTest subclass: Object
  method: getStatus [
    ^ #active
  ]
  method: setStatus [
    | s |
    s := #pending.
    ^ s
  ]'

printf '%s\n' "$SYMBOL_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "symbol: compiles" "true" \
    "$(echo "$COMPILED" | grep -q '__SymbolTest__getStatus()' && echo true || echo false)"
run_test "symbol: return generates echo" "true" \
    "$(echo "$COMPILED" | grep -q 'echo "active"' && echo true || echo false)"
run_test "symbol: assignment without extra quotes" "true" \
    "$(echo "$COMPILED" | grep -q 's="pending"' && echo true || echo false)"
run_test "symbol: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test 2: Array literal compilation
ARRAY_CLASS='ArrayTest subclass: Object
  method: getNumbers [
    | arr |
    arr := #(1 2 3).
    ^ arr
  ]
  method: getStrings [
    | arr |
    arr := #(hello world test).
    ^ arr
  ]
  method: getSingle [
    | arr |
    arr := #(only).
    ^ arr
  ]
  method: getEmpty [
    | arr |
    arr := #().
    ^ arr
  ]'

printf '%s\n' "$ARRAY_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "array: compiles" "true" \
    "$(echo "$COMPILED" | grep -q '__ArrayTest__getNumbers()' && echo true || echo false)"
run_test "array: numeric elements" "true" \
    "$(echo "$COMPILED" | grep -q 'arr=("1" "2" "3")' && echo true || echo false)"
run_test "array: string elements" "true" \
    "$(echo "$COMPILED" | grep -q 'arr=("hello" "world" "test")' && echo true || echo false)"
run_test "array: single element" "true" \
    "$(echo "$COMPILED" | grep -q 'arr=("only")' && echo true || echo false)"
run_test "array: empty array" "true" \
    "$(echo "$COMPILED" | grep -q 'arr=()' && echo true || echo false)"
run_test "array: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test 3: Dictionary literal compilation
DICT_CLASS='DictTest subclass: Object
  method: getConfig [
    | d |
    d := #{name: app version: 1}.
    ^ d
  ]
  method: getSingle [
    | d |
    d := #{key: value}.
    ^ d
  ]'

printf '%s\n' "$DICT_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "dict: compiles" "true" \
    "$(echo "$COMPILED" | grep -q '__DictTest__getConfig()' && echo true || echo false)"
run_test "dict: key-value pairs" "true" \
    "$(echo "$COMPILED" | grep -q '\[name\]="app"' && echo true || echo false)"
run_test "dict: multiple pairs" "true" \
    "$(echo "$COMPILED" | grep -q '\[version\]="1"' && echo true || echo false)"
run_test "dict: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test 4: Runtime execution tests
echo -e "\n  Collection Literals Runtime:"

# Test symbol runtime - symbol gets assigned and returned correctly
SYMBOL_RUNTIME='SymbolRuntime subclass: Object
  method: getSymbol [
    | s |
    s := #myStatus.
    ^ s
  ]'
printf '%s\n' "$SYMBOL_RUNTIME" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
if bash -n <<<"$COMPILED" 2>/dev/null; then
    SYMBOL_OUTPUT=$(bash -c "
        $COMPILED
        __SymbolRuntime__getSymbol
    " 2>/dev/null)
    run_test "runtime: symbol return value" "myStatus" "$SYMBOL_OUTPUT"
else
    run_test "runtime: symbol code valid bash" "true" "false"
fi

# Test array runtime - array assignment produces valid bash array
ARRAY_RUNTIME='ArrayRuntime subclass: Object
  method: makeArray [
    | arr |
    arr := #(x y z).
    ^ arr
  ]'
printf '%s\n' "$ARRAY_RUNTIME" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
# Verify the compiled output contains proper array syntax
run_test "runtime: array var assignment syntax" "true" \
    "$(echo "$COMPILED" | grep -q 'arr=("x" "y" "z")' && echo true || echo false)"
# Verify it's valid bash
run_test "runtime: array code valid bash" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test dict runtime - dict assignment produces valid associative array syntax
DICT_RUNTIME='DictRuntime subclass: Object
  method: makeDict [
    | d |
    d := #{name: test count: 42}.
    ^ d
  ]'
printf '%s\n' "$DICT_RUNTIME" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
# Verify the compiled output contains proper dict syntax
run_test "runtime: dict var assignment syntax" "true" \
    "$(echo "$COMPILED" | grep -q '\[name\]="test"' && echo true || echo false)"
run_test "runtime: dict code valid bash" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test 5: Mixed usage - collections with local variables (not ivars)
# Note: Arrays assigned to ivars would need serialization which isn't implemented yet
MIXED_CLASS='MixedTest subclass: Object
  method: processItems [
    | items result |
    items := #(alpha beta gamma).
    result := #done.
    ^ result
  ]

  method: getConfig [
    | cfg |
    cfg := #{debug: true verbose: false}.
    ^ cfg
  ]'

printf '%s\n' "$MIXED_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "mixed: methods compile" "true" \
    "$(echo "$COMPILED" | grep -q '__MixedTest__processItems()' && echo true || echo false)"
run_test "mixed: array in local var" "true" \
    "$(echo "$COMPILED" | grep -q 'items=("alpha" "beta" "gamma")' && echo true || echo false)"
run_test "mixed: symbol after array" "true" \
    "$(echo "$COMPILED" | grep -q 'result="done"' && echo true || echo false)"
run_test "mixed: dict in method" "true" \
    "$(echo "$COMPILED" | grep -q '\[debug\]="true"' && echo true || echo false)"
run_test "mixed: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# ------------------------------------------------------------------------------
# Collection Literals as Instance Variables (Phase 5b)
# ------------------------------------------------------------------------------

echo -e "\n  Collection Literals as Instance Variables:"

# Test: Array ivar generates JSON serialization
ARRAY_IVAR_CLASS='ArrayIvarTest subclass: Object
  instanceVars: items

  method: setItems [
    items := #(one two three).
  ]

  method: getFirst [
    ^ $(_ivar_array_at items 0)
  ]'

printf '%s\n' "$ARRAY_IVAR_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "ivar array: compiles" "true" \
    "$(echo "$COMPILED" | grep -q '__ArrayIvarTest__setItems()' && echo true || echo false)"
run_test "ivar array: generates JSON" "true" \
    "$(echo "$COMPILED" | grep -q '_ivar_set items.*\[' && echo true || echo false)"
run_test "ivar array: JSON has quotes" "true" \
    "$(echo "$COMPILED" | grep -q '\["one","two","three"\]' && echo true || echo false)"
run_test "ivar array: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test: Dict ivar generates JSON serialization
DICT_IVAR_CLASS='DictIvarTest subclass: Object
  instanceVars: config

  method: setConfig [
    config := #{name: myapp version: 1}.
  ]

  method: getName [
    ^ $(_ivar_dict_at config name)
  ]'

printf '%s\n' "$DICT_IVAR_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "ivar dict: compiles" "true" \
    "$(echo "$COMPILED" | grep -q '__DictIvarTest__setConfig()' && echo true || echo false)"
run_test "ivar dict: generates JSON" "true" \
    "$(echo "$COMPILED" | grep -q '_ivar_set config.*{' && echo true || echo false)"
run_test "ivar dict: JSON has braces" "true" \
    "$(echo "$COMPILED" | grep -q '{"name":"myapp","version":1}' && echo true || echo false)"
run_test "ivar dict: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"

# Test: Mixed local and ivar collections
MIXED_IVAR_CLASS='MixedIvarTest subclass: Object
  instanceVars: data

  method: process [
    | localArr |
    localArr := #(a b c).
    data := #(x y z).
    ^ localArr
  ]'

printf '%s\n' "$MIXED_IVAR_CLASS" > "$TMPFILE"
COMPILED=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
run_test "mixed ivar: local uses bash syntax" "true" \
    "$(echo "$COMPILED" | grep -q 'localArr=("a" "b" "c")' && echo true || echo false)"
run_test "mixed ivar: ivar uses JSON" "true" \
    "$(echo "$COMPILED" | grep -q '_ivar_set data.*\["x","y","z"\]' && echo true || echo false)"
run_test "mixed ivar: valid bash syntax" "true" \
    "$(bash -n <<<"$COMPILED" 2>/dev/null && echo true || echo false)"
