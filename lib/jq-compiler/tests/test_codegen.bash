# ==============================================================================
# Codegen Tests
# ==============================================================================
# Tests for codegen.jq - validates generated bash code
# ==============================================================================

DRIVER="$COMPILER_DIR/driver.bash"
TMPFILE="/tmp/test_codegen_$$.trash"

# Cleanup on exit
trap 'rm -f "$TMPFILE"' EXIT

# Helper to compile input and search for pattern
compile_contains() {
    local input="$1"
    local pattern="$2"
    local output
    printf '%s\n' "$input" > "$TMPFILE"
    output=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
    if printf '%s\n' "$output" | grep -qF "$pattern"; then
        echo "true"
    else
        echo "false"
    fi
}

# Helper to compile and get specific line
compile_grep() {
    local input="$1"
    local pattern="$2"
    printf '%s\n' "$input" > "$TMPFILE"
    "$DRIVER" compile "$TMPFILE" 2>/dev/null | grep -E "$pattern" | head -1
}

# ------------------------------------------------------------------------------
# Header Generation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Header Generation:"

INPUT_SIMPLE='Counter subclass: Object'

run_test "generates shebang" "true" \
    "$(compile_contains "$INPUT_SIMPLE" '#!/bin/bash')"

run_test "generates DO NOT EDIT" "true" \
    "$(compile_contains "$INPUT_SIMPLE" 'DO NOT EDIT')"

run_test "generates jq marker" "true" \
    "$(compile_contains "$INPUT_SIMPLE" '(jq)')"

# ------------------------------------------------------------------------------
# Metadata Generation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Metadata Generation:"

run_test "generates superclass" "true" \
    "$(compile_contains "$INPUT_SIMPLE" '__Counter__superclass="Object"')"

INPUT_WITH_IVARS='Counter subclass: Object
  instanceVars: value:0 step:1'

run_test "generates instanceVars" "true" \
    "$(compile_contains "$INPUT_WITH_IVARS" '__Counter__instanceVars="value:0 step:1"')"

INPUT_WITH_TRAIT='Counter subclass: Object
  include: Debuggable'

run_test "generates traits" "true" \
    "$(compile_contains "$INPUT_WITH_TRAIT" '__Counter__traits="Debuggable"')"

INPUT_TRAIT='Debuggable trait'

run_test "trait generates is_trait" "true" \
    "$(compile_contains "$INPUT_TRAIT" '__Debuggable__is_trait="1"')"

# ------------------------------------------------------------------------------
# Method Name Generation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Method Names:"

INPUT_INSTANCE='Counter subclass: Object
  method: increment [
    ^ 1
  ]'

run_test "instance method name" "true" \
    "$(compile_contains "$INPUT_INSTANCE" '__Counter__increment()')"

INPUT_CLASS='Counter subclass: Object
  classMethod: description [
    ^ "test"
  ]'

run_test "class method name" "true" \
    "$(compile_contains "$INPUT_CLASS" '__Counter__class__description()')"

INPUT_KEYWORD='Counter subclass: Object
  method: setValue: val [
    ^ 1
  ]'

run_test "keyword method name" "true" \
    "$(compile_contains "$INPUT_KEYWORD" '__Counter__setValue()')"

INPUT_MULTI_KW='Store subclass: Object
  method: getField: id field: name [
    ^ 1
  ]'

run_test "multi-keyword method name" "true" \
    "$(compile_contains "$INPUT_MULTI_KW" '__Store__getField_field()')"

# ------------------------------------------------------------------------------
# Argument Binding Tests
# ------------------------------------------------------------------------------

echo -e "\n  Argument Bindings:"

run_test "keyword arg binding" "true" \
    "$(compile_contains "$INPUT_KEYWORD" 'local val="$1"')"

run_test "multi-keyword first arg" "true" \
    "$(compile_contains "$INPUT_MULTI_KW" 'local id="$1"')"

run_test "multi-keyword second arg" "true" \
    "$(compile_contains "$INPUT_MULTI_KW" 'local name="$2"')"

# ------------------------------------------------------------------------------
# Local Variable Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Local Variable Transformation:"

INPUT_LOCALS='Counter subclass: Object
  method: test [
    | x y z |
    ^ x
  ]'

run_test "local vars transformed" "true" \
    "$(compile_contains "$INPUT_LOCALS" 'local x y z')"

# ------------------------------------------------------------------------------
# Return Statement Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Return Statement Transformation:"

INPUT_RETURN='Counter subclass: Object
  method: test [
    ^ 42
  ]'

run_test "return becomes echo" "true" \
    "$(compile_contains "$INPUT_RETURN" 'echo 42')"

INPUT_RETURN_VAR='Counter subclass: Object
  method: test [
    | x |
    ^ $x
  ]'

run_test "return var" "true" \
    "$(compile_contains "$INPUT_RETURN_VAR" 'echo $x')"

# ------------------------------------------------------------------------------
# Self Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Self Transformation:"

INPUT_SELF='Counter subclass: Object
  method: test [
    ^ self
  ]'

run_test "self becomes _RECEIVER" "true" \
    "$(compile_contains "$INPUT_SELF" 'echo $_RECEIVER')"

INPUT_SELF_MSG='Counter subclass: Object
  method: test [
    @ self getValue
  ]'

run_test "@ self message" "true" \
    "$(compile_contains "$INPUT_SELF_MSG" '@ "$_RECEIVER" getValue')"

# ------------------------------------------------------------------------------
# Assignment Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Assignment Transformation:"

INPUT_ASSIGN='Counter subclass: Object
  method: test [
    | x |
    x := 5
  ]'

run_test "assignment := to =" "true" \
    "$(compile_contains "$INPUT_ASSIGN" 'x=5')"

INPUT_ASSIGN_EXPR='Counter subclass: Object
  method: test [
    | x |
    x := $(echo hello)
  ]'

run_test "assignment with subshell" "true" \
    "$(compile_contains "$INPUT_ASSIGN_EXPR" 'x=$(echo hello)')"

# ------------------------------------------------------------------------------
# Message Send Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Message Send Transformation:"

INPUT_MSG='Counter subclass: Object
  method: test [
    @ obj getValue
  ]'

run_test "simple message" "true" \
    "$(compile_contains "$INPUT_MSG" '@ obj getValue')"

INPUT_MSG_KW='Counter subclass: Object
  method: test [
    @ obj setValue: 5
  ]'

# Note: Arguments are quoted in the output
run_test "keyword message colon removed" "true" \
    "$(compile_contains "$INPUT_MSG_KW" '@ obj setValue')"

INPUT_MSG_MULTI='Counter subclass: Object
  method: test [
    @ obj setX: "1" y: "2"
  ]'

run_test "multi-keyword joined" "true" \
    "$(compile_contains "$INPUT_MSG_MULTI" '@ obj setX_y')"

INPUT_MSG_VAR='Counter subclass: Object
  method: test [
    | x |
    @ self setValue: $x
  ]'

run_test "var arg unquoted" "true" \
    "$(compile_contains "$INPUT_MSG_VAR" 'setValue $x')"

# ------------------------------------------------------------------------------
# Subshell Transformation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Subshell Transformation:"

INPUT_SUBSHELL='Counter subclass: Object
  method: test [
    | x |
    x := $(@ self getValue)
  ]'

run_test "subshell preserved" "true" \
    "$(compile_contains "$INPUT_SUBSHELL" 'x=$(@ $_RECEIVER getValue)')"

INPUT_SUBSHELL_KW='Counter subclass: Object
  method: test [
    | x |
    x := $(@ Store getClass: $id)
  ]'

run_test "subshell keyword transformed" "true" \
    "$(compile_contains "$INPUT_SUBSHELL_KW" '$(@ Store getClass $id)')"

# ------------------------------------------------------------------------------
# Raw Method Tests
# ------------------------------------------------------------------------------

echo -e "\n  Raw Method Handling:"

INPUT_RAW='Process subclass: Object
  rawMethod: test [
    if [[ -n "$var" ]]; then
      echo "yes"
    fi
  ]'

run_test "raw preserves if" "true" \
    "$(compile_contains "$INPUT_RAW" 'if [[')"

run_test "raw preserves then" "true" \
    "$(compile_contains "$INPUT_RAW" 'then')"

run_test "raw preserves fi" "true" \
    "$(compile_contains "$INPUT_RAW" 'fi')"

# ------------------------------------------------------------------------------
# Whitespace Normalization Tests
# ------------------------------------------------------------------------------

echo -e "\n  Whitespace Normalization:"

INPUT_REGEX='Counter subclass: Object
  rawMethod: test [
    if [[ "$x" =~ ^[0-9]+$ ]]; then
      echo "number"
    fi
  ]'

run_test "regex quantifier +" "true" \
    "$(compile_contains "$INPUT_REGEX" '[0-9]+')"

INPUT_PATH='Counter subclass: Object
  rawMethod: test [
    echo "test" >/dev/null
  ]'

run_test "path /dev/null" "true" \
    "$(compile_contains "$INPUT_PATH" '>/dev/null')"

# ------------------------------------------------------------------------------
# Conditional Bracket Tests
# ------------------------------------------------------------------------------

echo -e "\n  Conditional Brackets:"

INPUT_COND='Counter subclass: Object
  rawMethod: test [
    [[ -n "$x" ]] && echo yes
  ]'

run_test "double brackets spaced" "true" \
    "$(compile_contains "$INPUT_COND" '[[ -n')"

run_test "closing brackets spaced" "true" \
    "$(compile_contains "$INPUT_COND" ' ]]')"

# ------------------------------------------------------------------------------
# Process.trash Issue Tests - Assignment Spacing
# ------------------------------------------------------------------------------

echo -e "\n  Assignment Spacing (Process.trash issues):"

# Test that bash = assignments don't get extra spaces
INPUT_BASH_ASSIGN='Counter subclass: Object
  rawMethod: test [
    local x="hello"
    local y=5
  ]'

run_test "local assignment no space before =" "true" \
    "$(compile_contains "$INPUT_BASH_ASSIGN" 'local x="hello"')"

run_test "local number assignment no space" "true" \
    "$(compile_contains "$INPUT_BASH_ASSIGN" 'local y=5')"

# Test environment variable assignments
INPUT_ENV_VAR='Counter subclass: Object
  rawMethod: test [
    TRASH_DEBUG=0 @ obj test
  ]'

run_test "env var assignment no space" "true" \
    "$(compile_contains "$INPUT_ENV_VAR" 'TRASH_DEBUG=0')"

# ------------------------------------------------------------------------------
# Process.trash Issue Tests - Special Variables
# ------------------------------------------------------------------------------

echo -e "\n  Special Variables (Process.trash issues):"

# Test $$ (current PID)
INPUT_DOLLAR_DOLLAR='Counter subclass: Object
  rawMethod: test [
    local id="prefix_$$_suffix"
  ]'

run_test "double dollar preserved" "true" \
    "$(compile_contains "$INPUT_DOLLAR_DOLLAR" '$$')"

# Test $! (background PID)
INPUT_DOLLAR_BANG='Counter subclass: Object
  rawMethod: test [
    local bg_pid=$!
  ]'

run_test "dollar bang preserved" "true" \
    "$(compile_contains "$INPUT_DOLLAR_BANG" 'bg_pid=$!')"

# ------------------------------------------------------------------------------
# Process.trash Issue Tests - Negation Spacing
# ------------------------------------------------------------------------------

echo -e "\n  Negation Spacing (Process.trash issues):"

INPUT_NEGATION='Counter subclass: Object
  rawMethod: test [
    if ! kill -0 "$pid" 2>/dev/null; then
      echo "dead"
    fi
  ]'

run_test "negation has space after !" "true" \
    "$(compile_contains "$INPUT_NEGATION" '! kill')"

# ------------------------------------------------------------------------------
# Process.trash Issue Tests - Increment/Decrement
# ------------------------------------------------------------------------------

echo -e "\n  Increment/Decrement (Process.trash issues):"

INPUT_INCREMENT='Counter subclass: Object
  rawMethod: test [
    ((count++))
    ((i--))
  ]'

run_test "increment operator" "true" \
    "$(compile_contains "$INPUT_INCREMENT" '((count++))')"

run_test "decrement operator" "true" \
    "$(compile_contains "$INPUT_INCREMENT" '((i--))')"

# ------------------------------------------------------------------------------
# Process.trash Issue Tests - Raw Method Structure
# ------------------------------------------------------------------------------

echo -e "\n  Raw Method Structure (Process.trash issues):"

INPUT_RAW_STRUCT='Counter subclass: Object
  rawMethod: test [
    while true; do
        echo "loop"
        sleep 1
    done
  ]'

run_test "while loop preserved" "true" \
    "$(compile_contains "$INPUT_RAW_STRUCT" 'while true; do')"

run_test "done keyword preserved" "true" \
    "$(compile_contains "$INPUT_RAW_STRUCT" 'done')"

# Test case statement
INPUT_CASE='Counter subclass: Object
  rawMethod: test [
    case "$x" in
        "a") echo "A" ;;
        "b") echo "B" ;;
        *) echo "other" ;;
    esac
  ]'

run_test "case statement preserved" "true" \
    "$(compile_contains "$INPUT_CASE" 'case "$x" in')"

run_test "esac preserved" "true" \
    "$(compile_contains "$INPUT_CASE" 'esac')"
