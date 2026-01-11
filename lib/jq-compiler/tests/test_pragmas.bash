#!/usr/bin/env bash
# ==============================================================================
# Pragma Tests
# ==============================================================================
# Tests for pragma: directive parsing and code generation
# ==============================================================================

# Source shared test helper for standalone execution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.bash"

TOKENIZER="$COMPILER_DIR/tokenizer.bash"
PARSER="$COMPILER_DIR/parser.jq"
DRIVER="$COMPILER_DIR/driver.bash"
TMPFILE="/tmp/test_pragmas_$$.trash"

# Cleanup on exit
trap 'rm -f "$TMPFILE"' EXIT

# Helper to parse input and get specific field
parse_field() {
    local input="$1"
    local field="$2"
    echo "$input" | "$TOKENIZER" | jq -f "$PARSER" | jq -r "$field // \"null\""
}

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

# Helper to compile and check pattern does NOT exist
compile_not_contains() {
    local input="$1"
    local pattern="$2"
    local output
    printf '%s\n' "$input" > "$TMPFILE"
    output=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
    if printf '%s\n' "$output" | grep -qF "$pattern"; then
        echo "false"
    else
        echo "true"
    fi
}

# ------------------------------------------------------------------------------
# Parser: Pragma Extraction Tests
# ------------------------------------------------------------------------------

echo -e "\n  Pragma Parsing:"

INPUT_PRAGMA_DIRECT='TestClass subclass: Object
  rawMethod: fastMethod [
    pragma: direct
    echo "fast"
  ]'

run_test "method has pragmas array" "1" \
    "$(parse_field "$INPUT_PRAGMA_DIRECT" '.methods[0].pragmas | length')"

run_test "pragma value is direct" "direct" \
    "$(parse_field "$INPUT_PRAGMA_DIRECT" '.methods[0].pragmas[0]')"

# Check that no token has value "pragma:" - count should be 0
run_test "pragma removed from body" "0" \
    "$(parse_field "$INPUT_PRAGMA_DIRECT" '[.methods[0].body.tokens[] | select(.value == "pragma:")] | length')"

INPUT_NO_PRAGMA='TestClass subclass: Object
  rawMethod: normalMethod [
    echo "normal"
  ]'

run_test "method without pragma has empty pragmas" "0" \
    "$(parse_field "$INPUT_NO_PRAGMA" '.methods[0].pragmas | length')"

INPUT_MULTIPLE_METHODS='TestClass subclass: Object
  rawMethod: fastMethod [
    pragma: direct
    echo "fast"
  ]
  rawMethod: normalMethod [
    echo "normal"
  ]'

run_test "first method has pragma" "1" \
    "$(parse_field "$INPUT_MULTIPLE_METHODS" '.methods[0].pragmas | length')"

run_test "second method has no pragma" "0" \
    "$(parse_field "$INPUT_MULTIPLE_METHODS" '.methods[1].pragmas | length')"

# Test pragma with keyword method
INPUT_PRAGMA_KEYWORD='TestClass subclass: Object
  rawMethod: dispatch: event [
    pragma: direct
    echo "$event"
  ]'

run_test "keyword method parses pragma" "direct" \
    "$(parse_field "$INPUT_PRAGMA_KEYWORD" '.methods[0].pragmas[0]')"

run_test "keyword method selector correct" "dispatch_" \
    "$(parse_field "$INPUT_PRAGMA_KEYWORD" '.methods[0].selector')"

# ------------------------------------------------------------------------------
# Codegen: Pragma Marker Tests
# ------------------------------------------------------------------------------

echo -e "\n  Pragma Code Generation:"

run_test "generates direct marker for pragma method" "true" \
    "$(compile_contains "$INPUT_PRAGMA_DIRECT" 'declare -g __TestClass__fastMethod__direct=1')"

run_test "generates function after marker" "true" \
    "$(compile_contains "$INPUT_PRAGMA_DIRECT" '__TestClass__fastMethod() {')"

run_test "no marker for method without pragma" "true" \
    "$(compile_not_contains "$INPUT_NO_PRAGMA" '__direct=1')"

run_test "normal method still generated" "true" \
    "$(compile_contains "$INPUT_NO_PRAGMA" '__TestClass__normalMethod() {')"

# Test that multiple methods are handled correctly
run_test "only pragma method gets marker" "true" \
    "$(compile_contains "$INPUT_MULTIPLE_METHODS" '__TestClass__fastMethod__direct=1')"

run_test "non-pragma method has no marker" "true" \
    "$(compile_not_contains "$INPUT_MULTIPLE_METHODS" '__TestClass__normalMethod__direct=1')"

# Test keyword method marker generation
run_test "keyword method gets correct marker" "true" \
    "$(compile_contains "$INPUT_PRAGMA_KEYWORD" '__TestClass__dispatch___direct=1')"

# Test with namespaced class
INPUT_NAMESPACED='package: MyApp

TestClass subclass: Object
  rawMethod: fastMethod [
    pragma: direct
    echo "fast"
  ]'

run_test "namespaced class gets correct marker" "true" \
    "$(compile_contains "$INPUT_NAMESPACED" '__MyApp__TestClass__fastMethod__direct=1')"

# Test pragma doesn't affect method body
INPUT_PRAGMA_BODY='TestClass subclass: Object
  rawMethod: testMethod [
    pragma: direct
    MYVAR="value"
    echo "$MYVAR"
  ]'

# Helper to extract just the method function body (between { and })
compile_method_body_not_contains() {
    local input="$1"
    local pattern="$2"
    local output
    printf '%s\n' "$input" > "$TMPFILE"
    output=$("$DRIVER" compile "$TMPFILE" 2>/dev/null)
    # Extract method body: from __TestClass__testMethod() { to the closing }
    local body
    body=$(printf '%s\n' "$output" | sed -n '/__TestClass__testMethod() {/,/^}/p')
    if printf '%s\n' "$body" | grep -qF "$pattern"; then
        echo "false"
    else
        echo "true"
    fi
}

run_test "pragma not in method function body" "true" \
    "$(compile_method_body_not_contains "$INPUT_PRAGMA_BODY" 'pragma:')"

run_test "body code preserved" "true" \
    "$(compile_contains "$INPUT_PRAGMA_BODY" 'MYVAR="value"')"
