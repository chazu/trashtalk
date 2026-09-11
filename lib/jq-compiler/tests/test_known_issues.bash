#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# ==============================================================================
# Regression tests for known issues documented in CLAUDE.md
# ==============================================================================
# Former defects remain ordinary failing regressions if they return.
# The production tokenizer, parser, and generator are used below.
# ==============================================================================

source "$(dirname "${BASH_SOURCE[0]}")/test_helper.bash"

# Helper: compile source and return output
compile() {
    local input="$1"
    echo "$input" | "$COMPILER_DIR/tokenizer.bash" /dev/stdin | \
        jq -f "$COMPILER_DIR/parser.jq" | \
        jq -r -f "$COMPILER_DIR/codegen.jq" 2>/dev/null
}

compile_contains() {
    local input="$1"
    local pattern="$2"
    local output
    output=$(compile "$input")
    if echo "$output" | grep -qF "$pattern"; then
        echo "true"
    else
        echo "false"
    fi
}

# ==============================================================================
# Bug 1: Method name collision (keyword vs unary with same base)
# Unary and keyword selectors must have distinct generated names.
# ==============================================================================
CURRENT_SECTION="Method Name Collision"
echo ""
echo "  Method Name Collision:"

INPUT_COLLISION='TestCollision subclass: Object
  method: skip [
    ^ "unary"
  ]

  method: skip: count [
    ^ count
  ]'

# The compiler should generate distinct function names
compiled=$(compile "$INPUT_COLLISION")
has_unary=$(echo "$compiled" | grep -c '^__TestCollision__skip()' || true)
has_keyword=$(echo "$compiled" | grep -c '^__TestCollision__skip_()' || true)

run_test "unary 'skip' generates __TestCollision__skip()" "1" "$has_unary"
run_test "keyword 'skip:' generates __TestCollision__skip_()" "1" "$has_keyword"
run_test "distinct function names for skip vs skip:" "true" \
    "$(if [[ "$has_unary" -ge 1 && "$has_keyword" -ge 1 ]]; then echo true; else echo false; fi)"

# ==============================================================================
# Bug 2: Negative numbers in arguments
# Preserve negative literals in messages and arithmetic.
# ==============================================================================
CURRENT_SECTION="Negative Number Arguments"
echo ""
echo "  Negative Number Arguments:"

INPUT_NEGNUM='TestNeg subclass: Object
  method: test [
    | result |
    result := @ self range: 0 to: -1
    ^ result
  ]'

compiled=$(compile "$INPUT_NEGNUM")
# The compiled output should keep the negative number intact
run_test "negative number preserved in message send" "true" \
    "$(if echo "$compiled" | grep -qF 'to: -1'; then echo true; else echo false; fi)"

INPUT_NEGARITH='TestNeg2 subclass: Object
  instanceVars: value:10
  method: test [
    value := value + -1
    ^ value
  ]'

compiled=$(compile "$INPUT_NEGARITH")
run_test "negative number preserved in arithmetic" "true" \
    "$(if echo "$compiled" | grep -qF '+ -1'; then echo true; else echo false; fi)"

# ==============================================================================
# Bug 3: ifTrue: with non-predicate expressions
# Compare predicate message results as strings, not arithmetic.
# ==============================================================================
CURRENT_SECTION="ifTrue with Message Send"
echo ""
echo "  ifTrue with Message Send:"

INPUT_IFTRUE='TestIfTrue subclass: Object
  instanceVars: data:hello
  method: testBug [
    (@ String contains: "x" in: data) ifTrue: [
      ^ "found"
    ]
    ^ "not found"
  ]'

compiled=$(compile "$INPUT_IFTRUE")

# FIXED: a message send used as an ifTrue: condition now compiles to a string
# comparison ([[ "$(@ ...)" == "true" ]]) instead of an invalid (( @ ... ))
# arithmetic context.
has_arithmetic_send=$(echo "$compiled" | grep -c '(( @' || true)
run_test "message send inside ifTrue: avoids (( )) arithmetic" "0" "$has_arithmetic_send"

uses_string_compare=$(echo "$compiled" | grep -c '\[\[ "\$(@ String contains' || true)
run_test "message send inside ifTrue: uses string comparison" "1" "$uses_string_compare"

# ==============================================================================
# Bug 4: Namespace references in rawMethod bodies
# Reconstruct qualified names without spaces in raw bodies.
# ==============================================================================
CURRENT_SECTION="Namespace in rawMethod"
echo ""
echo "  Namespace in rawMethod:"

INPUT_NS_RAW='package: MyPkg

TestNsRaw subclass: Object
  rawMethod: test [
    @ MyPkg::Helper doSomething
  ]'

compiled=$(compile "$INPUT_NS_RAW")

# FIXED: in rawMethod bodies, Pkg::Class is now preserved intact rather than
# being split into "@ MyPkg :: Helper".
has_split=$(echo "$compiled" | grep -c '@ MyPkg :: Helper' || true)
has_intact=$(echo "$compiled" | grep -c '@ MyPkg::Helper' || true)

run_test "namespace ref in rawMethod body is not split" "0" "$has_split"
run_test "namespace ref in rawMethod body stays intact" "1" "$has_intact"

# ==============================================================================
# Summary
# ==============================================================================

print_test_summary
