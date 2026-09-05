#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# ==============================================================================
# Tests for standalone test_expr codegen (predicates as values)
# ==============================================================================
# Verifies that test predicates work as return values, assignments,
# and inside ifTrue: blocks.
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
# Test: Return value predicates (^ path fileExists)
# ==============================================================================
CURRENT_SECTION="Return Value Predicates"
echo ""
echo "  Return Value Predicates:"

INPUT_RETURN='TestRet subclass: Object
  instanceVars: path:/tmp/test

  method: testExists [
    ^ path fileExists
  ]

  method: testFile [
    ^ path isFile
  ]

  method: testDir [
    ^ path isDirectory
  ]

  method: testEmpty [
    ^ path isEmpty
  ]

  method: testNotEmpty [
    ^ path notEmpty
  ]

  method: testReadable [
    ^ path isReadable
  ]

  method: testWritable [
    ^ path isWritable
  ]

  method: testExecutable [
    ^ path isExecutable
  ]

  method: testSymlink [
    ^ path isSymlink
  ]

  method: testFifo [
    ^ path isFifo
  ]'

compiled=$(compile "$INPUT_RETURN")

run_test "^ fileExists generates && echo true || echo false" "true" \
    "$(echo "$compiled" | grep -qF '[[ -e "$(_ivar path)" ]] && echo "true" || echo "false"; return' && echo true || echo false)"

run_test "^ isFile generates [[ -f ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -f "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ isDirectory generates [[ -d ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -d "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ isEmpty generates [[ -z ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -z "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ notEmpty generates [[ -n ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -n "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ isReadable generates [[ -r ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -r "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ isSymlink generates [[ -L ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -L "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "^ isFifo generates [[ -p ]]" "true" \
    "$(echo "$compiled" | grep -qF '[[ -p "$(_ivar path)" ]] && echo "true"' && echo true || echo false)"

run_test "compiled output is valid bash" "0" \
    "$(echo "$compiled" | bash -n 2>&1; echo $?)"

# ==============================================================================
# Test: Assignment predicates (result := path fileExists)
# ==============================================================================
CURRENT_SECTION="Assignment Predicates"
echo ""
echo "  Assignment Predicates:"

INPUT_ASSIGN='TestAssign subclass: Object
  instanceVars: path:/tmp/test

  method: testAssign [
    | result |
    result := path fileExists
    ^ result
  ]'

compiled=$(compile "$INPUT_ASSIGN")

run_test "assignment uses subshell for test_expr" "true" \
    "$(echo "$compiled" | grep -qF 'result="$([[ -e' && echo true || echo false)"

run_test "assignment subshell has && echo true" "true" \
    "$(echo "$compiled" | grep -qF '&& echo true || echo false)' && echo true || echo false)"

run_test "assignment output is valid bash" "0" \
    "$(echo "$compiled" | bash -n 2>&1; echo $?)"

# ==============================================================================
# Test: Local variable predicates (^ localVar isEmpty)
# ==============================================================================
CURRENT_SECTION="Local Variable Predicates"
echo ""
echo "  Local Variable Predicates:"

INPUT_LOCAL='TestLocal subclass: Object
  method: testLocal [
    | name |
    name := "hello"
    ^ name isEmpty
  ]'

compiled=$(compile "$INPUT_LOCAL")

run_test "local var predicate uses $name not _ivar" "true" \
    "$(echo "$compiled" | grep -qF '[[ -z "$name" ]]' && echo true || echo false)"

run_test "local var predicate output is valid bash" "0" \
    "$(echo "$compiled" | bash -n 2>&1; echo $?)"

# ==============================================================================
# Test: Predicate still works inside ifTrue: (regression)
# ==============================================================================
CURRENT_SECTION="ifTrue Predicate (Regression)"
echo ""
echo "  ifTrue Predicate (Regression):"

INPUT_IFTRUE='TestIfTrue subclass: Object
  instanceVars: path:/tmp/test

  method: testIfTrue [
    (path fileExists) ifTrue: [
      ^ "yes"
    ]
    ^ "no"
  ]'

compiled=$(compile "$INPUT_IFTRUE")

run_test "ifTrue still uses if [[ -e ]] syntax" "true" \
    "$(echo "$compiled" | grep -qF 'if [[ -e "$(_ivar path)" ]]; then' && echo true || echo false)"

run_test "ifTrue output is valid bash" "0" \
    "$(echo "$compiled" | bash -n 2>&1; echo $?)"

# ==============================================================================
# Summary
# ==============================================================================

print_test_summary
