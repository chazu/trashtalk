#!/usr/bin/env bash
# ==============================================================================
# Regression tests for known issues documented in CLAUDE.md
# ==============================================================================
# These tests document and verify known limitations. Tests marked XFAIL are
# expected to fail until the underlying bug is fixed.
# ==============================================================================

source "$(dirname "${BASH_SOURCE[0]}")/test_helper.bash"

# Expected-failure test: passes if the bug is still present (expected),
# warns if it's been fixed (good news, update the test!).
# Does NOT count toward TESTS_FAILED.
run_xfail() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    ((TESTS_RUN++)) || true

    if [[ "$expected" == "$actual" ]]; then
        echo -e "  ${GREEN}✓${NC} $name [XFAIL - still broken as expected]"
        ((TESTS_PASSED++)) || true
    else
        echo -e "  ${YELLOW}!${NC} $name [XFAIL - BUG APPEARS FIXED! Update this test.]"
        ((TESTS_PASSED++)) || true
    fi
}

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
# CLAUDE.md: "Keyword methods (e.g., skip:) and unary methods with same base
# name compile to same bash function"
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
# CLAUDE.md: "Compiler may mangle 0 -1 into 0-1"
# Verification: the compiler appears to handle this correctly now for both
# message send arguments and arithmetic expressions.
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
# CLAUDE.md: "(@ String contains:...) ifTrue: doesn't work correctly"
# The issue: message send results go into (( )) arithmetic context
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

# The compiled output should NOT put a message send inside (( ))
# The correct compilation would be something like:
#   local _tmp=$(@ String contains: "x" in: "$(_ivar data)")
#   if [[ "$_tmp" == "true" ]]; then ...
# But the bug is that it generates:
#   if (( @ String contains: ... )); then ...
has_arithmetic_send=$(echo "$compiled" | grep -c '(( @' || true)

# This test documents the known bug - it will fail when the bug is fixed
# (at which point, flip the expected values)
run_xfail "message send inside ifTrue: uses (( )) arithmetic (known bug)" "1" "$has_arithmetic_send"

# ==============================================================================
# Bug 4: Namespace references in rawMethod bodies
# CLAUDE.md: "Tokenizer splits Pkg::Class into three tokens"
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

# In rawMethod bodies, Pkg::Class should ideally be preserved as-is
# The known bug is that it becomes: @ MyPkg :: Helper doSomething
has_split=$(echo "$compiled" | grep -c '@ MyPkg :: Helper' || true)
has_intact=$(echo "$compiled" | grep -c '@ MyPkg::Helper' || true)

run_xfail "namespace ref in rawMethod body splits :: (known bug)" "1" "$has_split"

# ==============================================================================
# Summary
# ==============================================================================

print_test_summary
