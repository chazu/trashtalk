#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test the expression parser in codegen.jq

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
TOKENIZER="$COMPILER_DIR/tokenizer.bash"
CODEGEN="$COMPILER_DIR/codegen.jq"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Test counters
PASSED=0
FAILED=0

pass() {
    echo -e "  ${GREEN}✓${NC} $1"
    ((PASSED++)) || true
}

fail() {
    echo -e "  ${RED}✗${NC} $1: expected '$2', got '$3'"
    ((FAILED++)) || true
}

assert_json_has() {
    local json="$1"
    local jq_filter="$2"
    local expected="$3"
    local desc="$4"
    local actual
    actual=$(echo "$json" | jq -r "$jq_filter" 2>/dev/null || echo "ERROR")
    if [[ "$actual" == "$expected" ]]; then
        pass "$desc"
    else
        fail "$desc" "$expected" "$actual"
    fi
}

echo "Expression Parser Tests"
echo "======================="

# Create a minimal test jq file that can call the expr parser functions
# Exercise the definitions from the production generator, stopping before its
# class-generation entry point. No parser implementation is duplicated here.
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-expr-parser.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
awk '/^# Entry Point$/ { exit } { print }' "$CODEGEN" > "$scratch/parser.jq"
printf '%s\n' '{tokens: ., pos: 0} | expr_parse_stmts' >> "$scratch/parser.jq"


echo ""
echo "Test 1: Simple assignment (result := value)"
TOKENS='[
  {"type":"IDENTIFIER","value":"result","line":1,"col":0},
  {"type":"ASSIGN","value":":=","line":1,"col":7},
  {"type":"IDENTIFIER","value":"value","line":1,"col":10}
]'
RESULT=$(echo "$TOKENS" | jq -f "$scratch/parser.jq" 2>&1)
assert_json_has "$RESULT" '.type' 'statements' "Test 1: parses as statements"
assert_json_has "$RESULT" '.body[0].type' 'assignment' "Test 1: first stmt is assignment"
assert_json_has "$RESULT" '.body[0].target' 'result' "Test 1: assignment target is 'result'"

echo ""
echo "Test 2: Binary expression (value + step)"
TOKENS='[
  {"type":"IDENTIFIER","value":"value","line":1,"col":0},
  {"type":"PLUS","value":"+","line":1,"col":6},
  {"type":"IDENTIFIER","value":"step","line":1,"col":8}
]'
RESULT=$(echo "$TOKENS" | jq -f "$scratch/parser.jq" 2>&1)
assert_json_has "$RESULT" '.body[0].type' 'binary' "Test 2: parses as binary expression"
assert_json_has "$RESULT" '.body[0].op' '+' "Test 2: operator is +"
assert_json_has "$RESULT" '.body[0].left.name' 'value' "Test 2: left operand is 'value'"
assert_json_has "$RESULT" '.body[0].right.name' 'step' "Test 2: right operand is 'step'"

echo ""
echo "Test 3: Full assignment (result := value + step)"
TOKENS='[
  {"type":"IDENTIFIER","value":"result","line":1,"col":0},
  {"type":"ASSIGN","value":":=","line":1,"col":7},
  {"type":"IDENTIFIER","value":"value","line":1,"col":10},
  {"type":"PLUS","value":"+","line":1,"col":16},
  {"type":"IDENTIFIER","value":"step","line":1,"col":18}
]'
RESULT=$(echo "$TOKENS" | jq -f "$scratch/parser.jq" 2>&1)
assert_json_has "$RESULT" '.body[0].type' 'assignment' "Test 3: parses as assignment"
assert_json_has "$RESULT" '.body[0].target' 'result' "Test 3: target is 'result'"
assert_json_has "$RESULT" '.body[0].value.type' 'binary' "Test 3: value is binary expression"
assert_json_has "$RESULT" '.body[0].value.op' '+' "Test 3: value operator is +"

echo ""
echo "Test 4: Local declaration and assignment"
TOKENS='[
  {"type":"PIPE","value":"|","line":1,"col":0},
  {"type":"IDENTIFIER","value":"x","line":1,"col":2},
  {"type":"PIPE","value":"|","line":1,"col":4},
  {"type":"NEWLINE","value":"\n","line":1,"col":5},
  {"type":"IDENTIFIER","value":"x","line":2,"col":0},
  {"type":"ASSIGN","value":":=","line":2,"col":2},
  {"type":"NUMBER","value":"5","line":2,"col":5}
]'
RESULT=$(echo "$TOKENS" | jq -f "$scratch/parser.jq" 2>&1)
assert_json_has "$RESULT" '.body[0].type' 'locals' "Test 4: first stmt is locals declaration"
assert_json_has "$RESULT" '.body[0].names[0]' 'x' "Test 4: declares local 'x'"
assert_json_has "$RESULT" '.body[1].type' 'assignment' "Test 4: second stmt is assignment"
assert_json_has "$RESULT" '.body[1].target' 'x' "Test 4: assigns to 'x'"

echo ""
echo "Test 5: Return statement (^ result)"
TOKENS='[
  {"type":"CARET","value":"^","line":1,"col":0},
  {"type":"IDENTIFIER","value":"result","line":1,"col":2}
]'
RESULT=$(echo "$TOKENS" | jq -f "$scratch/parser.jq" 2>&1)
assert_json_has "$RESULT" '.body[0].type' 'return' "Test 5: parses as return"
assert_json_has "$RESULT" '.body[0].value.type' 'identifier' "Test 5: return value is identifier"
assert_json_has "$RESULT" '.body[0].value.name' 'result' "Test 5: return value is 'result'"

# Summary
echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed"

[[ $FAILED -eq 0 ]] || exit 1
