#!/usr/bin/env bash
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
cat > /tmp/expr_test.jq << 'EXPR_EOF'
# Expression Parser Test Harness
# Import all the expression parser functions from codegen.jq by copying them

# Binding powers for operators: [left_bp, right_bp]
def expr_infix_bp:
  {
    ":=": [2, 1],
    "+":  [10, 11],
    "-":  [10, 11],
    "*":  [20, 21],
    "/":  [20, 21],
    "%":  [20, 21]
  };

def expr_peek:
  if .pos >= (.tokens | length) then null else .tokens[.pos] end;

def expr_peek_type:
  expr_peek as $tok | if $tok != null then $tok.type else null end;

def expr_advance:
  .pos += 1;

def expr_at_end:
  .pos >= (.tokens | length);

def expr_skip_ws:
  expr_peek as $tok |
  if $tok != null and ($tok.type == "NEWLINE" or $tok.type == "COMMENT") then
    expr_advance | expr_skip_ws
  else .
  end;

def expr_is_operator:
  expr_peek as $tok |
  $tok != null and (
    $tok.type == "PLUS" or $tok.type == "MINUS" or
    $tok.type == "STAR" or $tok.type == "PERCENT" or
    $tok.type == "SLASH" or $tok.type == "ASSIGN"
  );

def expr_op_value:
  expr_peek as $tok |
  if $tok == null then null
  elif $tok.type == "PLUS" then "+"
  elif $tok.type == "MINUS" then "-"
  elif $tok.type == "STAR" then "*"
  elif $tok.type == "PERCENT" then "%"
  elif $tok.type == "SLASH" then "/"
  elif $tok.type == "ASSIGN" then ":="
  else null
  end;

def expr_parse_message_send:
  (. | expr_advance) |
  expr_skip_ws |
  expr_peek as $recv_tok |
  (if $recv_tok == null then
    { state: ., result: null }
  elif $recv_tok.type == "IDENTIFIER" then
    if $recv_tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $recv_tok.value } }
    end
  elif $recv_tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $recv_tok.value } }
  elif $recv_tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $recv_tok.value } }
  else
    { state: ., result: null }
  end) as $recv |
  if $recv.result == null then
    { state: $recv.state, result: null }
  else
    $recv.state | expr_skip_ws |
    { state: ., selector: "", args: [], keywords: [] } |
    if (.state | expr_peek_type) == "KEYWORD" then
      until((.state | expr_peek_type) != "KEYWORD";
        ((.state | expr_peek.value) // "" | rtrimstr(":")) as $kw |
        .keywords += [$kw] |
        .selector = (if .selector == "" then $kw else "\(.selector)_\($kw)" end) |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        (.state | expr_peek) as $arg_tok |
        if $arg_tok != null and ($arg_tok.type == "IDENTIFIER" or $arg_tok.type == "NUMBER" or
           $arg_tok.type == "STRING" or $arg_tok.type == "DSTRING" or
           $arg_tok.type == "VARIABLE" or $arg_tok.type == "SUBSHELL" or
           $arg_tok.type == "ARITHMETIC" or $arg_tok.type == "PATH") then
          (if $arg_tok.type == "IDENTIFIER" then
            if $arg_tok.value == "self" then { type: "self" }
            else { type: "identifier", name: $arg_tok.value }
            end
          elif $arg_tok.type == "NUMBER" then { type: "number", value: $arg_tok.value }
          elif $arg_tok.type == "STRING" then { type: "string", value: ($arg_tok.value | ltrimstr("'") | rtrimstr("'")) }
          elif $arg_tok.type == "DSTRING" then { type: "dstring", value: $arg_tok.value }
          elif $arg_tok.type == "VARIABLE" then { type: "variable", value: $arg_tok.value }
          elif $arg_tok.type == "SUBSHELL" then { type: "subshell", value: $arg_tok.value }
          elif $arg_tok.type == "ARITHMETIC" then { type: "arithmetic", value: $arg_tok.value }
          elif $arg_tok.type == "PATH" then { type: "path", value: $arg_tok.value }
          else null
          end) as $arg |
          .args += [$arg] |
          .state |= expr_advance
        else .
        end |
        .state |= expr_skip_ws
      )
    elif (.state | expr_peek_type) == "IDENTIFIER" then
      .selector = ((.state | expr_peek.value) // "") |
      .state |= expr_advance
    else .
    end |
    {
      state: .state,
      result: {
        type: "message_send",
        receiver: $recv.result,
        selector: .selector,
        args: .args
      }
    }
  end;

def expr_parse_expr(min_bp):
  (expr_skip_ws |
  expr_peek as $tok |
  if $tok == null then
    { state: ., result: null }
  elif $tok.type == "NUMBER" then
    { state: (. | expr_advance), result: { type: "number", value: $tok.value } }
  elif $tok.type == "STRING" then
    { state: (. | expr_advance), result: { type: "string", value: ($tok.value | ltrimstr("'") | rtrimstr("'")) } }
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }
  elif $tok.type == "IDENTIFIER" then
    if $tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $tok.value } }
    end
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }
  elif $tok.type == "SUBSHELL" then
    { state: (. | expr_advance), result: { type: "subshell", value: $tok.value } }
  elif $tok.type == "ARITHMETIC" then
    { state: (. | expr_advance), result: { type: "arithmetic", value: $tok.value } }
  elif $tok.type == "ARITH_CMD" then
    { state: (. | expr_advance), result: { type: "arith_cmd", value: $tok.value } }
  elif $tok.type == "PATH" then
    { state: (. | expr_advance), result: { type: "path", value: $tok.value } }
  elif $tok.type == "LPAREN" then
    (. | expr_advance) | expr_parse_expr(0) as $inner |
    $inner.state |
    if expr_peek_type == "RPAREN" then
      { state: (. | expr_advance), result: $inner.result }
    else
      { state: ., result: $inner.result }
    end
  elif $tok.type == "MINUS" then
    (. | expr_advance) | expr_parse_expr(50) as $operand |
    { state: $operand.state, result: { type: "unary", op: "-", operand: $operand.result } }
  elif $tok.type == "CARET" then
    (. | expr_advance) | expr_skip_ws |
    if expr_at_end or expr_peek_type == "NEWLINE" or expr_peek_type == "DOT" then
      { state: ., result: { type: "return", value: null } }
    else
      expr_parse_expr(1) as $val |
      { state: $val.state, result: { type: "return", value: $val.result } }
    end
  elif $tok.type == "AT" then
    expr_parse_message_send
  else
    { state: (. | expr_advance), result: { type: "passthrough", token: $tok } }
  end) as $prefix |
  if $prefix.result == null then $prefix
  else
    $prefix |
    def infix_loop:
      .state |= expr_skip_ws |
      (.state | expr_is_operator) as $is_op |
      if $is_op then
        (.state | expr_op_value) as $op |
        (expr_infix_bp[$op] // null) as $bp |
        if $bp == null or $bp[0] < min_bp then .
        else
          .state |= expr_advance |
          .result as $lhs |
          (.state | expr_parse_expr($bp[1])) as $rhs |
          (if $op == ":=" then
            if $lhs.type == "identifier" then
              { state: $rhs.state, result: { type: "assignment", target: $lhs.name, value: $rhs.result } }
            else
              { state: $rhs.state, result: { type: "binary", op: $op, left: $lhs, right: $rhs.result } }
            end
          else
            { state: $rhs.state, result: { type: "binary", op: $op, left: $lhs, right: $rhs.result } }
          end)
          | infix_loop
        end
      else .
      end;
    infix_loop
  end;

def expr_parse_locals:
  if expr_peek_type != "PIPE" then { state: ., result: null }
  else
    (. | expr_advance) |
    { state: ., names: [] } |
    until((.state | expr_peek_type) == "PIPE" or (.state | expr_at_end);
      (.state | expr_peek) as $t |
      if $t != null and $t.type == "IDENTIFIER" then
        .names += [$t.value] | .state |= expr_advance
      else
        .state |= expr_advance
      end |
      .state |= expr_skip_ws
    ) |
    .state |= expr_advance |
    { state: .state, result: { type: "locals", names: .names } }
  end;

def expr_skip_term:
  expr_peek as $tok |
  if $tok != null and ($tok.type == "DOT" or $tok.type == "NEWLINE") then
    expr_advance | expr_skip_term
  else .
  end;

def expr_parse_stmt:
  expr_skip_term |
  if expr_at_end then { state: ., result: null }
  elif expr_peek_type == "PIPE" then expr_parse_locals
  else expr_parse_expr(0)
  end;

def expr_parse_stmts:
  { state: ., stmts: [] } |
  until((.state | expr_at_end);
    (.state | expr_parse_stmt) as $s |
    if $s.result != null then
      .stmts += [$s.result] | .state = $s.state
    else
      .state = $s.state
    end |
    .state |= expr_skip_term
  ) |
  { type: "statements", body: .stmts };

# Test entry point - parse tokens into AST
{ tokens: ., pos: 0 } | expr_parse_stmts
EXPR_EOF

echo ""
echo "Test 1: Simple assignment (result := value)"
TOKENS='[
  {"type":"IDENTIFIER","value":"result","line":1,"col":0},
  {"type":"ASSIGN","value":":=","line":1,"col":7},
  {"type":"IDENTIFIER","value":"value","line":1,"col":10}
]'
RESULT=$(echo "$TOKENS" | jq -f /tmp/expr_test.jq 2>&1)
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
RESULT=$(echo "$TOKENS" | jq -f /tmp/expr_test.jq 2>&1)
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
RESULT=$(echo "$TOKENS" | jq -f /tmp/expr_test.jq 2>&1)
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
RESULT=$(echo "$TOKENS" | jq -f /tmp/expr_test.jq 2>&1)
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
RESULT=$(echo "$TOKENS" | jq -f /tmp/expr_test.jq 2>&1)
assert_json_has "$RESULT" '.body[0].type' 'return' "Test 5: parses as return"
assert_json_has "$RESULT" '.body[0].value.type' 'identifier' "Test 5: return value is identifier"
assert_json_has "$RESULT" '.body[0].value.name' 'result' "Test 5: return value is 'result'"

# Summary
echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed"

[[ $FAILED -eq 0 ]] || exit 1
