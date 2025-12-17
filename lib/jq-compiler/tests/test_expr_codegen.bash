#!/usr/bin/env bash
# Test the expression code generator in codegen.jq

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

pass() { echo -e "  ${GREEN}✓${NC} $1"; }
fail() { echo -e "  ${RED}✗${NC} $1"; echo "    Expected: $2"; echo "    Got: $3"; }

echo "Expression Code Generator Tests"
echo "================================"

# Create a test jq file that includes both parsing and code generation
cat > /tmp/expr_codegen_test.jq << 'EXPR_EOF'
# Expression Parser + Code Generator Test Harness

# Binding powers for operators
def expr_infix_bp:
  { ":=": [2, 1], "+": [10, 11], "-": [10, 11], "*": [20, 21], "/": [20, 21], "%": [20, 21] };

def expr_peek:
  if .pos >= (.tokens | length) then null else .tokens[.pos] end;

def expr_peek_type:
  expr_peek as $tok | if $tok != null then $tok.type else null end;

def expr_advance: .pos += 1;
def expr_at_end: .pos >= (.tokens | length);

def expr_skip_ws:
  expr_peek as $tok |
  if $tok != null and ($tok.type == "NEWLINE" or $tok.type == "COMMENT") then
    expr_advance | expr_skip_ws
  else . end;

def expr_is_operator:
  expr_peek as $tok |
  $tok != null and ($tok.type == "PLUS" or $tok.type == "MINUS" or $tok.type == "STAR" or
    $tok.type == "PERCENT" or $tok.type == "SLASH" or $tok.type == "ASSIGN");

def expr_op_value:
  expr_peek as $tok |
  if $tok == null then null
  elif $tok.type == "PLUS" then "+"
  elif $tok.type == "MINUS" then "-"
  elif $tok.type == "STAR" then "*"
  elif $tok.type == "PERCENT" then "%"
  elif $tok.type == "SLASH" then "/"
  elif $tok.type == "ASSIGN" then ":="
  else null end;

def expr_parse_expr(min_bp):
  (expr_skip_ws |
  expr_peek as $tok |
  if $tok == null then { state: ., result: null }
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
  elif $tok.type == "CARET" then
    (. | expr_advance) | expr_skip_ws |
    if expr_at_end or expr_peek_type == "NEWLINE" or expr_peek_type == "DOT" then
      { state: ., result: { type: "return", value: null } }
    else
      expr_parse_expr(1) as $val |
      { state: $val.state, result: { type: "return", value: $val.result } }
    end
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
          end) | infix_loop
        end
      else . end;
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
      else .state |= expr_advance end |
      .state |= expr_skip_ws
    ) |
    .state |= expr_advance |
    { state: .state, result: { type: "locals", names: .names } }
  end;

def expr_skip_term:
  expr_peek as $tok |
  if $tok != null and ($tok.type == "DOT" or $tok.type == "NEWLINE") then
    expr_advance | expr_skip_term
  else . end;

def expr_parse_stmt:
  expr_skip_term |
  if expr_at_end then { state: ., result: null }
  elif expr_peek_type == "PIPE" then expr_parse_locals
  else expr_parse_expr(0) end;

def expr_parse_stmts:
  { state: ., stmts: [] } |
  until((.state | expr_at_end);
    (.state | expr_parse_stmt) as $s |
    if $s.result != null then
      .stmts += [$s.result] | .state = $s.state
    else .state = $s.state end |
    .state |= expr_skip_term
  ) |
  { type: "statements", body: .stmts };

# Code generation helpers
def expr_is_local($name; $locals):
  ($locals // []) | any(. == $name);

def expr_is_ivar($name; $ivars):
  ($ivars // []) | any(. == $name);

def expr_gen_arith($locals; $ivars):
  if . == null then "0"
  elif .type == "number" then .value
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    else "$\(.name)" end
  elif .type == "variable" then .value
  elif .type == "subshell" then .value
  elif .type == "arithmetic" then (.value | gsub("^\\$\\(\\(|\\)\\)$"; ""))
  elif .type == "binary" then
    "(\(.left | expr_gen_arith($locals; $ivars)) \(.op) \(.right | expr_gen_arith($locals; $ivars)))"
  elif .type == "unary" and .op == "-" then
    "(-\(.operand | expr_gen_arith($locals; $ivars)))"
  else "0" end;

def expr_gen($locals; $ivars):
  if . == null then ""
  elif .type == "number" then .value
  elif .type == "string" then "'\(.value)'"
  elif .type == "dstring" then .value
  elif .type == "self" then "\"$_RECEIVER\""
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    else .name end
  elif .type == "variable" then .value
  elif .type == "subshell" then .value
  elif .type == "arithmetic" then .value
  elif .type == "binary" then
    "$(( \(.left | expr_gen_arith($locals; $ivars)) \(.op) \(.right | expr_gen_arith($locals; $ivars)) ))"
  elif .type == "assignment" then
    "\(.target)=\"\(.value | expr_gen($locals; $ivars))\""
  elif .type == "return" then
    if .value == null then "return"
    else "echo \"\(.value | expr_gen($locals; $ivars))\"; return" end
  elif .type == "passthrough" then .token.value
  else "# unknown: \(.type)" end;

def expr_collect_locals:
  if .type == "statements" and .body != null then
    [(.body // [])[] | select(.type == "locals") | (.names // [])[]] | unique
  else [] end;

def expr_gen_stmts($locals; $ivars):
  reduce (.body // [])[] as $stmt ({ lines: [], locals: $locals };
    # Capture .locals before any pipes to avoid jq scoping issues
    .locals as $current_locals |
    if $stmt.type == "locals" then
      .lines += ["  local \(($stmt.names // []) | join(" "))"] |
      .locals += ($stmt.names // [])
    elif $stmt.type == "assignment" then
      if expr_is_local($stmt.target; $current_locals) then
        .lines += ["  \($stmt.target)=\"\($stmt.value | expr_gen($current_locals; $ivars))\""]
      elif expr_is_ivar($stmt.target; $ivars) then
        .lines += ["  _ivar_set \($stmt.target) \"\($stmt.value | expr_gen($current_locals; $ivars))\""]
      else
        .lines += ["  \($stmt.target)=\"\($stmt.value | expr_gen($current_locals; $ivars))\""]
      end
    elif $stmt.type == "return" then
      if $stmt.value == null then
        .lines += ["  return"]
      else
        .lines += ["  echo \"\($stmt.value | expr_gen($current_locals; $ivars))\"; return"]
      end
    else
      ($stmt | expr_gen($current_locals; $ivars)) as $code |
      if $code != "" then .lines += ["  \($code)"] else . end
    end
  ) | .lines | join("\n");

# Entry point: parse tokens, generate code
# Input: { tokens, ivars, args }
(.tokens) as $tokens |
(.ivars // []) as $ivars |
(.args // []) as $args |
{ tokens: $tokens, pos: 0 } | expr_parse_stmts |
. as $ast |
if ($ast.body == null) or ($ast.type != "statements") then
  "# ERROR: AST parsing failed"
else
  (($ast | expr_collect_locals) + $args) | unique | . as $all_locals |
  $ast | expr_gen_stmts($all_locals; $ivars)
end
EXPR_EOF

echo ""
echo "Test 1: Simple local assignment (x := 5)"
INPUT='{"tokens": [
  {"type":"IDENTIFIER","value":"x"},
  {"type":"ASSIGN","value":":="},
  {"type":"NUMBER","value":"5"}
], "ivars": [], "args": ["x"]}'
EXPECTED='  x="5"'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Simple local assignment"
else
    fail "Simple local assignment" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 2: Instance variable assignment (result := value)"
INPUT='{"tokens": [
  {"type":"IDENTIFIER","value":"result"},
  {"type":"ASSIGN","value":":="},
  {"type":"IDENTIFIER","value":"value"}
], "ivars": ["value", "step"], "args": []}'
EXPECTED='  result="$(_ivar value)"'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Instance variable assignment"
else
    fail "Instance variable assignment" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 3: Binary arithmetic (value + step)"
INPUT='{"tokens": [
  {"type":"IDENTIFIER","value":"value"},
  {"type":"PLUS","value":"+"},
  {"type":"IDENTIFIER","value":"step"}
], "ivars": ["value", "step"], "args": []}'
EXPECTED='  $(( $(_ivar value) + $(_ivar step) ))'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Binary arithmetic with ivars"
else
    fail "Binary arithmetic with ivars" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 4: Assignment with arithmetic (result := value + step)"
INPUT='{"tokens": [
  {"type":"IDENTIFIER","value":"result"},
  {"type":"ASSIGN","value":":="},
  {"type":"IDENTIFIER","value":"value"},
  {"type":"PLUS","value":"+"},
  {"type":"IDENTIFIER","value":"step"}
], "ivars": ["value", "step"], "args": []}'
EXPECTED='  result="$(( $(_ivar value) + $(_ivar step) ))"'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Assignment with arithmetic"
else
    fail "Assignment with arithmetic" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 5: Full method body (| result | result := value + step. ^ result)"
INPUT='{"tokens": [
  {"type":"PIPE","value":"|"},
  {"type":"IDENTIFIER","value":"result"},
  {"type":"PIPE","value":"|"},
  {"type":"NEWLINE","value":"\n"},
  {"type":"IDENTIFIER","value":"result"},
  {"type":"ASSIGN","value":":="},
  {"type":"IDENTIFIER","value":"value"},
  {"type":"PLUS","value":"+"},
  {"type":"IDENTIFIER","value":"step"},
  {"type":"DOT","value":"."},
  {"type":"NEWLINE","value":"\n"},
  {"type":"CARET","value":"^"},
  {"type":"IDENTIFIER","value":"result"}
], "ivars": ["value", "step"], "args": []}'
EXPECTED='  local result
  result="$(( $(_ivar value) + $(_ivar step) ))"
  echo "$result"; return'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Full method body"
else
    fail "Full method body" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 6: Return bare identifier (^ result) - local"
INPUT='{"tokens": [
  {"type":"CARET","value":"^"},
  {"type":"IDENTIFIER","value":"result"}
], "ivars": [], "args": ["result"]}'
EXPECTED='  echo "$result"; return'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Return local variable"
else
    fail "Return local variable" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 7: Return bare identifier (^ value) - ivar"
INPUT='{"tokens": [
  {"type":"CARET","value":"^"},
  {"type":"IDENTIFIER","value":"value"}
], "ivars": ["value"], "args": []}'
EXPECTED='  echo "$(_ivar value)"; return'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Return ivar"
else
    fail "Return ivar" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 8: Instance variable assignment (value := value + 5)"
INPUT='{"tokens": [
  {"type":"IDENTIFIER","value":"value"},
  {"type":"ASSIGN","value":":="},
  {"type":"IDENTIFIER","value":"value"},
  {"type":"PLUS","value":"+"},
  {"type":"NUMBER","value":"5"}
], "ivars": ["value"], "args": []}'
EXPECTED='  _ivar_set value "$(( $(_ivar value) + 5 ))"'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Instance variable assignment"
else
    fail "Instance variable assignment" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Test 9: Local assignment when local declared (| x | x := 5)"
INPUT='{"tokens": [
  {"type":"PIPE","value":"|"},
  {"type":"IDENTIFIER","value":"x"},
  {"type":"PIPE","value":"|"},
  {"type":"NEWLINE","value":"\n"},
  {"type":"IDENTIFIER","value":"x"},
  {"type":"ASSIGN","value":":="},
  {"type":"NUMBER","value":"5"}
], "ivars": ["value"], "args": []}'
EXPECTED='  local x
  x="5"'
RESULT=$(echo "$INPUT" | jq -f /tmp/expr_codegen_test.jq -r 2>&1)
if [[ "$RESULT" == "$EXPECTED" ]]; then
    pass "Local assignment with local declaration"
else
    fail "Local assignment with local declaration" "$EXPECTED" "$RESULT"
fi

echo ""
echo "Done!"
