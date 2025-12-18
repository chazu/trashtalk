# ==============================================================================
# Trashtalk Code Generator
# ==============================================================================
#
# Generates bash code from the parsed AST.
#
# Input: JSON AST from parser
# Output: Bash source code
#
# Transformations applied in method bodies:
#   | var1 var2 |     →  local var1 var2
#   ^ expression      →  echo expression
#   self              →  $_RECEIVER
#   @ self method     →  @ $_RECEIVER method
#   @ recv key: val   →  @ "$recv" key "val"
#   var := expr       →  var=expr
#
# NEW (Phase 1): Expression parsing with instance variable inference
#   value + step      →  $(( $(_ivar value) + $(_ivar step) ))
#   newVal := value   →  newVal="$(_ivar value)"
#   ^ newVal          →  echo "$newVal"; return
#
# ==============================================================================

# ==============================================================================
# Expression Parser (Pratt Parser)
# ==============================================================================
# Parses Smalltalk-like expressions into an AST, then generates bash code
# with proper instance variable inference.

# Binding powers for operators: [left_bp, right_bp]
def expr_infix_bp:
  {
    ":=": [2, 1],       # assignment, right-assoc
    ">":  [5, 6],       # comparison, left-assoc
    "<":  [5, 6],       # comparison, left-assoc
    ">=": [5, 6],       # comparison, left-assoc
    "<=": [5, 6],       # comparison, left-assoc
    "==": [5, 6],       # comparison, left-assoc
    "!=": [5, 6],       # comparison, left-assoc
    "+":  [10, 11],     # addition, left-assoc
    "-":  [10, 11],     # subtraction, left-assoc
    "*":  [20, 21],     # multiplication, left-assoc
    "/":  [20, 21],     # division, left-assoc
    "%":  [20, 21]      # modulo, left-assoc
  };

# State helpers for expression parser
def expr_peek:
  if .pos >= (.tokens | length) then null else .tokens[.pos] end;

# Safe type accessor - returns null if token is null
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

# Check if current token is an infix operator
def expr_is_operator:
  expr_peek as $tok |
  $tok != null and (
    $tok.type == "PLUS" or $tok.type == "MINUS" or
    $tok.type == "STAR" or $tok.type == "PERCENT" or
    $tok.type == "SLASH" or $tok.type == "ASSIGN" or
    $tok.type == "GT" or $tok.type == "LT" or
    $tok.type == "GE" or $tok.type == "LE" or
    $tok.type == "EQ" or $tok.type == "NE"
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
  elif $tok.type == "GT" then ">"
  elif $tok.type == "LT" then "<"
  elif $tok.type == "GE" then ">="
  elif $tok.type == "LE" then "<="
  elif $tok.type == "EQ" then "=="
  elif $tok.type == "NE" then "!="
  else null
  end;

# Check if current token is a control flow keyword
def expr_is_control_flow:
  expr_peek as $tok |
  $tok != null and $tok.type == "KEYWORD" and
  ($tok.value == "ifTrue:" or $tok.value == "ifFalse:" or
   $tok.value == "whileTrue:" or $tok.value == "whileFalse:" or
   $tok.value == "timesRepeat:");

# Check if current token terminates a keyword argument
# (used to stop expression parsing at message boundaries)
def expr_is_arg_terminator:
  expr_peek as $tok |
  $tok == null or
  $tok.type == "KEYWORD" or
  $tok.type == "DOT" or
  $tok.type == "SEMI" or
  $tok.type == "RBRACKET" or
  $tok.type == "NEWLINE";

# Parse a simple atom (for message receivers that can't be full expressions)
def expr_parse_atom:
  expr_peek as $tok |
  if $tok == null then
    { state: ., result: null }
  elif $tok.type == "IDENTIFIER" then
    if $tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $tok.value } }
    end
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }
  elif $tok.type == "NUMBER" then
    { state: (. | expr_advance), result: { type: "number", value: $tok.value } }
  elif $tok.type == "STRING" then
    { state: (. | expr_advance), result: { type: "string", value: ($tok.value | ltrimstr("'") | rtrimstr("'")) } }
  elif $tok.type == "SUBSHELL" then
    { state: (. | expr_advance), result: { type: "subshell", value: $tok.value } }
  elif $tok.type == "ARITHMETIC" then
    { state: (. | expr_advance), result: { type: "arithmetic", value: $tok.value } }
  elif $tok.type == "PATH" then
    { state: (. | expr_advance), result: { type: "path", value: $tok.value } }
  elif $tok.type == "SYMBOL" then
    { state: (. | expr_advance), result: { type: "symbol", value: $tok.value } }
  else
    { state: ., result: null }
  end;

# Mutually recursive expression parsers
# Message send parsing is inlined in expr_parse_expr to avoid forward reference issues

# Parse block without infix continuation - used for control flow arguments
def expr_parse_block:
  expr_skip_ws |
  if expr_peek_type != "LBRACKET" then
    { state: ., result: null }
  else
    (. | expr_advance) |
    expr_skip_ws |
    { state: ., tokens: [], depth: 1 } |
    until(.depth == 0 or (.state | expr_at_end);
      (.state | expr_peek) as $t |
      if $t == null then .
      elif $t.type == "LBRACKET" then
        .tokens += [$t] | .depth += 1 | .state |= expr_advance
      elif $t.type == "RBRACKET" then
        .depth -= 1 |
        if .depth > 0 then .tokens += [$t] else . end |
        .state |= expr_advance
      else
        .tokens += [$t] | .state |= expr_advance
      end
    ) |
    { state: .state, result: { type: "block", tokens: .tokens } }
  end;

def expr_parse_expr(min_bp):
  # Helper: Parse a single message (selector + args) without receiver
  # Used for cascade parsing
  def parse_single_message:
    { state: ., selector: "", args: [], keywords: [] } |
    if (.state | expr_peek_type) == "KEYWORD" then
      # Keyword method
      until((.state | expr_peek_type) != "KEYWORD";
        ((.state | expr_peek.value) // "" | rtrimstr(":")) as $kw |
        .keywords += [$kw] |
        .selector = (if .selector == "" then $kw else "\(.selector)_\($kw)" end) |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        if (.state | expr_is_arg_terminator) then .
        else
          (.state | expr_parse_expr(0)) as $arg_result |
          if $arg_result.result != null then
            .args += [$arg_result.result] |
            .state = $arg_result.state
          else .
          end
        end |
        .state |= expr_skip_ws
      )
    elif (.state | expr_peek_type) == "IDENTIFIER" then
      # Unary method
      .selector = ((.state | expr_peek.value) // "") |
      .state |= expr_advance
    else .
    end |
    { state: .state, selector: .selector, args: .args };

  # Helper: Parse message send inline (called when we see AT token)
  # Supports cascades: @ obj foo; bar; baz.
  def parse_message_send_inline:
    (. | expr_advance) |  # consume @
    expr_skip_ws |
    # Parse receiver (simple atom only)
    expr_parse_atom |
    if .result == null then .
    else
      .result as $recv |
      .state | expr_skip_ws |
      # Parse first message
      parse_single_message |
      . as $first_msg |
      # Check for cascades (SEMI token)
      { state: $first_msg.state, messages: [{ selector: $first_msg.selector, args: $first_msg.args }] } |
      until((.state | expr_peek_type) != "SEMI";
        .state |= expr_advance |  # consume ;
        .state |= expr_skip_ws |
        (.state | parse_single_message) as $next_msg |
        .messages += [{ selector: $next_msg.selector, args: $next_msg.args }] |
        .state = $next_msg.state |
        .state |= expr_skip_ws
      ) |
      # Generate result based on number of messages
      if (.messages | length) == 1 then
        {
          state: .state,
          result: {
            type: "message_send",
            receiver: $recv,
            selector: .messages[0].selector,
            args: .messages[0].args
          }
        }
      else
        {
          state: .state,
          result: {
            type: "cascade",
            receiver: $recv,
            messages: .messages
          }
        }
      end
    end;

  # Parse prefix
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
  elif $tok.type == "SYMBOL" then
    { state: (. | expr_advance), result: { type: "symbol", value: $tok.value } }
  elif $tok.type == "HASH_LPAREN" then
    # Array literal: #(1 2 3) -> bash array (1 2 3)
    (. | expr_advance) |
    { state: ., elements: [] } |
    until((.state | expr_peek_type) == "RPAREN" or (.state | expr_at_end);
      .state |= expr_skip_ws |
      if (.state | expr_peek_type) == "RPAREN" then .
      else
        (.state | expr_parse_expr(0)) as $elem |
        if $elem.result != null then
          .elements += [$elem.result] | .state = $elem.state
        else
          .state |= expr_advance
        end
      end
    ) |
    # Consume closing paren
    (if (.state | expr_peek_type) == "RPAREN" then .state |= expr_advance else . end) |
    { state: .state, result: { type: "array_literal", elements: .elements } }
  elif $tok.type == "HASH_LBRACE" then
    # Dictionary literal: #{a: 1 b: 2} -> bash assoc array ([a]=1 [b]=2)
    (. | expr_advance) |
    { state: ., pairs: [] } |
    until((.state | expr_peek_type) == "RBRACE" or (.state | expr_at_end);
      .state |= expr_skip_ws |
      if (.state | expr_peek_type) == "RBRACE" then .
      elif (.state | expr_peek_type) == "KEYWORD" then
        # Key is a keyword like "foo:"
        ((.state | expr_peek.value) | rtrimstr(":")) as $key |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        # Parse value
        (.state | expr_parse_expr(0)) as $val |
        if $val.result != null then
          .pairs += [{ key: $key, value: $val.result }] | .state = $val.state
        else .
        end
      else
        # Skip unknown tokens
        .state |= expr_advance
      end
    ) |
    # Consume closing brace
    (if (.state | expr_peek_type) == "RBRACE" then .state |= expr_advance else . end) |
    { state: .state, result: { type: "dict_literal", pairs: .pairs } }
  elif $tok.type == "LPAREN" then
    (. | expr_advance) | expr_parse_expr(0) as $inner |
    $inner.state |
    if expr_peek_type == "RPAREN" then
      { state: (. | expr_advance), result: $inner.result }
    else
      { state: ., result: $inner.result }
    end
  elif $tok.type == "LBRACKET" then
    # Block expression - may have parameters [:x :y | body]
    (. | expr_advance) |
    expr_skip_ws |
    # First, check for block parameters
    { state: ., params: [] } |
    until((.state | expr_peek_type) != "BLOCK_PARAM";
      (.state | expr_peek) as $p |
      .params += [$p.value] |
      .state |= expr_advance |
      .state |= expr_skip_ws
    ) |
    # If we have params, expect a PIPE separator
    (if (.params | length) > 0 and (.state | expr_peek_type) == "PIPE" then
      .state |= expr_advance | .state |= expr_skip_ws
    else . end) |
    # Now collect the body tokens
    .params as $params |
    .state |
    { state: ., tokens: [], depth: 1 } |
    until(.depth == 0 or (.state | expr_at_end);
      (.state | expr_peek) as $t |
      if $t == null then .
      elif $t.type == "LBRACKET" then
        .tokens += [$t] | .depth += 1 | .state |= expr_advance
      elif $t.type == "RBRACKET" then
        .depth -= 1 |
        if .depth > 0 then .tokens += [$t] else . end |
        .state |= expr_advance
      else
        .tokens += [$t] | .state |= expr_advance
      end
    ) |
    # Return block_literal if has params, regular block otherwise
    if ($params | length) > 0 then
      { state: .state, result: { type: "block_literal", params: $params, tokens: .tokens } }
    else
      { state: .state, result: { type: "block", tokens: .tokens } }
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
    parse_message_send_inline
  else
    { state: (. | expr_advance), result: { type: "passthrough", token: $tok } }
  end) as $prefix |
  # Parse infix (inline to avoid forward reference)
  if $prefix.result == null then $prefix
  else
    $prefix |
    # Infix loop - operates on { state, result } object
    def infix_loop:
      # Skip whitespace in state, then check for operators or control flow
      .state |= expr_skip_ws |
      (.state | expr_is_control_flow) as $is_cf |
      (.state | expr_is_operator) as $is_op |
      if $is_cf then
        # Control flow keyword - parse block argument
        .result as $receiver |
        (.state | expr_peek.value) as $keyword |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        # Parse block argument (use expr_parse_block to avoid infix continuation)
        if (.state | expr_peek_type) == "LBRACKET" then
          (.state | expr_parse_block) as $block |
          # Check for ifTrue:ifFalse: pattern
          (if $keyword == "ifTrue:" then
            ($block.state | expr_skip_ws) as $after_block |
            if ($after_block | expr_peek_type) == "KEYWORD" and ($after_block | expr_peek.value) == "ifFalse:" then
              (($after_block | expr_advance | expr_skip_ws) | expr_parse_block) as $else_block |
              {
                state: $else_block.state,
                result: {
                  type: "control_flow",
                  kind: "if_else",
                  condition: $receiver,
                  true_block: $block.result,
                  false_block: $else_block.result
                }
              }
            else
              {
                state: $block.state,
                result: {
                  type: "control_flow",
                  kind: "if_true",
                  condition: $receiver,
                  block: $block.result
                }
              }
            end
          elif $keyword == "ifFalse:" then
            {
              state: $block.state,
              result: {
                type: "control_flow",
                kind: "if_false",
                condition: $receiver,
                block: $block.result
              }
            }
          elif $keyword == "whileTrue:" then
            {
              state: $block.state,
              result: {
                type: "control_flow",
                kind: "while_true",
                condition: $receiver,
                block: $block.result
              }
            }
          elif $keyword == "whileFalse:" then
            {
              state: $block.state,
              result: {
                type: "control_flow",
                kind: "while_false",
                condition: $receiver,
                block: $block.result
              }
            }
          elif $keyword == "timesRepeat:" then
            {
              state: $block.state,
              result: {
                type: "control_flow",
                kind: "times_repeat",
                count: $receiver,
                block: $block.result
              }
            }
          else
            { state: $block.state, result: $receiver }
          end)
          | infix_loop
        else
          .
        end
      elif $is_op then
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

# Parse local declaration: | var1 var2 |
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

# Skip statement terminators
def expr_skip_term:
  expr_peek as $tok |
  if $tok != null and ($tok.type == "DOT" or $tok.type == "NEWLINE") then
    expr_advance | expr_skip_term
  else .
  end;

# Parse a single statement
def expr_parse_stmt:
  expr_skip_term |
  if expr_at_end then { state: ., result: null }
  elif expr_peek_type == "PIPE" then expr_parse_locals
  else expr_parse_expr(0)
  end;

# Parse all statements
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

# ==============================================================================
# Expression Code Generator
# ==============================================================================

# Check if identifier is a local variable
def expr_is_local($name; $locals):
  ($locals // []) | any(. == $name);

# Check if identifier is an instance variable
def expr_is_ivar($name; $ivars):
  ($ivars // []) | any(. == $name);

# Check if identifier is a class instance variable
def expr_is_cvar($name; $cvars):
  ($cvars // []) | any(. == $name);

# Generate code for arithmetic context (no wrapper)
# Must be defined before expr_gen since they're mutually recursive
def expr_gen_arith($locals; $ivars; $cvars):
  if . == null then "0"
  elif .type == "number" then .value
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    elif expr_is_cvar(.name; $cvars) then "$(_cvar \(.name))"
    else "$\(.name)"
    end
  elif .type == "variable" then .value
  elif .type == "subshell" then .value
  elif .type == "arithmetic" then (.value | gsub("^\\$\\(\\(|\\)\\)$"; ""))
  elif .type == "binary" then
    "(\(.left | expr_gen_arith($locals; $ivars; $cvars)) \(.op) \(.right | expr_gen_arith($locals; $ivars; $cvars)))"
  elif .type == "unary" and .op == "-" then
    "(-\(.operand | expr_gen_arith($locals; $ivars; $cvars)))"
  else
    # For non-arithmetic types, fall through to main generator
    if .type == "self" then "\"$_RECEIVER\""
    elif .type == "subshell" then .value
    elif .type == "variable" then .value
    else "0"
    end
  end;

# Generate code for an expression
def expr_gen($locals; $ivars; $cvars):
  if . == null then ""
  elif .type == "number" then .value
  elif .type == "string" then "'\(.value)'"
  elif .type == "dstring" then .value
  elif .type == "self" then "\"$_RECEIVER\""
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    elif expr_is_cvar(.name; $cvars) then "$(_cvar \(.name))"
    else .name  # bare identifier (command name, etc.)
    end
  elif .type == "variable" then .value
  elif .type == "subshell" then .value
  elif .type == "arithmetic" then .value
  elif .type == "arith_cmd" then .value
  elif .type == "path" then .value
  elif .type == "symbol" then .value
  elif .type == "array_literal" then
    # Bash indexed array: (elem1 elem2 elem3)
    "(" + ([.elements[] | expr_gen($locals; $ivars; $cvars)] | map("\"\(.)\"") | join(" ")) + ")"
  elif .type == "dict_literal" then
    # Bash associative array: ([key1]=val1 [key2]=val2)
    "(" + ([.pairs[] | "[\(.key)]=\"\(.value | expr_gen($locals; $ivars; $cvars))\""] | join(" ")) + ")"
  elif .type == "unary" then
    if .op == "-" then
      "$(( -\(.operand | expr_gen_arith($locals; $ivars; $cvars)) ))"
    else
      "\(.op)\(.operand | expr_gen($locals; $ivars; $cvars))"
    end
  elif .type == "binary" then
    "$(( \(.left | expr_gen_arith($locals; $ivars; $cvars)) \(.op) \(.right | expr_gen_arith($locals; $ivars; $cvars)) ))"
  elif .type == "message_send" then
    (.receiver | expr_gen($locals; $ivars; $cvars)) as $recv |
    (if ((.args // []) | length) > 0 then
      " " + ([(.args // [])[] | expr_gen($locals; $ivars; $cvars)] | join(" "))
    else ""
    end) as $args |
    "@ \($recv) \(.selector // "")\($args)"
  elif .type == "cascade" then
    # Cascade: send multiple messages to same receiver
    # In expression context, we capture receiver and return last result
    (.receiver | expr_gen($locals; $ivars; $cvars)) as $recv |
    (.messages | map(
      (if ((.args // []) | length) > 0 then
        " " + ([(.args // [])[] | expr_gen($locals; $ivars; $cvars)] | join(" "))
      else ""
      end) as $args |
      "@ \($recv) \(.selector // "")\($args)"
    ) | join("; "))
  elif .type == "assignment" then
    # Check if target is ivar - need to use _ivar_set
    # Note: collection literals in blocks handled by expr_gen_stmts, not here
    (.value.type == "array_literal" or .value.type == "dict_literal") as $is_collection |
    (.value | expr_gen($locals; $ivars; $cvars)) as $val_code |
    if expr_is_local(.target; $locals) then
      if $is_collection then "\(.target)=\($val_code)"
      else "\(.target)=\"\($val_code)\""
      end
    elif expr_is_ivar(.target; $ivars) then
      # For ivars, use _ivar_set (collection literals rare in loop bodies)
      "_ivar_set \(.target) \"\($val_code)\""
    elif expr_is_cvar(.target; $cvars) then
      # For cvars, use _cvar_set
      "_cvar_set \(.target) \"\($val_code)\""
    else
      if $is_collection then "\(.target)=\($val_code)"
      else "\(.target)=\"\($val_code)\""
      end
    end
  elif .type == "return" then
    if .value == null then "return"
    else "echo \"\(.value | expr_gen($locals; $ivars; $cvars))\"; return"
    end
  elif .type == "passthrough" then
    .token.value
  elif .type == "block" then
    # Block without explicit parameters - create a Block object with empty params
    # This allows blocks like [@ self doSomething] to be first-class
    (if .tokens != null then
      ({ tokens: .tokens, pos: 0 } | expr_parse_stmts) as $parsed |
      # Inline expr_collect_locals logic
      (if $parsed.type == "statements" and $parsed.body != null then
        [($parsed.body // [])[] | select(.type == "locals") | (.names // [])[]] | unique
      else [] end) as $declared_locals |
      # Generate body - wrap last expression in echo if it's just a value
      (($parsed.body // []) | length) as $stmt_count |
      if $stmt_count == 0 then ""
      elif $stmt_count == 1 then
        ($parsed.body[0]) as $stmt |
        if $stmt.type == "return" or $stmt.type == "message_send" or $stmt.type == "cascade" then
          $stmt | expr_gen($declared_locals; $ivars; $cvars)
        else
          "echo \"\($stmt | expr_gen($declared_locals; $ivars; $cvars))\""
        end
      else
        ([($parsed.body[:-1])[] | expr_gen($declared_locals; $ivars; $cvars)] | join("; ")) as $init_code |
        ($parsed.body[-1]) as $last_stmt |
        (if $last_stmt.type == "return" or $last_stmt.type == "message_send" or $last_stmt.type == "cascade" or $last_stmt.type == "locals" or $last_stmt.type == "assignment" then
          $last_stmt | expr_gen($declared_locals; $ivars; $cvars)
        else
          "echo \"\($last_stmt | expr_gen($declared_locals; $ivars; $cvars))\""
        end) as $last_code |
        if $init_code == "" then $last_code else "\($init_code); \($last_code)" end
      end
    elif .body != null then
      [(.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
    else
      ""
    end) as $body_code |
    # Escape single quotes in body for bash string
    ($body_code | gsub("'"; "'\\''")) as $escaped_body |
    # Create Block with empty params
    "$(@ Block params_code_captured '[]' '\($escaped_body)' '{\"_RECEIVER\":\"'\"$_RECEIVER\"'\"}')"
  elif .type == "block_literal" then
    # Block literal with parameters: [:x :y | body] -> Block creation
    # Params become JSON array, body gets compiled, captured vars include _RECEIVER
    (.params // []) as $block_params |
    # Compile the block body with params as locals
    # The last expression in a block should produce output (echo)
    (if .tokens != null then
      ({ tokens: .tokens, pos: 0 } | expr_parse_stmts) as $parsed |
      # Inline expr_collect_locals logic (can't call it due to jq ordering)
      (if $parsed.type == "statements" and $parsed.body != null then
        [($parsed.body // [])[] | select(.type == "locals") | (.names // [])[]] | unique
      else [] end) as $declared_locals |
      ($declared_locals + $block_params) as $block_locals |
      # Generate body - wrap last expression in echo if it's just a value
      (($parsed.body // []) | length) as $stmt_count |
      if $stmt_count == 0 then ""
      elif $stmt_count == 1 then
        # Single statement - wrap in echo if it produces a value
        ($parsed.body[0]) as $stmt |
        if $stmt.type == "return" or $stmt.type == "message_send" or $stmt.type == "cascade" then
          $stmt | expr_gen($block_locals; $ivars; $cvars)
        else
          # Value expression - wrap in echo
          "echo \"\($stmt | expr_gen($block_locals; $ivars; $cvars))\""
        end
      else
        # Multiple statements - all but last are normal, last gets echo wrapper if value
        ([($parsed.body[:-1])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")) as $init_code |
        ($parsed.body[-1]) as $last_stmt |
        (if $last_stmt.type == "return" or $last_stmt.type == "message_send" or $last_stmt.type == "cascade" or $last_stmt.type == "locals" or $last_stmt.type == "assignment" then
          $last_stmt | expr_gen($block_locals; $ivars; $cvars)
        else
          "echo \"\($last_stmt | expr_gen($block_locals; $ivars; $cvars))\""
        end) as $last_code |
        if $init_code == "" then $last_code else "\($init_code); \($last_code)" end
      end
    else
      ""
    end) as $body_code |
    # Generate the Block creation call
    # Escape single quotes in body for bash string
    ($body_code | gsub("'"; "'\\''")) as $escaped_body |
    # Build params JSON array: ["x", "y"]
    ($block_params | map("\"\(.)\"") | join(",")) as $params_json |
    "$(@ Block params_code_captured '[\($params_json)]' '\($escaped_body)' '{\"_RECEIVER\":\"'\"$_RECEIVER\"'\"}')"
  elif .type == "control_flow" then
    # Inline control flow generation to avoid forward reference
    # Helper to generate condition
    (if .condition.type == "binary" then
      "\(.condition.left | expr_gen_arith($locals; $ivars; $cvars)) \(.condition.op) \(.condition.right | expr_gen_arith($locals; $ivars; $cvars))"
    elif .condition.type == "identifier" then
      if expr_is_local(.condition.name; $locals) then "$\(.condition.name)"
      elif expr_is_ivar(.condition.name; $ivars) then "$(_ivar \(.condition.name))"
      else .condition.name
      end
    elif .condition.type == "variable" then .condition.value
    elif .condition.type == "boolean" then (if .condition.value then "1" else "0" end)
    else .condition | expr_gen($locals; $ivars; $cvars)
    end) as $cond |
    # Generate block body inline (can't use nested def due to jq scoping)
    if .kind == "if_true" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      "if (( \($cond) )); then \($block_code); fi"
    elif .kind == "if_false" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      "if (( !(\($cond)) )); then \($block_code); fi"
    elif .kind == "if_else" then
      (if .true_block.tokens != null then
        ({ tokens: .true_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .true_block.body != null then
        [(.true_block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $true_code |
      (if .false_block.tokens != null then
        ({ tokens: .false_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .false_block.body != null then
        [(.false_block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $false_code |
      "if (( \($cond) )); then \($true_code); else \($false_code); fi"
    elif .kind == "times_repeat" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      "for ((_i=0; _i<\(.count | expr_gen_arith($locals; $ivars; $cvars)); _i++)); do \($block_code); done"
    elif .kind == "while_true" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      if .condition.type == "block" then
        (if .condition.tokens != null then
          ({ tokens: .condition.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        else "" end) as $cond_code |
        "while \($cond_code); do \($block_code); done"
      else
        "while (( \($cond) )); do \($block_code); done"
      end
    elif .kind == "while_false" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      if .condition.type == "block" then
        (if .condition.tokens != null then
          ({ tokens: .condition.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        else "" end) as $cond_code |
        "while ! \($cond_code); do \($block_code); done"
      else
        "while (( !(\($cond)) )); do \($block_code); done"
      end
    else
      "# ERROR: unknown control flow kind \(.kind)"
    end
  else
    "# unknown: \(.type)"
  end;

# Generate JSON for collection literals (used for ivar storage)
# Arrays: ["elem1", "elem2"]
# Dicts: {"key1": "val1", "key2": "val2"}
def expr_gen_json($locals; $ivars; $cvars):
  if .type == "array_literal" then
    "[" + ([.elements[] |
      if .type == "number" then .value
      elif .type == "string" then "\"\(.value)\""
      elif .type == "symbol" then "\"\(.value)\""
      elif .type == "identifier" then
        # For identifiers, we need to generate shell that evaluates to JSON
        # This gets complex - for now just quote the identifier reference
        if expr_is_local(.name; $locals) then "\"$\(.name)\""
        elif expr_is_ivar(.name; $ivars) then "\"$(_ivar \(.name))\""
        else "\"\(.name)\""
        end
      else "\"\(. | expr_gen($locals; $ivars; $cvars))\""
      end
    ] | join(",")) + "]"
  elif .type == "dict_literal" then
    "{" + ([.pairs[] |
      "\"\(.key)\":" + (
        if .value.type == "number" then .value.value
        elif .value.type == "string" then "\"\(.value.value)\""
        elif .value.type == "symbol" then "\"\(.value.value)\""
        elif .value.type == "identifier" then
          if expr_is_local(.value.name; $locals) then "\"$\(.value.name)\""
          elif expr_is_ivar(.value.name; $ivars) then "\"$(_ivar \(.value.name))\""
          else "\"\(.value.name)\""
          end
        else "\"\(.value | expr_gen($locals; $ivars; $cvars))\""
        end
      )
    ] | join(",")) + "}"
  else
    # Fallback for non-collection types
    expr_gen($locals; $ivars; $cvars)
  end;

# Generate condition for control flow
def expr_gen_condition($locals; $ivars; $cvars):
  if .type == "binary" then
    "\(.left | expr_gen_arith($locals; $ivars; $cvars)) \(.op) \(.right | expr_gen_arith($locals; $ivars; $cvars))"
  elif .type == "block" then
    [(.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    else .name
    end
  elif .type == "variable" then .value
  elif .type == "boolean" then (if .value then "1" else "0" end)
  else expr_gen($locals; $ivars; $cvars)
  end;

# Helper to generate block body (handles both tokens and body array)
def expr_gen_block_body($locals; $ivars; $cvars):
  if .tokens != null then
    ({ tokens: .tokens, pos: 0 } | expr_parse_stmts) as $parsed |
    [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
  elif .body != null then
    [(.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
  else
    ""
  end;

# Generate code for control flow constructs
def expr_gen_control_flow($locals; $ivars; $cvars):
  if .kind == "if_true" then
    "if (( \(.condition | expr_gen_condition($locals; $ivars; $cvars)) )); then \(.block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "if_false" then
    "if (( !(\(.condition | expr_gen_condition($locals; $ivars; $cvars))) )); then \(.block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "if_else" then
    "if (( \(.condition | expr_gen_condition($locals; $ivars; $cvars)) )); then \(.true_block | expr_gen_block_body($locals; $ivars; $cvars)); else \(.false_block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "times_repeat" then
    "for ((_i=0; _i<\(.count | expr_gen_arith($locals; $ivars; $cvars)); _i++)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
  elif .kind == "while_true" then
    if .condition.type == "block" then
      "while \(.condition | expr_gen_block_body($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    else
      "while (( \(.condition | expr_gen_condition($locals; $ivars; $cvars)) )); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    end
  elif .kind == "while_false" then
    if .condition.type == "block" then
      "while ! \(.condition | expr_gen_block_body($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    else
      "while (( !(\(.condition | expr_gen_condition($locals; $ivars; $cvars))) )); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    end
  else
    "# ERROR: unknown control flow kind \(.kind)"
  end;

# Collect local names from statements
def expr_collect_locals:
  if .type == "statements" and .body != null then
    [(.body // [])[] | select(.type == "locals") | (.names // [])[]] | unique
  else []
  end;

# Generate code for all statements
def expr_gen_stmts($locals; $ivars; $cvars):
  reduce (.body // [])[] as $stmt ({ lines: [], locals: $locals };
    # Capture .locals before any pipes to avoid jq scoping issues
    .locals as $current_locals |
    if $stmt.type == "locals" then
      .lines += ["  local \(($stmt.names // []) | join(" "))"] |
      .locals += ($stmt.names // [])
    elif $stmt.type == "assignment" then
      # If target is a local variable, use regular assignment
      # If target is an ivar (not local), use _ivar_set
      # Collection literals (array/dict) need special handling:
      #   - For locals: use bash array syntax
      #   - For ivars: use JSON serialization
      ($stmt.value.type == "array_literal" or $stmt.value.type == "dict_literal") as $is_collection |
      ($stmt.value | expr_gen($current_locals; $ivars; $cvars)) as $val_code |
      ($stmt.value | expr_gen_json($current_locals; $ivars; $cvars)) as $json_code |
      if expr_is_local($stmt.target; $current_locals) then
        if $is_collection then
          .lines += ["  \($stmt.target)=\($val_code)"]
        else
          .lines += ["  \($stmt.target)=\"\($val_code)\""]
        end
      elif expr_is_ivar($stmt.target; $ivars) then
        if $is_collection then
          # Use JSON serialization for collection ivars
          .lines += ["  _ivar_set \($stmt.target) '\($json_code)'"]
        else
          .lines += ["  _ivar_set \($stmt.target) \"\($val_code)\""]
        end
      elif expr_is_cvar($stmt.target; $cvars) then
        if $is_collection then
          # Use JSON serialization for collection cvars
          .lines += ["  _cvar_set \($stmt.target) '\($json_code)'"]
        else
          .lines += ["  _cvar_set \($stmt.target) \"\($val_code)\""]
        end
      else
        # Unknown target - treat as regular assignment (could be global/env var)
        if $is_collection then
          .lines += ["  \($stmt.target)=\($val_code)"]
        else
          .lines += ["  \($stmt.target)=\"\($val_code)\""]
        end
      end
    elif $stmt.type == "return" then
      if $stmt.value == null then
        .lines += ["  return"]
      else
        .lines += ["  echo \"\($stmt.value | expr_gen($current_locals; $ivars; $cvars))\"; return"]
      end
    elif $stmt.type == "message_send" then
      .lines += ["  \($stmt | expr_gen($current_locals; $ivars; $cvars))"]
    elif $stmt.type == "cascade" then
      # Generate each message on its own line, all to same receiver
      ($stmt.receiver | expr_gen($current_locals; $ivars; $cvars)) as $recv |
      .lines += [
        $stmt.messages[] |
        (if ((.args // []) | length) > 0 then
          " " + ([(.args // [])[] | expr_gen($current_locals; $ivars; $cvars)] | join(" "))
        else ""
        end) as $args |
        "  @ \($recv) \(.selector // "")\($args)"
      ]
    else
      ($stmt | expr_gen($current_locals; $ivars; $cvars)) as $code |
      if $code != "" then
        .lines += ["  \($code)"]
      else .
      end
    end
  ) | .lines | join("\n");

# Check if method body should use expression parsing
# Returns true if body contains Smalltalk-style expressions AND no bash constructs
def should_use_expr_parser:
  . as $tokens |
  if ($tokens | length) < 3 then false
  else
    # First, check for strong Smalltalk signals that should always use expr parser
    # Collection literals are unambiguous Smalltalk syntax
    (any($tokens[]; .type == "SYMBOL" or .type == "HASH_LPAREN" or .type == "HASH_LBRACE")) as $has_collection_literals |
    if $has_collection_literals then true
    else
    # Check for exclusions: bash constructs that shouldn't use expr parser
    # Bash commands that appear as bare identifiers (not after @)
    def is_bash_command:
      . as $v | ["echo", "printf", "jq", "sed", "awk", "grep", "cat", "ls", "cd",
                 "read", "eval", "exec", "export", "source", "test", "true", "false",
                 "local", "declare", "typeset", "unset", "shift", "exit", "return",
                 "break", "continue", "wait", "kill", "trap", "set", "shopt"] | any(. == $v);

    # Bash control keywords
    def is_bash_control:
      . as $v | ["if", "then", "else", "elif", "fi", "for", "in", "do", "done",
                 "while", "until", "case", "esac", "function"] | any(. == $v);

    # Check for bash construct exclusions
    (any($tokens[] |
      # Bash control keyword anywhere
      (.type == "IDENTIFIER" and (.value | is_bash_control))
      or
      # Bare bash command (not preceded by @ in context - simplify: just check if common cmd exists)
      (.type == "IDENTIFIER" and (.value | is_bash_command))
    )) as $has_bash_constructs |

    # Check for pipe used for command chaining (not local var decl)
    # Local var decl pattern: PIPE IDENTIFIER+ PIPE (all on same logical unit)
    # Command pipe: something | something (where something is a command output)
    # We detect command pipes by: DSTRING PIPE or RPAREN PIPE patterns
    (any(range(0; $tokens | length) as $i |
      $tokens[$i].type == "PIPE" and
      $i > 0 and
      # Definitely command pipe if preceded by string output or subshell close
      ($tokens[$i - 1].type == "DSTRING" or $tokens[$i - 1].type == "RPAREN")
    )) as $has_command_pipe |

    if $has_bash_constructs or $has_command_pipe then false
    else
      # Check for patterns that indicate new Smalltalk-like syntax:
      any(range(0; ($tokens | length) - 2) as $i |
        # Pattern 1: identifier := identifier (ivar inference)
        ($tokens[$i].type == "IDENTIFIER" and
         $tokens[$i + 1].type == "ASSIGN" and
         $tokens[$i + 2].type == "IDENTIFIER" and
         ($tokens[$i + 2].value != null) and
         ($tokens[$i + 2].value | test("^[a-z]")))
        or
        # Pattern 2: identifier := number followed by DOT (Smalltalk-style)
        ($tokens[$i].type == "IDENTIFIER" and
         $tokens[$i + 1].type == "ASSIGN" and
         $tokens[$i + 2].type == "NUMBER" and
         (($tokens[$i + 3].type // null) == "DOT"))
        or
        # Pattern 3: Arithmetic operator between identifiers/numbers (not in subshell)
        (($tokens[$i].type == "IDENTIFIER" or $tokens[$i].type == "NUMBER") and
         ($tokens[$i + 1].type == "PLUS" or $tokens[$i + 1].type == "STAR" or
          $tokens[$i + 1].type == "MINUS" or $tokens[$i + 1].type == "SLASH") and
         ($tokens[$i + 2].type == "IDENTIFIER" or $tokens[$i + 2].type == "NUMBER"))
        or
        # Pattern 4: Cascade syntax - SEMICOLON after identifier (@ self foo; bar)
        ($tokens[$i].type == "IDENTIFIER" and
         $tokens[$i + 1].type == "SEMICOLON")
        or
        # Pattern 5: Return bare identifier - CARET IDENTIFIER (DOT or NEWLINE or end)
        ($tokens[$i].type == "CARET" and
         $tokens[$i + 1].type == "IDENTIFIER" and
         (($tokens[$i + 2].type // "END") == "DOT" or ($tokens[$i + 2].type // "END") == "NEWLINE" or ($tokens[$i + 2].type // "END") == "END"))
        or
        # Pattern 6: Control flow keywords (ifTrue:, ifFalse:, whileTrue:, timesRepeat:)
        ($tokens[$i].type == "KEYWORD" and
         ($tokens[$i].value == "ifTrue:" or $tokens[$i].value == "ifFalse:" or
          $tokens[$i].value == "whileTrue:" or $tokens[$i].value == "whileFalse:" or
          $tokens[$i].value == "timesRepeat:"))
        or
        # Pattern 7: Comparison operators between identifiers/numbers
        (($tokens[$i].type == "IDENTIFIER" or $tokens[$i].type == "NUMBER" or $tokens[$i].type == "RPAREN") and
         ($tokens[$i + 1].type == "GT" or $tokens[$i + 1].type == "LT" or
          $tokens[$i + 1].type == "GE" or $tokens[$i + 1].type == "LE" or
          $tokens[$i + 1].type == "EQ" or $tokens[$i + 1].type == "NE") and
         ($tokens[$i + 2].type == "IDENTIFIER" or $tokens[$i + 2].type == "NUMBER"))
        or
        # Pattern 8: Collection literals - SYMBOL, HASH_LPAREN (#array), HASH_LBRACE (#dict)
        ($tokens[$i].type == "SYMBOL" or
         $tokens[$i].type == "HASH_LPAREN" or
         $tokens[$i].type == "HASH_LBRACE")
      )
    end
    end  # close if $has_collection_literals
  end;

# Parse and generate method body with expression parser
def expr_transform_body($className; $ivars; $cvars; $args):
  . as $tokens |
  # Parse tokens into AST
  { tokens: $tokens, pos: 0 } | expr_parse_stmts |
  # Collect declared locals and add args
  . as $ast |
  # Generate code - handle null body gracefully
  if ($ast.body == null) or ($ast.type != "statements") then
    "  # ERROR: AST parsing failed"
  else
    (($ast | expr_collect_locals) + ($args // [])) | unique | . as $all_locals |
    # Generate code
    $ast | expr_gen_stmts($all_locals; $ivars; $cvars)
  end;

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

# Get current timestamp in ISO format
def timestamp:
  now | strftime("%Y-%m-%dT%H:%M:%S");

# Convert instance var list to space-separated string for metadata
def varsToString:
  [.[] | .name + (if .default then ":\(.default.value)" else "" end)] | join(" ");

# ------------------------------------------------------------------------------
# Code Generation: Header
# ------------------------------------------------------------------------------

def generateHeader:
  "#!/bin/bash",
  "# Generated by Trashtalk Compiler (jq) - DO NOT EDIT",
  "# Source: \(.name).trash\(if .isTrait then " (trait)" else "" end)",
  "# Generated: \(timestamp)",
  "";

def generateMetadata:
  if .isTrait then
    "__\(.name)__is_trait=\"1\"",
    ""
  else
    "__\(.name)__superclass=\"\(.parent // "")\"",
    "__\(.name)__instanceVars=\"\(.instanceVars | varsToString)\"",
    "__\(.name)__classInstanceVars=\"\(.classInstanceVars | varsToString)\"",
    "__\(.name)__traits=\"\(.traits | join(" "))\"",
    (if (.methodRequirements | length) > 0 then
      "__\(.name)__requires=\"\(.methodRequirements | join(" "))\""
    else empty end),
    # Generate method categories metadata: "selector:category selector:category ..."
    .name as $className |
    ([.methods[] | select(.category != null) | "\(.selector):\(.category)"] as $cats |
    if ($cats | length) > 0 then
      "__\($className)__methodCategories=\"\($cats | join(" "))\""
    else empty end),
    ""
  end;

# Generate class instance variable initializer function
def generateClassVarsInit:
  .name as $className |
  if (.classInstanceVars | length) > 0 then
    "__\($className)__initClassVars() {",
    (.classInstanceVars[] |
      .name as $varName |
      (if .default then
        if .default.type == "number" then .default.value
        elif .default.type == "string" then .default.value
        else ""
        end
      else "" end) as $default |
      "  [[ -z \"$(kvget '__\($className)__cvar__\($varName)')\" ]] && kvset '__\($className)__cvar__\($varName)' '\($default)'"
    ),
    "}",
    ""
  else
    empty
  end;

def generateRequires:
  if (.requires | length) > 0 then
    "# Required dependencies",
    (.requires[] | "source \"\(.)\""),
    ""
  else
    empty
  end;

# ------------------------------------------------------------------------------
# Code Generation: Method Body
# ------------------------------------------------------------------------------

# Transform a sequence of tokens into bash code
# This processes the raw tokens from the method body
def transformMethodBody($className; $isRaw):
  # Transform a single @ message send (handles multi-keyword methods)
  # Input: string like "@ recv key1: arg1 key2: arg2"
  # Output: string like "@ recv key1_key2 arg1 arg2"
  def transformMessageSend:
    if test("^@ [^ ]+ [a-zA-Z_][a-zA-Z0-9_]*:") then
      # Multi-keyword method: @ recv key1: arg1 key2: arg2 → @ recv key1_key2 arg1 arg2
      capture("^(?<prefix>@ [^ ]+ )(?<rest>.*)") |
      .prefix as $prefix |
      .rest |
      {keywords: [], args: [], remaining: .} |
      until(
        (.remaining | test("^[a-zA-Z_][a-zA-Z0-9_]*:") | not);
        if (.remaining | test("^[a-zA-Z_][a-zA-Z0-9_]*: ")) then
          . as $state |
          ($state.remaining | capture("^(?<kw>[a-zA-Z_][a-zA-Z0-9_]*): (?<arg>[^ \"']+|\"[^\"]*\"|'[^']*') *(?<rest>.*)")) |
          {
            keywords: ($state.keywords + [.kw]),
            args: ($state.args + [.arg]),
            remaining: .rest
          }
        else
          .remaining = ""
        end
      ) |
      if (.keywords | length) > 0 then
        $prefix + (.keywords | join("_")) + " " + (.args | join(" ")) + (if .remaining != "" then " " + .remaining else "" end)
      else
        $prefix + .remaining
      end
    elif test("^@ [^ ]+ [a-zA-Z_][a-zA-Z0-9_]*$") then
      # Unary method - already correct
      .
    else
      .
    end;

  # Transform DSL constructs inside a subshell
  # Handles: self → $_RECEIVER, keyword methods (including multi-keyword)
  def transformSubshellContents:
    # Replace self with $_RECEIVER
    gsub("\\bself\\b"; "$_RECEIVER") |
    # Transform multi-keyword methods by finding patterns and merging selectors
    # Pattern: @ recv key1: arg1 key2: arg2 → @ recv key1_key2 arg1 arg2
    # First, handle 2-keyword methods with simple args
    gsub("(?<pre>@ [^ )\"']+ )(?<k1>[a-zA-Z_][a-zA-Z0-9_]*): (?<a1>[^ )\"':]+) (?<k2>[a-zA-Z_][a-zA-Z0-9_]*): (?<a2>[^ )\"':]+)(?<end>[)\"]|$)";
      "\(.pre)\(.k1)_\(.k2) \(.a1) \(.a2)\(.end)") |
    # Handle 2-keyword with quoted second arg
    gsub("(?<pre>@ [^ )\"']+ )(?<k1>[a-zA-Z_][a-zA-Z0-9_]*): (?<a1>[^ )\"':]+) (?<k2>[a-zA-Z_][a-zA-Z0-9_]*): (?<a2>\"[^\"]*\")";
      "\(.pre)\(.k1)_\(.k2) \(.a1) \(.a2)") |
    # Handle single keyword method: @ recv method: arg → @ recv method arg
    gsub("(?<pre>@ [^ )\"']+ )(?<m>[a-zA-Z_][a-zA-Z0-9_]*): (?<arg>[^ )\"':]+)(?<end>[) \"]|$)";
      "\(.pre)\(.m) \(.arg)\(.end)") |
    # Handle single keyword with quoted arg: @ recv method: "arg" → @ recv method "arg"
    gsub("(?<pre>@ [^ )\"']+ )(?<m>[a-zA-Z_][a-zA-Z0-9_]*): (?<arg>\"[^\"]*\")";
      "\(.pre)\(.m) \(.arg)");

  # Unified token-to-string converter
  # $raw: if true, preserve bash code (minimal transformation); if false, transform DSL
  def tokensToString($raw):
    # Token conversion phase
    reduce .[] as $tok ("";
      . + (
        if $tok.type == "NEWLINE" then "\n"
        # Comment handling (raw only)
        elif $tok.type == "COMMENT" then (if $raw then $tok.value else "" end)
        # DSL tokens (normal only)
        elif $tok.type == "AT" then (if $raw then $tok.value + " " else "@ " end)
        elif $tok.type == "ASSIGN" then (if $raw then $tok.value + " " else " := " end)
        elif $tok.type == "CARET" then "^"
        # Brackets
        elif $tok.type == "PIPE" then "| "
        elif $tok.type == "LBRACKET" then "["
        elif $tok.type == "RBRACKET" then "]"
        elif $tok.type == "DLBRACKET" then "[[ "
        elif $tok.type == "DRBRACKET" then " ]]"
        elif $tok.type == "LPAREN" then "("
        elif $tok.type == "RPAREN" then (if $raw then ") " else ")" end)
        # Subshell - transform in normal mode, preserve in raw
        elif $tok.type == "SUBSHELL" then
          (if $raw then $tok.value else ($tok.value | transformSubshellContents) end)
        # Arithmetic
        elif $tok.type == "ARITHMETIC" then $tok.value
        elif $tok.type == "ARITH_CMD" then $tok.value
        # Values with trailing space
        elif $tok.type == "VARIABLE" then $tok.value + " "
        elif $tok.type == "DSTRING" then $tok.value + " "
        elif $tok.type == "STRING" then $tok.value + " "
        elif $tok.type == "NUMBER" then $tok.value + " "
        elif $tok.type == "KEYWORD" then $tok.value + " "
        elif $tok.type == "PATH" then $tok.value + " "
        # Operators
        elif $tok.type == "SEMI" then "; "
        elif $tok.type == "AND" then " && "
        elif $tok.type == "OR" then " || "
        elif $tok.type == "REDIRECT" then $tok.value
        elif $tok.type == "GT" then " >"
        elif $tok.type == "LT" then " <"
        elif $tok.type == "HEREDOC" then "<<"
        elif $tok.type == "MATCH" then " =~ "
        elif $tok.type == "EQ" then " == "
        elif $tok.type == "NE" then " != "
        elif $tok.type == "EQUALS" then (if $raw then "= " else "=" end)
        elif $tok.type == "BANG" then "! "
        elif $tok.type == "AMP" then " &"
        # Punctuation
        elif $tok.type == "DOT" then "."
        elif $tok.type == "SLASH" then "/"
        elif $tok.type == "QUESTION" then "?"
        elif $tok.type == "PLUS" then "+"
        elif $tok.type == "MINUS" then "-"
        elif $tok.type == "STAR" then "*"
        elif $tok.type == "COMMA" then ", "
        elif $tok.type == "TILDE" then "~"
        elif $tok.type == "PERCENT" then "%"
        elif $tok.type == "BACKSLASH" then "\\"
        elif $tok.type == "LITERAL" then $tok.value
        elif $tok.type == "SYMBOL" then "\"\($tok.value)\" "
        elif $tok.type == "HASH_LPAREN" then "#( "
        elif $tok.type == "HASH_LBRACE" then "#{ "
        elif $tok.type == "LBRACE" then "{ "
        elif $tok.type == "RBRACE" then "} "
        else $tok.value + " "
        end
      )
    ) |
    # Post-processing: normalization gsubs
    # Common normalizations for both modes
    gsub(" +"; " ") |                  # Collapse multiple spaces
    gsub(" ;"; ";") |                  # Remove space before semicolon
    gsub(" \\]\\]"; " ]]") |           # Keep space before ]]
    gsub("\\[\\[ "; "[[ ") |           # Keep space after [[
    gsub("(?<a>[0-9]) -(?<b>[0-9])"; "\(.a)-\(.b)") |  # Fix char class ranges like [0-9]
    gsub("(?<a>[a-zA-Z0-9]) \\](?<b>[^\\]])"; "\(.a)]\(.b)") |  # Remove space before ] not followed by ]
    gsub("(?<n>[0-9]) >"; "\(.n)>") |  # Fix number before redirect: 2> not 2 >
    # Mode-specific normalizations
    (if $raw then
      # Raw mode: minimal normalization
      gsub("; ;"; ";;") |              # Fix double semicolon
      gsub("(?<a>[a-zA-Z0-9]) \\](?<b>[+*?$])"; "\(.a)]\(.b)") |  # Remove space before ] when followed by quantifier
      gsub("(?<a>[a-zA-Z0-9]) \\](?<b> \\]\\])"; "\(.a)]\(.b)") |  # Remove space before ] when followed by ]]
      gsub(" \\)"; ")") |              # Remove space before )
      gsub("\\( "; "(") |              # Remove space after (
      gsub("> /"; ">/") |              # Remove space after > before path
      gsub("< (?<c>[^<])"; "<\(.c)") |  # Remove space after < unless followed by < (process substitution)
      gsub("(?<a>[a-zA-Z0-9_]) = (?<c>[0-9\"'$])"; "\(.a)=\(.c)") |  # Fix assignments
      gsub("(?<a>[a-zA-Z0-9_]) = (?<c>[a-zA-Z])"; "\(.a)= \(.c)")   # Keep space for env var assignments
    else
      # Normal mode: full DSL normalization
      gsub(" \\| "; " | ") |           # Normalize pipe spacing
      # Fix regex quantifiers
      gsub("\\] \\?"; "]?") |
      gsub("\\] \\+"; "]+") |
      gsub("\\] \\*"; "]*") |
      gsub("\\$ \\?"; "$?") |
      gsub("- \\?"; "-?") |
      gsub("\\) \\?"; ")?") |
      # Fix file paths
      gsub("(?<pre>[a-zA-Z0-9_]) /(?<post>[a-zA-Z])"; "\(.pre)/\(.post)") |
      gsub("> ?/"; ">/") |
      gsub("2> ?/"; "2>/") |
      # Fix assignment spacing
      gsub(" =(?<c>[a-zA-Z0-9_$])"; "=\(.c)")
    end)
    ;

  # Convenience wrappers for backwards compatibility
  def tokensToCode: tokensToString(false);
  def tokensToRawCode: tokensToString(true);

  # Transform keyword method calls: @ recv key1: arg1 key2: arg2 → @ recv key1_key2 arg1 arg2
  def transformKeywordMethod:
    if test("^@ [^ ]+ [a-zA-Z_][a-zA-Z0-9_]*:") then
      # Extract prefix (@ receiver) and rest
      capture("^(?<prefix>@ [^ ]+ )(?<rest>.*)") |
      .prefix as $prefix |
      .rest |
      # Iteratively extract keyword:arg pairs
      {keywords: [], args: [], remaining: .} |
      until(
        (.remaining | test("^[a-zA-Z_][a-zA-Z0-9_]*:") | not);
        if (.remaining | test("^[a-zA-Z_][a-zA-Z0-9_]*: ")) then
          # Save current state before capture
          . as $state |
          ($state.remaining | capture("^(?<kw>[a-zA-Z_][a-zA-Z0-9_]*): (?<arg>[^ \"']+|\"[^\"]*\"|'[^']*') *(?<rest>.*)")) |
          {
            keywords: ($state.keywords + [.kw]),
            args: ($state.args + [.arg]),
            remaining: .rest
          }
        else
          # Keyword without space after colon or malformed - stop
          .remaining = ""
        end
      ) |
      if (.keywords | length) > 0 then
        $prefix + (.keywords | join("_")) + " " + (.args | join(" ")) + (if .remaining != "" then " " + .remaining else "" end)
      else
        $prefix + .remaining
      end
    else
      .
    end;

  # Apply DSL transformations to a line
  def transformLine:
    # Trim leading/trailing whitespace
    gsub("^\\s+|\\s+$"; "") |

    # Skip empty lines
    if . == "" then ""

    # Local variable declaration: | var1 var2 |
    elif test("^\\|.*\\|$") then
      gsub("^\\|\\s*|\\s*\\|$"; "") |
      "  local \(.)"

    # Return statement: ^ expression
    elif test("^\\^") then
      gsub("^\\^\\s*"; "") |
      # Replace self with $_RECEIVER
      gsub("\\bself\\b"; "$_RECEIVER") |
      "  echo \(.)"

    # Assignment: var := expression
    elif test(":=") then
      gsub("\\bself\\b"; "$_RECEIVER") |
      gsub("\\s*:=\\s*"; "=") |
      "  \(.)"

    # Message send starting with @
    elif test("^@") then
      # Replace self with $_RECEIVER
      gsub("\\bself\\b"; "$_RECEIVER") |
      # Transform keyword method calls (key1: arg1 key2: arg2 → key1_key2 arg1 arg2)
      transformKeywordMethod |
      # For simple method calls, just remove the trailing colon
      gsub("(?<m>[a-zA-Z_][a-zA-Z0-9_]*): "; "\(.m) ") |
      # Quote $_RECEIVER in @ message sends
      gsub("@ \\$_RECEIVER "; "@ \"$_RECEIVER\" ") |
      "  \(.)"

    # Other lines - pass through with self replacement
    else
      gsub("\\bself\\b"; "$_RECEIVER") |
      "  \(.)"
    end;

  # Helper to strip leading/trailing empty lines from array
  def stripEmptyLines:
    # Remove leading empty/whitespace-only lines
    until(length == 0 or (.[0] | test("^\\s*$") | not); .[1:]) |
    # Remove trailing empty/whitespace-only lines
    until(length == 0 or (.[-1] | test("^\\s*$") | not); .[:-1]);

  # Smart indentation for raw methods - tracks nesting, continuation, and heredocs
  def smartIndent:
    reduce .[] as $line ({lines: [], depth: 0, continuation: false, heredoc: null};
      ($line | gsub("^\\s+|\\s+$"; "")) as $trimmed |
      # Check if we're ending a heredoc (terminator must be at start of line, unindented)
      (if .heredoc != null and $trimmed == .heredoc then
        # This is the heredoc terminator - output without indent and clear heredoc state
        {
          lines: (.lines + [$trimmed]),
          depth: .depth,
          continuation: false,
          heredoc: null
        }
      elif .heredoc != null then
        # Inside heredoc - output line as-is without any indentation
        {
          lines: (.lines + [$line]),
          depth: .depth,
          continuation: .continuation,
          heredoc: .heredoc
        }
      else
        # Normal processing
        # Calculate indent for this line
        (if .continuation then .depth + 1 else .depth end) as $effectiveDepth |
        ("    " + ("    " * $effectiveDepth)) as $indent |
        # Determine if this line adjusts depth BEFORE indenting
        (if ($trimmed | test("^(done|fi|esac|;;)")) then .depth - 1 else .depth end) as $preDepth |
        # Use adjusted depth for dedent lines
        (if $preDepth < .depth then
          "    " + ("    " * ([0, $preDepth] | max))
        else
          $indent
        end) as $finalIndent |
        # Build line with indent
        (if $trimmed == "" then ""
         else $finalIndent + $trimmed
         end) as $indentedLine |
        # Update depth based on keywords
        (if ($trimmed | test("\\b(do|then)$")) then .depth + 1
         elif ($trimmed | test("^case .* in$")) then .depth + 1
         elif ($trimmed | test("^(done|fi|esac)$")) then [0, .depth - 1] | max
         else .depth
         end) as $newDepth |
        # Check if line ends with continuation
        ($trimmed | test("\\\\$")) as $isContinuation |
        # Check if line starts a heredoc (<<EOF or <<'EOF' or <<"EOF")
        (if ($trimmed | test("<<-?'?\"?([A-Za-z_][A-Za-z0-9_]*)'?\"?\\s*$")) then
          $trimmed | capture("<<-?'?\"?(?<term>[A-Za-z_][A-Za-z0-9_]*)'?\"?\\s*$") | .term
        else
          null
        end) as $heredocStart |
        {
          lines: (.lines + [$indentedLine]),
          depth: $newDepth,
          continuation: $isContinuation,
          heredoc: $heredocStart
        }
      end)
    ) | .lines;

  # Process the tokens
  if $isRaw then
    # Raw method - minimal processing with raw-specific conversion
    .tokens | tokensToRawCode | split("\n") | map(gsub("\\s+$"; "")) | smartIndent |
    stripEmptyLines | join("\n")
  else
    # Normal method - apply transformations
    .tokens | tokensToCode | split("\n") | map(transformLine) |
    # Filter out null results and strip leading/trailing empty lines
    map(select(. != null)) | stripEmptyLines | join("\n")
  end;

# ------------------------------------------------------------------------------
# Code Generation: Method
# ------------------------------------------------------------------------------

def generateMethod($className; $ivars; $cvars):
  # Build function name
  (if .kind == "class" then
    "__\($className)__class__\(.selector)"
  else
    "__\($className)__\(.selector)"
  end) as $funcName |

  # Generate argument bindings for keyword methods
  (if (.args | length) > 0 then
    [.args | to_entries[] | "  local \(.value)=\"$\(.key + 1)\""] | join("\n")
  else
    ""
  end) as $argBindings |

  # Get method args for local variable tracking
  (.args // []) as $methodArgs |

  # Generate body - use expression parser for non-raw methods with Smalltalk syntax
  .raw as $isRaw |
  (if $isRaw then
    # Raw method - use existing transformation
    .body | transformMethodBody($className; true)
  elif ((.body.tokens != null) and ((.body.tokens | should_use_expr_parser) // false)) then
    # New Smalltalk-style syntax - use expression parser with ivar inference
    .body.tokens | expr_transform_body($className; $ivars; $cvars; $methodArgs)
  else
    # Legacy bash-style syntax - use existing transformation
    .body | transformMethodBody($className; false)
  end) as $body |

  # Combine into function
  "\($funcName)() {",
  (if $argBindings != "" then $argBindings else empty end),
  $body,
  "}",
  "";

# ------------------------------------------------------------------------------
# Main Code Generator
# ------------------------------------------------------------------------------

def generate:
  . as $class |
  # Extract instance variable names for expression parser
  ([.instanceVars[]? | .name] // []) as $ivars |
  # Extract class instance variable names for expression parser
  ([.classInstanceVars[]? | .name] // []) as $cvars |
  (
    generateHeader,
    generateMetadata,
    generateClassVarsInit,
    generateRequires,
    (.methods[] | generateMethod($class.name; $ivars; $cvars))
  );

# ==============================================================================
# Entry Point
# ==============================================================================

generate
