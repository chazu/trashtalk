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
    ",":  [3, 4],       # string concatenation, left-assoc
    ">":  [5, 6],       # comparison, left-assoc
    "<":  [5, 6],       # comparison, left-assoc
    ">=": [5, 6],       # comparison, left-assoc
    "<=": [5, 6],       # comparison, left-assoc
    "==": [5, 6],       # comparison (numeric), left-assoc
    "!=": [5, 6],       # comparison (numeric), left-assoc
    "=":  [5, 6],       # string equality, left-assoc
    "~=": [5, 6],       # string inequality, left-assoc
    "=~": [5, 6],       # regex match, left-assoc
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

# Escape a string for bash $'...' ANSI-C quoting
# Escapes: backslash -> \\, single-quote -> \'
def ansi_c_escape:
  gsub("\\\\"; "\\\\") | gsub("'"; "\\'") | gsub("\n"; "\\n");

# Check if current token is an infix operator
def expr_is_operator:
  expr_peek as $tok |
  $tok != null and (
    $tok.type == "PLUS" or $tok.type == "MINUS" or
    $tok.type == "STAR" or $tok.type == "PERCENT" or
    $tok.type == "SLASH" or $tok.type == "ASSIGN" or
    $tok.type == "GT" or $tok.type == "LT" or
    $tok.type == "GE" or $tok.type == "LE" or
    $tok.type == "EQ" or $tok.type == "NE" or
    $tok.type == "EQUALS" or $tok.type == "STR_NE" or $tok.type == "MATCH" or
    $tok.type == "COMMA"
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
  elif $tok.type == "EQUALS" then "="
  elif $tok.type == "STR_NE" then "~="
  elif $tok.type == "MATCH" then "=~"
  elif $tok.type == "COMMA" then ","
  else null
  end;

# Check if current token is a control flow keyword
def expr_is_control_flow:
  expr_peek as $tok |
  $tok != null and $tok.type == "KEYWORD" and
  ($tok.value == "ifTrue:" or $tok.value == "ifFalse:" or
   $tok.value == "whileTrue:" or $tok.value == "whileFalse:" or
   $tok.value == "timesRepeat:" or $tok.value == "try:" or
   $tok.value == "ifNil:" or $tok.value == "ifNotNil:");
   # Note: "to:" removed - conflicts with keyword messages like "from:to:"
   # Range syntax "1 to: 10 do: [...]" is handled separately

# ==============================================================================
# JSON Primitive Detection
# ==============================================================================
# JSON primitives are special operations that compile to jq pipelines.
# They eliminate the need for raw jq in Array/Dictionary implementations.

# Array primitives that take arguments (keyword methods)
def expr_is_json_array_keyword:
  expr_peek as $tok |
  $tok != null and $tok.type == "KEYWORD" and
  ($tok.value == "arrayPush:" or $tok.value == "arrayPushJson:" or
   $tok.value == "arrayAt:" or $tok.value == "arrayAt:put:" or
   $tok.value == "arrayRemoveAt:");

# Object primitives that take arguments (keyword methods)
def expr_is_json_object_keyword:
  expr_peek as $tok |
  $tok != null and $tok.type == "KEYWORD" and
  ($tok.value == "objectAt:" or $tok.value == "objectAt:put:" or
   $tok.value == "objectAt:putJson:" or $tok.value == "objectHasKey:" or
   $tok.value == "objectRemoveKey:" or $tok.value == "jsonPath:");

# Unary JSON primitives (no arguments)
def expr_is_json_unary:
  expr_peek as $tok |
  $tok != null and $tok.type == "IDENTIFIER" and
  ($tok.value == "arrayLength" or $tok.value == "arrayFirst" or
   $tok.value == "arrayLast" or $tok.value == "arrayIsEmpty" or
   $tok.value == "objectKeys" or $tok.value == "objectValues" or
   $tok.value == "objectLength" or $tok.value == "objectIsEmpty" or
   $tok.value == "stringToJsonArray");

# Combined check for any JSON primitive
def expr_is_json_primitive:
  expr_is_json_array_keyword or expr_is_json_object_keyword or expr_is_json_unary;

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
      # Check for qualified name: Package::Class
      (. | expr_advance) as $after_id |
      if ($after_id | expr_peek_type) == "NAMESPACE_SEP" then
        # Consume :: and get the class name
        ($after_id | expr_advance) as $after_sep |
        ($after_sep | expr_peek) as $class_tok |
        if $class_tok != null and $class_tok.type == "IDENTIFIER" then
          { state: ($after_sep | expr_advance), result: { type: "qualified_name", package: $tok.value, name: $class_tok.value } }
        else
          # Malformed qualified name, just return the first identifier
          { state: $after_id, result: { type: "identifier", name: $tok.value } }
        end
      else
        { state: $after_id, result: { type: "identifier", name: $tok.value } }
      end
    end
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }
  elif $tok.type == "NUMBER" then
    { state: (. | expr_advance), result: { type: "number", value: $tok.value } }
  elif $tok.type == "STRING" then
    { state: (. | expr_advance), result: { type: "string", value: ($tok.value | ltrimstr("'") | rtrimstr("'")) } }
  elif $tok.type == "TRIPLESTRING" then
    { state: (. | expr_advance), result: { type: "triplestring", value: $tok.value } }
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
    # Check for block parameters [:x :y | body]
    { state: ., params: [] } |
    until((.state | expr_peek_type) != "BLOCK_PARAM";
      (.state | expr_peek) as $p |
      .params += [$p.value] |
      .state |= expr_advance |
      .state |= expr_skip_ws
    ) |
    # If we have params, skip the PIPE separator
    (if (.params | length) > 0 and (.state | expr_peek_type) == "PIPE" then
      .state |= expr_advance | .state |= expr_skip_ws
    else . end) |
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
    if ($params | length) > 0 then
      { state: .state, result: { type: "block_literal", params: $params, tokens: .tokens } }
    else
      { state: .state, result: { type: "block", tokens: .tokens } }
    end
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
  elif $tok.type == "TRIPLESTRING" then
    { state: (. | expr_advance), result: { type: "triplestring", value: $tok.value } }
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
          elif $keyword == "ifNil:" then
            # Check for ifNil:ifNotNil: pattern
            ($block.state | expr_skip_ws) as $after_block |
            if ($after_block | expr_peek_type) == "KEYWORD" and ($after_block | expr_peek.value) == "ifNotNil:" then
              (($after_block | expr_advance | expr_skip_ws) | expr_parse_block) as $else_block |
              {
                state: $else_block.state,
                result: {
                  type: "control_flow",
                  kind: "nil_else",
                  subject: $receiver,
                  nil_block: $block.result,
                  notnil_block: $else_block.result
                }
              }
            else
              {
                state: $block.state,
                result: {
                  type: "control_flow",
                  kind: "if_nil",
                  subject: $receiver,
                  block: $block.result
                }
              }
            end
          elif $keyword == "ifNotNil:" then
            {
              state: $block.state,
              result: {
                type: "control_flow",
                kind: "if_not_nil",
                subject: $receiver,
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
      elif (.state | expr_peek_type) == "IDENTIFIER" and
           ((.state | expr_peek.value) | . as $v |
            ["fileExists", "isFile", "isDirectory", "isFifo", "isSymlink",
             "isReadable", "isWritable", "isExecutable", "isEmpty", "notEmpty"] | index($v) != null) then
        # File/string test predicate (fileExists, isFile, isEmpty, etc.)
        .result as $subject |
        (.state | expr_peek.value) as $test |
        .state |= expr_advance |
        {
          state: .state,
          result: {
            type: "test_expr",
            test: $test,
            subject: $subject
          }
        }
        | infix_loop
      elif (.state | expr_is_json_unary) then
        # Unary JSON primitives: arr arrayLength, arr arrayFirst, etc.
        .result as $receiver |
        (.state | expr_peek.value) as $op |
        .state |= expr_advance |
        {
          state: .state,
          result: {
            type: "json_primitive",
            operation: $op,
            receiver: $receiver,
            args: []
          }
        }
        | infix_loop
      elif (.state | expr_is_json_array_keyword) or (.state | expr_is_json_object_keyword) then
        # Keyword JSON primitives: arr arrayPush: val, obj objectAt: key, etc.
        .result as $receiver |
        (.state | expr_peek.value) as $keyword |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        # Determine operation name from keyword
        ($keyword | rtrimstr(":")) as $op_base |
        # Check for compound keywords like arrayAt:put: or objectAt:put: or objectAt:putJson:
        if $keyword == "arrayAt:" or $keyword == "objectAt:" then
          # Parse first argument
          (.state | expr_parse_expr(0)) as $first_arg |
          $first_arg.state | expr_skip_ws |
          # Check for put: or putJson: continuation
          if expr_peek_type == "KEYWORD" and expr_peek.value == "put:" then
            expr_advance | expr_skip_ws |
            (. | expr_parse_expr(0)) as $second_arg |
            {
              state: $second_arg.state,
              result: {
                type: "json_primitive",
                operation: ($op_base + "Put"),
                receiver: $receiver,
                args: [$first_arg.result, $second_arg.result]
              }
            }
            | infix_loop
          elif expr_peek_type == "KEYWORD" and expr_peek.value == "putJson:" then
            # putJson: variant - inserts raw JSON without quoting
            expr_advance | expr_skip_ws |
            (. | expr_parse_expr(0)) as $second_arg |
            {
              state: $second_arg.state,
              result: {
                type: "json_primitive",
                operation: ($op_base + "PutJson"),
                receiver: $receiver,
                args: [$first_arg.result, $second_arg.result]
              }
            }
            | infix_loop
          else
            # Just arrayAt: or objectAt: without put:
            {
              state: .,
              result: {
                type: "json_primitive",
                operation: $op_base,
                receiver: $receiver,
                args: [$first_arg.result]
              }
            }
            | infix_loop
          end
        else
          # Single-arg keywords: arrayPush:, arrayPushJson:, arrayRemoveAt:, objectHasKey:, objectRemoveKey:, jsonPath:
          (.state | expr_parse_expr(0)) as $arg |
          {
            state: $arg.state,
            result: {
              type: "json_primitive",
              operation: $op_base,
              receiver: $receiver,
              args: [$arg.result]
            }
          }
          | infix_loop
        end
      elif (.state | expr_peek_type) == "KEYWORD" and
           ((.state | expr_peek.value) == "and:" or (.state | expr_peek.value) == "or:") then
        # Boolean operators: (cond1) and: [cond2] or (cond1) or: [cond2]
        # Only treat as boolean op if followed by a block - otherwise it's part of a keyword message
        (.state | expr_advance | expr_skip_ws | expr_peek_type) as $next_type |
        if $next_type == "LBRACKET" then
          .result as $left_cond |
          (.state | expr_peek.value | rtrimstr(":")) as $bool_op |
          .state |= expr_advance |
          .state |= expr_skip_ws |
          (.state | expr_parse_block) as $block |
          {
            state: $block.state,
            result: {
              type: "boolean_op",
              op: $bool_op,
              left: $left_cond,
              right: $block.result
            }
          }
          | infix_loop
        else
          # Not followed by block - not a boolean op, just return what we have
          .
        end
      elif (.state | expr_peek_type) == "KEYWORD" and (.state | expr_peek.value) == "matches:" then
        # Regex match: str matches: 'pattern'
        .result as $subject |
        .state |= expr_advance |
        .state |= expr_skip_ws |
        # Parse the pattern expression (should be a string)
        (.state | expr_parse_expr(0)) as $pattern |
        {
          state: $pattern.state,
          result: {
            type: "regex_match",
            subject: $subject,
            pattern: $pattern.result
          }
        }
        | infix_loop
      elif (.state | expr_peek_type) == "IDENTIFIER" and (.state | expr_peek.value) == "not" then
        # Boolean negation: (condition) not
        .result as $cond |
        .state |= expr_advance |
        {
          state: .state,
          result: {
            type: "not",
            condition: $cond
          }
        }
        | infix_loop
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
  elif expr_peek_type == "KEYWORD" and expr_peek.value == "try:" then
    # Parse try: [block] catch: [block] or try: [block] catch: [ :param | block ]
    expr_advance | expr_skip_ws |
    if expr_peek_type == "LBRACKET" then
      expr_parse_block as $try_block |
      $try_block.state | expr_skip_ws |
      if expr_peek_type == "KEYWORD" and expr_peek.value == "catch:" then
        expr_advance | expr_skip_ws |
        if expr_peek_type == "LBRACKET" then
          # Parse catch block which may have parameters [ :err | body ]
          (. | expr_advance) | expr_skip_ws |
          # Check for block parameters
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
          .params as $catch_params |
          # Collect the catch body tokens
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
          {
            state: .state,
            result: {
              type: "control_flow",
              kind: "try_catch",
              try_block: $try_block.result,
              catch_block: { type: "block", tokens: .tokens },
              error_param: (if ($catch_params | length) > 0 then $catch_params[0] else null end)
            }
          }
        else
          { state: ., result: null }
        end
      else
        { state: ., result: null }
      end
    else
      { state: ., result: null }
    end
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
    elif .type == "subshell" then
      # Replace self with "$_RECEIVER" in subshell content
      .value | gsub("@ self\\b"; "@ \"$_RECEIVER\"")
    elif .type == "variable" then .value
    else "0"
    end
  end;

# Transform instance variable references in a dstring
# Replaces $ivar and ${ivar} with $(_ivar ivar) for each known instance variable
def dstring_transform_ivars($ivars):
  . as $str |
  if ($ivars | length) == 0 then $str
  else
    # Process each ivar name
    reduce ($ivars // [])[] as $ivar (
      $str;
      # Replace ${ivar} form first (more specific)
      gsub("\\$\\{\($ivar)\\}"; "$(_ivar \($ivar))") |
      # Replace $ivar form (word boundary - followed by non-identifier char or end)
      gsub("\\$\($ivar)(?=[^a-zA-Z0-9_]|$)"; "$(_ivar \($ivar))")
    )
  end;

# Transform instance variable and local variable references in subshell message sends
# For $(@ receiver selector: arg), transforms:
# - @ self -> @ "$_RECEIVER"
# - @ localVar -> @ "$localVar" (receiver position)
# - selector: localVar -> selector: "$localVar" (argument position)
# - selector: ivar -> selector: "$(_ivar ivar)" (instance variable argument)
def subshell_transform_ivars($ivars; $locals):
  . as $str |
  # First, replace @ self with @ "$_RECEIVER"
  ($str | gsub("@ self\\b"; "@ \"$_RECEIVER\"")) |
  # Transform local variables as receivers: @ localVar -> @ "$localVar"
  # Pattern: @ followed by local variable name (not already quoted)
  (if ($locals | length) == 0 then .
  else
    reduce ($locals // [])[] as $local (
      .;
      # Match @ followed by local var name (must be word boundary after)
      gsub("@ \($local)(?=[^a-zA-Z0-9_]|$)"; "@ \"$\($local)\"")
    )
  end) |
  # Transform local variables as keyword arguments: selector: localVar -> selector: "$localVar"
  (if ($locals | length) == 0 then .
  else
    reduce ($locals // [])[] as $local (
      .;
      # Match bare local var after `: ` - transform to "$localVar"
      # The local must be followed by word boundary (space, ), ., etc.)
      gsub("(?<=: )\($local)(?=[^a-zA-Z0-9_]|$)"; "\"$\($local)\"") |
      # Also match at end of subshell before final )
      gsub("(?<=: )\($local)\\)$"; "\"$\($local)\")")
    )
  end) |
  # Then transform bare ivar identifiers that appear as keyword arguments
  # Pattern: after `: ` (keyword separator), a bare identifier that's an ivar
  if ($ivars | length) == 0 then .
  else
    reduce ($ivars // [])[] as $ivar (
      .;
      # Only transform if NOT a local variable (locals take precedence)
      if ($locals | any(. == $ivar)) then .
      else
        # Match bare ivar after `: ` - transform to "$(_ivar ivar)"
        # The ivar must be followed by word boundary (space, ), ., etc.)
        gsub("(?<=: )\($ivar)(?=[^a-zA-Z0-9_]|$)"; "\"$(_ivar \($ivar))\"") |
        # Also match at end of subshell before final )
        gsub("(?<=: )\($ivar)\\)$"; "\"$(_ivar \($ivar))\")")
      end
    )
  end;

# Generate code for an expression
def expr_gen($locals; $ivars; $cvars):
  if . == null then ""
  elif .type == "number" then .value
  elif .type == "string" then "'\(.value)'"
  elif .type == "triplestring" then "$'\(.value | ansi_c_escape)'"
  elif .type == "dstring" then .value | dstring_transform_ivars($ivars)
  elif .type == "self" then "\"$_RECEIVER\""
  elif .type == "identifier" then
    if expr_is_local(.name; $locals) then "$\(.name)"
    elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
    elif expr_is_cvar(.name; $cvars) then "$(_cvar \(.name))"
    else .name  # bare identifier (command name, etc.)
    end
  elif .type == "qualified_name" then
    # Qualified class reference: Package::Class
    "\(.package)::\(.name)"
  elif .type == "variable" then .value
  elif .type == "subshell" then
    # Transform self and instance variables in subshell message sends
    .value | subshell_transform_ivars($ivars; $locals)
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
    if .op == "," then
      # String concatenation: generate proper bash string concat
      # Each part needs to be suitable for embedding in double quotes
      def concat_part:
        if .type == "string" then
          # String literal (single-quoted) - use raw value
          .value
        elif .type == "dstring" then
          # Double-quoted string - strip the quotes from value
          .value | .[1:-1]
        elif .type == "identifier" then
          # Variable reference - use ${var} form
          if expr_is_local(.name; $locals) then "${\(.name)}"
          elif expr_is_ivar(.name; $ivars) then "$(_ivar \(.name))"
          elif expr_is_cvar(.name; $cvars) then "$(_cvar \(.name))"
          else "${\(.name)}"
          end
        elif .type == "binary" and .op == "," then
          # Nested concat - recurse
          (.left | concat_part) + (.right | concat_part)
        else
          # Other expressions - generate and wrap
          expr_gen($locals; $ivars; $cvars) as $code |
          if ($code | test("^\\$")) then $code
          elif ($code | test("^'.*'$")) then ($code | .[1:-1])  # Strip single quotes
          elif ($code | test("^\".*\"$")) then ($code | .[1:-1])  # Strip double quotes
          else $code
          end
        end;
      (.left | concat_part) + (.right | concat_part)
    else
      "$(( \(.left | expr_gen_arith($locals; $ivars; $cvars)) \(.op) \(.right | expr_gen_arith($locals; $ivars; $cvars)) ))"
    end
  elif .type == "message_send" then
    (.receiver | expr_gen($locals; $ivars; $cvars)) as $recv |
    # Quote receiver if it's a variable expansion (contains $)
    (if ($recv | test("^\\$")) then "\"\($recv)\"" else $recv end) as $quoted_recv |
    # Format selector with args - keyword methods need colons for dispatcher
    (if ((.args // []) | length) > 0 then
      # Keyword method: interleave keywords and args
      # selector "at_put" with args [idx, val] -> "at: idx put: val"
      (.selector | split("_")) as $keywords |
      # Quote args that are variable expansions (start with $)
      ([(.args // [])[] | expr_gen($locals; $ivars; $cvars) | if test("^\\$") then "\"\(.)\"" else . end]) as $arg_codes |
      ([$keywords, $arg_codes] | transpose | map("\(.[0]): \(.[1])") | join(" "))
    else
      # Unary method: just the selector
      .selector // ""
    end) as $msg |
    "@ \($quoted_recv) \($msg)"
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
    (.value.type == "string") as $is_string |
    (.value.type == "dstring") as $is_dstring |
    (.value.type == "message_send" or .value.type == "cascade") as $is_message |
    # Check for arithmetic binary ops - generate Procyon-compatible code (no subshell wrapper)
    (.value.type == "binary" and (.value.op == "+" or .value.op == "-" or .value.op == "*" or .value.op == "/" or .value.op == "%")) as $is_arithmetic |
    # Also check for unary minus (negation)
    (.value.type == "unary" and .value.op == "-") as $is_unary_arith |
    (.value | expr_gen($locals; $ivars; $cvars)) as $val_code |
    # For message sends, wrap in $() for command substitution
    (if $is_message then "$(\($val_code))" else $val_code end) as $final_val |
    if expr_is_local(.target; $locals) then
      if $is_arithmetic then
        # Use (( var = expr )) for Procyon compatibility - no subshell
        "(( \(.target) = \(.value | expr_gen_arith($locals; $ivars; $cvars)) ))"
      elif $is_unary_arith then
        "(( \(.target) = \(.value | expr_gen_arith($locals; $ivars; $cvars)) ))"
      elif $is_collection then "\(.target)=\($val_code)"
      elif $is_string then "\(.target)=\"\(.value.value)\""  # Use raw string value
      elif $is_dstring then "\(.target)=\($val_code)"  # dstrings already have quotes
      elif $is_message then "\(.target)=\"\($final_val)\""
      else "\(.target)=\"\($val_code)\""
      end
    elif expr_is_ivar(.target; $ivars) then
      # For ivars, use _ivar_set (collection literals rare in loop bodies)
      if $is_arithmetic then
        # Use temp var + arithmetic command for Procyon compatibility
        "local __arith__; (( __arith__ = \(.value | expr_gen_arith($locals; $ivars; $cvars)) )); _ivar_set \(.target) \"$__arith__\""
      elif $is_unary_arith then
        "local __arith__; (( __arith__ = \(.value | expr_gen_arith($locals; $ivars; $cvars)) )); _ivar_set \(.target) \"$__arith__\""
      elif $is_message then "_ivar_set \(.target) \"\($final_val)\""
      else "_ivar_set \(.target) \"\($val_code)\""
      end
    elif expr_is_cvar(.target; $cvars) then
      # For cvars, use _cvar_set
      if $is_arithmetic then
        "local __arith__; (( __arith__ = \(.value | expr_gen_arith($locals; $ivars; $cvars)) )); _cvar_set \(.target) \"$__arith__\""
      elif $is_unary_arith then
        "local __arith__; (( __arith__ = \(.value | expr_gen_arith($locals; $ivars; $cvars)) )); _cvar_set \(.target) \"$__arith__\""
      elif $is_message then "_cvar_set \(.target) \"\($final_val)\""
      else "_cvar_set \(.target) \"\($val_code)\""
      end
    else
      if $is_arithmetic then
        "(( \(.target) = \(.value | expr_gen_arith($locals; $ivars; $cvars)) ))"
      elif $is_unary_arith then
        "(( \(.target) = \(.value | expr_gen_arith($locals; $ivars; $cvars)) ))"
      elif $is_collection then "\(.target)=\($val_code)"
      elif $is_string then "\(.target)=\"\(.value.value)\""  # Use raw string value
      elif $is_dstring then "\(.target)=\($val_code)"  # dstrings already have quotes
      elif $is_message then "\(.target)=\"\($final_val)\""
      else "\(.target)=\"\($val_code)\""
      end
    end
  elif .type == "return" then
    if .value == null then "return"
    elif .value.type == "string" then "echo \"\(.value.value)\"; return"
    elif .value.type == "symbol" then
      # Check if symbol is an instance variable
      if expr_is_ivar(.value.value; $ivars) then "echo \"$(_ivar \(.value.value))\"; return"
      else "echo \"\(.value.value)\"; return"
      end
    elif .value.type == "binary" then
      # Handle comparison returns - evaluate and echo true/false
      # Helper to get value - string literals use raw value, others use expr_gen
      def str_val: if .type == "string" then .value else expr_gen($locals; $ivars; $cvars) end;
      .value as $bin |
      if $bin.op == "=" or $bin.op == "==" then
        # String/value equality - use [[ ]] for safety with strings
        ($bin.left | str_val) as $left |
        ($bin.right | str_val) as $right |
        "if [[ \"\($left)\" == \"\($right)\" ]]; then echo \"true\"; else echo \"false\"; fi; return"
      elif $bin.op == "~=" or $bin.op == "!=" then
        # Inequality
        ($bin.left | str_val) as $left |
        ($bin.right | str_val) as $right |
        "if [[ \"\($left)\" != \"\($right)\" ]]; then echo \"true\"; else echo \"false\"; fi; return"
      elif $bin.op == ">" or $bin.op == "<" or $bin.op == ">=" or $bin.op == "<=" then
        # Numeric comparisons - use (( )) arithmetic
        ($bin.left | expr_gen_arith($locals; $ivars; $cvars)) as $left |
        ($bin.right | expr_gen_arith($locals; $ivars; $cvars)) as $right |
        "if (( \($left) \($bin.op) \($right) )); then echo \"true\"; else echo \"false\"; fi; return"
      else
        # Unknown binary op - fall through to expr_gen
        "echo \"\(.value | expr_gen($locals; $ivars; $cvars))\"; return"
      end
    elif .value.type == "message_send" or .value.type == "cascade" then
      # Message sends output directly, no echo wrapper needed
      "\(.value | expr_gen($locals; $ivars; $cvars)); return"
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
    "$(@ Block params: '[]' code: '\($escaped_body)' captured: '{\"_RECEIVER\":\"'\"$_RECEIVER\"'\"}')"
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
        if $stmt.type == "return" or $stmt.type == "message_send" or $stmt.type == "cascade" or $stmt.type == "locals" or $stmt.type == "assignment" then
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
    "$(@ Block params: '[\($params_json)]' code: '\($escaped_body)' captured: '{\"_RECEIVER\":\"'\"$_RECEIVER\"'\"}')"
  elif .type == "control_flow" then
    # Inline control flow generation to avoid forward reference
    # Helper function to generate a condition with appropriate wrapper
    # Returns {code: "...", needs_wrapper: bool} where needs_wrapper indicates if (( )) is needed
    def gen_cond_part($cond):
      if $cond.type == "test_expr" then
        # Test expressions generate their own [[ ]] wrapper
        ($cond.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
        if $cond.test == "fileExists" then {code: "[[ -e \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isFile" then {code: "[[ -f \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isDirectory" then {code: "[[ -d \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isFifo" then {code: "[[ -p \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isSymlink" then {code: "[[ -L \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isReadable" then {code: "[[ -r \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isWritable" then {code: "[[ -w \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isExecutable" then {code: "[[ -x \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "isEmpty" then {code: "[[ -z \"\($subj)\" ]]", needs_wrapper: false}
        elif $cond.test == "notEmpty" then {code: "[[ -n \"\($subj)\" ]]", needs_wrapper: false}
        else {code: "# unknown test: \($cond.test)", needs_wrapper: false}
        end
      elif $cond.type == "boolean_op" then
        # Recursive boolean operation - each side generates its own wrapper
        (gen_cond_part($cond.left)) as $left |
        # Right side is a block - parse it and generate condition from first statement
        (if $cond.right.tokens != null then
          ({ tokens: $cond.right.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          if ($parsed.body | length) > 0 then
            gen_cond_part($parsed.body[0])
          else {code: "true", needs_wrapper: false}
          end
        else {code: "true", needs_wrapper: false}
        end) as $right |
        # Wrap each side appropriately
        (if $left.needs_wrapper then "(( \($left.code) ))" else $left.code end) as $left_code |
        (if $right.needs_wrapper then "(( \($right.code) ))" else $right.code end) as $right_code |
        (if $cond.op == "and" then "&&" else "||" end) as $bash_op |
        {code: "\($left_code) \($bash_op) \($right_code)", needs_wrapper: false}
      elif $cond.type == "not" then
        # Boolean negation - negate the inner condition
        (gen_cond_part($cond.condition)) as $inner |
        # Wrap inner if needed, then negate
        (if $inner.needs_wrapper then "! (( \($inner.code) ))" else "! \($inner.code)" end) as $negated |
        {code: $negated, needs_wrapper: false}
      elif $cond.type == "regex_match" then
        # Regex match: subject matches: 'pattern'
        ($cond.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
        ($cond.pattern | expr_gen($locals; $ivars; $cvars)) as $pat |
        {code: "[[ \"\($subj)\" =~ \($pat) ]]", needs_wrapper: false}
      elif $cond.type == "binary" then
        # String comparison operators generate [[ ]] syntax, arithmetic use (( ))
        # Helper to get value - string literals use raw value, message sends wrapped in $(), others use expr_gen
        def str_val:
          if .type == "string" then .value
          elif .type == "message_send" or .type == "cascade" then "$(\(expr_gen($locals; $ivars; $cvars)))"
          else expr_gen($locals; $ivars; $cvars)
          end;
        if $cond.op == "=" then
          # String equality
          ($cond.left | str_val) as $left |
          ($cond.right | str_val) as $right |
          {code: "[[ \"\($left)\" == \"\($right)\" ]]", needs_wrapper: false}
        elif $cond.op == "~=" then
          # String inequality
          ($cond.left | str_val) as $left |
          ($cond.right | str_val) as $right |
          {code: "[[ \"\($left)\" != \"\($right)\" ]]", needs_wrapper: false}
        elif $cond.op == "=~" then
          # Regex match
          ($cond.left | expr_gen($locals; $ivars; $cvars)) as $left |
          ($cond.right | expr_gen($locals; $ivars; $cvars)) as $right |
          {code: "[[ \"\($left)\" =~ \($right) ]]", needs_wrapper: false}
        else
          # Arithmetic comparison (>, <, ==, !=, etc.)
          {code: "\($cond.left | expr_gen_arith($locals; $ivars; $cvars)) \($cond.op) \($cond.right | expr_gen_arith($locals; $ivars; $cvars))", needs_wrapper: true}
        end
      elif $cond.type == "identifier" then
        if expr_is_local($cond.name; $locals) then {code: "$\($cond.name)", needs_wrapper: true}
        elif expr_is_ivar($cond.name; $ivars) then {code: "$(_ivar \($cond.name))", needs_wrapper: true}
        else {code: $cond.name, needs_wrapper: true}
        end
      elif $cond.type == "variable" then {code: $cond.value, needs_wrapper: true}
      elif $cond.type == "boolean" then {code: (if $cond.value then "1" else "0" end), needs_wrapper: true}
      elif $cond.type == "json_primitive" and ($cond.operation == "objectHasKey" or $cond.operation == "objectIsEmpty" or $cond.operation == "arrayIsEmpty") then
        # Boolean json_primitives return "true"/"false" strings - use string comparison
        ($cond | expr_gen($locals; $ivars; $cvars)) as $code |
        {code: "[[ \"\($code)\" == \"true\" ]]", needs_wrapper: false}
      else {code: ($cond | expr_gen($locals; $ivars; $cvars)), needs_wrapper: true}
      end;
    # Handle nil checks separately (they use .subject not .condition)
    if .kind == "if_nil" then
      (.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      "if [[ -z \"\($subj)\" ]]; then \($block_code); fi"
    elif .kind == "if_not_nil" then
      (.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
      # Check if block has parameters (for binding)
      # Block may be block_literal (with params) or block (without)
      (if .block.type == "block_literal" then .block.params else [] end) as $params |
      (if ($params | length) > 0 then
        ($params + $locals) as $block_locals |
        (if .block.tokens != null then
          ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
        elif .block.body != null then
          [(.block.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
        else "" end) as $block_code |
        "if [[ -n \"\($subj)\" ]]; then local \($params[0])=\"\($subj)\"; \($block_code); fi"
      else
        (if .block.tokens != null then
          ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        elif .block.body != null then
          [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        else "" end) as $block_code |
        "if [[ -n \"\($subj)\" ]]; then \($block_code); fi"
      end)
    elif .kind == "nil_else" then
      (.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
      (if .nil_block.tokens != null then
        ({ tokens: .nil_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .nil_block.body != null then
        [(.nil_block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $nil_code |
      # Check if notnil block has parameters (for binding)
      (if .notnil_block.type == "block_literal" then .notnil_block.params else [] end) as $params |
      (if ($params | length) > 0 then
        ($params + $locals) as $block_locals |
        (if .notnil_block.tokens != null then
          ({ tokens: .notnil_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
        elif .notnil_block.body != null then
          [(.notnil_block.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
        else "" end) as $notnil_code |
        "if [[ -z \"\($subj)\" ]]; then \($nil_code); else local \($params[0])=\"\($subj)\"; \($notnil_code); fi"
      else
        (if .notnil_block.tokens != null then
          ({ tokens: .notnil_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
          [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        elif .notnil_block.body != null then
          [(.notnil_block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
        else "" end) as $notnil_code |
        "if [[ -z \"\($subj)\" ]]; then \($nil_code); else \($notnil_code); fi"
      end)
    else
    # Generate the condition for other control flow kinds
    (gen_cond_part(.condition)) as $cond_info |
    $cond_info.code as $cond |
    $cond_info.needs_wrapper as $needs_wrapper |
    # Generate block body inline (can't use nested def due to jq scoping)
    if .kind == "if_true" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      if $needs_wrapper then
        "if (( \($cond) )); then \($block_code); fi"
      else
        "if \($cond); then \($block_code); fi"
      end
    elif .kind == "if_false" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      if $needs_wrapper then
        "if (( !(\($cond)) )); then \($block_code); fi"
      else
        "if ! \($cond); then \($block_code); fi"
      end
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
      if $needs_wrapper then
        "if (( \($cond) )); then \($true_code); else \($false_code); fi"
      else
        "if \($cond); then \($true_code); else \($false_code); fi"
      end
    elif .kind == "times_repeat" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      "for ((_i=0; _i<\(.count | expr_gen_arith($locals; $ivars; $cvars)); _i++)); do \($block_code); done"
    elif .kind == "range_do" then
      # Range iteration: start to: end do: [:i | body]
      # Block should be block_literal with params array
      (if .block.type == "block_literal" and (.block.params | length) > 0 then
        .block.params[0]
      else "_i" end) as $loop_var |
      # Add loop var to locals for block code generation
      ([$loop_var] + $locals) as $block_locals |
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($block_locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      (.start | expr_gen_arith($locals; $ivars; $cvars)) as $start_code |
      (.end | expr_gen_arith($locals; $ivars; $cvars)) as $end_code |
      "for ((\($loop_var)=\($start_code); \($loop_var)<\($end_code); \($loop_var)++)); do \($block_code); done"
    elif .kind == "while_true" then
      (if .block.tokens != null then
        ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .block.body != null then
        [(.block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $block_code |
      if .condition.type == "block" then
        # Parse condition block and use gen_cond_part for proper string/arithmetic handling
        (if .condition.tokens != null then
          ({ tokens: .condition.tokens, pos: 0 } | expr_parse_stmts) as $cond_parsed |
          if ($cond_parsed.body | length) == 1 then
            (gen_cond_part($cond_parsed.body[0])) as $cond_info |
            if $cond_info.needs_wrapper then
              "(( \($cond_info.code) ))"
            else
              $cond_info.code
            end
          else
            [($cond_parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
          end
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
        # Parse condition block and use gen_cond_part for proper string/arithmetic handling
        (if .condition.tokens != null then
          ({ tokens: .condition.tokens, pos: 0 } | expr_parse_stmts) as $cond_parsed |
          if ($cond_parsed.body | length) == 1 then
            (gen_cond_part($cond_parsed.body[0])) as $cond_info |
            if $cond_info.needs_wrapper then
              "(( !(\($cond_info.code)) ))"
            else
              "! \($cond_info.code)"
            end
          else
            [($cond_parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
          end
        else "" end) as $cond_code |
        "while \($cond_code); do \($block_code); done"
      else
        "while (( !(\($cond)) )); do \($block_code); done"
      end
    elif .kind == "try_catch" then
      # Generate try/catch using bash error handling
      (if .try_block.tokens != null then
        ({ tokens: .try_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      elif .try_block.body != null then
        [(.try_block.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
      else "" end) as $try_code |
      # Get error parameter name (default to "error")
      (.error_param // "error") as $error_var |
      # Generate catch block with error param as local
      (if .catch_block.tokens != null then
        ({ tokens: .catch_block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
        ([$error_var] + $locals) as $catch_locals |
        [($parsed.body // [])[] | expr_gen($catch_locals; $ivars; $cvars)] | join("; ")
      elif .catch_block.body != null then
        ([$error_var] + $locals) as $catch_locals |
        [(.catch_block.body // [])[] | expr_gen($catch_locals; $ivars; $cvars)] | join("; ")
      else "" end) as $catch_code |
      # Generate: if ! try_code; then error_var="..."; catch_code; _clear_error; fi
      "if ! { \($try_code); }; then local \($error_var)=\"$_ERROR_TYPE: $_ERROR_MSG\"; \($catch_code); _clear_error; fi"
    else
      "# ERROR: unknown control flow kind \(.kind)"
    end
    end
  elif .type == "json_primitive" then
    # JSON primitive operations - generate jq pipelines
    (.receiver | expr_gen($locals; $ivars; $cvars)) as $recv |
    # Determine how to pipe receiver to jq:
    # - Variables ($...) need: echo "$var" |
    # - Message sends (@...) need: $( @ ... ) | (no echo, wrap in subshell)
    # - Already-evaluated subshells ($(...)) need: echo "$(...)"|
    (if ($recv | test("^@")) then
      # Message send - needs subshell wrapper, no echo
      { "prefix": "$(", "recv": $recv, "suffix": ") |" }
    elif ($recv | test("^\\$")) then
      # Variable or subshell - needs echo with quotes
      { "prefix": "$(echo \"", "recv": $recv, "suffix": "\" |" }
    else
      # Literal or other - use echo
      { "prefix": "$(echo ", "recv": $recv, "suffix": " |" }
    end) as $pipe_recv |
    # For backward compat, also set quoted_recv
    (if ($recv | test("^\\$")) then "\"\($recv)\"" else $recv end) as $quoted_recv |
    # Helper: generate proper jq pipe - handles message sends vs variables/literals
    # Message sends (@...) are executed directly, others need echo
    def jq_pipe($jq_cmd):
      if ($recv | test("^@")) then
        # Message send - execute directly and pipe to jq
        "$(\($recv) | \($jq_cmd))"
      else
        # Variable or literal - use echo
        "$(echo \($quoted_recv) | \($jq_cmd))"
      end;
    # Generate arguments - for jq --arg, strings need raw values without outer quotes
    # But variables and other expressions need proper quoting
    def gen_jq_arg($arg):
      if $arg.type == "string" then
        # String literal - use raw value for jq --arg
        $arg.value
      else
        # Variables and expressions need the generated code
        $arg | expr_gen($locals; $ivars; $cvars)
      end;
    ([(.args // [])[] | . as $a | gen_jq_arg($a)]) as $arg_codes |
    if .operation == "arrayPush" then
      # arr arrayPush: val -> $(echo "$arr" | jq -c --arg v "$val" '. + [$v]')
      jq_pipe("jq -c --arg v \"\($arg_codes[0])\" '. + [$v]'")
    elif .operation == "arrayAt" then
      # arr arrayAt: idx -> $(echo "$arr" | jq -r --argjson i "$idx" '.[$i] // empty')
      jq_pipe("jq -r --argjson i \"\($arg_codes[0])\" '.[$i] // empty'")
    elif .operation == "arrayAtPut" then
      # arr arrayAt: idx put: val -> $(echo "$arr" | jq -c --argjson i "$idx" --arg v "$val" '.[$i] = $v')
      jq_pipe("jq -c --argjson i \"\($arg_codes[0])\" --arg v \"\($arg_codes[1])\" '.[$i] = $v'")
    elif .operation == "arrayRemoveAt" then
      # arr arrayRemoveAt: idx -> $(echo "$arr" | jq -c --argjson i "$idx" 'del(.[$i])')
      jq_pipe("jq -c --argjson i \"\($arg_codes[0])\" 'del(.[$i])'")
    elif .operation == "arrayLength" then
      # arr arrayLength -> $(echo "$arr" | jq 'length')
      jq_pipe("jq 'length'")
    elif .operation == "arrayFirst" then
      # arr arrayFirst -> $(echo "$arr" | jq -r '.[0] // empty')
      jq_pipe("jq -r '.[0] // empty'")
    elif .operation == "arrayLast" then
      # arr arrayLast -> $(echo "$arr" | jq -r '.[-1] // empty')
      jq_pipe("jq -r '.[-1] // empty'")
    elif .operation == "arrayIsEmpty" then
      # arr arrayIsEmpty -> $(echo "$arr" | jq 'length == 0')
      jq_pipe("jq 'length == 0'")
    elif .operation == "objectAt" then
      # obj objectAt: key -> $(echo "$obj" | jq -r --arg k "$key" '.[$k] // empty')
      jq_pipe("jq -r --arg k \"\($arg_codes[0])\" '.[$k] // empty'")
    elif .operation == "objectAtPut" then
      # obj objectAt: key put: val -> $(echo "$obj" | jq -c --arg k "$key" --arg v "$val" '.[$k] = $v')
      jq_pipe("jq -c --arg k \"\($arg_codes[0])\" --arg v \"\($arg_codes[1])\" '.[$k] = $v'")
    elif .operation == "objectHasKey" then
      # obj objectHasKey: key -> $(echo "$obj" | jq --arg k "$key" 'has($k)')
      jq_pipe("jq --arg k \"\($arg_codes[0])\" 'has($k)'")
    elif .operation == "objectRemoveKey" then
      # obj objectRemoveKey: key -> $(echo "$obj" | jq -c --arg k "$key" 'del(.[$k])')
      jq_pipe("jq -c --arg k \"\($arg_codes[0])\" 'del(.[$k])'")
    elif .operation == "objectKeys" then
      # obj objectKeys -> $(echo "$obj" | jq -c 'keys')
      jq_pipe("jq -c 'keys'")
    elif .operation == "objectValues" then
      # obj objectValues -> $(echo "$obj" | jq -c '[.[]]')
      jq_pipe("jq -c '[.[]]'")
    elif .operation == "objectLength" then
      # obj objectLength -> $(echo "$obj" | jq 'length')
      jq_pipe("jq 'length'")
    elif .operation == "objectIsEmpty" then
      # obj objectIsEmpty -> $(echo "$obj" | jq 'length == 0')
      jq_pipe("jq 'length == 0'")
    elif .operation == "stringToJsonArray" then
      # str stringToJsonArray -> $(echo "$str" | jq -Rc '[., inputs]')
      # Converts newline-separated string to JSON array
      jq_pipe("jq -Rc '[., inputs]'")
    elif .operation == "arrayPushJson" then
      # arr arrayPushJson: jsonVal -> $(echo "$arr" | jq -c --argjson v "$jsonVal" '. + [$v]')
      # Pushes raw JSON value without quoting (for nested objects/arrays)
      jq_pipe("jq -c --argjson v \"\($arg_codes[0])\" '. + [$v]'")
    elif .operation == "objectAtPutJson" then
      # obj objectAt: key putJson: jsonVal -> $(echo "$obj" | jq -c --arg k "$key" --argjson v "$jsonVal" '.[$k] = $v')
      # Sets key to raw JSON value without quoting (for nested objects/arrays)
      jq_pipe("jq -c --arg k \"\($arg_codes[0])\" --argjson v \"\($arg_codes[1])\" '.[$k] = $v'")
    elif .operation == "jsonPath" then
      # json jsonPath: 'a.b.c' -> $(echo "$json" | jq -r '.a.b.c // empty')
      # Extracts value at dot-separated path
      jq_pipe("jq -r '.\($arg_codes[0]) // empty'")
    else
      "# ERROR: unknown json_primitive operation \(.operation)"
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

# Generate a test expression (file tests, string tests)
def expr_gen_test_expr($locals; $ivars; $cvars):
  (.subject | expr_gen($locals; $ivars; $cvars)) as $subj |
  if .test == "fileExists" then "[[ -e \"\($subj)\" ]]"
  elif .test == "isFile" then "[[ -f \"\($subj)\" ]]"
  elif .test == "isDirectory" then "[[ -d \"\($subj)\" ]]"
  elif .test == "isFifo" then "[[ -p \"\($subj)\" ]]"
  elif .test == "isSymlink" then "[[ -L \"\($subj)\" ]]"
  elif .test == "isReadable" then "[[ -r \"\($subj)\" ]]"
  elif .test == "isWritable" then "[[ -w \"\($subj)\" ]]"
  elif .test == "isExecutable" then "[[ -x \"\($subj)\" ]]"
  elif .test == "isEmpty" then "[[ -z \"\($subj)\" ]]"
  elif .test == "notEmpty" then "[[ -n \"\($subj)\" ]]"
  else "# unknown test: \(.test)"
  end;

# Check if a condition is a test expression (needs [[ ]] wrapper instead of (( )))
def is_test_condition:
  .type == "test_expr";

# Generate condition for control flow (inner part, no wrapper)
def expr_gen_condition_inner($locals; $ivars; $cvars):
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

# Generate condition with appropriate wrapper
def expr_gen_condition($locals; $ivars; $cvars):
  if .type == "test_expr" then
    # Test expressions generate their own [[ ]] wrapper
    expr_gen_test_expr($locals; $ivars; $cvars)
  else
    expr_gen_condition_inner($locals; $ivars; $cvars)
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

# Helper to generate block as a while condition
# For single comparison expressions, uses (( )) instead of $(( ))
def expr_gen_block_as_condition($locals; $ivars; $cvars):
  if .tokens != null then
    ({ tokens: .tokens, pos: 0 } | expr_parse_stmts) as $parsed |
    # If single expression that's a comparison, wrap in (( ))
    if ($parsed.body | length) == 1 and ($parsed.body[0].type == "binary") then
      "(( \($parsed.body[0] | expr_gen_condition_inner($locals; $ivars; $cvars)) ))"
    else
      [($parsed.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
    end
  elif .body != null then
    if (.body | length) == 1 and (.body[0].type == "binary") then
      "(( \(.body[0] | expr_gen_condition_inner($locals; $ivars; $cvars)) ))"
    else
      [(.body // [])[] | expr_gen($locals; $ivars; $cvars)] | join("; ")
    end
  else
    ""
  end;

# Wrap a condition with appropriate syntax based on type
# Test expressions already have [[ ]], arithmetic needs (( ))
def wrap_condition($locals; $ivars; $cvars):
  if .type == "test_expr" then
    # Test expressions generate their own [[ ]] wrapper
    expr_gen_test_expr($locals; $ivars; $cvars)
  else
    # Arithmetic conditions need (( )) wrapper
    "(( \(expr_gen_condition_inner($locals; $ivars; $cvars)) ))"
  end;

# Wrap a negated condition
def wrap_condition_negated($locals; $ivars; $cvars):
  if .type == "test_expr" then
    # Negate the test expression
    "! \(expr_gen_test_expr($locals; $ivars; $cvars))"
  else
    # Arithmetic conditions with negation
    "(( !(\(expr_gen_condition_inner($locals; $ivars; $cvars))) ))"
  end;

# Generate code for control flow constructs
def expr_gen_control_flow($locals; $ivars; $cvars):
  if .kind == "if_true" then
    "if \(.condition | wrap_condition($locals; $ivars; $cvars)); then \(.block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "if_false" then
    "if \(.condition | wrap_condition_negated($locals; $ivars; $cvars)); then \(.block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "if_else" then
    "if \(.condition | wrap_condition($locals; $ivars; $cvars)); then \(.true_block | expr_gen_block_body($locals; $ivars; $cvars)); else \(.false_block | expr_gen_block_body($locals; $ivars; $cvars)); fi"
  elif .kind == "times_repeat" then
    "for ((_i=0; _i<\(.count | expr_gen_arith($locals; $ivars; $cvars)); _i++)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
  elif .kind == "range_do" then
    # Range iteration with loop variable from block parameter
    (if .block.type == "block_literal" and (.block.params | length) > 0 then
      .block.params[0]
    else "_i" end) as $loop_var |
    ([$loop_var] + $locals) as $block_locals |
    (.start | expr_gen_arith($locals; $ivars; $cvars)) as $start_code |
    (.end | expr_gen_arith($locals; $ivars; $cvars)) as $end_code |
    "for ((\($loop_var)=\($start_code); \($loop_var)<\($end_code); \($loop_var)++)); do \(.block | expr_gen_block_body($block_locals; $ivars; $cvars)); done"
  elif .kind == "while_true" then
    if .condition.type == "block" then
      "while \(.condition | expr_gen_block_as_condition($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    else
      "while \(.condition | wrap_condition($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    end
  elif .kind == "while_false" then
    if .condition.type == "block" then
      "while ! \(.condition | expr_gen_block_as_condition($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    else
      "while \(.condition | wrap_condition_negated($locals; $ivars; $cvars)); do \(.block | expr_gen_block_body($locals; $ivars; $cvars)); done"
    end
  elif .kind == "try_catch" then
    (.error_param // "error") as $error_var |
    ([$error_var] + $locals) as $catch_locals |
    "if ! { \(.try_block | expr_gen_block_body($locals; $ivars; $cvars)); }; then local \($error_var)=\"$_ERROR_TYPE: $_ERROR_MSG\"; \(.catch_block | expr_gen_block_body($catch_locals; $ivars; $cvars)); _clear_error; fi"
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
      ($stmt.value.type == "triplestring") as $is_ansi_quoted |
      ($stmt.value.type == "string") as $is_string |
      ($stmt.value.type == "dstring") as $is_dstring |
      ($stmt.value.type == "message_send" or $stmt.value.type == "cascade") as $is_message |
      # Check for arithmetic binary ops - generate Procyon-compatible code (no subshell wrapper)
      ($stmt.value.type == "binary" and ($stmt.value.op == "+" or $stmt.value.op == "-" or $stmt.value.op == "*" or $stmt.value.op == "/" or $stmt.value.op == "%")) as $is_arithmetic |
      # Also check for unary minus (negation)
      ($stmt.value.type == "unary" and $stmt.value.op == "-") as $is_unary_arith |
      ($stmt.value | expr_gen($current_locals; $ivars; $cvars)) as $val_code |
      ($stmt.value | expr_gen_json($current_locals; $ivars; $cvars)) as $json_code |
      # For message sends, wrap in $() for command substitution
      (if $is_message then "$(\($val_code))" else $val_code end) as $msg_code |
      if expr_is_local($stmt.target; $current_locals) then
        if $is_arithmetic or $is_unary_arith then
          # Use (( var = expr )) for Procyon compatibility - no subshell
          .lines += ["  (( \($stmt.target) = \($stmt.value | expr_gen_arith($current_locals; $ivars; $cvars)) ))"]
        elif $is_collection or $is_ansi_quoted then
          .lines += ["  \($stmt.target)=\($val_code)"]
        elif $is_string then
          .lines += ["  \($stmt.target)=\"\($stmt.value.value)\""]  # Use raw string value
        elif $is_dstring then
          .lines += ["  \($stmt.target)=\($val_code)"]  # dstrings already have quotes
        elif $is_message then
          .lines += ["  \($stmt.target)=\"\($msg_code)\""]
        else
          .lines += ["  \($stmt.target)=\"\($val_code)\""]
        end
      elif expr_is_ivar($stmt.target; $ivars) then
        if $is_arithmetic or $is_unary_arith then
          # Use temp var + arithmetic command for Procyon compatibility - no subshell
          .lines += ["  local __arith__; (( __arith__ = \($stmt.value | expr_gen_arith($current_locals; $ivars; $cvars)) )); _ivar_set \($stmt.target) \"$__arith__\""]
        elif $is_collection then
          # Use JSON serialization for collection ivars
          .lines += ["  _ivar_set \($stmt.target) '\($json_code)'"]
        elif $is_ansi_quoted then
          .lines += ["  _ivar_set \($stmt.target) \($val_code)"]
        elif $is_string then
          .lines += ["  _ivar_set \($stmt.target) \"\($stmt.value.value)\""]  # Use raw string value
        elif $is_dstring then
          .lines += ["  _ivar_set \($stmt.target) \($val_code)"]  # dstrings already have quotes
        elif $is_message then
          .lines += ["  _ivar_set \($stmt.target) \"\($msg_code)\""]
        else
          .lines += ["  _ivar_set \($stmt.target) \"\($val_code)\""]
        end
      elif expr_is_cvar($stmt.target; $cvars) then
        if $is_arithmetic or $is_unary_arith then
          # Use temp var + arithmetic command for Procyon compatibility - no subshell
          .lines += ["  local __arith__; (( __arith__ = \($stmt.value | expr_gen_arith($current_locals; $ivars; $cvars)) )); _cvar_set \($stmt.target) \"$__arith__\""]
        elif $is_collection then
          # Use JSON serialization for collection cvars
          .lines += ["  _cvar_set \($stmt.target) '\($json_code)'"]
        elif $is_ansi_quoted then
          .lines += ["  _cvar_set \($stmt.target) \($val_code)"]
        elif $is_message then
          .lines += ["  _cvar_set \($stmt.target) \"\($msg_code)\""]
        else
          .lines += ["  _cvar_set \($stmt.target) \"\($val_code)\""]
        end
      else
        # Unknown target - treat as regular assignment (could be global/env var)
        if $is_arithmetic or $is_unary_arith then
          # Use (( var = expr )) for Procyon compatibility - no subshell
          .lines += ["  (( \($stmt.target) = \($stmt.value | expr_gen_arith($current_locals; $ivars; $cvars)) ))"]
        elif $is_collection or $is_ansi_quoted then
          .lines += ["  \($stmt.target)=\($val_code)"]
        elif $is_message then
          .lines += ["  \($stmt.target)=\"\($msg_code)\""]
        else
          .lines += ["  \($stmt.target)=\"\($val_code)\""]
        end
      end
    elif $stmt.type == "return" then
      # Delegate to expr_gen which has proper comparison handling for returns
      .lines += ["  \($stmt | expr_gen($current_locals; $ivars; $cvars))"]
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
  if ($tokens | length) < 2 then false
  else
    # First, check for strong Smalltalk signals that should always use expr parser
    # Collection literals, try:, triple-quoted strings, and test predicates are unambiguous Smalltalk syntax
    # Test predicates are now IDENTIFIER tokens with specific values
    def is_test_predicate: . as $v |
      ["fileExists", "isFile", "isDirectory", "isFifo", "isSymlink",
       "isReadable", "isWritable", "isExecutable", "isEmpty", "notEmpty"] | index($v) != null;
    # JSON primitive keywords
    def is_json_primitive_keyword: . as $v |
      ["arrayPush:", "arrayPushJson:", "arrayAt:", "arrayRemoveAt:",
       "objectAt:", "objectHasKey:", "objectRemoveKey:", "jsonPath:"] | index($v) != null;
    # JSON primitive unary identifiers
    def is_json_primitive_unary: . as $v |
      ["arrayLength", "arrayFirst", "arrayLast", "arrayIsEmpty",
       "objectKeys", "objectValues", "objectLength", "objectIsEmpty",
       "stringToJsonArray"] | index($v) != null;
    (any($tokens[]; .type == "SYMBOL" or .type == "HASH_LPAREN" or .type == "HASH_LBRACE" or .type == "TRIPLESTRING" or
                    (.type == "IDENTIFIER" and (.value | is_test_predicate)))) as $has_collection_literals |
    (any($tokens[]; .type == "KEYWORD" and .value == "try:")) as $has_try_catch |
    # JSON primitives are strong Smalltalk signals
    (any($tokens[]; (.type == "KEYWORD" and (.value | is_json_primitive_keyword)) or
                    (.type == "IDENTIFIER" and (.value | is_json_primitive_unary)))) as $has_json_primitives |
    # Block literals must use expr parser to compile to Block objects
    (any(range(0; ($tokens | length) - 1) as $i |
      $tokens[$i].type == "LBRACKET" and $tokens[$i + 1].type == "BLOCK_PARAM")) as $has_block_literal |
    if $has_collection_literals or $has_try_catch or $has_block_literal or $has_json_primitives then true
    else
    # Check for exclusions: bash constructs that shouldn't use expr parser
    # Bash commands that appear as bare identifiers (not after @)
    def is_bash_command:
      # Note: "wait" removed - commonly used as method name in OOP
      # Note: "kill" removed - commonly used as method name in OOP
      . as $v | ["echo", "printf", "jq", "sed", "awk", "grep", "cat", "ls", "cd",
                 "read", "eval", "exec", "export", "source", "test", "true", "false",
                 "local", "declare", "typeset", "unset", "shift", "exit", "return",
                 "break", "continue", "trap", "set", "shopt"] | any(. == $v);

    # Bash control keywords
    def is_bash_control:
      . as $v | ["if", "then", "else", "elif", "fi", "for", "in", "do", "done",
                 "while", "until", "case", "esac", "function"] | any(. == $v);

    # Check for bash construct exclusions
    (any(range(0; $tokens | length) as $i |
      # Bash control keyword anywhere
      ($tokens[$i].type == "IDENTIFIER" and ($tokens[$i].value | is_bash_control))
      or
      # Bare bash command used as actual command at start of statement
      # Only consider it a command if:
      # 1. Preceded by NEWLINE or DOT (start of statement) or at position 0
      # 2. NOT followed by := (which would make it an assignment target)
      ($tokens[$i].type == "IDENTIFIER" and ($tokens[$i].value | is_bash_command) and
       ($i == 0 or $tokens[$i - 1].type == "NEWLINE" or $tokens[$i - 1].type == "DOT") and
       (($i + 1 >= ($tokens | length)) or $tokens[$i + 1].type != "ASSIGN"))
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
      # Note: Use length-1 to allow 2-token patterns like "^ session"
      any(range(0; ($tokens | length) - 1) as $i |
        # Pattern 1: identifier := identifier/string (ivar inference)
        ($tokens[$i].type == "IDENTIFIER" and
         $tokens[$i + 1].type == "ASSIGN" and
         (($tokens[$i + 2].type == "IDENTIFIER" and
           ($tokens[$i + 2].value != null) and
           ($tokens[$i + 2].value | test("^[a-z]"))) or
          $tokens[$i + 2].type == "STRING"))
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
        # Pattern 4: Cascade syntax - SEMI after identifier (@ self foo; bar)
        ($tokens[$i].type == "IDENTIFIER" and
         $tokens[$i + 1].type == "SEMI")
        or
        # Pattern 5: Return bare identifier - CARET IDENTIFIER (DOT or NEWLINE or RBRACKET or end)
        ($tokens[$i].type == "CARET" and
         $tokens[$i + 1].type == "IDENTIFIER" and
         (($tokens[$i + 2].type // "END") == "DOT" or ($tokens[$i + 2].type // "END") == "NEWLINE" or ($tokens[$i + 2].type // "END") == "RBRACKET" or ($tokens[$i + 2].type // "END") == "END"))
        or
        # Pattern 6: Control flow keywords (ifTrue:, ifFalse:, whileTrue:, timesRepeat:, try:, and:, or:, ifNil:, ifNotNil:)
        # Note: "to:" removed - conflicts with keyword messages like "from:to:"
        ($tokens[$i].type == "KEYWORD" and
         ($tokens[$i].value == "ifTrue:" or $tokens[$i].value == "ifFalse:" or
          $tokens[$i].value == "whileTrue:" or $tokens[$i].value == "whileFalse:" or
          $tokens[$i].value == "timesRepeat:" or $tokens[$i].value == "try:" or
          $tokens[$i].value == "and:" or $tokens[$i].value == "or:" or
          $tokens[$i].value == "ifNil:" or $tokens[$i].value == "ifNotNil:"))
        or
        # Pattern 7: Comparison operators between identifiers/numbers/strings
        # Note: EQUALS is "=", EQ is "==", NE is "!="
        (($tokens[$i].type == "IDENTIFIER" or $tokens[$i].type == "NUMBER" or $tokens[$i].type == "STRING" or $tokens[$i].type == "RPAREN") and
         ($tokens[$i + 1].type == "GT" or $tokens[$i + 1].type == "LT" or
          $tokens[$i + 1].type == "GE" or $tokens[$i + 1].type == "LE" or
          $tokens[$i + 1].type == "EQ" or $tokens[$i + 1].type == "NE" or
          $tokens[$i + 1].type == "EQUALS" or $tokens[$i + 1].type == "STR_NE") and
         ($tokens[$i + 2].type == "IDENTIFIER" or $tokens[$i + 2].type == "NUMBER" or $tokens[$i + 2].type == "STRING"))
        or
        # Pattern 8: Collection literals - SYMBOL, HASH_LPAREN (#array), HASH_LBRACE (#dict)
        ($tokens[$i].type == "SYMBOL" or
         $tokens[$i].type == "HASH_LPAREN" or
         $tokens[$i].type == "HASH_LBRACE")
        or
        # Pattern 9: Message send to variable - AT IDENTIFIER (KEYWORD or IDENTIFIER or NAMESPACE_SEP)
        # e.g., @ aBlock valueWith: or @ aBlock value or @ Yutani::Widget new
        # This enables proper variable expansion for message receivers
        ($tokens[$i].type == "AT" and
         $tokens[$i + 1].type == "IDENTIFIER" and
         ($tokens[$i + 2].type == "KEYWORD" or $tokens[$i + 2].type == "IDENTIFIER" or $tokens[$i + 2].type == "NAMESPACE_SEP"))
        or
        # Pattern 10: Block literal - LBRACKET BLOCK_PARAM (e.g., [:x | ...])
        ($tokens[$i].type == "LBRACKET" and
         $tokens[$i + 1].type == "BLOCK_PARAM")
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
# Note: We escape double quotes so JSON defaults survive bash string parsing
def varsToString:
  [.[] | .name + (if .default then ":\(.default.value | gsub("\""; "\\\""))" else "" end)] | join(" ");

# Get the fully qualified class name (Package::ClassName or just ClassName)
# Input: class AST with .package and .name
def qualifiedName:
  if .package != null and .package != "" then
    "\(.package)::\(.name)"
  else
    .name
  end;

# Get the function name prefix (__Package__ClassName or __ClassName)
# This converts :: to __ for valid bash function names
# Input: class AST with .package and .name
def funcPrefix:
  if .package != null and .package != "" then
    "__\(.package)__\(.name)"
  else
    "__\(.name)"
  end;

# Get the function name prefix from a pre-computed qualified name string
# Converts :: to __ for bash function names
def funcPrefixFromName($name):
  "__\($name | gsub("::"; "__"))";


# ------------------------------------------------------------------------------
# Code Generation: Header
# ------------------------------------------------------------------------------

def generateHeader:
  "#!/usr/bin/env bash",
  "# Generated by Trashtalk Compiler (jq) - DO NOT EDIT",
  "# Source: \(.name).trash\(if .isTrait then " (trait)" else "" end)",
  "# Generated: \(timestamp)",
  "";

def generateMetadata:
  funcPrefix as $prefix |
  qualifiedName as $qname |
  # Qualify parent name with package if unqualified and class is in a package
  # Exception: core/global classes should never be qualified
  # These are base classes that exist at the global level, not in any package
  .parent as $parentName |
  ["Object", "Tool", "TestCase"] as $globalClasses |
  (if $parentName == null or $parentName == "" then ""
   elif ($parentName | contains("::")) then $parentName
   elif ($globalClasses | index($parentName)) then $parentName
   elif .package != null then "\(.package)::\($parentName)"
   else $parentName end) as $qualifiedParent |
  if .isTrait then
    "\($prefix)__is_trait=\"1\"",
    "\($prefix)__sourceHash=\"\(.sourceHash // "")\"",
    ""
  else
    "\($prefix)__superclass=\"\($qualifiedParent)\"",
    "\($prefix)__instanceVars=\"\(.instanceVars | varsToString)\"",
    "\($prefix)__classInstanceVars=\"\(.classInstanceVars | varsToString)\"",
    "\($prefix)__traits=\"\(.traits | join(" "))\"",
    "\($prefix)__sourceHash=\"\(.sourceHash // "")\"",
    (if .package != null then
      "\($prefix)__package=\"\(.package)\"",
      "\($prefix)__qualifiedName=\"\($qname)\""
    else empty end),
    (if (.methodRequirements | length) > 0 then
      "\($prefix)__requires=\"\(.methodRequirements | join(" "))\""
    else empty end),
    # Generate method categories metadata: "selector:category selector:category ..."
    ([.methods[] | select(.category != null) | "\(.selector):\(.category)"] as $cats |
    if ($cats | length) > 0 then
      "\($prefix)__methodCategories=\"\($cats | join(" "))\""
    else empty end),
    # Generate test methods metadata: list of test method selectors
    ([.methods[] | select(.kind == "test") | .selector] as $tests |
    if ($tests | length) > 0 then
      "\($prefix)__tests=\"\($tests | join(" "))\""
    else empty end),
    ""
  end;

# Source embed removed - no longer needed

# Generate class instance variable initializer function
def generateClassVarsInit:
  funcPrefix as $prefix |
  .name as $className |
  if (.classInstanceVars | length) > 0 then
    "\($prefix)__initClassVars() {",
    (.classInstanceVars[] |
      .name as $varName |
      (if .default then
        if .default.type == "number" then .default.value
        elif .default.type == "string" then .default.value
        elif .default.type == "triplestring" then .default.value
        else ""
        end
      else "" end) as $default |
      "  [[ -z \"$(kv_get '\($className)__cvar__\($varName)')\" ]] && kv_set '\($className)__cvar__\($varName)' '\($default)'"
    ),
    "}",
    ""
  else
    empty
  end;

# Generate accessor methods (getters/setters) for instance variables
# These are generated at compile time so they're available immediately
# Uses Smalltalk-style naming: foo (getter), foo: value (setter)
def generateAccessors:
  funcPrefix as $prefix |
  if (.instanceVars | length) > 0 then
    (.instanceVars[] |
      .name as $varName |
      # Capitalize first letter for legacy method names
      ($varName | split("") | .[0] |= ascii_upcase | join("")) as $capName |
      # Smalltalk-style getter: foo() - just the variable name
      "\($prefix)__\($varName)() {",
      "  echo \"$(_ivar \($varName))\"; return",
      "}",
      "",
      # Smalltalk-style setter: foo_() - called as "foo: value"
      "\($prefix)__\($varName)_() {",
      "  _ivar_set \($varName) \"$1\"",
      "}",
      "",
      # Legacy getter: getFoo() for backwards compatibility
      "\($prefix)__get\($capName)() {",
      "  echo \"$(_ivar \($varName))\"; return",
      "}",
      "",
      # Legacy setter: setFoo_() for backwards compatibility (called as setFoo: value)
      "\($prefix)__set\($capName)_() {",
      "  _ivar_set \($varName) \"$1\"",
      "}"
    ),
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
        elif $tok.type == "TRIPLESTRING" then "$'\($tok.value | gsub("\\\\"; "\\\\") | gsub("'"; "\\'") | gsub("\n"; "\\n"))' "
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
        elif $tok.type == "HEREDOC_BLOCK" then $tok.value + "\n"
        elif $tok.type == "HERESTRING" then "<<< "
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
    gsub("\\[(?<a>[0-9]) -(?<b>[0-9])"; "[\(.a)-\(.b)") |  # Fix char class ranges like [0-9]
    gsub("(?<a>[a-zA-Z0-9]) \\](?<b>[^\\]])"; "\(.a)]\(.b)") |  # Remove space before ] not followed by ]
    gsub("(?<n>[0-9]) >"; "\(.n)>") |  # Fix number before redirect: 2> not 2 >
    # Mode-specific normalizations
    (if $raw then
      # Raw mode: minimal normalization
      gsub("; ;"; ";;") |              # Fix double semicolon
      gsub("(?<a>[a-zA-Z_][a-zA-Z0-9_]*) \\[(?<b>[\"$])"; "\(.a)[\(.b)") |  # Fix array access: VAR ["$key"] → VAR["$key"]
      gsub(" \\]= "; "]=") |           # Fix array assignment: ]= → ]=
      gsub("\" \\*"; "\"*") |          # Fix pattern glob: "${prefix}" * → "${prefix}"*
      gsub("} \\*"; "}*") |            # Fix pattern glob: ${prefix} * → ${prefix}*
      gsub("(?<a>[a-zA-Z0-9_]) \\*"; "\(.a)*") |  # Fix path glob: /path_ * → /path_*
      gsub("(?<a>[a-zA-Z0-9]) \\](?<b>[+*?$])"; "\(.a)]\(.b)") |  # Remove space before ] when followed by quantifier
      gsub("(?<a>[a-zA-Z0-9]) \\](?<b> \\]\\])"; "\(.a)]\(.b)") |  # Remove space before ] when followed by ]]
      gsub(" \\)"; ")") |              # Remove space before )
      gsub("\\( "; "(") |              # Remove space after (
      gsub("> /"; ">/") |              # Remove space after > before path
      gsub("< (?<c>[^<])"; "<\(.c)") |  # Remove space after < unless followed by < (process substitution)
      gsub("(?<a>[a-zA-Z0-9_]) = (?<c>[0-9\"'$])"; "\(.a)=\(.c)") |  # Fix assignments: var = "val" → var="val"
      gsub("(?<a>[a-zA-Z0-9_]) =(?<c>[\"'$])"; "\(.a)=\(.c)") |  # Fix assignments: var ="val" → var="val"
      gsub("(?<a>[a-zA-Z0-9_])= (?<c>true|false|yes|no)(?<d>[ \n;)$])"; "\(.a)=\(.c)\(.d)") |  # Fix bool: var= true → var=true
      gsub("(?<a>[a-zA-Z0-9_]) = (?<c>true|false|yes|no)(?<d>[ \n;)$])"; "\(.a)=\(.c)\(.d)") |  # Fix bool: var = true → var=true
      gsub("(?<a>[a-zA-Z0-9_]) = (?<c>[a-zA-Z])"; "\(.a)= \(.c)")   # Keep space for env var assignments: IFS= read
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
      gsub(" =(?<c>[a-zA-Z0-9_$\"'])"; "=\(.c)")
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

  # Fix heredoc indentation - strips indent from heredoc content and terminators
  # Heredoc content and terminators must not be indented in bash
  def fixHeredocIndent:
    reduce .[] as $line ({lines: [], heredoc: null};
      if .heredoc != null then
        # We're inside a heredoc
        # Check if this line (after stripping indent) matches the terminator
        ($line | gsub("^\\s+"; "")) as $stripped |
        if $stripped == .heredoc then
          # This is the terminator - output unindented
          {lines: (.lines + [$stripped]), heredoc: null}
        else
          # Heredoc content - output unindented (strip any leading whitespace)
          {lines: (.lines + [$stripped]), heredoc: .heredoc}
        end
      else
        # Check if this line starts a heredoc
        # Pattern: <<MARKER or <<'MARKER' or <<"MARKER" or <<-MARKER
        if ($line | test("<<-?['\"]?[A-Za-z_][A-Za-z0-9_]*['\"]?\\s*$")) then
          # Extract the heredoc marker (without quotes)
          ($line | capture("<<-?['\"]?(?<marker>[A-Za-z_][A-Za-z0-9_]*)['\"]?\\s*$").marker) as $marker |
          {lines: (.lines + [$line]), heredoc: $marker}
        else
          # Regular line - keep as-is
          {lines: (.lines + [$line]), heredoc: .heredoc}
        end
      end
    ) | .lines;

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
    # Filter out null results, fix heredoc indentation, and strip empty lines
    map(select(. != null)) | fixHeredocIndent | stripEmptyLines | join("\n")
  end;

# ------------------------------------------------------------------------------
# Code Generation: Method
# ------------------------------------------------------------------------------

# $funcPrefix is the bash function prefix (e.g., "__MyApp__Counter" or "__Counter")
def generateMethod($funcPrefix; $ivars; $cvars):
  # Build function name
  (if .kind == "class" then
    "\($funcPrefix)__class__\(.selector)"
  elif .kind == "test" then
    "\($funcPrefix)__test__\(.selector)"
  else
    "\($funcPrefix)__\(.selector)"
  end) as $funcName |

  # Check for pragmas and emit markers (supports multiple pragmas)
  ((.pragmas // []) | map(
    if . == "direct" then "declare -g \($funcName)__direct=1"
    elif . == "procyonOnly" then "declare -g \($funcName)__procyonOnly=1"
    elif . == "bashOnly" then "declare -g \($funcName)__bashOnly=1"
    elif . == "procyonNative" then "declare -g \($funcName)__procyonNative=1"
    else null end
  ) | map(select(. != null))) as $pragmaMarkers |

  # Generate argument bindings for keyword methods
  (if (.args | length) > 0 then
    [.args | to_entries[] | "  local \(.value)=\"$\(.key + 1)\""] | join("\n")
  else
    ""
  end) as $argBindings |

  # Get method args for local variable tracking
  (.args // []) as $methodArgs |

  # For transformMethodBody we need the class name (without prefix)
  # Extract from funcPrefix: "__MyApp__Counter" -> "MyApp__Counter" or "__Counter" -> "Counter"
  ($funcPrefix | ltrimstr("__")) as $className |

  # Generate body - use expression parser for non-raw methods with Smalltalk syntax
  # Treat pragma: primitive same as raw method (bash passthrough)
  (.raw or ((.pragmas // []) | contains(["primitive"]))) as $isRaw |
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

  # For procyonOnly methods, replace body with error-throwing stub
  (if (.pragmas // []) | contains(["procyonOnly"]) then
    "  _throw \"NotImplemented\" \"Method \(.selector) requires native Procyon runtime\"\n  return 1"
  else $body end) as $finalBody |

  # Emit pragma markers if present (may be multiple)
  (if ($pragmaMarkers | length) > 0 then $pragmaMarkers[] else empty end),
  # Combine into function
  "\($funcName)() {",
  (if $argBindings != "" then $argBindings else empty end),
  $finalBody,
  "}",
  "";

# ------------------------------------------------------------------------------
# Code Generation: Method Alias
# ------------------------------------------------------------------------------

def generateAlias($funcPrefix):
  # Generate wrapper functions for both instance and class methods
  # Input: {type: "alias", aliasName: "size", originalMethod: "count"}
  # Instance method alias
  "\($funcPrefix)__\(.aliasName)() {",
  "  \($funcPrefix)__\(.originalMethod) \"$@\"",
  "}",
  "",
  # Class method alias
  "\($funcPrefix)__class__\(.aliasName)() {",
  "  \($funcPrefix)__class__\(.originalMethod) \"$@\"",
  "}",
  "";

# ------------------------------------------------------------------------------
# Code Generation: Method Advice (Before/After Hooks)
# ------------------------------------------------------------------------------

def generateAdvice($funcPrefix; $qualifiedName; $ivars; $cvars):
  # Generate a handler function and registration call
  # Input: {type: "advice", adviceType: "before"|"after", selector: "save", block: {...}}
  .adviceType as $adviceType |
  .selector as $selector |
  # Generate function name: __ClassName__before__selector or __ClassName__after__selector
  "\($funcPrefix)__\($adviceType)__\($selector)" as $funcName |
  # Compile the block body
  (if .block.tokens != null then
    ({ tokens: .block.tokens, pos: 0 } | expr_parse_stmts) as $parsed |
    (if $parsed.type == "statements" and $parsed.body != null then
      [($parsed.body // [])[] | select(.type == "locals") | (.names // [])[]] | unique
    else [] end) as $locals |
    $parsed | expr_gen_stmts($locals; $ivars; $cvars)
  else
    "  : # empty block"
  end) as $body |
  # Generate the handler function
  "\($funcName)() {",
  "  # Advice handler receives: class selector [args...]",
  "  local _adv_class=\"$1\" _adv_selector=\"$2\"",
  "  shift 2",
  $body,
  "}",
  "",
  # Generate the registration call (will be executed when class is sourced)
  # Use qualified name for the runtime so it can look up the correct class
  "_add_\($adviceType)_advice \"\($qualifiedName)\" \"\($selector)\" \"\($funcName)\"",
  "";

# ------------------------------------------------------------------------------
# Main Code Generator
# ------------------------------------------------------------------------------

def generate:
  . as $class |
  # Compute the function prefix and qualified name for namespaced classes
  funcPrefix as $funcPrefix |
  qualifiedName as $qname |
  # Extract instance variable names for expression parser (own + inherited)
  (([.instanceVars[]? | .name] // []) + (.inheritedInstanceVars // [])) as $ivars |
  # Extract class instance variable names for expression parser
  ([.classInstanceVars[]? | .name] // []) as $cvars |
  (
    generateHeader,
    generateMetadata,
    generateClassVarsInit,
    generateAccessors,
    generateRequires,
    (.methods[] | generateMethod($funcPrefix; $ivars; $cvars)),
    ((.aliases // [])[] | generateAlias($funcPrefix)),
    ((.advice // [])[] | generateAdvice($funcPrefix; $qname; $ivars; $cvars))
  );

# ==============================================================================
# Entry Point
# ==============================================================================

# Handle both plain Class input and CompilationUnit { "class": {...}, "traits": {...} }
if .class != null then
  # When using CompilationUnit, merge top-level metadata into the class before generating
  (.inheritedInstanceVars // []) as $inherited |
  (.sourceHash // "") as $hash |
  .class + {inheritedInstanceVars: $inherited, sourceHash: $hash} | generate
else
  generate
end
