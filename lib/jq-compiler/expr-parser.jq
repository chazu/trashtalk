# ==============================================================================
# Trashtalk Expression Parser
# ==============================================================================
#
# Pratt parser for expressions within method bodies.
# Transforms Smalltalk-like syntax into AST nodes.
#
# Input: Array of tokens from a method body
# Output: AST with expression nodes
#
# Handles:
#   | var1 var2 |     →  { type: "locals", names: [...] }
#   ^ expression      →  { type: "return", value: ... }
#   var := expr       →  { type: "assignment", target: "var", value: ... }
#   x + y * z         →  { type: "binary", op: "+", left: ..., right: ... }
#   @ self method     →  { type: "message_send", receiver: ..., selector: ..., args: [...] }
#
# ==============================================================================

# ==============================================================================
# Binding Power Tables
# ==============================================================================

# Infix binding powers: [left_bp, right_bp]
# Higher = tighter binding
# Left-associative: right_bp = left_bp + 1
# Right-associative: right_bp = left_bp
def infix_bp:
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

# Prefix binding power (for unary operators)
def prefix_bp:
  {
    "-": 50,            # unary minus (high precedence)
    "^": 1              # return (lowest, captures full expr)
  };

# ==============================================================================
# State Helpers
# ==============================================================================

def expr_peek:
  if .pos >= (.tokens | length) then null
  else .tokens[.pos]
  end;

def expr_peek_type:
  expr_peek.type // null;

def expr_peek_value:
  expr_peek.value // null;

def expr_advance:
  .pos += 1;

def expr_at_end:
  .pos >= (.tokens | length);

def expr_skip_newlines:
  if expr_peek_type == "NEWLINE" or expr_peek_type == "COMMENT" then
    expr_advance | expr_skip_newlines
  else .
  end;

# ==============================================================================
# Token Type Checks
# ==============================================================================

def is_operator_token:
  expr_peek_type == "PLUS" or
  expr_peek_type == "MINUS" or
  expr_peek_type == "STAR" or
  expr_peek_type == "PERCENT" or
  expr_peek_type == "SLASH" or
  expr_peek_type == "ASSIGN" or
  expr_peek_type == "GT" or
  expr_peek_type == "LT" or
  expr_peek_type == "GE" or
  expr_peek_type == "LE" or
  expr_peek_type == "EQ" or
  expr_peek_type == "NE";

def get_operator_value:
  if expr_peek_type == "PLUS" then "+"
  elif expr_peek_type == "MINUS" then "-"
  elif expr_peek_type == "STAR" then "*"
  elif expr_peek_type == "PERCENT" then "%"
  elif expr_peek_type == "SLASH" then "/"
  elif expr_peek_type == "ASSIGN" then ":="
  elif expr_peek_type == "GT" then ">"
  elif expr_peek_type == "LT" then "<"
  elif expr_peek_type == "GE" then ">="
  elif expr_peek_type == "LE" then "<="
  elif expr_peek_type == "EQ" then "=="
  elif expr_peek_type == "NE" then "!="
  else null
  end;

# ==============================================================================
# Prefix Parsers (atoms and prefix operators)
# ==============================================================================

# Forward declarations for mutual recursion
def parse_expr(min_bp): parse_expr_impl(min_bp);
def parse_message_send: parse_message_send_impl;
def parse_block: parse_block_impl;

def parse_prefix:
  expr_skip_newlines |
  expr_peek as $tok |
  if $tok == null then
    { state: ., result: null, error: "unexpected end of input" }

  # Integer/number literal
  elif $tok.type == "NUMBER" then
    { state: (. | expr_advance), result: { type: "number", value: $tok.value } }

  # String literal (single-quoted)
  elif $tok.type == "STRING" then
    { state: (. | expr_advance), result: { type: "string", value: ($tok.value | ltrimstr("'") | rtrimstr("'")) } }

  # Double-quoted string
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }

  # Identifier (variable reference)
  elif $tok.type == "IDENTIFIER" then
    if $tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    elif $tok.value == "true" then
      { state: (. | expr_advance), result: { type: "boolean", value: true } }
    elif $tok.value == "false" then
      { state: (. | expr_advance), result: { type: "boolean", value: false } }
    elif $tok.value == "nil" then
      { state: (. | expr_advance), result: { type: "nil" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $tok.value } }
    end

  # Variable ($var)
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }

  # Subshell $(...) - preserved verbatim
  elif $tok.type == "SUBSHELL" then
    { state: (. | expr_advance), result: { type: "subshell", value: $tok.value } }

  # Arithmetic $((...))
  elif $tok.type == "ARITHMETIC" then
    { state: (. | expr_advance), result: { type: "arithmetic", value: $tok.value } }

  # Arithmetic command ((...))
  elif $tok.type == "ARITH_CMD" then
    { state: (. | expr_advance), result: { type: "arith_cmd", value: $tok.value } }

  # Parenthesized expression
  elif $tok.type == "LPAREN" then
    (. | expr_advance) |
    parse_expr(0) as $inner |
    $inner.state |
    if expr_peek_type == "RPAREN" then
      { state: (. | expr_advance), result: $inner.result }
    else
      { state: ., result: $inner.result, error: "expected )" }
    end

  # Block expression [ ... ]
  elif $tok.type == "LBRACKET" then
    parse_block

  # Unary minus
  elif $tok.type == "MINUS" then
    (. | expr_advance) |
    parse_expr(prefix_bp["-"]) as $operand |
    {
      state: $operand.state,
      result: { type: "unary", op: "-", operand: $operand.result }
    }

  # Return (^)
  elif $tok.type == "CARET" then
    (. | expr_advance) |
    expr_skip_newlines |
    # Check if there's an expression to return or if it's bare ^
    if expr_at_end or expr_peek_type == "NEWLINE" or expr_peek_type == "DOT" or expr_peek_type == "RBRACKET" then
      { state: ., result: { type: "return", value: null } }
    else
      parse_expr(1) as $value |
      { state: $value.state, result: { type: "return", value: $value.result } }
    end

  # Message send with @
  elif $tok.type == "AT" then
    parse_message_send

  # Path (for file paths in bash)
  elif $tok.type == "PATH" then
    { state: (. | expr_advance), result: { type: "path", value: $tok.value } }

  # Pass-through tokens for bash constructs
  elif $tok.type == "DLBRACKET" or $tok.type == "DRBRACKET" or
       $tok.type == "HEREDOC" or $tok.type == "REDIRECT" or
       $tok.type == "SEMI" or $tok.type == "AND" or $tok.type == "OR" or
       $tok.type == "PIPE" or $tok.type == "GT" or $tok.type == "LT" or
       $tok.type == "AMP" or $tok.type == "BANG" or
       $tok.type == "EQ" or $tok.type == "NE" or $tok.type == "MATCH" or
       $tok.type == "EQUALS" then
    { state: (. | expr_advance), result: { type: "bash_token", token: $tok } }

  # Keyword (like "for:", "if:", etc. in method calls) - shouldn't appear as prefix
  elif $tok.type == "KEYWORD" then
    { state: (. | expr_advance), result: { type: "keyword", value: $tok.value } }

  else
    { state: ., result: null, error: "unexpected token: \($tok.type) '\($tok.value)'" }
  end;

# ==============================================================================
# Infix Loop (for binary operators)
# ==============================================================================

# Control flow keyword names
def is_control_flow_keyword:
  . as $kw |
  ($kw == "ifTrue:" or $kw == "ifFalse:" or $kw == "ifTrue:ifFalse:" or
   $kw == "whileTrue:" or $kw == "whileFalse:" or $kw == "timesRepeat:");

def parse_infix_loop(min_bp):
  expr_skip_newlines |
  # Check if next token is a control flow keyword (higher priority check)
  if expr_peek_type == "KEYWORD" and (expr_peek.value | is_control_flow_keyword) then
    .result as $receiver |
    expr_peek.value as $keyword |
    (. | expr_advance) |  # consume keyword
    expr_skip_newlines |
    # Parse block argument
    if expr_peek_type == "LBRACKET" then
      parse_block as $block |
      if $keyword == "ifTrue:" and ($block.state | expr_peek_type) == "KEYWORD" and
         ($block.state | expr_peek.value) == "ifFalse:" then
        # ifTrue:ifFalse: case
        $block.state | expr_advance | expr_skip_newlines |
        if expr_peek_type == "LBRACKET" then
          parse_block as $else_block |
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
          { state: ., result: null, error: "expected block after ifFalse:" }
        end
      elif $keyword == "ifTrue:" then
        {
          state: $block.state,
          result: {
            type: "control_flow",
            kind: "if_true",
            condition: $receiver,
            block: $block.result
          }
        }
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
        { state: $block.state, result: null, error: "unknown control flow: \($keyword)" }
      end
      # Continue looking for more operators
      | parse_infix_loop(min_bp)
    else
      { state: ., result: null, error: "expected block after \($keyword)" }
    end
  # Check if next token is an infix operator
  elif is_operator_token then
    get_operator_value as $op |
    (infix_bp[$op] // null) as $bp |
    if $bp == null or $bp[0] < min_bp then
      # Not an operator or binding power too low - stop
      .
    else
      # Consume operator
      .state |= expr_advance |
      .result as $lhs |
      # Parse right-hand side with right binding power
      (.state | parse_expr($bp[1])) as $rhs |
      if $op == ":=" then
        # Assignment: left must be identifier
        if $lhs.type == "identifier" then
          {
            state: $rhs.state,
            result: { type: "assignment", target: $lhs.name, value: $rhs.result }
          }
        else
          { state: $rhs.state, result: null, error: "assignment target must be identifier" }
        end
      else
        # Binary operation
        {
          state: $rhs.state,
          result: { type: "binary", op: $op, left: $lhs, right: $rhs.result }
        }
      end
      # Continue looking for more infix operators
      | parse_infix_loop(min_bp)
    end
  else
    .
  end;

# Main expression parser implementation
def parse_expr_impl(min_bp):
  parse_prefix as $prefix |
  if $prefix.result == null then $prefix
  else $prefix | parse_infix_loop(min_bp)
  end;

# Convenience: parse with minimum binding power 0
def parse_expr:
  parse_expr(0);

# ==============================================================================
# Message Send Parser
# ==============================================================================

# Parse receiver (self, identifier, variable, or subshell)
def parse_receiver:
  expr_skip_newlines |
  expr_peek as $tok |
  if $tok.type == "IDENTIFIER" then
    if $tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $tok.value } }
    end
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }
  elif $tok.type == "SUBSHELL" then
    { state: (. | expr_advance), result: { type: "subshell", value: $tok.value } }
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }
  else
    { state: ., result: null, error: "expected receiver after @" }
  end;

# Parse a simple argument (identifier, literal, variable, or subshell)
def parse_simple_arg:
  expr_skip_newlines |
  expr_peek as $tok |
  if $tok == null then
    { state: ., result: null }
  elif $tok.type == "IDENTIFIER" then
    if $tok.value == "self" then
      { state: (. | expr_advance), result: { type: "self" } }
    else
      { state: (. | expr_advance), result: { type: "identifier", name: $tok.value } }
    end
  elif $tok.type == "NUMBER" then
    { state: (. | expr_advance), result: { type: "number", value: $tok.value } }
  elif $tok.type == "STRING" then
    { state: (. | expr_advance), result: { type: "string", value: ($tok.value | ltrimstr("'") | rtrimstr("'")) } }
  elif $tok.type == "DSTRING" then
    { state: (. | expr_advance), result: { type: "dstring", value: $tok.value } }
  elif $tok.type == "VARIABLE" then
    { state: (. | expr_advance), result: { type: "variable", value: $tok.value } }
  elif $tok.type == "SUBSHELL" then
    { state: (. | expr_advance), result: { type: "subshell", value: $tok.value } }
  elif $tok.type == "ARITHMETIC" then
    { state: (. | expr_advance), result: { type: "arithmetic", value: $tok.value } }
  elif $tok.type == "PATH" then
    { state: (. | expr_advance), result: { type: "path", value: $tok.value } }
  else
    { state: ., result: null }
  end;

# Parse selector and arguments
# Handles both unary (no args) and keyword (one or more keyword:arg pairs) methods
def parse_selector_and_args:
  expr_skip_newlines |
  { state: ., selector: "", args: [], keywords: [] } |
  # First check for keyword or unary selector
  if (.state | expr_peek_type) == "KEYWORD" then
    # Keyword method: collect keyword:arg pairs
    until((.state | expr_peek_type) != "KEYWORD";
      (.state | expr_peek.value | rtrimstr(":")) as $kw |
      .keywords += [$kw] |
      .selector = (if .selector == "" then $kw else "\(.selector)_\($kw)" end) |
      .state |= expr_advance |
      .state |= expr_skip_newlines |
      # Parse argument after keyword
      (.state | parse_simple_arg) as $arg |
      if $arg.result != null then
        .args += [$arg.result] |
        .state = $arg.state
      else
        .
      end |
      .state |= expr_skip_newlines
    )
  elif (.state | expr_peek_type) == "IDENTIFIER" then
    # Unary method (no colon, no args)
    .selector = (.state | expr_peek.value) |
    .state |= expr_advance
  else
    # No selector found
    .
  end |
  { state: .state, selector: .selector, args: .args, keywords: .keywords };

def parse_message_send_impl:
  # Consume @
  (. | expr_advance) |
  # Parse receiver
  parse_receiver as $recv |
  if $recv.result == null then
    { state: $recv.state, result: null, error: "expected receiver after @" }
  else
    $recv.state |
    # Parse selector and args
    parse_selector_and_args as $sel |
    {
      state: $sel.state,
      result: {
        type: "message_send",
        receiver: $recv.result,
        selector: $sel.selector,
        args: $sel.args,
        keywords: $sel.keywords
      }
    }
  end;

# ==============================================================================
# Block Parser
# ==============================================================================

# Parse a block: [ statements ]
# Blocks can optionally have parameters: [ :arg1 :arg2 | statements ]
def parse_block_impl:
  # Consume opening [
  (. | expr_advance) |
  expr_skip_newlines |
  # Check for block parameters (colon-prefixed identifiers followed by |)
  { state: ., params: [] } |
  # Look for parameter pattern: :param1 :param2 ... |
  until((.state | expr_peek_type) != "KEYWORD" or
        ((.state | expr_peek.value) | startswith(":") | not);
    (.state | expr_peek.value | ltrimstr(":") | rtrimstr(":")) as $param |
    .params += [$param] |
    .state |= expr_advance |
    .state |= expr_skip_newlines
  ) |
  # If we found params, expect a pipe separator
  if (.params | length) > 0 and (.state | expr_peek_type) == "PIPE" then
    .state |= expr_advance
  else .
  end |
  .state |= expr_skip_newlines |
  # Parse body statements until closing ]
  .state as $s | .params as $params |
  ($s | { tokens: .tokens, pos: .pos } |
    { state: ., statements: [] } |
    until((.state | expr_at_end) or (.state | expr_peek_type) == "RBRACKET";
      (.state | parse_statement) as $stmt |
      if $stmt.result != null then
        .statements += [$stmt.result] |
        .state = $stmt.state
      else
        .state = $stmt.state
      end |
      .state |= skip_stmt_terminator
    )
  ) as $body |
  $body.state |
  # Consume closing ]
  if expr_peek_type == "RBRACKET" then
    {
      state: (. | expr_advance),
      result: {
        type: "block",
        params: $params,
        body: $body.statements
      }
    }
  else
    { state: ., result: null, error: "expected ] to close block" }
  end;

# ==============================================================================
# Statement Parsers
# ==============================================================================

# Skip statement terminator (. or newline)
def skip_stmt_terminator:
  if expr_peek_type == "DOT" or expr_peek_type == "NEWLINE" then
    expr_advance | skip_stmt_terminator
  else .
  end;

# Parse local variable declaration: | var1 var2 |
def parse_local_declaration:
  if expr_peek_type != "PIPE" then
    { state: ., result: null }
  else
    (. | expr_advance) |  # consume opening |
    { state: ., names: [] } |
    until((.state | expr_peek_type) == "PIPE" or (.state | expr_at_end);
      (.state | expr_peek) as $t |
      if $t.type == "IDENTIFIER" then
        .names += [$t.value] |
        .state |= expr_advance
      else
        .state |= expr_advance  # skip unexpected
      end |
      .state |= expr_skip_newlines
    ) |
    .state |= expr_advance |  # consume closing |
    { state: .state, result: { type: "locals", names: .names } }
  end;

# Parse a single statement
def parse_statement:
  skip_stmt_terminator |
  if expr_at_end or expr_peek_type == "RBRACKET" then
    { state: ., result: null }
  elif expr_peek_type == "PIPE" then
    parse_local_declaration
  else
    parse_expr
  end;

# Parse all statements in a method body
def parse_statements:
  { state: ., statements: [] } |
  until((.state | expr_at_end) or (.state | expr_peek_type) == "RBRACKET";
    (.state | parse_statement) as $stmt |
    if $stmt.result != null then
      .statements += [$stmt.result] |
      .state = $stmt.state
    else
      .state = $stmt.state
    end |
    .state |= skip_stmt_terminator
  ) |
  { state: .state, result: { type: "statements", body: .statements } };

# ==============================================================================
# Entry Point
# ==============================================================================

# Parse a method body (array of tokens) into an AST
def parse_method_body:
  { tokens: ., pos: 0 } |
  parse_statements |
  .result;

# Check if an AST contains only simple bash pass-through tokens
# (Used to decide whether to use the new expression codegen or fall back to old behavior)
def is_simple_bash_body:
  if .type == "statements" then
    all(.body[]; .type == "bash_token" or .type == "keyword" or .type == "identifier" or
                  .type == "variable" or .type == "subshell" or .type == "path" or
                  .type == "dstring" or .type == "string" or .type == "number")
  else
    false
  end;
