# ==============================================================================
# Trashtalk Expression Code Generator
# ==============================================================================
#
# Generates bash code from expression AST nodes.
# Handles local variable tracking and instance variable inference.
#
# Context object:
#   { locals: ["var1", "var2"], ivars: ["value", "step"], class: "Counter" }
#
# ==============================================================================

# ==============================================================================
# Context Management
# ==============================================================================

# Initial context with no locals
def init_context($class; $ivars):
  { locals: [], ivars: $ivars, class: $class };

# Add locals to context
def add_locals($names):
  .locals += $names;

# Check if name is a local variable
def is_local($name):
  .locals | any(. == $name);

# Check if name is an instance variable
def is_ivar($name):
  .ivars | any(. == $name);

# ==============================================================================
# Expression Code Generation
# ==============================================================================

# Generate code for an expression
# Input: AST node
# ctx: context object with locals/ivars
def gen_expr(ctx):
  if . == null then
    ""

  elif .type == "number" then
    .value

  elif .type == "string" then
    "'\(.value)'"

  elif .type == "dstring" then
    .value

  elif .type == "boolean" then
    if .value then "true" else "false" end

  elif .type == "nil" then
    ""

  elif .type == "self" then
    "\"$_RECEIVER\""

  elif .type == "identifier" then
    # Key decision: is this a local or instance var?
    if ctx | is_local(.name) then
      "$\(.name)"
    elif ctx | is_ivar(.name) then
      "$(_ivar \(.name))"
    else
      # Unknown - assume it's a bash command/builtin or undeclared local
      .name
    end

  elif .type == "variable" then
    # Bash variable like $foo or ${foo}
    .value

  elif .type == "subshell" then
    # Pass through subshell as-is
    .value

  elif .type == "arithmetic" then
    # Pass through arithmetic as-is
    .value

  elif .type == "arith_cmd" then
    # Pass through arithmetic command as-is
    .value

  elif .type == "path" then
    .value

  elif .type == "unary" then
    if .op == "-" then
      # Unary minus in arithmetic context
      "$(( -\(.operand | gen_arithmetic(ctx)) ))"
    else
      "\(.op)\(.operand | gen_expr(ctx))"
    end

  elif .type == "binary" then
    # All arithmetic operations generate $(( ))
    "$(( \(.left | gen_arithmetic(ctx)) \(.op) \(.right | gen_arithmetic(ctx)) ))"

  elif .type == "message_send" then
    gen_message_send(ctx)

  elif .type == "assignment" then
    "\(.target)=\"\(.value | gen_expr(ctx))\""

  elif .type == "return" then
    if .value == null then
      "return"
    else
      "echo \"\(.value | gen_expr(ctx))\"; return"
    end

  elif .type == "bash_token" then
    # Pass through bash tokens
    .token.value

  elif .type == "keyword" then
    .value

  elif .type == "block" then
    # Block expression - generate as inline code
    gen_block(ctx)

  elif .type == "control_flow" then
    # Control flow construct
    gen_control_flow(ctx)

  else
    "# ERROR: unknown expr type \(.type)"
  end;

# Generate code for a block (inline)
def gen_block(ctx):
  # Add block params to context
  (.params // []) as $params |
  (ctx | add_locals($params)) as $block_ctx |
  # Generate body statements
  [.body[] | gen_statement($block_ctx).code] | join("; ");

# Generate condition for control flow
# For comparisons, generate arithmetic condition
# For blocks, generate command substitution to evaluate
def gen_condition(ctx):
  if .type == "binary" then
    # Comparison - use arithmetic evaluation
    "\(.left | gen_arithmetic(ctx)) \(.op) \(.right | gen_arithmetic(ctx))"
  elif .type == "block" then
    # Block condition - evaluate as command and check result
    # The block should echo/return something truthy
    . | gen_block(ctx)
  elif .type == "identifier" then
    if ctx | is_local(.name) then
      "$\(.name)"
    elif ctx | is_ivar(.name) then
      "$(_ivar \(.name))"
    else
      .name
    end
  elif .type == "variable" then
    .value
  elif .type == "boolean" then
    if .value then "1" else "0" end
  else
    gen_expr(ctx)
  end;

# Generate code for control flow constructs
def gen_control_flow(ctx):
  if .kind == "if_true" then
    "if (( \(.condition | gen_condition(ctx)) )); then \(.block | gen_block(ctx)); fi"
  elif .kind == "if_false" then
    "if (( !(\(.condition | gen_condition(ctx))) )); then \(.block | gen_block(ctx)); fi"
  elif .kind == "if_else" then
    "if (( \(.condition | gen_condition(ctx)) )); then \(.true_block | gen_block(ctx)); else \(.false_block | gen_block(ctx)); fi"
  elif .kind == "times_repeat" then
    "for ((_i=0; _i<\(.count | gen_arithmetic(ctx)); _i++)); do \(.block | gen_block(ctx)); done"
  elif .kind == "while_true" then
    # If condition is a block, evaluate it each iteration
    if .condition.type == "block" then
      "while \(.condition | gen_block(ctx)); do \(.block | gen_block(ctx)); done"
    else
      "while (( \(.condition | gen_condition(ctx)) )); do \(.block | gen_block(ctx)); done"
    end
  elif .kind == "while_false" then
    if .condition.type == "block" then
      "while ! \(.condition | gen_block(ctx)); do \(.block | gen_block(ctx)); done"
    else
      "while (( !(\(.condition | gen_condition(ctx))) )); do \(.block | gen_block(ctx)); done"
    end
  else
    "# ERROR: unknown control flow kind \(.kind)"
  end;

# Generate code for arithmetic context (no $(( )) wrapper)
def gen_arithmetic(ctx):
  if . == null then
    "0"

  elif .type == "number" then
    .value

  elif .type == "identifier" then
    if ctx | is_local(.name) then
      "$\(.name)"
    elif ctx | is_ivar(.name) then
      "$(_ivar \(.name))"
    else
      "$\(.name)"
    end

  elif .type == "variable" then
    .value

  elif .type == "subshell" then
    .value

  elif .type == "arithmetic" then
    # Strip $(( )) wrapper if present
    .value | gsub("^\\$\\(\\(|\\)\\)$"; "")

  elif .type == "binary" then
    "(\(.left | gen_arithmetic(ctx)) \(.op) \(.right | gen_arithmetic(ctx)))"

  elif .type == "unary" and .op == "-" then
    "(-\(.operand | gen_arithmetic(ctx)))"

  elif .type == "self" then
    "\"$_RECEIVER\""

  else
    gen_expr(ctx)
  end;

# Generate code for message send
def gen_message_send(ctx):
  .receiver as $recv |
  .selector as $sel |
  .args as $args |
  .keywords as $keywords |

  # Generate receiver
  ($recv | gen_expr(ctx)) as $recv_code |

  # Build message send
  if ($keywords | length) > 0 then
    # Keyword message - interleave keywords with args
    # e.g., keywords=["at","put"], args=[1,"value"] -> "at: 1 put: value"
    ([range($keywords | length)] | map("\($keywords[.]): \($args[.] | gen_expr(ctx))") | join(" ")) as $kw_args |
    "@ \($recv_code) \($kw_args)"
  elif ($args | length) > 0 then
    # Has args but no keywords (shouldn't happen normally)
    ([$args[] | gen_expr(ctx)] | join(" ")) as $args_code |
    "@ \($recv_code) \($sel) \($args_code)"
  else
    # Unary message (no args, no keywords)
    "@ \($recv_code) \($sel)"
  end;

# ==============================================================================
# Statement Code Generation
# ==============================================================================

# Generate code for a statement, returning { code, ctx }
def gen_statement(ctx):
  if .type == "locals" then
    # Local declaration - update context and emit
    {
      code: "local \(.names | join(" "))",
      ctx: (ctx | add_locals(.names))
    }

  elif .type == "assignment" then
    {
      code: "\(.target)=\"\(.value | gen_expr(ctx))\"",
      ctx: ctx
    }

  elif .type == "return" then
    {
      code: (if .value == null then "return" else "echo \"\(.value | gen_expr(ctx))\"; return" end),
      ctx: ctx
    }

  elif .type == "message_send" then
    {
      code: gen_message_send(ctx),
      ctx: ctx
    }

  elif .type == "binary" then
    # Standalone binary expression (probably for side effects)
    {
      code: gen_expr(ctx),
      ctx: ctx
    }

  elif .type == "control_flow" then
    # Control flow statement
    {
      code: gen_control_flow(ctx),
      ctx: ctx
    }

  else
    # Other expressions
    {
      code: gen_expr(ctx),
      ctx: ctx
    }
  end;

# Generate code for all statements
# Input: statements AST node { type: "statements", body: [...] }
# Returns string of bash code
def gen_statements(ctx):
  reduce .body[] as $stmt (
    { lines: [], ctx: ctx };
    ($stmt | gen_statement(.ctx)) as $gen |
    if $gen.code != "" then
      .lines += [$gen.code]
    else .
    end |
    .ctx = $gen.ctx
  ) |
  .lines | map("  " + .) | join("\n");

# ==============================================================================
# Method Body Generation
# ==============================================================================

# Collect all local names from a statements AST
def collect_locals:
  if .type == "statements" then
    [.body[] | select(.type == "locals") | .names[]] | unique
  else
    []
  end;

# Generate method body from parsed AST
# Input: statements AST
# $class: class name
# $ivars: array of instance variable names
# $args: array of method argument names
def gen_method_body($class; $ivars; $args):
  . as $body |
  # Initialize context with method args as locals
  init_context($class; $ivars) | add_locals($args) | . as $ctx |
  # Add any declared locals from the body
  ($body | collect_locals) as $declared_locals |
  ($ctx | add_locals($declared_locals)) as $full_ctx |
  # Generate code
  $body | gen_statements($full_ctx);

# ==============================================================================
# Top-level Entry Point
# ==============================================================================

# Check if we should use expression parsing for this body
# Returns true if the body contains Smalltalk-like expressions
def should_parse_expressions:
  # Look for assignment operators, arithmetic operators, or caret (return)
  any(.[]; .type == "ASSIGN" or .type == "CARET" or
           .type == "PLUS" or .type == "MINUS" or
           .type == "STAR" or .type == "PERCENT");

# Combined parse and generate for a method body
# Input: array of tokens
# $class: class name
# $ivars: array of instance variable names
# $args: array of method argument names
def parse_and_generate($class; $ivars; $args):
  . as $tokens |
  # Import the expression parser (will be included via driver)
  # For now, we import its parse_method_body function
  import "expr-parser" as ep |
  ($tokens | ep::parse_method_body) as $ast |
  $ast | gen_method_body($class; $ivars; $args);
