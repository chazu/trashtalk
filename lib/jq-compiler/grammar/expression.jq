# ==============================================================================
# Expression Grammar for Trashtalk Parser
# ==============================================================================
#
# Parses method body expressions:
#
#   | var1 var2 |          Local variable declarations
#   ^ expression           Return statement
#   var := expression      Assignment
#   @ receiver message     Message send (unary)
#   @ receiver key: arg    Message send (keyword)
#   $(...) or $(...)       Subshell capture
#   "string" or 'string'   String literals
#   123                    Number literals
#   identifier             Variable references
#
# For "raw" methods, we skip DSL parsing and preserve the body as-is.
#
# Grammar (EBNF):
#   statement     = localVars | returnStmt | assignment | messageSend | bashCode
#   localVars     = "|" identifier+ "|"
#   returnStmt    = "^" expression
#   assignment    = identifier ":=" expression
#   messageSend   = "@" receiver (unaryMsg | keywordMsg)
#   receiver      = "self" | identifier | "$" identifier
#   unaryMsg      = identifier
#   keywordMsg    = (keyword expression)+
#
# ==============================================================================

# ------------------------------------------------------------------------------
# Local Variable Declarations
# ------------------------------------------------------------------------------

# Parse: | var1 var2 var3 |
def localVarsDecl:
  literal("|") as $open |
  if $open.result != null then
    $open | ws |
    many1(token("IDENTIFIER") | map(.value) | ws) as $vars |
    if $vars.result != null then
      $vars | ws |
      literal("|") as $close |
      if $close.result != null then
        $close | succeed({
          type: "localVars",
          names: $vars.result
        })
      else
        $close
      end
    else
      $vars
    end
  else
    $open
  end;

# ------------------------------------------------------------------------------
# Expressions
# ------------------------------------------------------------------------------

# Parse identifier expression
def identifierExpr:
  token("IDENTIFIER") |
  map({type: "identifier", name: .value});

# Parse string expression
def stringExpr:
  token("STRING") |
  map({type: "string", value: (.value | ltrimstr("'") | rtrimstr("'"))});

# Parse triple-quoted string expression (multi-line)
def tripleStringExpr:
  token("TRIPLESTRING") |
  map({type: "triplestring", value: .value});

# Parse number expression
def numberExpr:
  token("NUMBER") |
  map({type: "number", value: .value});

# Parse simple expression (not message send)
def simpleExpr:
  choice([
    tripleStringExpr,
    stringExpr,
    numberExpr,
    identifierExpr
  ]);

# ------------------------------------------------------------------------------
# Message Send
# ------------------------------------------------------------------------------

# Parse receiver of message send
# Can be: self, identifier, or $identifier
def receiver:
  ws |
  choice2(
    (literal("self") | succeed({type: "identifier", name: "self"}));
    (token("IDENTIFIER") | map({type: "identifier", name: .value}))
  );

# Parse unary message (no arguments)
# Example: @ self getValue -> selector "getValue"
def unaryMsg:
  token("IDENTIFIER") |
  map({selector: .value, args: []});

# Parse keyword message argument - consume tokens until next keyword or end
def keywordArg:
  # For now, just take the next identifier/string/number as the argument
  # This is simplified - real implementation would handle complex expressions
  choice([
    stringExpr,
    numberExpr,
    identifierExpr
  ]);

# Parse a single keyword:arg in a message
def keywordMsgPart:
  token("KEYWORD") as $kw |
  if $kw.result != null then
    $kw | ws |
    keywordArg as $arg |
    if $arg.result != null then
      $arg | succeed({
        keyword: ($kw.result.value | rtrimstr(":")),
        arg: $arg.result
      })
    else
      $arg
    end
  else
    $kw
  end;

# Parse keyword message (one or more keyword:arg pairs)
def keywordMsg:
  many1(keywordMsgPart | map(.) | ws) |
  map(
    reduce .[] as $part ({selector: "", args: []};
      .selector = (if .selector == "" then $part.keyword else "\(.selector)_\($part.keyword)" end) |
      .args += [$part.arg]
    )
  );

# Parse message (unary or keyword)
def message:
  choice2(keywordMsg; unaryMsg);

# Parse: @ receiver message
def messageSend:
  literal("@") as $at |
  if $at.result != null then
    $at | ws |
    receiver as $rcvr |
    if $rcvr.result != null then
      $rcvr | ws |
      message as $msg |
      if $msg.result != null then
        $msg | succeed({
          type: "messageSend",
          receiver: $rcvr.result,
          selector: $msg.result.selector,
          args: $msg.result.args
        })
      else
        $msg
      end
    else
      $rcvr
    end
  else
    $at
  end;

# ------------------------------------------------------------------------------
# Return Statement
# ------------------------------------------------------------------------------

# Parse: ^ expression
def returnStmt:
  literal("^") as $caret |
  if $caret.result != null then
    $caret | ws |
    # Return can be followed by message send or simple expression
    choice2(messageSend; simpleExpr) as $expr |
    if $expr.result != null then
      $expr | succeed({
        type: "return",
        expression: $expr.result
      })
    else
      $expr
    end
  else
    $caret
  end;

# ------------------------------------------------------------------------------
# Assignment
# ------------------------------------------------------------------------------

# Parse: var := expression
def assignment:
  token("IDENTIFIER") as $var |
  if $var.result != null then
    $var | ws |
    literal(":=") as $assign |
    if $assign.result != null then
      $assign | ws |
      # Assignment value can be message send or simple expression
      choice2(messageSend; simpleExpr) as $expr |
      if $expr.result != null then
        $expr | succeed({
          type: "assignment",
          variable: $var.result.value,
          expression: $expr.result
        })
      else
        $expr
      end
    else
      # Not an assignment, reset
      $var | .result = null
    end
  else
    $var
  end;

# ------------------------------------------------------------------------------
# Bash Passthrough
# ------------------------------------------------------------------------------

# For tokens that don't match DSL patterns, preserve as bash code
# This captures the raw token value
def bashPassthrough:
  current as $tok |
  if $tok != null and $tok.type != "NEWLINE" then
    advance | succeed({
      type: "bash",
      code: $tok.value,
      token: $tok
    })
  else
    fail("no bash code")
  end;

# ------------------------------------------------------------------------------
# Statement Parsing
# ------------------------------------------------------------------------------

# Parse a single statement in a method body
def statement:
  ws |
  choice([
    localVarsDecl,
    returnStmt,
    assignment,
    messageSend,
    bashPassthrough
  ]);

# Parse multiple statements
def statements:
  many(statement | map(.) | ws);

# ------------------------------------------------------------------------------
# Method Body Parsing
# ------------------------------------------------------------------------------

# Parse a block of statements from tokens captured in methodBody
# This is called after method parsing to process the body tokens
def parseBlock($tokens):
  {tokens: $tokens, pos: 0, result: null, errors: []} |
  statements |
  .result as $stmts |
  {type: "block", statements: ($stmts // [])};

# Transform raw method body tokens into either:
# - Parsed statements (for normal methods)
# - Preserved raw code (for raw methods)
def processMethodBody($isRaw):
  if $isRaw then
    # Raw method - convert tokens back to approximate source text
    {
      type: "rawBlock",
      tokens: .tokens,
      # Reconstruct approximate source from tokens
      source: ([.tokens[] | .value] | join(" "))
    }
  else
    # Normal method - parse the body tokens
    parseBlock(.tokens)
  end;

# ==============================================================================
# End of expression.jq
# ==============================================================================
