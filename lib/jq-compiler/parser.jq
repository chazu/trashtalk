# ==============================================================================
# Trashtalk Parser - Main Entry Point
# ==============================================================================
#
# This is the main parser module that combines all grammar components.
# It takes a JSON token array (from tokenizer.bash) and produces a JSON AST.
#
# Usage:
#   cat tokens.json | jq -f parser.jq
#
# Input: JSON array of tokens from tokenizer
#   [{"type": "IDENTIFIER", "value": "Counter", "line": 1, "col": 0}, ...]
#
# Output: JSON AST
#   {
#     "type": "class",
#     "name": "Counter",
#     "parent": "Object",
#     "isTrait": false,
#     "instanceVars": [...],
#     "traits": [...],
#     "requires": [...],
#     "methods": [...]
#   }
#
# ==============================================================================

# ==============================================================================
# PEG Combinators - Iterative Implementation
# ==============================================================================

# Get current token
def current:
  if .pos < (.tokens | length) then .tokens[.pos] else null end;

# Check if at end
def atEnd:
  .pos >= (.tokens | length);

# Advance position
def advance:
  .pos += 1;

# Set result
def succeed($v):
  .result = $v;

# Mark failure
def fail:
  .result = null;

# Transform result if not null
def map(f):
  if .result != null then .result |= f else . end;

# Skip NEWLINE and COMMENT tokens
def skipNewlines:
  if current.type == "NEWLINE" or current.type == "COMMENT" then advance | skipNewlines else . end;

# Match token type
def token($t):
  if current.type == $t then
    .result = current | advance
  else
    .result = null
  end;

# Match literal value
def literal($v):
  if current.value == $v then
    .result = current | advance
  else
    .result = null
  end;

# ==============================================================================
# Grammar Rules - Direct Implementation
# ==============================================================================

# Parse class header: ClassName subclass: Parent | ClassName trait
def parseClassHeader:
  skipNewlines |
  if current.type == "IDENTIFIER" then
    .result = current.value | advance | skipNewlines |
    .result as $name |
    if current.value == "subclass:" then
      advance | skipNewlines |
      if current.type == "IDENTIFIER" then
        .result = {type: "class", name: $name, parent: current.value, isTrait: false} |
        advance
      else
        fail
      end
    elif current.value == "trait" then
      advance |
      .result = {type: "class", name: $name, parent: null, isTrait: true}
    else
      fail
    end
  else
    fail
  end;

# Parse instance variable: name or name:default
def parseVarSpec:
  if current.type == "KEYWORD" then
    # name:default pattern
    current.value as $kw |
    advance | skipNewlines |
    if current.type == "NUMBER" then
      .result = {name: ($kw | rtrimstr(":")), default: {type: "number", value: current.value}} |
      advance
    elif current.type == "STRING" then
      .result = {name: ($kw | rtrimstr(":")), default: {type: "string", value: (current.value | ltrimstr("'") | rtrimstr("'"))}} |
      advance
    else
      # Keyword without value - treat as error or plain var
      fail
    end
  elif current.type == "IDENTIFIER" then
    .result = {name: current.value, default: null} | advance
  else
    fail
  end;

# Parse: instanceVars: var1 var2:0
def parseInstanceVars:
  if current.value == "instanceVars:" then
    advance | skipNewlines |
    # Collect variables until we hit something that's not a var
    .result = [] |
    until(
      .result == null or
      (current.type != "KEYWORD" and current.type != "IDENTIFIER") or
      current.value == "method:" or current.value == "classMethod:" or
      current.value == "rawMethod:" or current.value == "rawClassMethod:" or
      current.value == "include:" or current.value == "requires:" or
      current.value == "instanceVars:";

      parseVarSpec |
      if .result != null then
        . as $state |
        $state.result as $var |
        $state | skipNewlines | .result = ($state | .result = .result) |
        # Store accumulated vars
        (.result // []) + [$var] | . as $vars |
        $state | skipNewlines | .result = $vars
      else
        .
      end
    ) |
    if .result != null and (.result | length) > 0 then
      .result = {type: "instanceVars", vars: .result}
    else
      fail
    end
  else
    fail
  end;

# Simpler instanceVars parser - just collect keywords and identifiers
def parseInstanceVarsSimple:
  if current.value == "instanceVars:" then
    advance | skipNewlines |
    # Collect all var specs manually
    {vars: [], state: .} |
    until(
      .state.pos >= (.state.tokens | length) or
      (.state | current.type) == "NEWLINE" or
      (.state | current.value) == "method:" or
      (.state | current.value) == "classMethod:" or
      (.state | current.value) == "rawMethod:" or
      (.state | current.value) == "include:";

      if (.state | current.type) == "KEYWORD" then
        # name:default
        (.state | current.value | rtrimstr(":")) as $name |
        .state |= advance |
        .state |= skipNewlines |
        if (.state | current.type) == "NUMBER" then
          .vars += [{name: $name, default: {type: "number", value: (.state | current.value)}}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "STRING" then
          .vars += [{name: $name, default: {type: "string", value: ((.state | current.value) | ltrimstr("'") | rtrimstr("'"))}}] |
          .state |= advance |
          .state |= skipNewlines
        else
          .
        end
      elif (.state | current.type) == "IDENTIFIER" then
        .vars += [{name: (.state | current.value), default: null}] |
        .state |= advance |
        .state |= skipNewlines
      else
        # Unknown token, stop
        .stop = true
      end
    ) |
    .vars as $vars |
    .state | .result = {type: "instanceVars", vars: $vars}
  else
    fail
  end;

# Parse: include: TraitName
def parseInclude:
  if current.value == "include:" then
    advance | skipNewlines |
    if current.type == "IDENTIFIER" then
      .result = {type: "include", trait: current.value} | advance
    else
      fail
    end
  else
    fail
  end;

# Parse: requires: 'path'
def parseRequires:
  if current.value == "requires:" then
    advance | skipNewlines |
    if current.type == "STRING" then
      .result = {type: "requires", path: (current.value | ltrimstr("'") | rtrimstr("'"))} |
      advance
    else
      fail
    end
  else
    fail
  end;

# Collect method body tokens between [ and ]
def collectMethodBody:
  if current.type == "LBRACKET" then
    advance |
    # Collect tokens tracking bracket depth
    {depth: 1, tokens: [], state: .} |
    until(.depth == 0 or (.state | atEnd);
      if (.state | current.type) == "LBRACKET" then
        .depth += 1 |
        .tokens += [.state | current] |
        .state |= advance
      elif (.state | current.type) == "RBRACKET" then
        .depth -= 1 |
        if .depth > 0 then
          .tokens += [.state | current]
        else
          .
        end |
        .state |= advance
      else
        .tokens += [.state | current] |
        .state |= advance
      end
    ) |
    .tokens as $body |
    .state | .result = {type: "block", tokens: $body}
  else
    fail
  end;

# Parse method signature (unary or keyword)
def parseMethodSig:
  skipNewlines |
  if current.type == "KEYWORD" then
    # Keyword method: key1: arg1 key2: arg2 ...
    {selector: "", keywords: [], args: [], state: .} |
    until((.state | current.type) != "KEYWORD";
      (.state | current.value | rtrimstr(":")) as $kw |
      .state |= advance |
      .state |= skipNewlines |
      if (.state | current.type) == "IDENTIFIER" then
        .keywords += [$kw] |
        .args += [.state | current.value] |
        .selector = (if .selector == "" then $kw else "\(.selector)_\($kw)" end) |
        .state |= advance |
        .state |= skipNewlines
      else
        .stop = true
      end
    ) |
    {selector: .selector, keywords: .keywords, args: .args} as $sig |
    .state | .result = $sig
  elif current.type == "IDENTIFIER" then
    # Unary method
    .result = {selector: current.value, keywords: [], args: []} | advance
  else
    fail
  end;

# Parse method declaration
def parseMethod:
  skipNewlines |
  # Check for method keyword
  (if current.value == "method:" then {kind: "instance", raw: false}
   elif current.value == "rawMethod:" then {kind: "instance", raw: true}
   elif current.value == "classMethod:" then {kind: "class", raw: false}
   elif current.value == "rawClassMethod:" then {kind: "class", raw: true}
   else null end) as $kind |
  if $kind != null then
    advance | skipNewlines |
    parseMethodSig |
    if .result != null then
      .result as $sig |
      skipNewlines |
      collectMethodBody |
      if .result != null then
        .result = {
          type: "method",
          kind: $kind.kind,
          raw: $kind.raw,
          selector: $sig.selector,
          keywords: $sig.keywords,
          args: $sig.args,
          body: .result
        }
      else
        .
      end
    else
      .
    end
  else
    fail
  end;

# Parse class body elements
def parseClassBody:
  {instanceVars: [], traits: [], requires: [], methods: [], state: .} |
  until((.state | atEnd);
    .state |= skipNewlines |
    if (.state | atEnd) then
      .
    elif (.state | current.value) == "instanceVars:" then
      (.state | parseInstanceVarsSimple) as $r |
      if $r.result != null then
        .instanceVars = ($r.result.vars // []) |
        .state = $r
      else
        .state |= advance  # Skip problematic token
      end
    elif (.state | current.value) == "include:" then
      (.state | parseInclude) as $r |
      if $r.result != null then
        .traits += [$r.result.trait] |
        .state = $r
      else
        .state |= advance
      end
    elif (.state | current.value) == "requires:" then
      (.state | parseRequires) as $r |
      if $r.result != null then
        .requires += [$r.result.path] |
        .state = $r
      else
        .state |= advance
      end
    elif (.state | current.value) == "method:" or
         (.state | current.value) == "rawMethod:" or
         (.state | current.value) == "classMethod:" or
         (.state | current.value) == "rawClassMethod:" then
      (.state | parseMethod) as $r |
      if $r.result != null then
        .methods += [$r.result] |
        .state = $r
      else
        .state |= advance
      end
    else
      .state |= advance  # Skip unknown tokens
    end
  ) |
  {
    instanceVars: .instanceVars,
    traits: .traits,
    requires: .requires,
    methods: .methods
  } as $body |
  .state | .result = $body;

# ==============================================================================
# Main Parser
# ==============================================================================

def parseClass:
  parseClassHeader |
  if .result != null then
    .result as $header |
    parseClassBody |
    if .result != null then
      .result = ($header + .result)
    else
      .result = ($header + {instanceVars: [], traits: [], requires: [], methods: []})
    end
  else
    .
  end;

# ==============================================================================
# Entry Point
# ==============================================================================

# Input is token array from tokenizer
{tokens: ., pos: 0, result: null, errors: []} |
parseClass |
if .result != null then
  .result
else
  {error: true, message: "Parse failed", pos: .pos}
end
