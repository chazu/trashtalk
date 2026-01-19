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
def mapResult(f):
  if .result != null then .result |= f else . end;

# Skip NEWLINE and COMMENT tokens
def skipNewlines:
  if current.type == "NEWLINE" or current.type == "COMMENT" then advance | skipNewlines else . end;

# Check if current token is a synchronization point (class-level keyword)
def isSyncPoint:
  current.value == "method:" or
  current.value == "rawMethod:" or
  current.value == "classMethod:" or
  current.value == "rawClassMethod:" or
  current.value == "instanceVars:" or
  current.value == "classInstanceVars:" or
  current.value == "include:" or
  current.value == "requires:" or
  current.value == "category:" or
  current.value == "alias:" or
  current.value == "before:" or
  current.value == "after:" or
  current.value == "pragma:";

# Synchronization points for error recovery
# Skips tokens until we find a class-level keyword to resume parsing
def synchronize:
  until(atEnd or isSyncPoint; advance);

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

# Parse a class reference: IDENTIFIER or IDENTIFIER :: IDENTIFIER
# Returns: {type: "classRef", name: "ClassName"} for unqualified
#      or: {type: "classRef", package: "Pkg", name: "ClassName"} for qualified
def parseClassRef:
  if current.type == "IDENTIFIER" then
    current.value as $first |
    advance | skipNewlines |
    if current.type == "NAMESPACE_SEP" then
      # Qualified: Package::Class
      advance | skipNewlines |
      if current.type == "IDENTIFIER" then
        .result = {type: "classRef", package: $first, name: current.value} |
        advance
      else
        fail
      end
    else
      # Unqualified: Class
      .result = {type: "classRef", package: null, name: $first}
    end
  else
    fail
  end;

# Format a class reference as a string for AST
# Qualified: "Package::Class", Unqualified: "Class"
def formatClassRef($ref):
  if $ref.package != null then
    "\($ref.package)::\($ref.name)"
  else
    $ref.name
  end;

# Parse class header: ClassName subclass: Parent | ClassName trait
# Parent can be qualified: Counter subclass: Core::Object
def parseClassHeader:
  skipNewlines |
  if current.type == "IDENTIFIER" then
    # Capture location from class name token
    {line: current.line, col: current.col} as $location |
    .result = current.value | advance | skipNewlines |
    .result as $name |
    if current.value == "subclass:" then
      advance | skipNewlines |
      # Parse parent class reference (may be qualified)
      parseClassRef |
      if .result != null then
        .result as $parentRef |
        .result = {
          type: "class",
          name: $name,
          parent: formatClassRef($parentRef),
          parentPackage: $parentRef.package,
          isTrait: false,
          location: $location
        }
      else
        fail
      end
    elif current.value == "trait" then
      advance |
      .result = {type: "class", name: $name, parent: null, parentPackage: null, isTrait: true, location: $location}
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
    elif current.type == "TRIPLESTRING" then
      .result = {name: ($kw | rtrimstr(":")), default: {type: "triplestring", value: current.value}} |
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
    {vars: [], warnings: [], state: ., stop: false} |
    until(
      .stop or
      .state.pos >= (.state.tokens | length) or
      (.state | current.type) == "NEWLINE" or
      (.state | current.value) == "method:" or
      (.state | current.value) == "classMethod:" or
      (.state | current.value) == "rawMethod:" or
      (.state | current.value) == "rawClassMethod:" or
      (.state | current.value) == "include:" or
      (.state | isSyncPoint);

      if (.state | current.type) == "KEYWORD" then
        # Keyword token may be "name:" or "name:42" (for varspec with inline numeric default)
        {line: (.state | current.line), col: (.state | current.col)} as $loc |
        (.state | current.value) as $kw |
        # Split on first colon to get name and potential embedded numeric default
        ($kw | split(":")) as $parts |
        ($parts[0]) as $name |
        (if ($parts | length) > 1 and $parts[1] != "" then $parts[1:] | join(":") else null end) as $embedded_default |
        .state |= advance |
        .state |= skipNewlines |
        if $embedded_default != null then
          # Numeric default was embedded in the keyword token (e.g., "value:42")
          .vars += [{name: $name, default: {type: "number", value: $embedded_default}, location: $loc}]
        elif (.state | current.type) == "NUMBER" then
          .vars += [{name: $name, default: {type: "number", value: (.state | current.value)}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "STRING" then
          .vars += [{name: $name, default: {type: "string", value: ((.state | current.value) | ltrimstr("'") | rtrimstr("'"))}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "TRIPLESTRING" then
          .vars += [{name: $name, default: {type: "triplestring", value: (.state | current.value)}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "IDENTIFIER" then
          # Bare identifier after keyword with space (e.g., "name: value") - treat as two vars
          .warnings += [{
            type: "possible_typo",
            message: ("instanceVars: '" + $name + ": " + (.state | current.value) + "' - if this is meant to be a default, remove the space: '" + $name + ":" + (.state | current.value) + "'"),
            token: {line: $loc.line, col: $loc.col}
          }] |
          # Treat keyword without value (name gets no default)
          .vars += [{name: $name, default: null, location: $loc}]
          # Don't advance - let the identifier be parsed as a separate var
        else
          .vars += [{name: $name, default: null, location: $loc}]
        end
      elif (.state | current.type) == "IDENTIFIER" then
        # Capture location from identifier token
        {line: (.state | current.line), col: (.state | current.col)} as $loc |
        .vars += [{name: (.state | current.value), default: null, location: $loc}] |
        .state |= advance |
        .state |= skipNewlines
      else
        # Unknown token, stop
        .stop = true
      end
    ) |
    .vars as $vars |
    .warnings as $warnings |
    .state | .result = {type: "instanceVars", vars: $vars, warnings: $warnings}
  else
    fail
  end;

# Simpler classInstanceVars parser - just collect keywords and identifiers
def parseClassInstanceVarsSimple:
  if current.value == "classInstanceVars:" then
    advance | skipNewlines |
    # Collect all var specs manually
    {vars: [], warnings: [], state: ., stop: false} |
    until(
      .stop or
      .state.pos >= (.state.tokens | length) or
      (.state | current.type) == "NEWLINE" or
      (.state | current.value) == "method:" or
      (.state | current.value) == "classMethod:" or
      (.state | current.value) == "rawMethod:" or
      (.state | current.value) == "rawClassMethod:" or
      (.state | current.value) == "include:" or
      (.state | isSyncPoint);

      if (.state | current.type) == "KEYWORD" then
        # Keyword token may be "name:" or "name:42" (for varspec with inline numeric default)
        {line: (.state | current.line), col: (.state | current.col)} as $loc |
        (.state | current.value) as $kw |
        # Split on first colon to get name and potential embedded numeric default
        ($kw | split(":")) as $parts |
        ($parts[0]) as $name |
        (if ($parts | length) > 1 and $parts[1] != "" then $parts[1:] | join(":") else null end) as $embedded_default |
        .state |= advance |
        .state |= skipNewlines |
        if $embedded_default != null then
          # Numeric default was embedded in the keyword token (e.g., "value:42")
          .vars += [{name: $name, default: {type: "number", value: $embedded_default}, location: $loc}]
        elif (.state | current.type) == "NUMBER" then
          .vars += [{name: $name, default: {type: "number", value: (.state | current.value)}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "STRING" then
          .vars += [{name: $name, default: {type: "string", value: ((.state | current.value) | ltrimstr("'") | rtrimstr("'"))}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "TRIPLESTRING" then
          .vars += [{name: $name, default: {type: "triplestring", value: (.state | current.value)}, location: $loc}] |
          .state |= advance |
          .state |= skipNewlines
        elif (.state | current.type) == "IDENTIFIER" then
          # Bare identifier after keyword with space (e.g., "name: value") - treat as two vars
          .warnings += [{
            type: "possible_typo",
            message: ("classInstanceVars: '" + $name + ": " + (.state | current.value) + "' - if this is meant to be a default, remove the space: '" + $name + ":" + (.state | current.value) + "'"),
            token: {line: $loc.line, col: $loc.col}
          }] |
          # Treat keyword without value (name gets no default)
          .vars += [{name: $name, default: null, location: $loc}]
          # Don't advance - let the identifier be parsed as a separate var
        else
          .vars += [{name: $name, default: null, location: $loc}]
        end
      elif (.state | current.type) == "IDENTIFIER" then
        # Capture location from identifier token
        {line: (.state | current.line), col: (.state | current.col)} as $loc |
        .vars += [{name: (.state | current.value), default: null, location: $loc}] |
        .state |= advance |
        .state |= skipNewlines
      else
        # Unknown token, stop
        .stop = true
      end
    ) |
    .vars as $vars |
    .warnings as $warnings |
    .state | .result = {type: "classInstanceVars", vars: $vars, warnings: $warnings}
  else
    fail
  end;

# Parse: include: TraitName (may be qualified: include: OtherPkg::Debuggable)
def parseInclude:
  if current.value == "include:" then
    # Capture location from include: keyword
    {line: current.line, col: current.col} as $location |
    advance | skipNewlines |
    # Parse trait reference (may be qualified)
    parseClassRef |
    if .result != null then
      .result as $traitRef |
      .result = {
        type: "include",
        trait: formatClassRef($traitRef),
        traitPackage: $traitRef.package,
        location: $location
      }
    else
      fail
    end
  else
    fail
  end;

# Parse: requires: 'path' (file dependency) OR requires: methodSelector (protocol requirement)
def parseRequires:
  if current.value == "requires:" then
    # Capture location from requires: keyword
    {line: current.line, col: current.col} as $location |
    advance | skipNewlines |
    if current.type == "STRING" then
      # File dependency: requires: 'path/to/file'
      .result = {type: "requires", path: (current.value | ltrimstr("'") | rtrimstr("'")), location: $location} |
      advance
    elif current.type == "KEYWORD" then
      # Protocol method requirement: requires: do: or requires: inject: into:
      # Collect consecutive KEYWORDs to form the selector (stop at sync points like requires:)
      {selector: "", state: .} |
      until((.state | current.type) != "KEYWORD" or (.state | isSyncPoint);
        .selector = (if .selector == "" then (.state | current.value) else "\(.selector)\(.state | current.value | rtrimstr(":")):" end) |
        .state |= advance |
        .state |= skipNewlines
      ) |
      .selector as $sel |
      .state | .result = {type: "methodRequirement", selector: $sel, location: $location}
    else
      fail
    end
  else
    fail
  end;

# Parse: alias: newName for: existingMethod
def parseAlias:
  if current.value == "alias:" then
    # Capture location from alias: keyword
    {line: current.line, col: current.col} as $location |
    advance | skipNewlines |
    if current.type == "IDENTIFIER" then
      current.value as $aliasName |
      advance | skipNewlines |
      if current.value == "for:" then
        advance | skipNewlines |
        if current.type == "IDENTIFIER" then
          .result = {type: "alias", aliasName: $aliasName, originalMethod: current.value, location: $location} |
          advance
        else
          fail
        end
      else
        fail
      end
    else
      fail
    end
  else
    fail
  end;

# Parse: before: selector do: [block] OR after: selector do: [block]
def parseAdvice:
  if current.value == "before:" or current.value == "after:" then
    # Capture location and advice type
    {line: current.line, col: current.col} as $location |
    (if current.value == "before:" then "before" else "after" end) as $adviceType |
    advance | skipNewlines |
    if current.type == "IDENTIFIER" then
      current.value as $selector |
      advance | skipNewlines |
      if current.value == "do:" then
        advance | skipNewlines |
        if current.type == "LBRACKET" then
          # Collect the block tokens
          advance |
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
          .tokens as $blockTokens |
          .state | .result = {
            type: "advice",
            adviceType: $adviceType,
            selector: $selector,
            block: {type: "block", tokens: $blockTokens},
            location: $location
          }
        else
          fail
        end
      else
        fail
      end
    else
      fail
    end
  else
    fail
  end;

# Parse: pragma: pragmaName (class-level pragma)
def parseClassPragma:
  if current.value == "pragma:" then
    # Capture location from pragma: keyword
    {line: current.line, col: current.col} as $location |
    advance | skipNewlines |
    if current.type == "IDENTIFIER" then
      .result = {type: "classPragma", name: current.value, location: $location} |
      advance
    else
      fail
    end
  else
    fail
  end;

# Extract pragmas from the start of method body tokens
# Pragma format: pragma: <name> (e.g., pragma: direct)
# Returns {pragmas: [...], remaining_tokens: [...]}
def extractPragmas:
  . as $tokens |
  # Skip leading newlines
  ([$tokens[] | select(.type != "NEWLINE")] | .[0:3]) as $first3 |
  if ($first3 | length) >= 2 and
     ($first3[0].type == "KEYWORD") and
     ($first3[0].value == "pragma:") and
     ($first3[1].type == "IDENTIFIER") then
    # Found a pragma - extract value and continue checking for more
    $first3[1].value as $pragmaValue |
    # Find where pragma: value ends in original tokens (skip newlines, find pragma:, skip it and value)
    ($tokens | to_entries | map(select(.value.type != "NEWLINE")) | .[0:2] | .[-1].key + 1) as $skipCount |
    # Recursively check for more pragmas
    ($tokens[$skipCount:] | extractPragmas) as $rest |
    {pragmas: ([$pragmaValue] + $rest.pragmas), remaining_tokens: $rest.remaining_tokens}
  else
    {pragmas: [], remaining_tokens: $tokens}
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
    # Append trailing underscore to keyword selectors to distinguish from unary
    # e.g., skip: -> skip_, at:put: -> at_put_
    {selector: "\(.selector)_", keywords: .keywords, args: .args} as $sig |
    .state | .result = $sig
  elif current.type == "IDENTIFIER" then
    # Unary method (no trailing underscore)
    .result = {selector: current.value, keywords: [], args: []} | advance
  else
    fail
  end;

# Parse method declaration
def parseMethod:
  skipNewlines |
  # Capture location from the method keyword token
  {line: current.line, col: current.col} as $location |
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
        # Extract pragmas from body tokens
        (.result.tokens | extractPragmas) as $pragmaResult |
        .result = {
          type: "method",
          kind: $kind.kind,
          raw: $kind.raw,
          selector: $sig.selector,
          keywords: $sig.keywords,
          args: $sig.args,
          body: {type: "block", tokens: $pragmaResult.remaining_tokens},
          pragmas: $pragmaResult.pragmas,
          location: $location
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
  {instanceVars: [], classInstanceVars: [], traits: [], requires: [], methodRequirements: [], methods: [], aliases: [], advice: [], classPragmas: [], errors: [], currentCategory: null, state: .} |
  until((.state | atEnd);
    .state |= skipNewlines |
    if (.state | atEnd) then
      .
    elif (.state | current.value) == "category:" then
      # Parse category directive: category: "name" or category: 'name'
      .state |= advance |
      .state |= skipNewlines |
      if (.state | current.type) == "STRING" or (.state | current.type) == "DSTRING" then
        .currentCategory = ((.state | current.value) | ltrimstr("'") | rtrimstr("'") | ltrimstr("\"") | rtrimstr("\"")) |
        .state |= advance
      else
        .errors += [{
          type: "parse_error",
          message: "Expected string after category:",
          token: (.state | current),
          context: "category"
        }] |
        .state |= synchronize
      end
    elif (.state | current.value) == "instanceVars:" then
      (.state | parseInstanceVarsSimple) as $r |
      if $r.result != null then
        .instanceVars = ($r.result.vars // []) |
        .errors += ($r.result.warnings // []) |
        .state = $r
      else
        # Record error and synchronize to next declaration
        .errors += [{
          type: "parse_error",
          message: "Failed to parse instanceVars declaration",
          token: (.state | current),
          context: "instanceVars"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "classInstanceVars:" then
      (.state | parseClassInstanceVarsSimple) as $r |
      if $r.result != null then
        .classInstanceVars = ($r.result.vars // []) |
        .errors += ($r.result.warnings // []) |
        .state = $r
      else
        # Record error and synchronize to next declaration
        .errors += [{
          type: "parse_error",
          message: "Failed to parse classInstanceVars declaration",
          token: (.state | current),
          context: "classInstanceVars"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "include:" then
      (.state | parseInclude) as $r |
      if $r.result != null then
        .traits += [$r.result.trait] |
        .state = $r
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse include declaration",
          token: (.state | current),
          context: "include"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "requires:" then
      (.state | parseRequires) as $r |
      if $r.result != null then
        if $r.result.type == "requires" then
          # File dependency
          .requires += [$r.result.path] |
          .state = $r
        elif $r.result.type == "methodRequirement" then
          # Protocol method requirement
          .methodRequirements += [$r.result.selector] |
          .state = $r
        else
          .state = $r
        end
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse requires declaration",
          token: (.state | current),
          context: "requires"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "alias:" then
      (.state | parseAlias) as $r |
      if $r.result != null then
        .aliases += [$r.result] |
        .state = $r
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse alias declaration",
          token: (.state | current),
          context: "alias"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "pragma:" then
      (.state | parseClassPragma) as $r |
      if $r.result != null then
        .classPragmas += [$r.result.name] |
        .state = $r
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse class pragma declaration",
          token: (.state | current),
          context: "pragma"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "before:" or (.state | current.value) == "after:" then
      (.state | parseAdvice) as $r |
      if $r.result != null then
        .advice += [$r.result] |
        .state = $r
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse advice declaration",
          token: (.state | current),
          context: "advice"
        }] |
        .state |= (advance | synchronize)
      end
    elif (.state | current.value) == "method:" or
         (.state | current.value) == "rawMethod:" or
         (.state | current.value) == "classMethod:" or
         (.state | current.value) == "rawClassMethod:" then
      .currentCategory as $cat |
      (.state | parseMethod) as $r |
      if $r.result != null then
        # Add category to method if one is set
        .methods += [if $cat != null then $r.result + {category: $cat} else $r.result end] |
        .state = $r
      else
        .errors += [{
          type: "parse_error",
          message: "Failed to parse method declaration",
          token: (.state | current),
          context: "method"
        }] |
        .state |= (advance | synchronize)
      end
    else
      # Unknown token - synchronize to next declaration
      (if (.state | current.type) == "NEWLINE" or (.state | current.type) == "COMMENT" then
        .state |= advance
      else
        .errors += [{
          type: "unknown_token",
          message: "Unexpected token in class body",
          token: (.state | current),
          context: "class_body"
        }] |
        .state |= synchronize
      end)
    end
  ) |
  {
    instanceVars: .instanceVars,
    classInstanceVars: .classInstanceVars,
    traits: .traits,
    requires: .requires,
    methodRequirements: .methodRequirements,
    methods: .methods,
    aliases: .aliases,
    advice: .advice,
    classPragmas: .classPragmas
  } as $body |
  .errors as $errors |
  .state | .errors = (.errors + $errors) | .result = $body;

# ==============================================================================
# Package Declaration Parsing
# ==============================================================================

# Parse: package: PackageName
#        import: OtherPackage
#        import: AnotherPackage
# Returns: {package: "PackageName", imports: ["OtherPackage", "AnotherPackage"]}
# or null if no package declaration
def parsePackageDecl:
  skipNewlines |
  if current.value == "package:" then
    # Capture location from package: keyword
    {line: current.line, col: current.col} as $location |
    advance | skipNewlines |
    if current.type == "IDENTIFIER" then
      current.value as $packageName |
      advance | skipNewlines |
      # Collect imports
      {imports: [], state: .} |
      until((.state | current.value) != "import:";
        .state |= advance |
        .state |= skipNewlines |
        if (.state | current.type) == "IDENTIFIER" then
          .imports += [.state | current.value] |
          .state |= advance |
          .state |= skipNewlines
        else
          # Error: expected identifier after import:
          .
        end
      ) |
      .imports as $imports |
      .state | .result = {package: $packageName, imports: $imports, packageLocation: $location}
    else
      # Error: expected identifier after package:
      fail
    end
  else
    # No package declaration - that's OK
    .result = null
  end;

# ==============================================================================
# Main Parser
# ==============================================================================

def parseClass:
  # First check for package declaration
  parsePackageDecl |
  .result as $pkgDecl |
  parseClassHeader |
  if .result != null then
    .result as $header |
    parseClassBody |
    if .result != null then
      # Combine header, body, and package info
      ($header + .result) as $class |
      # Add package and imports if present
      .result = (if $pkgDecl != null then
        $class + {package: $pkgDecl.package, imports: $pkgDecl.imports}
      else
        $class + {package: null, imports: []}
      end)
    else
      ($header + {instanceVars: [], classInstanceVars: [], traits: [], requires: [], methodRequirements: [], methods: [], aliases: [], advice: [], classPragmas: []}) as $class |
      .result = (if $pkgDecl != null then
        $class + {package: $pkgDecl.package, imports: $pkgDecl.imports}
      else
        $class + {package: null, imports: []}
      end)
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
  # Include warnings if there were non-fatal errors during parsing
  if (.errors | length) > 0 then
    .result + {warnings: .errors}
  else
    .result
  end
else
  {error: true, message: "Parse failed", pos: .pos, errors: .errors}
end
