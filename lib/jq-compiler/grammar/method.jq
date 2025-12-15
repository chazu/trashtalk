# ==============================================================================
# Method Grammar for Trashtalk Parser
# ==============================================================================
#
# Parses method declarations:
#
#   method: name [body]
#   method: name: arg [body]
#   method: name: arg1 otherName: arg2 [body]
#   rawMethod: name [body]
#   classMethod: name [body]
#   rawClassMethod: name [body]
#
# Grammar (EBNF):
#   methodDecl    = methodKind methodSig "[" methodBody "]"
#   methodKind    = "method:" | "rawMethod:" | "classMethod:" | "rawClassMethod:"
#   methodSig     = unarySig | keywordSig
#   unarySig      = identifier
#   keywordSig    = (keyword identifier)+
#   keyword       = identifier ":"
#   methodBody    = token*  (captured as raw text for later processing)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# Method Kind
# ------------------------------------------------------------------------------

# Parse the method declaration keyword
def methodKind:
  ws |
  choice([
    (literal("method:") | succeed({kind: "instance", raw: false}));
    (literal("rawMethod:") | succeed({kind: "instance", raw: true}));
    (literal("classMethod:") | succeed({kind: "class", raw: false}));
    (literal("rawClassMethod:") | succeed({kind: "class", raw: true}))
  ]);

# ------------------------------------------------------------------------------
# Method Signature
# ------------------------------------------------------------------------------

# Parse unary selector (no arguments)
# Example: increment
def unarySig:
  token("IDENTIFIER") |
  map({
    selector: .value,
    keywords: [],
    args: []
  });

# Parse a single keyword:argument pair
# Example: setValue: newVal
def keywordPart:
  token("KEYWORD") as $kw |
  if $kw.result != null then
    $kw | ws |
    token("IDENTIFIER") as $arg |
    if $arg.result != null then
      $arg | succeed({
        keyword: ($kw.result.value | rtrimstr(":")),
        arg: $arg.result.value
      })
    else
      $arg
    end
  else
    $kw
  end;

# Parse keyword selector (one or more keyword:arg pairs)
# Example: setValue: newVal -> {selector: "setValue", keywords: ["setValue"], args: ["newVal"]}
# Example: at: idx put: val -> {selector: "at_put", keywords: ["at", "put"], args: ["idx", "val"]}
def keywordSig:
  many1(keywordPart | map(.) | ws) |
  map(
    reduce .[] as $part ({selector: "", keywords: [], args: []};
      .selector = (if .selector == "" then $part.keyword else "\(.selector)_\($part.keyword)" end) |
      .keywords += [$part.keyword] |
      .args += [$part.arg]
    )
  );

# Parse method signature (unary or keyword)
def methodSig:
  ws |
  # Try keyword first (more specific), then unary
  choice2(keywordSig; unarySig);

# ------------------------------------------------------------------------------
# Method Body
# ------------------------------------------------------------------------------

# Collect all tokens between [ and ] preserving bracket nesting
# Returns array of tokens that form the method body
def collectBodyTokens:
  # Track bracket depth and collect tokens
  def go($depth):
    if $depth == 0 then
      succeed([])
    elif eof then
      fail("unexpected end of input in method body")
    else
      current as $tok |
      if $tok.type == "LBRACKET" then
        advance | go($depth + 1) | map([$tok] + .)
      elif $tok.type == "RBRACKET" then
        if $depth == 1 then
          # Don't include the final closing bracket
          succeed([])
        else
          advance | go($depth - 1) | map([$tok] + .)
        end
      else
        advance | go($depth) | map([$tok] + .)
      end
    end;
  # Start with depth 1 (we've already consumed the opening bracket)
  go(1);

# Parse the method body (between brackets)
# For now, we capture raw tokens; expression parsing happens later
def methodBody:
  ws |
  literal("[") as $open |
  if $open.result != null then
    $open | ws |
    collectBodyTokens as $body |
    if $body.result != null then
      $body | ws |
      literal("]") as $close |
      if $close.result != null then
        $close | succeed({
          type: "block",
          tokens: $body.result
        })
      else
        $close
      end
    else
      $body
    end
  else
    $open
  end;

# ------------------------------------------------------------------------------
# Complete Method Declaration
# ------------------------------------------------------------------------------

# Parse a complete method declaration
def methodDecl:
  methodKind as $kind |
  if $kind.result != null then
    $kind | ws |
    methodSig as $sig |
    if $sig.result != null then
      $sig | ws |
      methodBody as $body |
      if $body.result != null then
        $body |
        # Store location from current position for error reporting
        succeed({
          type: "method",
          kind: $kind.result.kind,
          raw: $kind.result.raw,
          selector: $sig.result.selector,
          keywords: $sig.result.keywords,
          args: $sig.result.args,
          body: $body.result,
          location: {
            line: (if $sig.result | has("line") then $sig.result.line else 0 end),
            col: 0
          }
        })
      else
        $body
      end
    else
      $sig
    end
  else
    $kind
  end;

# ==============================================================================
# End of method.jq
# ==============================================================================
