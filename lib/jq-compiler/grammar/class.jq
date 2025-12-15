# ==============================================================================
# Class Grammar for Trashtalk Parser
# ==============================================================================
#
# Parses class and trait declarations:
#
#   ClassName subclass: ParentClass
#     instanceVars: var1 var2:default
#     include: TraitName
#     requires: 'path/to/file'
#     method: ...
#
# Or for traits:
#
#   TraitName trait
#     method: ...
#
# Grammar (EBNF):
#   classDecl     = className ("subclass:" className | "trait") classBody
#   classBody     = (instanceVars | include | requires | method)*
#   instanceVars  = "instanceVars:" varSpec+
#   varSpec       = identifier (":" (number | string))?
#   include       = "include:" identifier
#   requires      = "requires:" string
#
# ==============================================================================

# Import combinators (will be included via main parser)
# include "peg/combinators";

# ------------------------------------------------------------------------------
# Class Declaration
# ------------------------------------------------------------------------------

# Parse class name (an identifier)
def className:
  token("IDENTIFIER") | map(.value);

# Parse: ClassName subclass: ParentClass
def subclassDecl:
  ws |
  className as $name |
  if $name.result != null then
    $name | ws |
    literal("subclass:") as $kw |
    if $kw.result != null then
      $kw | ws |
      className as $parent |
      if $parent.result != null then
        $parent | succeed({
          type: "class",
          name: $name.result,
          parent: $parent.result,
          isTrait: false,
          location: {line: ($name.result | . as $n | $name | .tokens[$name.pos - 1].line // 1), col: 0}
        })
      else
        $parent
      end
    else
      $kw
    end
  else
    $name
  end;

# Parse: TraitName trait
def traitDecl:
  ws |
  className as $name |
  if $name.result != null then
    $name | ws |
    literal("trait") as $kw |
    if $kw.result != null then
      $kw | succeed({
        type: "class",
        name: $name.result,
        parent: null,
        isTrait: true,
        location: {line: 1, col: 0}
      })
    else
      $kw
    end
  else
    $name
  end;

# Parse class or trait declaration header
def classHeader:
  choice2(subclassDecl; traitDecl);

# ------------------------------------------------------------------------------
# Instance Variables
# ------------------------------------------------------------------------------

# Parse a single variable spec: name or name:default
def varSpec:
  # First try: keyword (name:) followed by value
  choice2(
    # name:value pattern
    (token("KEYWORD") as $kw |
     if $kw.result != null then
       $kw | ws |
       # Get default value (number or string)
       choice2(
         (token("NUMBER") | map({name: ($kw.result.value | rtrimstr(":")), default: {type: "number", value: .value}}));
         (token("STRING") | map({name: ($kw.result.value | rtrimstr(":")), default: {type: "string", value: (.value | ltrimstr("'") | rtrimstr("'"))}}))
       )
     else
       $kw
     end);
    # Plain identifier (no default)
    (token("IDENTIFIER") | map({name: .value, default: null}))
  );

# Parse: instanceVars: var1 var2:0 var3:'hello'
def instanceVars:
  ws |
  literal("instanceVars:") as $kw |
  if $kw.result != null then
    $kw | ws |
    many1(varSpec | map(.) | ws) as $vars |
    if $vars.result != null then
      $vars | succeed({
        type: "instanceVars",
        vars: $vars.result
      })
    else
      $vars
    end
  else
    $kw
  end;

# ------------------------------------------------------------------------------
# Include (Traits)
# ------------------------------------------------------------------------------

# Parse: include: TraitName
def includeDecl:
  ws |
  literal("include:") as $kw |
  if $kw.result != null then
    $kw | ws |
    token("IDENTIFIER") as $name |
    if $name.result != null then
      $name | succeed({
        type: "include",
        trait: $name.result.value
      })
    else
      $name
    end
  else
    $kw
  end;

# ------------------------------------------------------------------------------
# Requires (Dependencies)
# ------------------------------------------------------------------------------

# Parse: requires: 'path/to/file'
def requiresDecl:
  ws |
  literal("requires:") as $kw |
  if $kw.result != null then
    $kw | ws |
    token("STRING") as $path |
    if $path.result != null then
      $path | succeed({
        type: "requires",
        path: ($path.result.value | ltrimstr("'") | rtrimstr("'"))
      })
    else
      $path
    end
  else
    $kw
  end;

# ------------------------------------------------------------------------------
# Class Body
# ------------------------------------------------------------------------------

# Parse a single class body element
# (methods are handled by method.jq, imported here)
def classBodyElement:
  choice([
    instanceVars,
    includeDecl,
    requiresDecl,
    methodDecl  # Defined in method.jq
  ]);

# Parse entire class body (multiple elements)
def classBody:
  many(ws | classBodyElement) |
  map(
    # Group elements by type and structure the result
    group_by(.type) |
    reduce .[] as $group ({instanceVars: [], traits: [], requires: [], methods: []};
      if ($group[0].type == "instanceVars") then
        .instanceVars = ($group | map(.vars) | add // [])
      elif ($group[0].type == "include") then
        .traits = ($group | map(.trait))
      elif ($group[0].type == "requires") then
        .requires = ($group | map(.path))
      elif ($group[0].type == "method") then
        .methods = $group
      else
        .
      end
    )
  );

# ------------------------------------------------------------------------------
# Main Class Parser
# ------------------------------------------------------------------------------

# Parse a complete class file
def parseClass:
  classHeader as $header |
  if $header.result != null then
    $header | classBody as $body |
    if $body.result != null then
      $body | succeed(
        $header.result + $body.result
      )
    else
      # Even with empty body, return header
      $header | succeed(
        $header.result + {instanceVars: [], traits: [], requires: [], methods: []}
      )
    end
  else
    $header
  end;

# ==============================================================================
# End of class.jq
# ==============================================================================
