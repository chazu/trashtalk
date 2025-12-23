# ==============================================================================
# Parser Tests
# ==============================================================================
# Tests for parser.jq - validates AST structure
# ==============================================================================

TOKENIZER="$COMPILER_DIR/tokenizer.bash"
PARSER="$COMPILER_DIR/parser.jq"

# Helper to parse input and get specific field
parse_field() {
    local input="$1"
    local field="$2"
    echo "$input" | "$TOKENIZER" | jq -f "$PARSER" | jq -r "$field // \"null\""
}

# Helper to parse and get full AST
parse_ast() {
    echo "$1" | "$TOKENIZER" | jq -f "$PARSER"
}

# ------------------------------------------------------------------------------
# Class Header Tests
# ------------------------------------------------------------------------------

echo -e "\n  Class Headers:"

run_test "class name" "Counter" \
    "$(parse_field 'Counter subclass: Object' '.name')"

run_test "parent class" "Object" \
    "$(parse_field 'Counter subclass: Object' '.parent')"

# isTrait is only set to true for traits; null/missing means it's a class
run_test "class has no isTrait true" "false" \
    "$(parse_field 'Counter subclass: Object' 'if .isTrait == true then "true" else "false" end')"

run_test "trait declaration" "true" \
    "$(parse_field 'Debuggable trait' '.isTrait')"

run_test "trait has no parent" "null" \
    "$(parse_field 'Debuggable trait' '.parent')"

# ------------------------------------------------------------------------------
# Instance Variable Tests
# ------------------------------------------------------------------------------

echo -e "\n  Instance Variables:"

INPUT_IVARS='Counter subclass: Object
  instanceVars: value step'

run_test "ivar count" "2" \
    "$(parse_field "$INPUT_IVARS" '.instanceVars | length')"

run_test "first ivar name" "value" \
    "$(parse_field "$INPUT_IVARS" '.instanceVars[0].name')"

run_test "second ivar name" "step" \
    "$(parse_field "$INPUT_IVARS" '.instanceVars[1].name')"

INPUT_IVARS_DEFAULT='Counter subclass: Object
  instanceVars: value:0 step:1'

run_test "ivar with default" "0" \
    "$(parse_field "$INPUT_IVARS_DEFAULT" '.instanceVars[0].default.value')"

run_test "ivar default type" "number" \
    "$(parse_field "$INPUT_IVARS_DEFAULT" '.instanceVars[0].default.type')"

INPUT_IVARS_STRING="Array subclass: Object
  instanceVars: items:'[]'"

run_test "ivar string default" "[]" \
    "$(parse_field "$INPUT_IVARS_STRING" '.instanceVars[0].default.value')"

# Bare identifier after keyword triggers warning and is treated as separate var
INPUT_IVARS_BARE_STRING='Config subclass: Object
  instanceVars: name:unknown count:0'

# name:unknown becomes two vars: 'name' (no default) and 'unknown' (no default)
run_test "ivar keyword without quoted default has no default" "null" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.instanceVars[0].default')"

run_test "ivar keyword without quoted default name" "name" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.instanceVars[0].name')"

run_test "bare identifier becomes separate var" "unknown" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.instanceVars[1].name')"

run_test "third ivar numeric default" "0" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.instanceVars[2].default.value')"

run_test "warning emitted for bare identifier" "1" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.warnings | length')"

run_test "warning type is possible_typo" "possible_typo" \
    "$(parse_field "$INPUT_IVARS_BARE_STRING" '.warnings[0].type')"

# ------------------------------------------------------------------------------
# Trait Include Tests
# ------------------------------------------------------------------------------

echo -e "\n  Trait Includes:"

INPUT_INCLUDE='Counter subclass: Object
  include: Debuggable'

run_test "include trait" "Debuggable" \
    "$(parse_field "$INPUT_INCLUDE" '.traits[0]')"

INPUT_MULTI_INCLUDE='Counter subclass: Object
  include: Debuggable
  include: Serializable'

run_test "multiple traits" "2" \
    "$(parse_field "$INPUT_MULTI_INCLUDE" '.traits | length')"

# ------------------------------------------------------------------------------
# Requires Tests
# ------------------------------------------------------------------------------

echo -e "\n  Requires:"

INPUT_REQUIRES="Store subclass: Object
  requires: 'lib/database.bash'"

run_test "requires path" "lib/database.bash" \
    "$(parse_field "$INPUT_REQUIRES" '.requires[0]')"

# ------------------------------------------------------------------------------
# Method Declaration Tests
# ------------------------------------------------------------------------------

echo -e "\n  Method Declarations:"

INPUT_UNARY='Counter subclass: Object
  method: increment [
    ^ 1
  ]'

run_test "unary method selector" "increment" \
    "$(parse_field "$INPUT_UNARY" '.methods[0].selector')"

run_test "unary method kind" "instance" \
    "$(parse_field "$INPUT_UNARY" '.methods[0].kind')"

# raw is only set to true for rawMethod; null/missing means it's not raw
run_test "unary method not raw" "false" \
    "$(parse_field "$INPUT_UNARY" 'if .methods[0].raw == true then "true" else "false" end')"

INPUT_KEYWORD='Counter subclass: Object
  method: setValue: newVal [
    value := newVal
  ]'

run_test "keyword method selector" "setValue_" \
    "$(parse_field "$INPUT_KEYWORD" '.methods[0].selector')"

run_test "keyword method arg" "newVal" \
    "$(parse_field "$INPUT_KEYWORD" '.methods[0].args[0]')"

INPUT_MULTI_KEYWORD='Store subclass: Object
  method: getField: id field: name [
    ^ 1
  ]'

run_test "multi-keyword selector" "getField_field_" \
    "$(parse_field "$INPUT_MULTI_KEYWORD" '.methods[0].selector')"

run_test "multi-keyword first arg" "id" \
    "$(parse_field "$INPUT_MULTI_KEYWORD" '.methods[0].args[0]')"

run_test "multi-keyword second arg" "name" \
    "$(parse_field "$INPUT_MULTI_KEYWORD" '.methods[0].args[1]')"

# ------------------------------------------------------------------------------
# Class Method Tests
# ------------------------------------------------------------------------------

echo -e "\n  Class Methods:"

INPUT_CLASS_METHOD='Counter subclass: Object
  classMethod: description [
    ^ "A counter"
  ]'

run_test "class method kind" "class" \
    "$(parse_field "$INPUT_CLASS_METHOD" '.methods[0].kind')"

# ------------------------------------------------------------------------------
# Raw Method Tests
# ------------------------------------------------------------------------------

echo -e "\n  Raw Methods:"

INPUT_RAW='Process subclass: Object
  rawMethod: spawn: name [
    echo "spawning"
  ]'

run_test "raw method flag" "true" \
    "$(parse_field "$INPUT_RAW" '.methods[0].raw')"

INPUT_RAW_CLASS='Store subclass: Object
  rawClassMethod: init [
    db_init
  ]'

run_test "raw class method" "class" \
    "$(parse_field "$INPUT_RAW_CLASS" '.methods[0].kind')"

run_test "raw class method flag" "true" \
    "$(parse_field "$INPUT_RAW_CLASS" '.methods[0].raw')"

# ------------------------------------------------------------------------------
# Method Body Tests
# ------------------------------------------------------------------------------

echo -e "\n  Method Bodies:"

INPUT_BODY='Counter subclass: Object
  method: test [
    | x y |
    x := 5
    ^ x
  ]'

run_test "body has tokens" "true" \
    "$(parse_field "$INPUT_BODY" '.methods[0].body.tokens | length > 0')"

run_test "body type is block" "block" \
    "$(parse_field "$INPUT_BODY" '.methods[0].body.type')"

# ------------------------------------------------------------------------------
# Complex Class Tests
# ------------------------------------------------------------------------------

echo -e "\n  Complex Classes:"

INPUT_COMPLEX='Counter subclass: Object
  include: Debuggable
  instanceVars: value:0 step:1

  method: new [
    | id |
    id := $(_generate_instance_id Counter)
    ^ $id
  ]

  method: increment [
    | current |
    current := $(@ self getValue)
    @ self setValue: $((current + 1))
  ]

  classMethod: description [
    ^ "A counter class"
  ]'

run_test "complex class name" "Counter" \
    "$(parse_field "$INPUT_COMPLEX" '.name')"

run_test "complex class traits" "1" \
    "$(parse_field "$INPUT_COMPLEX" '.traits | length')"

run_test "complex class ivars" "2" \
    "$(parse_field "$INPUT_COMPLEX" '.instanceVars | length')"

run_test "complex class methods" "3" \
    "$(parse_field "$INPUT_COMPLEX" '.methods | length')"

run_test "complex has instance method" "new" \
    "$(parse_field "$INPUT_COMPLEX" '.methods[0].selector')"

run_test "complex has class method" "class" \
    "$(parse_field "$INPUT_COMPLEX" '.methods[2].kind')"

# ------------------------------------------------------------------------------
# Error Handling Tests
# ------------------------------------------------------------------------------

echo -e "\n  Error Handling:"

# Parser should handle malformed input gracefully
INPUT_MALFORMED='not a valid class'
RESULT=$(parse_field "$INPUT_MALFORMED" '.error // false')
run_test "malformed input has error" "true" "$RESULT"

# ------------------------------------------------------------------------------
# AST Structure Tests - Detailed validation
# ------------------------------------------------------------------------------

echo -e "\n  AST Structure (Detailed):"

# Test full AST structure for a simple class
INPUT_SIMPLE='Counter subclass: Object'
AST_SIMPLE=$(parse_ast "$INPUT_SIMPLE")

run_test "ast: has type field" "class" \
    "$(echo "$AST_SIMPLE" | jq -r '.type')"

run_test "ast: has name field" "Counter" \
    "$(echo "$AST_SIMPLE" | jq -r '.name')"

run_test "ast: has parent field" "Object" \
    "$(echo "$AST_SIMPLE" | jq -r '.parent')"

run_test "ast: has methods array" "true" \
    "$(echo "$AST_SIMPLE" | jq 'has("methods")')"

run_test "ast: has instanceVars array" "true" \
    "$(echo "$AST_SIMPLE" | jq 'has("instanceVars")')"

run_test "ast: has traits array" "true" \
    "$(echo "$AST_SIMPLE" | jq 'has("traits")')"

# ------------------------------------------------------------------------------
# Method Signature Edge Cases
# ------------------------------------------------------------------------------

echo -e "\n  Method Signature Edge Cases:"

# 3-keyword method
INPUT_3KW='Store subclass: Object
  method: at: key put: value with: options [
    ^ 1
  ]'

run_test "3-keyword selector" "at_put_with_" \
    "$(parse_field "$INPUT_3KW" '.methods[0].selector')"

run_test "3-keyword args count" "3" \
    "$(parse_field "$INPUT_3KW" '.methods[0].args | length')"

run_test "3-keyword arg 1" "key" \
    "$(parse_field "$INPUT_3KW" '.methods[0].args[0]')"

run_test "3-keyword arg 2" "value" \
    "$(parse_field "$INPUT_3KW" '.methods[0].args[1]')"

run_test "3-keyword arg 3" "options" \
    "$(parse_field "$INPUT_3KW" '.methods[0].args[2]')"

run_test_json "3-keyword keywords array" '["at","put","with"]' \
    "$(parse_field "$INPUT_3KW" '.methods[0].keywords')"

# Method with underscore in name
INPUT_UNDERSCORE='Store subclass: Object
  method: get_value [
    ^ 1
  ]'

run_test "method with underscore" "get_value" \
    "$(parse_field "$INPUT_UNDERSCORE" '.methods[0].selector')"

# Multiple methods
INPUT_MULTI_METHODS='Counter subclass: Object
  method: getValue [
    ^ 1
  ]
  method: setValue: v [
    value := v
  ]
  classMethod: create [
    ^ new
  ]'

run_test "multi-method count" "3" \
    "$(parse_field "$INPUT_MULTI_METHODS" '.methods | length')"

run_test "multi-method: first is instance" "instance" \
    "$(parse_field "$INPUT_MULTI_METHODS" '.methods[0].kind')"

run_test "multi-method: second is instance" "instance" \
    "$(parse_field "$INPUT_MULTI_METHODS" '.methods[1].kind')"

run_test "multi-method: third is class" "class" \
    "$(parse_field "$INPUT_MULTI_METHODS" '.methods[2].kind')"

# ------------------------------------------------------------------------------
# Instance Variable Edge Cases
# ------------------------------------------------------------------------------

echo -e "\n  Instance Variable Edge Cases:"

# Empty instanceVars (no vars declared)
INPUT_NO_IVARS='Counter subclass: Object
  method: test [ ^ 1 ]'

run_test "no ivars: empty array" "0" \
    "$(parse_field "$INPUT_NO_IVARS" '.instanceVars | length')"

# Multiple ivars with mixed defaults
INPUT_MIXED_IVARS='Store subclass: Object
  instanceVars: count:0 name items:'"'"'[]'"'"''

run_test "mixed ivars: count" "3" \
    "$(parse_field "$INPUT_MIXED_IVARS" '.instanceVars | length')"

run_test "mixed ivars: first has default" "0" \
    "$(parse_field "$INPUT_MIXED_IVARS" '.instanceVars[0].default.value')"

run_test "mixed ivars: second no default" "null" \
    "$(parse_field "$INPUT_MIXED_IVARS" '.instanceVars[1].default')"

run_test "mixed ivars: third has string default" "[]" \
    "$(parse_field "$INPUT_MIXED_IVARS" '.instanceVars[2].default.value')"

# ------------------------------------------------------------------------------
# Trait and Inheritance Tests
# ------------------------------------------------------------------------------

echo -e "\n  Trait and Inheritance:"

# Trait definition with methods
INPUT_TRAIT_FULL='Debuggable trait
  method: debug [
    echo "debug"
  ]
  method: trace: msg [
    echo $msg
  ]'

run_test "trait: isTrait true" "true" \
    "$(parse_field "$INPUT_TRAIT_FULL" '.isTrait')"

run_test "trait: has methods" "2" \
    "$(parse_field "$INPUT_TRAIT_FULL" '.methods | length')"

run_test "trait: no parent" "null" \
    "$(parse_field "$INPUT_TRAIT_FULL" '.parent')"

# Class with multiple traits
INPUT_MULTI_TRAIT='Widget subclass: View
  include: Debuggable
  include: Serializable
  include: Observable'

run_test "multi-trait: count" "3" \
    "$(parse_field "$INPUT_MULTI_TRAIT" '.traits | length')"

run_test "multi-trait: first" "Debuggable" \
    "$(parse_field "$INPUT_MULTI_TRAIT" '.traits[0]')"

run_test "multi-trait: second" "Serializable" \
    "$(parse_field "$INPUT_MULTI_TRAIT" '.traits[1]')"

run_test "multi-trait: third" "Observable" \
    "$(parse_field "$INPUT_MULTI_TRAIT" '.traits[2]')"

# ------------------------------------------------------------------------------
# Parser Warnings Tests
# ------------------------------------------------------------------------------

echo -e "\n  Parser Warnings:"

# Unknown tokens should generate warnings
INPUT_UNKNOWN='Counter subclass: Object
  unknownKeyword here
  method: test [ ^ 1 ]'

run_test "unknown tokens: has warnings" "true" \
    "$(parse_field "$INPUT_UNKNOWN" '.warnings | length > 0')"

run_test "unknown tokens: warning type" "unknown_token" \
    "$(parse_field "$INPUT_UNKNOWN" '.warnings[0].type')"

run_test "unknown tokens: still parses method" "test" \
    "$(parse_field "$INPUT_UNKNOWN" '.methods[0].selector')"

# ------------------------------------------------------------------------------
# Method Body Token Preservation
# ------------------------------------------------------------------------------

echo -e "\n  Method Body Token Preservation:"

INPUT_BODY_TOKENS='Counter subclass: Object
  method: test [
    | x y |
    x := $(echo "hello")
    ^ $x
  ]'

run_test "body tokens: has PIPE" "true" \
    "$(parse_field "$INPUT_BODY_TOKENS" '.methods[0].body.tokens | any(.type == "PIPE")')"

run_test "body tokens: has ASSIGN" "true" \
    "$(parse_field "$INPUT_BODY_TOKENS" '.methods[0].body.tokens | any(.type == "ASSIGN")')"

run_test "body tokens: has CARET" "true" \
    "$(parse_field "$INPUT_BODY_TOKENS" '.methods[0].body.tokens | any(.type == "CARET")')"

run_test "body tokens: has SUBSHELL" "true" \
    "$(parse_field "$INPUT_BODY_TOKENS" '.methods[0].body.tokens | any(.type == "SUBSHELL")')"

run_test "body tokens: has VARIABLE" "true" \
    "$(parse_field "$INPUT_BODY_TOKENS" '.methods[0].body.tokens | any(.type == "VARIABLE")')"

# ------------------------------------------------------------------------------
# Error Recovery / Synchronization Tests
# ------------------------------------------------------------------------------

echo -e "\n  Error Recovery (Synchronization):"

# Test that parser skips garbage and finds the next method
INPUT_SYNC_METHOD='Counter subclass: Object
  this is garbage that should be skipped
  more garbage here too
  method: goodMethod [
    ^ 1
  ]'

run_test "sync: skips garbage to find method" "goodMethod" \
    "$(parse_field "$INPUT_SYNC_METHOD" '.methods[0].selector')"

run_test "sync: records warnings for skipped tokens" "true" \
    "$(parse_field "$INPUT_SYNC_METHOD" '.warnings | length > 0')"

# Test that parser recovers after bad method and parses next one
INPUT_SYNC_MULTI='Counter subclass: Object
  method: badMethod
  this has no body brackets
  method: goodMethod [
    ^ 1
  ]
  method: anotherGood [
    ^ 2
  ]'

run_test "sync: parses methods after bad one" "2" \
    "$(parse_field "$INPUT_SYNC_MULTI" '.methods | length')"

run_test "sync: first recovered method" "goodMethod" \
    "$(parse_field "$INPUT_SYNC_MULTI" '.methods[0].selector')"

run_test "sync: second recovered method" "anotherGood" \
    "$(parse_field "$INPUT_SYNC_MULTI" '.methods[1].selector')"

# Test that instanceVars before garbage still works
# Note: Plain identifiers after instanceVars are parsed as additional ivars
# Use actual syntax errors (operators) to test recovery
INPUT_SYNC_IVARS='Counter subclass: Object
  instanceVars: value:0
  := bad syntax here
  method: getValue [
    ^ $value
  ]'

run_test "sync: instanceVars parsed" "1" \
    "$(parse_field "$INPUT_SYNC_IVARS" '.instanceVars | length')"

run_test "sync: method parsed after bad syntax" "getValue" \
    "$(parse_field "$INPUT_SYNC_IVARS" '.methods[0].selector')"

# Test include before garbage
INPUT_SYNC_INCLUDE='Counter subclass: Object
  include: Debuggable
  random stuff
  method: test [
    ^ 1
  ]'

run_test "sync: include parsed before garbage" "Debuggable" \
    "$(parse_field "$INPUT_SYNC_INCLUDE" '.traits[0]')"

run_test "sync: method after include and garbage" "test" \
    "$(parse_field "$INPUT_SYNC_INCLUDE" '.methods[0].selector')"

# Test multiple sync points
# Use operators to ensure they're recognized as errors, not as identifiers
INPUT_SYNC_COMPLEX='Counter subclass: Object
  instanceVars: x:0
  include: Debuggable
  := syntax error
  method: first [
    ^ 1
  ]
  @@ more errors
  classMethod: second [
    ^ 2
  ]'

run_test "sync: complex - instanceVars" "1" \
    "$(parse_field "$INPUT_SYNC_COMPLEX" '.instanceVars | length')"

run_test "sync: complex - trait" "Debuggable" \
    "$(parse_field "$INPUT_SYNC_COMPLEX" '.traits[0]')"

run_test "sync: complex - method count" "2" \
    "$(parse_field "$INPUT_SYNC_COMPLEX" '.methods | length')"

run_test "sync: complex - has warnings" "true" \
    "$(parse_field "$INPUT_SYNC_COMPLEX" '.warnings | length > 0')"

# ------------------------------------------------------------------------------
# Source Location Tests
# ------------------------------------------------------------------------------

echo -e "\n  Source Locations:"

INPUT_LOCATIONS='Counter subclass: Object
  instanceVars: value:0 step:1
  include: Debuggable
  method: test [
    ^ 1
  ]
  classMethod: create [
    ^ new
  ]'

# Class location
run_test "location: class line" "1" \
    "$(parse_field "$INPUT_LOCATIONS" '.location.line')"

run_test "location: class col" "0" \
    "$(parse_field "$INPUT_LOCATIONS" '.location.col')"

# Instance variable locations
run_test "location: first ivar line" "2" \
    "$(parse_field "$INPUT_LOCATIONS" '.instanceVars[0].location.line')"

run_test "location: second ivar line" "2" \
    "$(parse_field "$INPUT_LOCATIONS" '.instanceVars[1].location.line')"

run_test "location: second ivar col > first" "true" \
    "$(parse_field "$INPUT_LOCATIONS" '.instanceVars[1].location.col > .instanceVars[0].location.col')"

# Method locations
run_test "location: method line" "4" \
    "$(parse_field "$INPUT_LOCATIONS" '.methods[0].location.line')"

run_test "location: classMethod line" "7" \
    "$(parse_field "$INPUT_LOCATIONS" '.methods[1].location.line')"

# Different methods have different locations
run_test "location: methods have different lines" "true" \
    "$(parse_field "$INPUT_LOCATIONS" '.methods[0].location.line != .methods[1].location.line')"

# Trait location
INPUT_TRAIT_LOC='Debuggable trait
  method: debug [
    echo "debug"
  ]'

run_test "location: trait line" "1" \
    "$(parse_field "$INPUT_TRAIT_LOC" '.location.line')"

run_test "location: trait method line" "2" \
    "$(parse_field "$INPUT_TRAIT_LOC" '.methods[0].location.line')"
