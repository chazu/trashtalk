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

run_test "keyword method selector" "setValue" \
    "$(parse_field "$INPUT_KEYWORD" '.methods[0].selector')"

run_test "keyword method arg" "newVal" \
    "$(parse_field "$INPUT_KEYWORD" '.methods[0].args[0]')"

INPUT_MULTI_KEYWORD='Store subclass: Object
  method: getField: id field: name [
    ^ 1
  ]'

run_test "multi-keyword selector" "getField_field" \
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
