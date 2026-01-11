#!/usr/bin/env bash
# ==============================================================================
# Namespace Tests
# ==============================================================================
# Tests for namespace parsing - package declarations, imports, and qualified refs
# ==============================================================================

# Source shared test helper for standalone execution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.bash"

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
# Package Declaration Tests
# ------------------------------------------------------------------------------

echo -e "\n  Package Declaration:"

INPUT_PKG='package: MyApp

Counter subclass: Object'

run_test "package name" "MyApp" \
    "$(parse_field "$INPUT_PKG" '.package')"

run_test "class name with package" "Counter" \
    "$(parse_field "$INPUT_PKG" '.name')"

run_test "empty imports" "0" \
    "$(parse_field "$INPUT_PKG" '.imports | length')"

# ------------------------------------------------------------------------------
# Import Declaration Tests
# ------------------------------------------------------------------------------

echo -e "\n  Import Declaration:"

INPUT_IMPORTS='package: MyApp
  import: Logging
  import: DataStructures

Counter subclass: Object'

run_test "package with imports" "MyApp" \
    "$(parse_field "$INPUT_IMPORTS" '.package')"

run_test "import count" "2" \
    "$(parse_field "$INPUT_IMPORTS" '.imports | length')"

run_test "first import" "Logging" \
    "$(parse_field "$INPUT_IMPORTS" '.imports[0]')"

run_test "second import" "DataStructures" \
    "$(parse_field "$INPUT_IMPORTS" '.imports[1]')"

# ------------------------------------------------------------------------------
# Backward Compatibility Tests
# ------------------------------------------------------------------------------

echo -e "\n  Backward Compatibility:"

INPUT_NO_PKG='Counter subclass: Object
  instanceVars: value:0'

run_test "no package = null" "null" \
    "$(parse_field "$INPUT_NO_PKG" '.package')"

run_test "no imports = empty array" "0" \
    "$(parse_field "$INPUT_NO_PKG" '.imports | length')"

run_test "class still parses" "Counter" \
    "$(parse_field "$INPUT_NO_PKG" '.name')"

# ------------------------------------------------------------------------------
# Qualified Class Reference Tests
# ------------------------------------------------------------------------------

echo -e "\n  Qualified Class References:"

INPUT_QUAL_PARENT='package: MyApp

Counter subclass: Core::Object'

run_test "qualified parent" "Core::Object" \
    "$(parse_field "$INPUT_QUAL_PARENT" '.parent')"

run_test "parent package" "Core" \
    "$(parse_field "$INPUT_QUAL_PARENT" '.parentPackage')"

INPUT_UNQUAL_PARENT='Counter subclass: Object'

run_test "unqualified parent" "Object" \
    "$(parse_field "$INPUT_UNQUAL_PARENT" '.parent')"

run_test "unqualified parent package" "null" \
    "$(parse_field "$INPUT_UNQUAL_PARENT" '.parentPackage')"

# ------------------------------------------------------------------------------
# Qualified Trait Reference Tests
# ------------------------------------------------------------------------------

echo -e "\n  Qualified Trait References:"

INPUT_QUAL_TRAIT='Counter subclass: Object
  include: Utils::Debuggable'

run_test "qualified trait" "Utils::Debuggable" \
    "$(parse_field "$INPUT_QUAL_TRAIT" '.traits[0]')"

INPUT_UNQUAL_TRAIT='Counter subclass: Object
  include: Debuggable'

run_test "unqualified trait" "Debuggable" \
    "$(parse_field "$INPUT_UNQUAL_TRAIT" '.traits[0]')"

# ------------------------------------------------------------------------------
# Tokenizer :: Tests
# ------------------------------------------------------------------------------

echo -e "\n  Namespace Separator Tokenization:"

# Test that :: is tokenized correctly
test_tokens() {
    local input="$1"
    echo "$input" | "$TOKENIZER"
}

TOKENS=$(test_tokens "Core::Object")
run_test ":: tokenization - Core" 'IDENTIFIER' \
    "$(echo "$TOKENS" | jq -r '.[0].type')"

run_test ":: tokenization - separator" 'NAMESPACE_SEP' \
    "$(echo "$TOKENS" | jq -r '.[1].type')"

run_test ":: tokenization - Object" 'IDENTIFIER' \
    "$(echo "$TOKENS" | jq -r '.[2].type')"

# Make sure single colons still work for keywords
TOKENS=$(test_tokens "method: foo")
run_test "single colon keyword" 'KEYWORD' \
    "$(echo "$TOKENS" | jq -r '.[0].type')"

# ------------------------------------------------------------------------------
# Codegen Namespace Tests
# ------------------------------------------------------------------------------

echo -e "\n  Codegen Namespacing:"

CODEGEN="$COMPILER_DIR/codegen.jq"

# Helper to compile and get specific line
compile_line() {
    local input="$1"
    local grep_pattern="$2"
    echo "$input" | "$TOKENIZER" | jq -f "$PARSER" | jq -f "$CODEGEN" -r | grep -m1 "$grep_pattern" || echo ""
}

# Helper to compile to bash
compile() {
    echo "$1" | "$TOKENIZER" | jq -f "$PARSER" | jq -f "$CODEGEN" -r
}

INPUT_NS='package: MyApp

Counter subclass: Object
  instanceVars: value:0

  method: increment [
    value := value + 1
  ]'

run_test "namespaced superclass var" '__MyApp__Counter__superclass' \
    "$(compile "$INPUT_NS" | grep -o '__MyApp__Counter__superclass')"

run_test "namespaced package metadata" '__MyApp__Counter__package="MyApp"' \
    "$(compile "$INPUT_NS" | grep '__MyApp__Counter__package' | head -1)"

run_test "namespaced qualified name" '__MyApp__Counter__qualifiedName="MyApp::Counter"' \
    "$(compile "$INPUT_NS" | grep '__MyApp__Counter__qualifiedName' | head -1)"

run_test "namespaced method function" '__MyApp__Counter__increment()' \
    "$(compile "$INPUT_NS" | grep -o '__MyApp__Counter__increment()')"

# Test that non-namespaced code stays backward compatible
INPUT_NO_NS='Counter subclass: Object
  instanceVars: value:0

  method: increment [
    value := value + 1
  ]'

run_test "non-namespaced superclass var" '__Counter__superclass' \
    "$(compile "$INPUT_NO_NS" | grep -o '__Counter__superclass')"

run_test "non-namespaced no package metadata" '' \
    "$(compile "$INPUT_NO_NS" | grep '__Counter__package' || echo '')"

run_test "non-namespaced method function" '__Counter__increment()' \
    "$(compile "$INPUT_NO_NS" | grep -o '__Counter__increment()')"
