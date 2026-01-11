#!/usr/bin/env bash
# ==============================================================================
# Tokenizer Tests
# ==============================================================================
# Tests for tokenizer.bash - validates token types and values
# ==============================================================================

# Source shared test helper for standalone execution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.bash"

TOKENIZER="$COMPILER_DIR/tokenizer.bash"

# Helper to get token type for input (using printf to avoid echo issues)
get_token_type() {
    printf '%s\n' "$1" | "$TOKENIZER" | jq -r '.[0].type // "null"'
}

# Helper to get token value for input
get_token_value() {
    printf '%s\n' "$1" | "$TOKENIZER" | jq -r '.[0].value // "null"'
}

# Helper to get all token types as comma-separated list (excluding NEWLINE)
get_all_types() {
    printf '%s\n' "$1" | "$TOKENIZER" | jq -r '[.[] | select(.type != "NEWLINE") | .type] | join(",")'
}

# Helper to count tokens (excluding NEWLINE)
count_tokens() {
    printf '%s\n' "$1" | "$TOKENIZER" | jq '[.[] | select(.type != "NEWLINE")] | length'
}

echo "Testing basic token types..."

# ------------------------------------------------------------------------------
# Identifier Tests
# ------------------------------------------------------------------------------

echo -e "\n  Identifiers:"
run_test "simple identifier" "IDENTIFIER" "$(get_token_type 'Counter')"
run_test "identifier with underscore" "IDENTIFIER" "$(get_token_type '_private')"
run_test "identifier with numbers" "IDENTIFIER" "$(get_token_type 'var123')"
run_test "identifier value" "myVariable" "$(get_token_value 'myVariable')"

# ------------------------------------------------------------------------------
# Keyword Tests
# ------------------------------------------------------------------------------

echo -e "\n  Keywords:"
run_test "keyword type" "KEYWORD" "$(get_token_type 'method:')"
run_test "keyword value" "subclass:" "$(get_token_value 'subclass:')"
run_test "keyword setValue:" "setValue:" "$(get_token_value 'setValue:')"

# ------------------------------------------------------------------------------
# Number Tests
# ------------------------------------------------------------------------------

echo -e "\n  Numbers:"
run_test "integer" "NUMBER" "$(get_token_type '42')"
run_test "negative integer" "NUMBER" "$(get_token_type '-5')"
run_test "decimal" "NUMBER" "$(get_token_type '3.14')"
run_test "number value" "42" "$(get_token_value '42')"
run_test "negative value" "-5" "$(get_token_value '-5')"

# ------------------------------------------------------------------------------
# String Tests
# ------------------------------------------------------------------------------

echo -e "\n  Strings:"
run_test "single-quoted string" "STRING" "$(get_token_type "'hello'")"
run_test "string value" "'hello'" "$(get_token_value "'hello'")"
run_test "double-quoted string" "DSTRING" "$(get_token_type '"world"')"
run_test "dstring value" '"world"' "$(get_token_value '"world"')"

# ------------------------------------------------------------------------------
# Bracket Tests
# ------------------------------------------------------------------------------

echo -e "\n  Brackets:"
run_test "left bracket" "LBRACKET" "$(get_token_type '[')"
run_test "right bracket" "RBRACKET" "$(get_token_type ']')"
run_test "double left bracket" "DLBRACKET" "$(get_token_type '[[')"
run_test "double right bracket" "DRBRACKET" "$(get_token_type ']]')"

# ------------------------------------------------------------------------------
# Operator Tests
# ------------------------------------------------------------------------------

echo -e "\n  Operators:"
run_test "pipe" "PIPE" "$(get_token_type '|')"
run_test "caret" "CARET" "$(get_token_type '^')"
run_test "at sign" "AT" "$(get_token_type '@')"
run_test "assign :=" "ASSIGN" "$(get_token_type ':=')"
run_test "semicolon" "SEMI" "$(get_token_type ';')"
run_test "equals" "EQUALS" "$(get_token_type '=')"

# ------------------------------------------------------------------------------
# Boolean Operator Tests
# ------------------------------------------------------------------------------

echo -e "\n  Boolean Operators:"
run_test "double ampersand" "AND" "$(get_token_type '&&')"
run_test "double pipe" "OR" "$(get_token_type '||')"
run_test "bang" "BANG" "$(get_token_type '!')"

# ------------------------------------------------------------------------------
# Comparison Operator Tests
# ------------------------------------------------------------------------------

echo -e "\n  Comparison Operators:"
run_test "greater than" "GT" "$(get_token_type '>')"
run_test "less than" "LT" "$(get_token_type '<')"
run_test "double equals" "EQ" "$(get_token_type '==')"
run_test "not equals" "NE" "$(get_token_type '!=')"
run_test "regex match" "MATCH" "$(get_token_type '=~')"

# ------------------------------------------------------------------------------
# Redirect Tests
# ------------------------------------------------------------------------------

echo -e "\n  Redirects:"
run_test "redirect append" "REDIRECT" "$(get_token_type '>>')"
run_test "redirect stderr" "REDIRECT" "$(get_token_type '>&')"
run_test "redirect value" ">>" "$(get_token_value '>>')"

# ------------------------------------------------------------------------------
# Variable Tests
# ------------------------------------------------------------------------------

echo -e "\n  Variables:"
run_test "simple variable" "VARIABLE" "$(get_token_type '$var')"
run_test "variable value" '$var' "$(get_token_value '$var')"
run_test "numbered variable" "VARIABLE" "$(get_token_type '$1')"
run_test "braced variable" "VARIABLE" "$(get_token_type '${var}')"

# ------------------------------------------------------------------------------
# Subshell Tests
# ------------------------------------------------------------------------------

echo -e "\n  Subshells:"
run_test "subshell type" "SUBSHELL" "$(get_token_type '$(echo hello)')"
run_test "subshell value" '$(echo hello)' "$(get_token_value '$(echo hello)')"
run_test "nested subshell" '$(echo $(pwd))' "$(get_token_value '$(echo $(pwd))')"

# ------------------------------------------------------------------------------
# Arithmetic Tests
# ------------------------------------------------------------------------------

echo -e "\n  Arithmetic:"
run_test "arithmetic type" "ARITHMETIC" "$(get_token_type '$((1 + 2))')"
run_test "arithmetic value" '$((1 + 2))' "$(get_token_value '$((1 + 2))')"

# ------------------------------------------------------------------------------
# Special Character Tests
# ------------------------------------------------------------------------------

echo -e "\n  Special Characters:"
run_test "slash" "SLASH" "$(get_token_type '/')"
run_test "question mark" "QUESTION" "$(get_token_type '?')"
run_test "plus" "PLUS" "$(get_token_type '+')"
run_test "star" "STAR" "$(get_token_type '*')"
run_test "dot" "DOT" "$(get_token_type '.')"

# ------------------------------------------------------------------------------
# Multi-token Tests
# ------------------------------------------------------------------------------

echo -e "\n  Multi-token sequences:"
run_test "method declaration tokens" "KEYWORD,IDENTIFIER,LBRACKET" \
    "$(get_all_types 'method: foo [')"

run_test "assignment tokens" "IDENTIFIER,ASSIGN,NUMBER" \
    "$(get_all_types 'x := 5')"

run_test "message send tokens" "AT,IDENTIFIER,IDENTIFIER" \
    "$(get_all_types '@ self getValue')"

run_test "local vars tokens" "PIPE,IDENTIFIER,IDENTIFIER,PIPE" \
    "$(get_all_types '| x y |')"

run_test "conditional tokens" "DLBRACKET,VARIABLE,EQ,DSTRING,DRBRACKET" \
    "$(get_all_types '[[ $x == "test" ]]')"

# ------------------------------------------------------------------------------
# Comment Tests
# ------------------------------------------------------------------------------

echo -e "\n  Comments:"
run_test "comment tokenized" "1" "$(count_tokens '# this is a comment')"
run_test "comment type" "COMMENT" "$(get_token_type '# this is a comment')"
run_test "code after comment" "IDENTIFIER" \
    "$(printf '# comment\nfoo\n' | "$TOKENIZER" | jq -r '[.[] | select(.type != "NEWLINE" and .type != "COMMENT")][0].type')"

# ------------------------------------------------------------------------------
# Symbol and Collection Literal Tests
# ------------------------------------------------------------------------------

echo -e "\n  Symbols and Collection Literals:"
run_test "symbol type" "SYMBOL" "$(get_token_type '#foo')"
run_test "symbol value" "foo" "$(get_token_value '#foo')"
run_test "symbol with underscore" "testValue" "$(get_token_value '#testValue')"
run_test "symbol with numbers" "test123" "$(get_token_value '#test123')"
run_test "array literal start" "HASH_LPAREN" "$(get_token_type '#(')"
run_test "dict literal start" "HASH_LBRACE" "$(get_token_type '#{')"
run_test "symbol in assignment" "IDENTIFIER,ASSIGN,SYMBOL" \
    "$(get_all_types 'x := #active')"
run_test "array literal tokens" "HASH_LPAREN,NUMBER,NUMBER,RPAREN" \
    "$(get_all_types '#(1 2)')"
run_test "dict literal tokens" "HASH_LBRACE,KEYWORD,NUMBER,RBRACE" \
    "$(get_all_types '#{a: 1}')"
run_test "brace type" "LBRACE" "$(get_token_type '{')"
run_test "rbrace type" "RBRACE" "$(get_token_type '}')"

# ------------------------------------------------------------------------------
# Edge Cases
# ------------------------------------------------------------------------------

echo -e "\n  Edge Cases:"
run_test "empty input" "0" "$(count_tokens '')"
run_test "whitespace only" "0" "$(count_tokens '   ')"
run_test "keyword vs identifier" "IDENTIFIER,KEYWORD" \
    "$(get_all_types 'foo bar:')"
