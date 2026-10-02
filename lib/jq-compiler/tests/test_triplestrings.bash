# ==============================================================================
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Triple-Quoted String Tests
# ==============================================================================
# Tests for '''...''' multi-line string syntax
# ==============================================================================

# Source shared test helper for standalone execution
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helper.bash"

ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-triplestrings.XXXXXX")
trap 'rm -rf "$scratch"; print_test_summary' EXIT

TOKENIZER="$COMPILER_DIR/tokenizer.bash"
DRIVER="$COMPILER_DIR/driver.bash"

# ------------------------------------------------------------------------------
# Tokenizer Tests
# ------------------------------------------------------------------------------

echo "Testing triple-quoted string tokenization..."

echo -e "\n  Basic tokenization:"

# Test simple triple-quoted string
result=$(printf "'''hello'''" | "$TOKENIZER" | jq -r '.[0].type')
run_test "triple-quoted string type" "TRIPLESTRING" "$result"

result=$(printf "'''hello'''" | "$TOKENIZER" | jq -r '.[0].value')
run_test "triple-quoted string value" "hello" "$result"

# Test multi-line content
result=$(printf "'''line one\nline two'''" | "$TOKENIZER" | jq -r '.[0].type')
run_test "multi-line triple string type" "TRIPLESTRING" "$result"

result=$(printf "'''line one\nline two'''" | "$TOKENIZER" | jq -r '.[0].value')
run_test "multi-line triple string value" "line one
line two" "$result"

# Test triple string with embedded single quotes
# Write to file to avoid shell escaping issues
echo "'''it's working'''" > $scratch/test_quote.txt
result=$("$TOKENIZER" $scratch/test_quote.txt | jq -r '.[0].value')
run_test "triple string with single quote" "it's working" "$result"

# Test empty triple string
result=$(printf "'''''''" | "$TOKENIZER" | jq -r '.[0].type')
run_test "empty triple string type" "TRIPLESTRING" "$result"

result=$(printf "'''''''" | "$TOKENIZER" | jq -r '.[0].value')
run_test "empty triple string value" "" "$result"

# Test triple string doesn't consume regular strings
result=$(printf "'hello' '''world'''" | "$TOKENIZER" | jq -r '.[0].type')
run_test "regular string before triple" "STRING" "$result"

result=$(printf "'hello' '''world'''" | "$TOKENIZER" | jq -r '.[1].type')
run_test "triple string after regular" "TRIPLESTRING" "$result"

# ------------------------------------------------------------------------------
# Parser Tests
# ------------------------------------------------------------------------------

echo -e "\n  Parser tests:"

# Test triple string in assignment
cat > $scratch/test_triple_parse.trash << 'EOF'
TestTriple subclass: Object
  method: test [
    | x |
    x := '''hello
world'''
  ]
EOF

result=$("$DRIVER" parse $scratch/test_triple_parse.trash 2>&1 | jq -r '.class.methods[0].body.tokens[] | select(.type == "TRIPLESTRING") | .type')
run_test "parser sees TRIPLESTRING token" "TRIPLESTRING" "$result"

result=$("$DRIVER" parse $scratch/test_triple_parse.trash 2>&1 | jq -r '.class.methods[0].body.tokens[] | select(.type == "TRIPLESTRING") | .value')
run_test "parser preserves newline in value" "hello
world" "$result"

# Test triple string as instance var default
cat > $scratch/test_triple_ivar.trash << 'EOF'
TestTriple subclass: Object
  instanceVars: text:'''default
value'''
EOF

result=$("$DRIVER" parse $scratch/test_triple_ivar.trash 2>&1 | jq -r '.class.instanceVars[0].default.type')
run_test "instance var default type" "triplestring" "$result"

# ------------------------------------------------------------------------------
# Code Generation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Code generation tests:"

# Generated Bash must keep the text literal in every position: newlines and
# quotes survive, and `$` is never expanded.
cat > $scratch/test_triple_codegen.trash << 'EOF'
TestTripleCodegen subclass: Object
  instanceVars: data:''

  method: local [
    | text |
    text := '''line one
line two
line three'''
    ^ text
  ]

  method: setData [
    data := '''multi
line $HOME'''
    ^ data
  ]

  method: arg [
    ^ @ self echo: '''hello
world $(echo INJECTED)'''
  ]

  method: echo: x [ ^ x ]

  method: special [
    | text |
    text := '''line with 'quotes' and $vars'''
    ^ text
  ]
EOF

"$DRIVER" compile $scratch/test_triple_codegen.trash > "$TRASHDIR/.compiled/TestTripleCodegen"
# A subshell keeps the runtime's own EXIT trap away from this file's summary.
send_codegen() { (source "$ROOT/lib/trash.bash" && id=$(@ TestTripleCodegen new) && @ "$id" "$@"); }

result=$(send_codegen local)
run_test "local assignment keeps newlines" "line one
line two
line three" "$result"

result=$(send_codegen setData)
run_test "ivar assignment keeps newlines and \$" "multi
line \$HOME" "$result"

result=$(send_codegen arg)
run_test "message arg is literal" "hello
world \$(echo INJECTED)" "$result"

result=$(send_codegen special)
run_test "single quotes and \$ survive" "line with 'quotes' and \$vars" "$result"

# ------------------------------------------------------------------------------
# Integration Tests (Runtime)
# ------------------------------------------------------------------------------

echo -e "\n  Integration tests:"

# Test actual runtime behavior
cat > $scratch/test_triple_runtime.trash << 'EOF'
TestTripleRuntime subclass: Object
  method: getPoem [
    | text |
    text := '''roses are red
violets are blue'''
    ^ text
  ]
EOF

"$DRIVER" compile $scratch/test_triple_runtime.trash > $scratch/TestTripleRuntime.bash 2>&1
source $scratch/TestTripleRuntime.bash
result=$(__TestTripleRuntime__getPoem)
expected="roses are red
violets are blue"
run_test "runtime produces correct multi-line output" "$expected" "$result"

# Test with backslash
cat > $scratch/test_triple_backslash.trash << 'EOF'
TestBackslash subclass: Object
  method: getPath [
    | path |
    path := '''C:\Users\test'''
    ^ path
  ]
EOF

"$DRIVER" compile $scratch/test_triple_backslash.trash > $scratch/TestBackslash.bash 2>&1
source $scratch/TestBackslash.bash
result=$(__TestBackslash__getPath)
run_test "runtime handles backslashes" 'C:\Users\test' "$result"


echo ""
