# ==============================================================================
# Triple-Quoted String Tests
# ==============================================================================
# Tests for '''...''' multi-line string syntax
# ==============================================================================

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
echo "'''it's working'''" > /tmp/test_quote.txt
result=$("$TOKENIZER" /tmp/test_quote.txt | jq -r '.[0].value')
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
cat > /tmp/test_triple_parse.trash << 'EOF'
TestTriple subclass: Object
  method: test [
    | x |
    x := '''hello
world'''
  ]
EOF

result=$("$DRIVER" parse /tmp/test_triple_parse.trash 2>&1 | jq -r '.methods[0].body.tokens[] | select(.type == "TRIPLESTRING") | .type')
run_test "parser sees TRIPLESTRING token" "TRIPLESTRING" "$result"

result=$("$DRIVER" parse /tmp/test_triple_parse.trash 2>&1 | jq -r '.methods[0].body.tokens[] | select(.type == "TRIPLESTRING") | .value')
run_test "parser preserves newline in value" "hello
world" "$result"

# Test triple string as instance var default
cat > /tmp/test_triple_ivar.trash << 'EOF'
TestTriple subclass: Object
  instanceVars: text:'''default
value'''
EOF

result=$("$DRIVER" parse /tmp/test_triple_ivar.trash 2>&1 | jq -r '.instanceVars[0].default.type')
run_test "instance var default type" "triplestring" "$result"

# ------------------------------------------------------------------------------
# Code Generation Tests
# ------------------------------------------------------------------------------

echo -e "\n  Code generation tests:"

# Test assignment to local variable
cat > /tmp/test_triple_codegen.trash << 'EOF'
TestTriple subclass: Object
  method: test [
    | text |
    text := '''line one
line two
line three'''
    ^ text
  ]
EOF

result=$("$DRIVER" compile /tmp/test_triple_codegen.trash 2>&1 | grep -o "text=\$'line one")
run_test "codegen uses ANSI-C quoting" "text=\$'line one" "$result"

# Count escaped newlines (looking for literal backslash-n in output)
result=$("$DRIVER" compile /tmp/test_triple_codegen.trash 2>&1 | grep 'text=' | grep -o '\\n' | wc -l | tr -d ' ')
run_test "codegen escapes newlines" "2" "$result"

# Test assignment to instance variable
cat > /tmp/test_triple_ivar_set.trash << 'EOF'
TestTriple subclass: Object
  instanceVars: data:''

  method: setData [
    data := '''multi
line'''
  ]
EOF

result=$("$DRIVER" compile /tmp/test_triple_ivar_set.trash 2>&1 | grep "_ivar_set data" | grep -o "\$'multi")
run_test "ivar assignment uses ANSI-C quoting" "\$'multi" "$result"

# Test triple string in message argument
cat > /tmp/test_triple_arg.trash << 'EOF'
TestTriple subclass: Object
  method: test [
    @ self log: '''hello
world'''
  ]
EOF

result=$("$DRIVER" compile /tmp/test_triple_arg.trash 2>&1 | grep -o "\$'hello")
run_test "message arg uses ANSI-C quoting" "\$'hello" "$result"

# Test triple string with special characters
cat > /tmp/test_triple_special.trash << 'EOF'
TestTriple subclass: Object
  method: test [
    | text |
    text := '''line with 'quotes' and $vars'''
    ^ text
  ]
EOF

# Check that single quotes are escaped with backslash in the output
result=$("$DRIVER" compile /tmp/test_triple_special.trash 2>&1 | grep "text=" | grep -c "\\\\'quotes\\\\'")
run_test "codegen escapes single quotes" "1" "$result"

# ------------------------------------------------------------------------------
# Integration Tests (Runtime)
# ------------------------------------------------------------------------------

echo -e "\n  Integration tests:"

# Test actual runtime behavior
cat > /tmp/test_triple_runtime.trash << 'EOF'
TestTripleRuntime subclass: Object
  method: getPoem [
    | text |
    text := '''roses are red
violets are blue'''
    ^ text
  ]
EOF

"$DRIVER" compile /tmp/test_triple_runtime.trash > /tmp/TestTripleRuntime.bash 2>&1
source /tmp/TestTripleRuntime.bash
result=$(__TestTripleRuntime__getPoem)
expected="roses are red
violets are blue"
run_test "runtime produces correct multi-line output" "$expected" "$result"

# Test with backslash
cat > /tmp/test_triple_backslash.trash << 'EOF'
TestBackslash subclass: Object
  method: getPath [
    | path |
    path := '''C:\Users\test'''
    ^ path
  ]
EOF

"$DRIVER" compile /tmp/test_triple_backslash.trash > /tmp/TestBackslash.bash 2>&1
source /tmp/TestBackslash.bash
result=$(__TestBackslash__getPath)
run_test "runtime handles backslashes" 'C:\Users\test' "$result"

# Cleanup
rm -f /tmp/test_triple*.trash /tmp/test_triple*.bash /tmp/TestTripleRuntime.bash /tmp/TestBackslash.bash /tmp/test_quote.txt

echo ""
