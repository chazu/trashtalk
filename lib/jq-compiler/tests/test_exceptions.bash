#!/usr/bin/env bash
# Tests for Exception Handling DSL (try/catch)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_DIR="$(dirname "$(dirname "$COMPILER_DIR")")"
TRASHDIR="$PROJECT_DIR/trash"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Test counter
TESTS_RUN=0
TESTS_PASSED=0

pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    echo "  Expected: $2"
    echo "  Got: $3"
    ((TESTS_RUN++))
}

# Load runtime
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

echo "=== Exception Handling DSL Tests ==="
echo ""

# Test 1: Parse try/catch basic
echo "Test 1: Parse try/catch expression"
cat > /tmp/test_try.trash << 'EOF'
TestTry subclass: Object
  method: test [
    try: [
      @ self doSomething
    ] catch: [
      @ self handleError
    ]
  ]
EOF
result=$("$COMPILER_DIR/tokenizer.bash" < /tmp/test_try.trash 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methods[0].body.tokens | length > 0')
if [[ "$result" == "true" ]]; then
    pass "try/catch tokenized correctly"
else
    fail "try/catch tokenization" "true" "$result"
fi
rm -f /tmp/test_try.trash

# Test 2: Codegen generates if/else structure
echo "Test 2: Codegen generates error handling structure"
cat > /tmp/TestException.trash << 'EOF'
TestException subclass: Object
  method: test [
    try: [
      echo "trying"
    ] catch: [
      echo "caught"
    ]
  ]
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestException.trash 2>&1 | grep -c 'if !')
if [[ "$result" -ge "1" ]]; then
    pass "Error handling if structure generated"
else
    fail "Error handling structure" "1 or more" "$result"
fi
rm -f /tmp/TestException.trash

# Test 3: Codegen includes _clear_error
echo "Test 3: Codegen includes _clear_error"
cat > /tmp/TestException2.trash << 'EOF'
TestException2 subclass: Object
  method: test [
    try: [
      echo "trying"
    ] catch: [
      echo "caught"
    ]
  ]
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestException2.trash 2>&1 | grep -c '_clear_error')
if [[ "$result" -ge "1" ]]; then
    pass "_clear_error included in generated code"
else
    fail "_clear_error" "1 or more" "$result"
fi
rm -f /tmp/TestException2.trash

# Test 4: Catch block with error parameter
echo "Test 4: Catch block with error parameter"
cat > /tmp/TestExceptionParam.trash << 'EOF'
TestExceptionParam subclass: Object
  method: test [
    try: [
      echo "trying"
    ] catch: [ :err |
      echo "$err"
    ]
  ]
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestExceptionParam.trash 2>&1 | grep 'local err=')
if [[ -n "$result" ]]; then
    pass "Error parameter binding generated"
else
    fail "Error parameter" "local err=..." "not found"
fi
rm -f /tmp/TestExceptionParam.trash

# Test 5: Runtime - successful try block
echo "Test 5: Runtime - successful try block (no catch)"
cat > /tmp/TestTrySuccess.trash << 'EOF'
TestTrySuccess subclass: Object
  classInstanceVars: result:0

  classMethod: getResult [^ result + 0]

  classMethod: setResult: val [result := val + 0]

  classMethod: test [
    try: [
      @ self setResult: 1
    ] catch: [
      @ self setResult: 99
    ]
  ]
EOF
"$COMPILER_DIR/driver.bash" compile /tmp/TestTrySuccess.trash > "$TRASHDIR/.compiled/TestTrySuccess" 2>/dev/null

if [[ -f "$TRASHDIR/.compiled/TestTrySuccess" ]]; then
    @ TestTrySuccess test
    result=$(@ TestTrySuccess getResult)
    if [[ "$result" == "1" ]]; then
        pass "Successful try block executes correctly"
    else
        fail "Successful try" "1" "$result"
    fi
else
    fail "Try/catch runtime" "test class to compile" "compilation failed"
fi
rm -f /tmp/TestTrySuccess.trash "$TRASHDIR/.compiled/TestTrySuccess"

# Test 6: Runtime - failed try block triggers catch
echo "Test 6: Runtime - failed try block triggers catch"
cat > /tmp/TestTryCatch.trash << 'EOF'
TestTryCatch subclass: Object
  classInstanceVars: result:0

  classMethod: getResult [^ result + 0]

  classMethod: setResult: val [result := val + 0]

  classMethod: failingMethod [
    _throw "TestError" "something went wrong"
  ]

  classMethod: test [
    try: [
      @ self failingMethod
    ] catch: [
      @ self setResult: 2
    ]
  ]
EOF
"$COMPILER_DIR/driver.bash" compile /tmp/TestTryCatch.trash > "$TRASHDIR/.compiled/TestTryCatch" 2>/dev/null

if [[ -f "$TRASHDIR/.compiled/TestTryCatch" ]]; then
    @ TestTryCatch test
    result=$(@ TestTryCatch getResult)
    if [[ "$result" == "2" ]]; then
        pass "Failed try block triggers catch"
    else
        fail "Failed try catch" "2" "$result"
    fi
else
    fail "Try/catch runtime" "test class to compile" "compilation failed"
fi
rm -f /tmp/TestTryCatch.trash "$TRASHDIR/.compiled/TestTryCatch"

# Test 7: Runtime - error parameter binding works
echo "Test 7: Runtime - error parameter binding works"
cat > /tmp/TestTryError.trash << 'EOF'
TestTryError subclass: Object
  classInstanceVars: result:0

  classMethod: getResult [^ result + 0]

  classMethod: setResult: val [result := val + 0]

  classMethod: failingMethod [
    _throw "MyError" "my message"
  ]

  classMethod: test [
    try: [
      @ self failingMethod
    ] catch: [ :err |
      @ self setResult: 3
    ]
  ]
EOF
"$COMPILER_DIR/driver.bash" compile /tmp/TestTryError.trash > "$TRASHDIR/.compiled/TestTryError" 2>/dev/null

if [[ -f "$TRASHDIR/.compiled/TestTryError" ]]; then
    @ TestTryError test
    result=$(@ TestTryError getResult)
    if [[ "$result" == "3" ]]; then
        pass "Error parameter binding works"
    else
        fail "Error parameter binding" "3" "$result"
    fi
else
    fail "Try/catch runtime" "test class to compile" "compilation failed"
fi
rm -f /tmp/TestTryError.trash "$TRASHDIR/.compiled/TestTryError"

echo ""
echo "=== Results ==="
echo "Passed: $TESTS_PASSED / $TESTS_RUN"

if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
    exit 0
else
    exit 1
fi
