#!/usr/bin/env bash
# Tests for Method Categories

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

echo "=== Method Categories Tests ==="
echo ""

# Test 1: Parse category directive
echo "Test 1: Parse category directive"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  category: "accessing"
  method: foo [
    ^ 1
  ]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methods[0].category')
if [[ "$result" == "accessing" ]]; then
    pass "Category directive parsed correctly"
else
    fail "Category parsing" "accessing" "$result"
fi

# Test 2: Multiple categories
echo "Test 2: Multiple categories"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  category: "first"
  method: foo [^ 1]

  category: "second"
  method: bar [^ 2]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '[.methods[].category] | join(",")')
if [[ "$result" == "first,second" ]]; then
    pass "Multiple categories parsed correctly"
else
    fail "Multiple categories" "first,second" "$result"
fi

# Test 3: Methods inherit current category
echo "Test 3: Methods inherit current category"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  category: "utils"
  method: foo [^ 1]
  method: bar [^ 2]
  method: baz [^ 3]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '[.methods[].category] | unique | .[0]')
if [[ "$result" == "utils" ]]; then
    pass "Methods inherit current category"
else
    fail "Category inheritance" "utils" "$result"
fi

# Test 4: Methods before any category have no category
echo "Test 4: Methods before any category have no category"
result=$("$COMPILER_DIR/tokenizer.bash" <<< 'Test subclass: Object
  method: nocat [^ 1]

  category: "utils"
  method: withcat [^ 2]' 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methods[0].category // "null"')
if [[ "$result" == "null" ]]; then
    pass "Uncategorized methods have no category"
else
    fail "No category" "null" "$result"
fi

# Test 5: Codegen generates methodCategories metadata
echo "Test 5: Codegen generates methodCategories metadata"
cat > /tmp/TestCat.trash << 'EOF'
TestCat subclass: Object
  category: "accessing"
  method: getValue [^ 1]
  method: setValue: v [^ 2]
EOF
result=$("$COMPILER_DIR/driver.bash" compile /tmp/TestCat.trash 2>&1 | grep '__TestCat__methodCategories')
# Note: keyword selectors compile with underscores, so setValue: becomes setValue_
expected='__TestCat__methodCategories="getValue:accessing setValue_:accessing"'
if [[ "$result" == "$expected" ]]; then
    pass "methodCategories metadata generated correctly"
else
    fail "methodCategories metadata" "$expected" "$result"
fi
rm -f /tmp/TestCat.trash

# Create a test class for runtime tests
cat > /tmp/TestCatRuntime.trash << 'EOF'
TestCatRuntime subclass: Object
  instanceVars: value:0

  category: "accessing"
  method: getValue [^ $(_ivar value)]
  method: setValue: val [_ivar_set value "$val"]

  category: "arithmetic"
  method: increment [| v | v := $(( $(_ivar value) + 1 )). _ivar_set value "$v"]
EOF

# Compile the test class
"$COMPILER_DIR/driver.bash" compile /tmp/TestCatRuntime.trash > "$TRASHDIR/.compiled/TestCatRuntime" 2>/dev/null

# Test 6: Runtime categoriesFor works
echo "Test 6: Runtime categoriesFor works"
if [[ -f "$TRASHDIR/.compiled/TestCatRuntime" ]]; then
    result=$(@ Trash categoriesFor: TestCatRuntime 2>&1 | grep -E "^ " | tr -d ' ' | sort | tr '\n' ',')
    expected="accessing,arithmetic,"
    if [[ "$result" == "$expected" ]]; then
        pass "categoriesFor returns correct categories"
    else
        fail "categoriesFor" "$expected" "$result"
    fi
else
    fail "categoriesFor" "test class to compile" "compilation failed"
fi

# Test 7: Runtime methodsIn_category works
echo "Test 7: Runtime methodsIn_category works"
if [[ -f "$TRASHDIR/.compiled/TestCatRuntime" ]]; then
    result=$(@ Trash methodsIn: TestCatRuntime category: accessing 2>&1 | grep -E "^ " | tr -d ' ' | sort | tr '\n' ',')
    expected="getValue,setValue_,"
    if [[ "$result" == "$expected" ]]; then
        pass "methodsIn_category returns correct methods"
    else
        fail "methodsIn_category" "$expected" "$result"
    fi
else
    fail "methodsIn_category" "test class to compile" "compilation failed"
fi

# Cleanup test files
rm -f /tmp/TestCatRuntime.trash "$TRASHDIR/.compiled/TestCatRuntime"

# Test 8: Category with single quotes
echo "Test 8: Category with single quotes"
result=$("$COMPILER_DIR/tokenizer.bash" <<< "Test subclass: Object
  category: 'single'
  method: foo [^ 1]" 2>/dev/null | jq -f "$COMPILER_DIR/parser.jq" 2>/dev/null | jq -r '.methods[0].category')
if [[ "$result" == "single" ]]; then
    pass "Single-quoted category parsed correctly"
else
    fail "Single-quoted category" "single" "$result"
fi

echo ""
echo "=== Results ==="
echo "Passed: $TESTS_PASSED / $TESTS_RUN"

if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
    exit 0
else
    exit 1
fi
