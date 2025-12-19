#!/opt/homebrew/bin/bash

# Test instance variable inheritance with modern .trash syntax
# Usage: bash tests/test_instance_var_inheritance.bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

PASSED=0
FAILED=0

pass() {
    echo "  PASS: $1"
    ((PASSED++)) || true
}

fail() {
    echo "  FAIL: $1"
    echo "    Expected: $2"
    echo "    Got: $3"
    ((FAILED++)) || true
}

cleanup_test_classes() {
    rm -f "$PROJECT_DIR/trash/TestParent.trash" "$PROJECT_DIR/trash/TestChild.trash"
    rm -f "$PROJECT_DIR/trash/TestGrandparent.trash" "$PROJECT_DIR/trash/TestParent2.trash" "$PROJECT_DIR/trash/TestChild2.trash"
    rm -f "$PROJECT_DIR/trash/TestBase.trash" "$PROJECT_DIR/trash/TestDerived.trash"
    rm -f "$PROJECT_DIR/trash/.compiled/TestParent" "$PROJECT_DIR/trash/.compiled/TestChild"
    rm -f "$PROJECT_DIR/trash/.compiled/TestGrandparent" "$PROJECT_DIR/trash/.compiled/TestParent2" "$PROJECT_DIR/trash/.compiled/TestChild2"
    rm -f "$PROJECT_DIR/trash/.compiled/TestBase" "$PROJECT_DIR/trash/.compiled/TestDerived"
    rm -f "$PROJECT_DIR/trash/TestParent" "$PROJECT_DIR/trash/TestChild"
    rm -f "$PROJECT_DIR/trash/TestGrandparent" "$PROJECT_DIR/trash/TestParent2" "$PROJECT_DIR/trash/TestChild2"
    rm -f "$PROJECT_DIR/trash/TestBase" "$PROJECT_DIR/trash/TestDerived"
}

# Clean up before starting
cleanup_test_classes

# Clean database
rm -f "$PROJECT_DIR/instances.db"
rm -f ~/.trashtalk/instances.db

# Source trash system
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

echo "=== Test: Instance Variable Inheritance ==="
echo ""

# ==========================================
echo "1. Basic Single-Level Inheritance"
# ==========================================

# Create parent class
cat > "$PROJECT_DIR/trash/TestParent.trash" << 'EOF'
TestParent subclass: Object
  instanceVars: parentVar:100

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

# Create child class
cat > "$PROJECT_DIR/trash/TestChild.trash" << 'EOF'
TestChild subclass: TestParent
  instanceVars: childVar:200

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

# Compile test classes
"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestParent.trash" > "$PROJECT_DIR/trash/.compiled/TestParent" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestParent" "$PROJECT_DIR/trash/TestParent"
"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestChild.trash" > "$PROJECT_DIR/trash/.compiled/TestChild" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestChild" "$PROJECT_DIR/trash/TestChild"

child=$(@ TestChild new)
parent_val=$(@ $child getParentVar)
child_val=$(@ $child getChildVar)

[[ "$parent_val" == "100" ]] && pass "Child inherits parentVar=100" || fail "Inherited parentVar" "100" "$parent_val"
[[ "$child_val" == "200" ]] && pass "Child has own childVar=200" || fail "Own childVar" "200" "$child_val"

@ $child delete 2>/dev/null

echo ""

# ==========================================
echo "2. Multi-Level Inheritance (3 levels)"
# ==========================================

cat > "$PROJECT_DIR/trash/TestGrandparent.trash" << 'EOF'
TestGrandparent subclass: Object
  instanceVars: gpVar:1

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

cat > "$PROJECT_DIR/trash/TestParent2.trash" << 'EOF'
TestParent2 subclass: TestGrandparent
  instanceVars: pVar:2

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

cat > "$PROJECT_DIR/trash/TestChild2.trash" << 'EOF'
TestChild2 subclass: TestParent2
  instanceVars: cVar:3

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestGrandparent.trash" > "$PROJECT_DIR/trash/.compiled/TestGrandparent" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestGrandparent" "$PROJECT_DIR/trash/TestGrandparent"
"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestParent2.trash" > "$PROJECT_DIR/trash/.compiled/TestParent2" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestParent2" "$PROJECT_DIR/trash/TestParent2"
"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestChild2.trash" > "$PROJECT_DIR/trash/.compiled/TestChild2" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestChild2" "$PROJECT_DIR/trash/TestChild2"

child2=$(@ TestChild2 new)
gp_val=$(@ $child2 getGpVar)
p_val=$(@ $child2 getPVar)
c_val=$(@ $child2 getCVar)

[[ "$gp_val" == "1" ]] && pass "3-level: grandparent gpVar=1" || fail "gpVar" "1" "$gp_val"
[[ "$p_val" == "2" ]] && pass "3-level: parent pVar=2" || fail "pVar" "2" "$p_val"
[[ "$c_val" == "3" ]] && pass "3-level: child cVar=3" || fail "cVar" "3" "$c_val"

@ $child2 delete 2>/dev/null

echo ""

# ==========================================
echo "3. Child Overrides Parent Default"
# ==========================================

cat > "$PROJECT_DIR/trash/TestBase.trash" << 'EOF'
TestBase subclass: Object
  instanceVars: value:10

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

cat > "$PROJECT_DIR/trash/TestDerived.trash" << 'EOF'
TestDerived subclass: TestBase
  instanceVars: value:99

  rawClassMethod: new [
    local id
    id=$(_generate_instance_id "$_CLASS")
    _create_instance "$_CLASS" "$id"
    echo "$id"
  ]
EOF

"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestBase.trash" > "$PROJECT_DIR/trash/.compiled/TestBase" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestBase" "$PROJECT_DIR/trash/TestBase"
"$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestDerived.trash" > "$PROJECT_DIR/trash/.compiled/TestDerived" 2>/dev/null
cp "$PROJECT_DIR/trash/.compiled/TestDerived" "$PROJECT_DIR/trash/TestDerived"

derived=$(@ TestDerived new)
val=$(@ $derived getValue)

[[ "$val" == "99" ]] && pass "Child overrides parent default (value=99)" || fail "Override" "99" "$val"

@ $derived delete 2>/dev/null

echo ""

# ==========================================
echo "4. _vars Array Includes Inherited Vars"
# ==========================================

# Reuse TestChild from test 1
child=$(@ TestChild new)
json=$(@ $child asJson)
vars=$(echo "$json" | jq -r '._vars | sort | join(",")')

[[ "$vars" == *"childVar"* ]] && pass "_vars includes childVar" || fail "_vars childVar" "contains childVar" "$vars"
[[ "$vars" == *"parentVar"* ]] && pass "_vars includes inherited parentVar" || fail "_vars parentVar" "contains parentVar" "$vars"

@ $child delete 2>/dev/null

echo ""

# ==========================================
echo "5. Setters Work for Inherited Vars"
# ==========================================

child=$(@ TestChild new)

@ $child setParentVar 42
@ $child setChildVar 84

new_parent=$(@ $child getParentVar)
new_child=$(@ $child getChildVar)

[[ "$new_parent" == "42" ]] && pass "Setter works for inherited parentVar=42" || fail "setParentVar" "42" "$new_parent"
[[ "$new_child" == "84" ]] && pass "Setter works for own childVar=84" || fail "setChildVar" "84" "$new_child"

@ $child delete 2>/dev/null

# Cleanup
cleanup_test_classes

echo ""
echo "=== Summary ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo ""

if [[ $FAILED -eq 0 ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
