#!/bin/bash

# Test instance variable inheritance feature
# Usage: bash tests/test_instance_var_inheritance.bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source trash system (suppress noisy output)
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

# Test counter for pass/fail
TESTS_RUN=0
TESTS_PASSED=0

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo "PASS: $1"
}

fail() {
    echo "FAIL: $1"
    echo "  Expected: $2"
    echo "  Got: $3"
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
    "$@"
}

# ============================================
# Test 1: Basic single-level inheritance
# ============================================
test_basic_inheritance() {
    echo "Test: Basic single-level inheritance"

    # Create parent class
    cat > "$PROJECT_DIR/trash/TestParent" << 'EOF'
is_a Object
instance_vars parentVar:100

new() {
    local id=$(_generate_instance_id "TestParent")
    _create_instance "TestParent" "$id"
    echo "$id"
}
EOF

    # Create child class
    cat > "$PROJECT_DIR/trash/TestChild" << 'EOF'
is_a TestParent
instance_vars childVar:200

new() {
    local id=$(_generate_instance_id "TestChild")
    _create_instance "TestChild" "$id"
    echo "$id"
}
EOF

    local child=$(@ TestChild new)

    # Check inherited var
    local parent_val=$(@ $child getParentVar)
    local child_val=$(@ $child getChildVar)

    if [[ "$parent_val" == "100" && "$child_val" == "200" ]]; then
        pass "Single-level inheritance (parentVar=100, childVar=200)"
    else
        fail "Single-level inheritance" "parentVar=100, childVar=200" "parentVar=$parent_val, childVar=$child_val"
    fi

    @ $child delete
    rm -f "$PROJECT_DIR/trash/TestParent" "$PROJECT_DIR/trash/TestChild"
}

# ============================================
# Test 2: Multi-level inheritance (3 levels)
# ============================================
test_multilevel_inheritance() {
    echo "Test: Multi-level inheritance (3 levels)"

    # Grandparent
    cat > "$PROJECT_DIR/trash/TestGrandparent" << 'EOF'
is_a Object
instance_vars gpVar:1

new() {
    local id=$(_generate_instance_id "TestGrandparent")
    _create_instance "TestGrandparent" "$id"
    echo "$id"
}
EOF

    # Parent
    cat > "$PROJECT_DIR/trash/TestParent2" << 'EOF'
is_a TestGrandparent
instance_vars pVar:2

new() {
    local id=$(_generate_instance_id "TestParent2")
    _create_instance "TestParent2" "$id"
    echo "$id"
}
EOF

    # Child
    cat > "$PROJECT_DIR/trash/TestChild2" << 'EOF'
is_a TestParent2
instance_vars cVar:3

new() {
    local id=$(_generate_instance_id "TestChild2")
    _create_instance "TestChild2" "$id"
    echo "$id"
}
EOF

    local child=$(@ TestChild2 new)

    local gp_val=$(@ $child getGpVar)
    local p_val=$(@ $child getPVar)
    local c_val=$(@ $child getCVar)

    if [[ "$gp_val" == "1" && "$p_val" == "2" && "$c_val" == "3" ]]; then
        pass "3-level inheritance (gpVar=1, pVar=2, cVar=3)"
    else
        fail "3-level inheritance" "gpVar=1, pVar=2, cVar=3" "gpVar=$gp_val, pVar=$p_val, cVar=$c_val"
    fi

    @ $child delete
    rm -f "$PROJECT_DIR/trash/TestGrandparent" "$PROJECT_DIR/trash/TestParent2" "$PROJECT_DIR/trash/TestChild2"
}

# ============================================
# Test 3: Child can override parent default
# ============================================
test_override_parent_default() {
    echo "Test: Child can override parent default"

    cat > "$PROJECT_DIR/trash/TestBase" << 'EOF'
is_a Object
instance_vars value:10

new() {
    local id=$(_generate_instance_id "TestBase")
    _create_instance "TestBase" "$id"
    echo "$id"
}
EOF

    cat > "$PROJECT_DIR/trash/TestDerived" << 'EOF'
is_a TestBase
instance_vars value:99

new() {
    local id=$(_generate_instance_id "TestDerived")
    _create_instance "TestDerived" "$id"
    echo "$id"
}
EOF

    local derived=$(@ TestDerived new)
    local val=$(@ $derived getValue)

    if [[ "$val" == "99" ]]; then
        pass "Child overrides parent default (value=99, not 10)"
    else
        fail "Override parent default" "99" "$val"
    fi

    @ $derived delete
    rm -f "$PROJECT_DIR/trash/TestBase" "$PROJECT_DIR/trash/TestDerived"
}

# ============================================
# Test 4: Inherited vars have working setters
# ============================================
test_inherited_setters() {
    echo "Test: Inherited vars have working setters"

    cat > "$PROJECT_DIR/trash/TestBaseSet" << 'EOF'
is_a Object
instance_vars baseVal:0

new() {
    local id=$(_generate_instance_id "TestBaseSet")
    _create_instance "TestBaseSet" "$id"
    echo "$id"
}
EOF

    cat > "$PROJECT_DIR/trash/TestDerivedSet" << 'EOF'
is_a TestBaseSet
instance_vars derivedVal:0

new() {
    local id=$(_generate_instance_id "TestDerivedSet")
    _create_instance "TestDerivedSet" "$id"
    echo "$id"
}
EOF

    local obj=$(@ TestDerivedSet new)

    # Set inherited var
    @ $obj setBaseVal 42
    local base_val=$(@ $obj getBaseVal)

    # Set own var
    @ $obj setDerivedVal 84
    local derived_val=$(@ $obj getDerivedVal)

    if [[ "$base_val" == "42" && "$derived_val" == "84" ]]; then
        pass "Setters work for both inherited (42) and own (84) vars"
    else
        fail "Inherited setters" "baseVal=42, derivedVal=84" "baseVal=$base_val, derivedVal=$derived_val"
    fi

    @ $obj delete
    rm -f "$PROJECT_DIR/trash/TestBaseSet" "$PROJECT_DIR/trash/TestDerivedSet"
}

# ============================================
# Test 5: Inherited vars appear in _vars array
# ============================================
test_vars_array_includes_inherited() {
    echo "Test: _vars array includes inherited vars"

    cat > "$PROJECT_DIR/trash/TestBaseVars" << 'EOF'
is_a Object
instance_vars alpha beta

new() {
    local id=$(_generate_instance_id "TestBaseVars")
    _create_instance "TestBaseVars" "$id"
    echo "$id"
}
EOF

    cat > "$PROJECT_DIR/trash/TestDerivedVars" << 'EOF'
is_a TestBaseVars
instance_vars gamma

new() {
    local id=$(_generate_instance_id "TestDerivedVars")
    _create_instance "TestDerivedVars" "$id"
    echo "$id"
}
EOF

    local obj=$(@ TestDerivedVars new)
    local json=$(@ $obj asJson)
    local vars=$(echo "$json" | jq -r '._vars | sort | join(",")')

    if [[ "$vars" == "alpha,beta,gamma" ]]; then
        pass "_vars includes all inherited and own vars (alpha,beta,gamma)"
    else
        fail "_vars array" "alpha,beta,gamma" "$vars"
    fi

    @ $obj delete
    rm -f "$PROJECT_DIR/trash/TestBaseVars" "$PROJECT_DIR/trash/TestDerivedVars"
}

# ============================================
# Test 6: Parent instance works independently
# ============================================
test_parent_works_independently() {
    echo "Test: Parent class instances work independently"

    cat > "$PROJECT_DIR/trash/TestIndepParent" << 'EOF'
is_a Object
instance_vars parentOnly:50

new() {
    local id=$(_generate_instance_id "TestIndepParent")
    _create_instance "TestIndepParent" "$id"
    echo "$id"
}
EOF

    cat > "$PROJECT_DIR/trash/TestIndepChild" << 'EOF'
is_a TestIndepParent
instance_vars childOnly:60

new() {
    local id=$(_generate_instance_id "TestIndepChild")
    _create_instance "TestIndepChild" "$id"
    echo "$id"
}
EOF

    # Create both parent and child instances
    local parent=$(@ TestIndepParent new)
    local child=$(@ TestIndepChild new)

    local parent_val=$(@ $parent getParentOnly)
    local child_parent_val=$(@ $child getParentOnly)
    local child_own_val=$(@ $child getChildOnly)

    if [[ "$parent_val" == "50" && "$child_parent_val" == "50" && "$child_own_val" == "60" ]]; then
        pass "Parent and child instances work independently"
    else
        fail "Independent instances" "parent=50, child.parent=50, child.own=60" "parent=$parent_val, child.parent=$child_parent_val, child.own=$child_own_val"
    fi

    @ $parent delete
    @ $child delete
    rm -f "$PROJECT_DIR/trash/TestIndepParent" "$PROJECT_DIR/trash/TestIndepChild"
}

# ============================================
# Run all tests
# ============================================
echo "========================================"
echo "Testing Instance Variable Inheritance"
echo "========================================"
echo ""

run_test test_basic_inheritance
echo ""
run_test test_multilevel_inheritance
echo ""
run_test test_override_parent_default
echo ""
run_test test_inherited_setters
echo ""
run_test test_vars_array_includes_inherited
echo ""
run_test test_parent_works_independently

echo ""
echo "========================================"
echo "Results: $TESTS_PASSED/$TESTS_RUN tests passed"
echo "========================================"

if [[ "$TESTS_PASSED" == "$TESTS_RUN" ]]; then
    exit 0
else
    exit 1
fi
