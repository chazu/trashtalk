#!/bin/bash

# Test instance variable defaults feature
# Usage: bash tests/test_instance_var_defaults.bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source trash system (suppress noisy output)
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

# Test counter for pass/fail
TESTS_RUN=0
TESTS_PASSED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}PASS${NC}: $1"
}

fail() {
    echo -e "${RED}FAIL${NC}: $1"
    echo "  Expected: $2"
    echo "  Got: $3"
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
    "$@"
}

# ============================================
# Test 1: Numeric default value
# ============================================
test_numeric_default() {
    echo "Test: Numeric default value"

    # Create a test class with numeric default
    cat > "$PROJECT_DIR/trash/TestNumericDefault" << 'EOF'
is_a Object
instance_vars count:0 step:5

new() {
    local id=$(_generate_instance_id "TestNumericDefault")
    _create_instance "TestNumericDefault" "$id"
    echo "$id"
}
EOF

    # Create instance and check defaults
    local instance=$(@ TestNumericDefault new)

    local count=$(@ $instance getCount)
    local step=$(@ $instance getStep)

    if [[ "$count" == "0" && "$step" == "5" ]]; then
        pass "Numeric defaults applied (count=0, step=5)"
    else
        fail "Numeric defaults" "count=0, step=5" "count=$count, step=$step"
    fi

    # Cleanup
    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestNumericDefault"
}

# ============================================
# Test 2: String default value
# ============================================
test_string_default() {
    echo "Test: String default value"

    cat > "$PROJECT_DIR/trash/TestStringDefault" << 'EOF'
is_a Object
instance_vars name:unknown status:active

new() {
    local id=$(_generate_instance_id "TestStringDefault")
    _create_instance "TestStringDefault" "$id"
    echo "$id"
}
EOF

    local instance=$(@ TestStringDefault new)

    local name=$(@ $instance getName)
    local status=$(@ $instance getStatus)

    if [[ "$name" == "unknown" && "$status" == "active" ]]; then
        pass "String defaults applied (name=unknown, status=active)"
    else
        fail "String defaults" "name=unknown, status=active" "name=$name, status=$status"
    fi

    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestStringDefault"
}

# ============================================
# Test 3: Mixed defaults and no-defaults
# ============================================
test_mixed_defaults() {
    echo "Test: Mixed defaults and no-defaults"

    cat > "$PROJECT_DIR/trash/TestMixedDefault" << 'EOF'
is_a Object
instance_vars count:10 label optional

new() {
    local id=$(_generate_instance_id "TestMixedDefault")
    _create_instance "TestMixedDefault" "$id"
    echo "$id"
}
EOF

    local instance=$(@ TestMixedDefault new)

    local count=$(@ $instance getCount)
    local label=$(@ $instance getLabel)
    local optional=$(@ $instance getOptional)

    # count should be 10 (default), label should be empty string, optional should be empty (null)
    if [[ "$count" == "10" && "$label" == "" && "$optional" == "" ]]; then
        pass "Mixed defaults: count=10 (default), label='' (default), optional=null (no default)"
    else
        fail "Mixed defaults" "count=10, label='', optional=''" "count=$count, label=$label, optional=$optional"
    fi

    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestMixedDefault"
}

# ============================================
# Test 4: Boolean default value
# ============================================
test_boolean_default() {
    echo "Test: Boolean default value"

    cat > "$PROJECT_DIR/trash/TestBoolDefault" << 'EOF'
is_a Object
instance_vars enabled:true visible:false

new() {
    local id=$(_generate_instance_id "TestBoolDefault")
    _create_instance "TestBoolDefault" "$id"
    echo "$id"
}
EOF

    local instance=$(@ TestBoolDefault new)

    # Check raw JSON since jq's // empty treats false as empty
    local json=$(@ $instance asJson)
    local enabled=$(echo "$json" | jq -r '.enabled')
    local visible=$(echo "$json" | jq -r '.visible')

    if [[ "$enabled" == "true" && "$visible" == "false" ]]; then
        pass "Boolean defaults applied (enabled=true, visible=false)"
    else
        fail "Boolean defaults" "enabled=true, visible=false" "enabled=$enabled, visible=$visible"
    fi

    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestBoolDefault"
}

# ============================================
# Test 5: Default can be overridden
# ============================================
test_default_override() {
    echo "Test: Default can be overridden after creation"

    cat > "$PROJECT_DIR/trash/TestOverride" << 'EOF'
is_a Object
instance_vars count:100

new() {
    local id=$(_generate_instance_id "TestOverride")
    _create_instance "TestOverride" "$id"
    echo "$id"
}
EOF

    local instance=$(@ TestOverride new)

    # Initial value should be default
    local initial=$(@ $instance getCount)

    # Override it
    @ $instance setCount 999

    local updated=$(@ $instance getCount)

    if [[ "$initial" == "100" && "$updated" == "999" ]]; then
        pass "Default overridden: 100 -> 999"
    else
        fail "Default override" "initial=100, updated=999" "initial=$initial, updated=$updated"
    fi

    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestOverride"
}

# ============================================
# Test 6: Empty array default
# ============================================
test_array_default() {
    echo "Test: Empty array default value"

    cat > "$PROJECT_DIR/trash/TestArrayDefault" << 'EOF'
is_a Object
instance_vars items:[]

new() {
    local id=$(_generate_instance_id "TestArrayDefault")
    _create_instance "TestArrayDefault" "$id"
    echo "$id"
}
EOF

    local instance=$(@ TestArrayDefault new)

    # Check raw JSON for the array value
    local json=$(@ $instance asJson)
    local items=$(echo "$json" | jq -c '.items')

    if [[ "$items" == "[]" ]]; then
        pass "Empty array default applied (items=[])"
    else
        fail "Array default" "[]" "$items"
    fi

    @ $instance delete
    rm -f "$PROJECT_DIR/trash/TestArrayDefault"
}

# ============================================
# Test 7: Counter with default (real-world usage)
# ============================================
test_counter_with_default() {
    echo "Test: Counter using default value"

    local counter=$(@ Counter new)
    local value=$(@ $counter getValue)

    if [[ "$value" == "0" ]]; then
        # Also verify increment works
        @ $counter increment
        local after_inc=$(@ $counter getValue)
        if [[ "$after_inc" == "1" ]]; then
            pass "Counter with default value=0 works correctly"
        else
            fail "Counter increment" "1" "$after_inc"
        fi
    else
        fail "Counter default" "0" "$value"
    fi

    @ $counter delete
}

# ============================================
# Run all tests
# ============================================
echo "========================================"
echo "Testing Instance Variable Defaults"
echo "========================================"
echo ""

run_test test_numeric_default
echo ""
run_test test_string_default
echo ""
run_test test_mixed_defaults
echo ""
run_test test_boolean_default
echo ""
run_test test_default_override
echo ""
run_test test_array_default
echo ""
run_test test_counter_with_default

echo ""
echo "========================================"
echo "Results: $TESTS_PASSED/$TESTS_RUN tests passed"
echo "========================================"

if [[ "$TESTS_PASSED" == "$TESTS_RUN" ]]; then
    exit 0
else
    exit 1
fi
