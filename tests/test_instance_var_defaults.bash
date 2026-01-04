#!/opt/homebrew/bin/bash

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
_RED='\033[0;31m'
_GREEN='\033[0;32m'
_NC='\033[0m' # No Color

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${_GREEN}PASS${_NC}: $1"
}

fail() {
    echo -e "${_RED}FAIL${_NC}: $1"
    echo "  Expected: $2"
    echo "  Got: $3"
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
    "$@"
}

# Helper to create, compile, and load a test class
create_test_class() {
    local class_name="$1"
    local class_content="$2"
    local trash_file="$PROJECT_DIR/trash/${class_name}.trash"
    local compiled_file="$PROJECT_DIR/trash/.compiled/$class_name"

    # Ensure .compiled directory exists
    mkdir -p "$PROJECT_DIR/trash/.compiled"

    # Write .trash file
    echo "$class_content" > "$trash_file"

    # Compile it (compiler outputs to stdout, redirect to file; stderr has noise)
    "$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$trash_file" 2>/dev/null > "$compiled_file"

    # Copy to runtime location
    cp "$compiled_file" "$PROJECT_DIR/trash/$class_name"

    # Source it
    source "$compiled_file"
}

# Helper to clean up a test class
cleanup_test_class() {
    local class_name="$1"
    rm -f "$PROJECT_DIR/trash/${class_name}.trash"
    rm -f "$PROJECT_DIR/trash/.compiled/$class_name"
    rm -f "$PROJECT_DIR/trash/$class_name"
}

# ============================================
# Test 1: Numeric default value
# ============================================
test_numeric_default() {
    echo "Test: Numeric default value"

    create_test_class "TestNumericDefault" 'TestNumericDefault subclass: Object
  instanceVars: count:0 step:5

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestNumericDefault")
    _create_instance "TestNumericDefault" "$id"
    echo "$id"
  ]
'

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
    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestNumericDefault"
}

# ============================================
# Test 2: Variable without default (null/empty)
# ============================================
test_no_default() {
    echo "Test: Variable without default value"

    create_test_class "TestNoDefault" 'TestNoDefault subclass: Object
  instanceVars: name status

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestNoDefault")
    _create_instance "TestNoDefault" "$id"
    echo "$id"
  ]
'

    local instance=$(@ TestNoDefault new)

    local name=$(@ $instance getName)
    local status=$(@ $instance getStatus)

    # Variables without defaults should be empty
    if [[ "$name" == "" && "$status" == "" ]]; then
        pass "No-default variables are empty"
    else
        fail "No-default variables" "name='', status=''" "name=$name, status=$status"
    fi

    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestNoDefault"
}

# ============================================
# Test 3: Mixed defaults and no-defaults
# ============================================
test_mixed_defaults() {
    echo "Test: Mixed defaults and no-defaults"

    create_test_class "TestMixedDefault" 'TestMixedDefault subclass: Object
  instanceVars: count:10 label optional

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestMixedDefault")
    _create_instance "TestMixedDefault" "$id"
    echo "$id"
  ]
'

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

    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestMixedDefault"
}

# ============================================
# Test 4: Boolean-like numeric defaults (1/0)
# ============================================
test_boolean_numeric_default() {
    echo "Test: Boolean-like numeric defaults (1=true, 0=false)"

    create_test_class "TestBoolNumeric" 'TestBoolNumeric subclass: Object
  instanceVars: enabled:1 visible:0

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestBoolNumeric")
    _create_instance "TestBoolNumeric" "$id"
    echo "$id"
  ]
'

    local instance=$(@ TestBoolNumeric new)

    local enabled=$(@ $instance getEnabled)
    local visible=$(@ $instance getVisible)

    if [[ "$enabled" == "1" && "$visible" == "0" ]]; then
        pass "Boolean-like numeric defaults applied (enabled=1, visible=0)"
    else
        fail "Boolean numeric defaults" "enabled=1, visible=0" "enabled=$enabled, visible=$visible"
    fi

    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestBoolNumeric"
}

# ============================================
# Test 5: Default can be overridden
# ============================================
test_default_override() {
    echo "Test: Default can be overridden after creation"

    create_test_class "TestOverride" 'TestOverride subclass: Object
  instanceVars: count:100

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestOverride")
    _create_instance "TestOverride" "$id"
    echo "$id"
  ]
'

    local instance=$(@ TestOverride new)

    # Initial value should be default
    local initial=$(@ $instance getCount)

    # Override it (use keyword syntax with colon)
    @ $instance setCount: 999

    local updated=$(@ $instance getCount)

    if [[ "$initial" == "100" && "$updated" == "999" ]]; then
        pass "Default overridden: 100 -> 999"
    else
        fail "Default override" "initial=100, updated=999" "initial=$initial, updated=$updated"
    fi

    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestOverride"
}

# ============================================
# Test 6: Numeric default values
# ============================================
test_numeric_defaults() {
    echo "Test: Numeric default values"

    create_test_class "TestNumericDefaults" 'TestNumericDefaults subclass: Object
  instanceVars: offset:0 limit:100

  rawClassMethod: new [
    local id=$(_generate_instance_id "TestNumericDefaults")
    _create_instance "TestNumericDefaults" "$id"
    echo "$id"
  ]
'

    local instance=$(@ TestNumericDefaults new)

    local offset=$(@ $instance getOffset)
    local limit=$(@ $instance getLimit)

    if [[ "$offset" == "0" && "$limit" == "100" ]]; then
        pass "Number defaults applied (offset=0, limit=100)"
    else
        fail "Number defaults" "offset=0, limit=100" "offset=$offset, limit=$limit"
    fi

    @ $instance delete 2>/dev/null || true
    cleanup_test_class "TestNumericDefaults"
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
run_test test_no_default
echo ""
run_test test_mixed_defaults
echo ""
run_test test_boolean_numeric_default
echo ""
run_test test_default_override
echo ""
run_test test_numeric_defaults
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
