#!/usr/bin/env bash
# Test script for tuplespace functionality

source "$(dirname "${BASH_SOURCE[0]}")/tuplespace.bash"

# Test colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

test_passed=0
test_failed=0

# Test helper functions
assert_equals() {
    local expected="$1"
    local actual="$2"
    local test_name="$3"
    
    if [[ "$expected" == "$actual" ]]; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((test_passed++))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo "  Expected: $expected"
        echo "  Actual: $actual"
        ((test_failed++))
    fi
}

assert_not_empty() {
    local actual="$1"
    local test_name="$2"
    
    if [[ -n "$actual" ]]; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((test_passed++))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo "  Expected non-empty result"
        ((test_failed++))
    fi
}

# Test basic functionality
test_basic_operations() {
    echo -e "${YELLOW}Testing basic operations...${NC}"
    
    # Clear space
    ts_clear
    
    # Test putting and getting tuples
    local tuple_id=$(ts_put "person" "name" "Alice" "age" "30")
    assert_not_empty "$tuple_id" "Put tuple returns ID"
    
    local result=$(ts_get "person" "name" "Alice")
    assert_not_empty "$result" "Get tuple by field"
    
    local name=$(echo "$result" | ts_field - "name")
    assert_equals "Alice" "$name" "Extract field from tuple"
    
    local age=$(echo "$result" | ts_field - "age")
    assert_equals "30" "$age" "Extract age field from tuple"
    
    # Test count
    local count=$(ts_count "person")
    assert_equals "1" "$count" "Count tuples by type"
    
    # Test take (remove)
    local taken=$(ts_take "person" "name" "Alice")
    assert_not_empty "$taken" "Take tuple removes and returns it"
    
    local count_after=$(ts_count "person")
    assert_equals "0" "$count_after" "Count after take is zero"
}

# Test key-value convenience functions
test_kv_functions() {
    echo -e "${YELLOW}Testing key-value functions...${NC}"
    
    ts_clear
    
    # Test kv put/get
    ts_kv_put "test_key" "test_value"
    local value=$(ts_kv_get "test_key")
    assert_equals "test_value" "$value" "Key-value put/get"
    
    # Test multiple kv pairs
    ts_kv_put "config.debug" "true"
    ts_kv_put "config.timeout" "30"
    
    local debug=$(ts_kv_get "config.debug")
    assert_equals "true" "$debug" "Config debug value"
    
    local timeout=$(ts_kv_get "config.timeout")
    assert_equals "30" "$timeout" "Config timeout value"
}

# Test event functions
test_event_functions() {
    echo -e "${YELLOW}Testing event functions...${NC}"
    
    ts_clear
    
    # Test event put
    ts_event_put "user_login" "alice@example.com"
    ts_event_put "user_logout" "bob@example.com"
    
    local login_events=$(ts_get "event" "name" "user_login")
    assert_not_empty "$login_events" "Event put creates event tuple"
    
    local event_count=$(ts_count "event")
    assert_equals "2" "$event_count" "Multiple events counted correctly"
}

# Test complex queries
test_complex_queries() {
    echo -e "${YELLOW}Testing complex queries...${NC}"
    
    ts_clear
    
    # Add multiple people
    ts_put "person" "name" "Alice" "age" "30" "city" "NYC"
    ts_put "person" "name" "Bob" "age" "25" "city" "SF"
    ts_put "person" "name" "Charlie" "age" "35" "city" "NYC"
    
    # Test getting by city
    local nyc_people=$(ts_get "person" "city" "NYC")
    local nyc_count=$(echo "$nyc_people" | grep -c "^Type:" || echo "0")
    assert_equals "2" "$nyc_count" "Query by city returns correct count"
    
    # Test getting all people
    local all_people=$(ts_get "person")
    local all_count=$(echo "$all_people" | grep -c "^Type:" || echo "0")
    assert_equals "3" "$all_count" "Get all people returns correct count"
}

# Test wait functionality (with timeout)
test_wait_functionality() {
    echo -e "${YELLOW}Testing wait functionality...${NC}"
    
    ts_clear
    
    # Test wait with immediate match
    ts_put "test" "status" "ready"
    local result=$(ts_wait "test" "status" "ready" 1)
    assert_not_empty "$result" "Wait finds existing tuple"
    
    # Test wait with timeout (should fail)
    local start_time=$(date +%s)
    ts_wait "test" "status" "missing" 2 >/dev/null
    local wait_result=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [[ $wait_result -eq 1 && $duration -ge 2 ]]; then
        echo -e "${GREEN}✓${NC} Wait timeout works correctly"
        ((test_passed++))
    else
        echo -e "${RED}✗${NC} Wait timeout failed"
        echo "  Exit code: $wait_result, Duration: $duration"
        ((test_failed++))
    fi
}

# Test data persistence
test_persistence() {
    echo -e "${YELLOW}Testing data persistence...${NC}"
    
    ts_clear
    
    # Add some data
    ts_put "persistent" "key" "value1"
    ts_kv_put "persist_kv" "value2"
    
    # Check that data file exists and has content
    if [[ -f "$TUPLESPACE_DB" && -s "$TUPLESPACE_DB" ]]; then
        echo -e "${GREEN}✓${NC} Tuplespace database file exists and has content"
        ((test_passed++))
    else
        echo -e "${RED}✗${NC} Tuplespace database file missing or empty"
        ((test_failed++))
    fi
    
    # Verify we can still read the data
    local value1=$(ts_get "persistent" "key" "value1")
    assert_not_empty "$value1" "Persistent data readable after operations"
    
    local value2=$(ts_kv_get "persist_kv")
    assert_equals "value2" "$value2" "Persistent KV data readable"
}

# Run all tests
run_tests() {
    echo "=== Tuplespace Test Suite ==="
    echo "Using database: $TUPLESPACE_DB"
    echo ""
    
    test_basic_operations
    echo ""
    
    test_kv_functions
    echo ""
    
    test_event_functions
    echo ""
    
    test_complex_queries
    echo ""
    
    test_wait_functionality
    echo ""
    
    test_persistence
    echo ""
    
    # Summary
    echo "=== Test Results ==="
    echo -e "${GREEN}Passed: $test_passed${NC}"
    echo -e "${RED}Failed: $test_failed${NC}"
    
    if [[ $test_failed -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed.${NC}"
        return 1
    fi
}

# Main execution
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    run_tests
fi
