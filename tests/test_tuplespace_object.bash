#!/bin/bash

# Test script for the Tuplespace object wrapper

# Set up environment
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Source trash system (which sources all dependencies)
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null

# Test counters
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

assert_eq() {
    local expected="$1"
    local actual="$2"
    local msg="$3"
    if [[ "$expected" == "$actual" ]]; then
        pass "$msg"
    else
        fail "$msg" "$expected" "$actual"
    fi
}

assert_not_empty() {
    local actual="$1"
    local msg="$2"
    if [[ -n "$actual" ]]; then
        pass "$msg"
    else
        fail "$msg" "non-empty value" "(empty)"
    fi
}

assert_contains() {
    local needle="$1"
    local haystack="$2"
    local msg="$3"
    if [[ "$haystack" == *"$needle"* ]]; then
        pass "$msg"
    else
        fail "$msg" "contains '$needle'" "$haystack"
    fi
}

echo "=== Testing Tuplespace Object ==="

# ==========================================
echo -e "\n1. Testing basic operations..."
# ==========================================
@ Tuplespace init
@ Tuplespace clear

# Verify clear worked
initial_count=$(@ Tuplespace count "person" 2>/dev/null || echo "0")
assert_eq "0" "$initial_count" "Tuplespace clear removes all tuples"

# ==========================================
echo -e "\n2. Testing put/get operations..."
# ==========================================
tuple_id=$(@ Tuplespace put "person" "name" "Alice" "age" "30")
assert_not_empty "$tuple_id" "put returns a tuple ID"

result=$(@ Tuplespace get "person" "name" "Alice")
assert_not_empty "$result" "get returns matching tuple"
assert_contains "Alice" "$result" "get result contains name value"
assert_contains "30" "$result" "get result contains age value"

# ==========================================
echo -e "\n3. Testing count operation..."
# ==========================================
count=$(@ Tuplespace count "person")
assert_eq "1" "$count" "count returns 1 after single put"

# Add another person
@ Tuplespace put "person" "name" "Bob" "age" "25"
count=$(@ Tuplespace count "person")
assert_eq "2" "$count" "count returns 2 after second put"

# ==========================================
echo -e "\n4. Testing key-value operations..."
# ==========================================
@ Tuplespace putKV "test.key" "test.value"
value=$(@ Tuplespace getKV "test.key")
assert_eq "test.value" "$value" "getKV returns value set by putKV"

# Test overwrite
@ Tuplespace putKV "test.key" "new.value"
value=$(@ Tuplespace getKV "test.key")
assert_eq "new.value" "$value" "putKV overwrites existing key"

# ==========================================
echo -e "\n5. Testing event operations..."
# ==========================================
# Clear first to get accurate count
@ Tuplespace clear
@ Tuplespace putEvent "test_event" "test_data"
event_count=$(@ Tuplespace count "event")
assert_eq "1" "$event_count" "putEvent creates one event tuple"

@ Tuplespace putEvent "another_event" "more_data"
event_count=$(@ Tuplespace count "event")
assert_eq "2" "$event_count" "putEvent increments event count"

# ==========================================
echo -e "\n6. Testing info output..."
# ==========================================
info_output=$(@ Tuplespace info 2>&1)
assert_not_empty "$info_output" "info returns output"

# ==========================================
echo -e "\n=== Summary ==="
# ==========================================
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
