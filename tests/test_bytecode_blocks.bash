#!/usr/bin/env bash
# test_bytecode_blocks.bash - System tests for bytecode block integration
# Tests the Bash-side functions that interact with the daemon for bytecode blocks
#
# These tests verify:
# 1. Block ID identification functions
# 2. Daemon communication functions (when daemon available)
# 3. Block registration, invocation, and serialization
#
# Run: ./tests/test_bytecode_blocks.bash

cd "$(dirname "$0")/.."

source lib/trash.bash

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

pass() {
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
    echo "  PASS: $1"
}

fail() {
    ((TESTS_FAILED++))
    ((TESTS_RUN++))
    echo "  FAIL: $1"
    echo "    Expected: $2"
    echo "    Got: $3"
}

skip() {
    ((TESTS_SKIPPED++))
    ((TESTS_RUN++))
    echo "  SKIP: $1 ($2)"
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

assert_true() {
    local result="$1"
    local msg="$2"
    if [[ "$result" == "true" ]] || [[ "$result" == "0" ]]; then
        pass "$msg"
    else
        fail "$msg" "true/0" "$result"
    fi
}

assert_false() {
    local result="$1"
    local msg="$2"
    if [[ "$result" == "false" ]] || [[ "$result" != "0" ]]; then
        pass "$msg"
    else
        fail "$msg" "false/non-0" "$result"
    fi
}

echo "=== Bytecode Block System Tests ==="
echo ""

# ============================================================
# Section 1: Block ID Identification Tests
# ============================================================
echo "--- Section 1: Block ID Identification ---"
echo ""

# Test 1.1: _is_bytecode_block identifies bytecode blocks
echo "Test 1.1: _is_bytecode_block identifies bytecode blocks"
if _is_bytecode_block "bytecode_block_123"; then
    pass "_is_bytecode_block returns true for bytecode_block_* prefix"
else
    fail "_is_bytecode_block returns true for bytecode_block_* prefix" "true" "false"
fi

# Test 1.2: _is_bytecode_block rejects non-bytecode blocks
echo "Test 1.2: _is_bytecode_block rejects non-bytecode blocks"
if ! _is_bytecode_block "counter_abc123"; then
    pass "_is_bytecode_block returns false for regular instance IDs"
else
    fail "_is_bytecode_block returns false for regular instance IDs" "false" "true"
fi

# Test 1.3: _is_bytecode_block rejects empty string
echo "Test 1.3: _is_bytecode_block rejects empty string"
if ! _is_bytecode_block ""; then
    pass "_is_bytecode_block returns false for empty string"
else
    fail "_is_bytecode_block returns false for empty string" "false" "true"
fi

# Test 1.4: _is_bytecode_block rejects partial match
echo "Test 1.4: _is_bytecode_block rejects partial match"
if ! _is_bytecode_block "bytecode_blocks_are_cool"; then
    pass "_is_bytecode_block returns false for partial prefix match"
else
    fail "_is_bytecode_block returns false for partial prefix match" "false" "true"
fi

# Test 1.5: _is_bytecode_block rejects bare prefix
echo "Test 1.5: _is_bytecode_block with just the prefix"
if _is_bytecode_block "bytecode_block_"; then
    pass "_is_bytecode_block returns true for bare prefix (valid pattern)"
else
    fail "_is_bytecode_block returns true for bare prefix" "true" "false"
fi

# Test 1.6: _is_bytecode_block with UUID-like suffix
echo "Test 1.6: _is_bytecode_block with UUID-like suffix"
if _is_bytecode_block "bytecode_block_550e8400-e29b-41d4-a716-446655440000"; then
    pass "_is_bytecode_block returns true for UUID-like ID"
else
    fail "_is_bytecode_block returns true for UUID-like ID" "true" "false"
fi

# Test 1.7: _is_bytecode_block with numeric suffix
echo "Test 1.7: _is_bytecode_block with numeric suffix"
if _is_bytecode_block "bytecode_block_42"; then
    pass "_is_bytecode_block returns true for numeric ID"
else
    fail "_is_bytecode_block returns true for numeric ID" "true" "false"
fi

# ============================================================
# Section 2: Daemon Communication Tests
# ============================================================
echo ""
echo "--- Section 2: Daemon Communication ---"
echo ""

# Check if daemon functions are exported
echo "Test 2.1: Bytecode block functions are exported"
if type _is_bytecode_block &>/dev/null && \
   type _register_bytecode_block &>/dev/null && \
   type _invoke_bytecode_block &>/dev/null && \
   type _serialize_bytecode_block &>/dev/null; then
    pass "All bytecode block functions are available"
else
    fail "All bytecode block functions are available" "all exported" "some missing"
fi

# Check if daemon availability check works
echo "Test 2.2: _native_daemon_available function exists"
if type _native_daemon_available &>/dev/null; then
    pass "_native_daemon_available function exists"
else
    fail "_native_daemon_available function exists" "function exists" "not found"
fi

# Check daemon availability
DAEMON_AVAILABLE=false
if _native_daemon_available 2>/dev/null; then
    DAEMON_AVAILABLE=true
    echo "Test 2.3: Daemon is available"
    pass "Daemon is running and available"
else
    echo "Test 2.3: Daemon availability check"
    skip "Daemon availability" "daemon not running"
fi

# ============================================================
# Section 3: Daemon-Dependent Tests (Block Operations)
# ============================================================
echo ""
echo "--- Section 3: Block Operations (requires daemon) ---"
echo ""

if [[ "$DAEMON_AVAILABLE" != "true" ]]; then
    echo "Skipping daemon-dependent tests (daemon not available)"
    echo "Start the daemon with: _native_daemon_ensure"
    echo ""

    # Count skipped tests
    skip "Block registration" "daemon not running"
    skip "Block invocation" "daemon not running"
    skip "Block invocation with arguments" "daemon not running"
    skip "Block serialization" "daemon not running"
    skip "Invalid bytecode handling" "daemon not running"
else
    # Test 3.1: Block registration error handling
    echo "Test 3.1: Block registration with invalid data"
    result=$(_register_bytecode_block "invalid_hex" "[]" 2>&1)
    exit_code=$?
    if [[ $exit_code -ne 0 ]] || [[ "$result" == *"Error"* ]] || [[ "$result" == *"error"* ]]; then
        pass "Invalid bytecode registration fails appropriately"
    else
        fail "Invalid bytecode registration fails appropriately" "error" "$result"
    fi

    # Test 3.2: Block invocation error handling (non-existent block)
    echo "Test 3.2: Block invocation with non-existent block"
    result=$(_invoke_bytecode_block "bytecode_block_nonexistent" "[]" 2>&1)
    exit_code=$?
    if [[ $exit_code -ne 0 ]] || [[ "$result" == *"not found"* ]] || [[ -z "$result" ]]; then
        pass "Non-existent block invocation fails appropriately"
    else
        fail "Non-existent block invocation fails appropriately" "error/empty" "$result"
    fi

    # Test 3.3: Block serialization error handling (non-existent block)
    echo "Test 3.3: Block serialization with non-existent block"
    result=$(_serialize_bytecode_block "bytecode_block_nonexistent" 2>&1)
    exit_code=$?
    if [[ $exit_code -ne 0 ]] || [[ "$result" == *"not found"* ]] || [[ -z "$result" ]]; then
        pass "Non-existent block serialization fails appropriately"
    else
        fail "Non-existent block serialization fails appropriately" "error/empty" "$result"
    fi
fi

# ============================================================
# Section 4: Integration with Trashtalk Runtime
# ============================================================
echo ""
echo "--- Section 4: Runtime Integration ---"
echo ""

# Test 4.1: Block type can be used in conditionals
echo "Test 4.1: Block type identification in conditionals"
test_id="bytecode_block_test123"
if _is_bytecode_block "$test_id"; then
    block_type="bytecode"
else
    block_type="regular"
fi
assert_eq "bytecode" "$block_type" "Block type correctly identified as bytecode"

# Test 4.2: Regular instance type can be distinguished
echo "Test 4.2: Regular instance type identification"
test_id="counter_abc123"
if _is_bytecode_block "$test_id"; then
    block_type="bytecode"
else
    block_type="regular"
fi
assert_eq "regular" "$block_type" "Regular instance correctly identified"

# Summary
echo ""
echo "=== Test Summary ==="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "Tests skipped: $TESTS_SKIPPED"

if [[ "$TESTS_FAILED" -gt 0 ]]; then
    echo ""
    echo "FAILED"
    exit 1
else
    echo ""
    echo "PASSED"
    exit 0
fi
