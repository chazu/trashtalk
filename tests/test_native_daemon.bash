#!/usr/bin/env bash
# test_native_daemon.bash - Tests for native daemon internal functions
# The native daemon manages a tt process that loads .dylib plugins

cd "$(dirname "$0")/.."

source lib/trash.bash

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

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
    ((TESTS_RUN++))
    echo "  SKIP: $1 ($2)"
}

echo "=== Native Daemon Tests (Internal Functions) ==="
echo ""

# Check if Counter.dylib exists (needed for dispatch tests)
COUNTER_DYLIB="$TRASHDIR/.compiled/Counter.dylib"
HAS_COUNTER_PLUGIN=false
if [[ -f "$COUNTER_DYLIB" ]]; then
    HAS_COUNTER_PLUGIN=true
fi

# Clean up any existing daemon
_native_daemon_reset 2>/dev/null || true

# Test 1: _native_daemon_ensure starts daemon
echo "Test 1: _native_daemon_ensure starts daemon"
if _native_daemon_ensure; then
    pass "_native_daemon_ensure starts daemon successfully"
else
    fail "_native_daemon_ensure starts daemon successfully" "exit 0" "exit $?"
fi

# Test 2: _native_daemon_available after ensure
echo "Test 2: _native_daemon_available after ensure"
if _native_daemon_available; then
    pass "_native_daemon_available returns true after ensure"
else
    fail "_native_daemon_available returns true after ensure" "exit 0" "exit $?"
fi

# Test 3: _native_daemon_is_running
echo "Test 3: _native_daemon_is_running"
running=$(_native_daemon_is_running)
if [[ "$running" == "true" ]]; then
    pass "_native_daemon_is_running returns true"
else
    fail "_native_daemon_is_running returns true" "true" "$running"
fi

# Test 4: _native_daemon_pid returns valid PID
echo "Test 4: _native_daemon_pid returns valid PID"
pid=$(_native_daemon_pid)
if [[ "$pid" =~ ^[0-9]+$ ]]; then
    pass "_native_daemon_pid returns valid PID: $pid"
else
    fail "_native_daemon_pid returns valid PID" "numeric" "$pid"
fi

# Test 5: _has_native_plugin for Counter
echo "Test 5: _has_native_plugin for Counter"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    if _has_native_plugin Counter; then
        pass "_has_native_plugin Counter returns true"
    else
        fail "_has_native_plugin Counter returns true" "exit 0" "exit $?"
    fi
else
    skip "_has_native_plugin Counter" "Counter.dylib not available"
fi

# Test 6: _has_native_plugin for NonExistent
echo "Test 6: _has_native_plugin for NonExistent"
if _has_native_plugin NonExistentClass; then
    fail "_has_native_plugin NonExistentClass returns false" "exit 1" "exit 0"
else
    pass "_has_native_plugin NonExistentClass returns false"
fi

# Test 7: _native_daemon_list_natives includes Counter
echo "Test 7: _native_daemon_list_natives includes Counter"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    natives=$(_native_daemon_list_natives)
    if echo "$natives" | grep -q "Counter"; then
        pass "_native_daemon_list_natives includes Counter"
    else
        fail "_native_daemon_list_natives includes Counter" "Counter in list" "$natives"
    fi
else
    skip "_native_daemon_list_natives includes Counter" "Counter.dylib not available"
fi

# Test 8: _native_daemon_dispatch getValue
echo "Test 8: _native_daemon_dispatch getValue"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    # Create a test instance in env store
    testCounter=$(_generate_instance_id Counter)
    _env_set "$testCounter" '{"class":"Counter","value":0,"step":1}'

    value=$(_native_daemon_dispatch Counter "$testCounter" getValue)
    if [[ "$value" == "0" ]]; then
        pass "_native_daemon_dispatch getValue returns 0"
    else
        fail "_native_daemon_dispatch getValue returns 0" "0" "$value"
    fi
else
    skip "_native_daemon_dispatch getValue" "Counter.dylib not available"
fi

# Test 9: _native_daemon_dispatch increment
echo "Test 9: _native_daemon_dispatch increment"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    _native_daemon_dispatch Counter "$testCounter" increment >/dev/null
    value=$(_native_daemon_dispatch Counter "$testCounter" getValue)
    if [[ "$value" == "1" ]]; then
        pass "_native_daemon_dispatch increment increases value to 1"
    else
        fail "_native_daemon_dispatch increment increases value to 1" "1" "$value"
    fi
else
    skip "_native_daemon_dispatch increment" "Counter.dylib not available"
fi

# Test 10: _native_daemon_dispatch incrementBy_ (with argument)
echo "Test 10: _native_daemon_dispatch incrementBy_"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    _native_daemon_dispatch Counter "$testCounter" incrementBy_ "5" >/dev/null
    value=$(_native_daemon_dispatch Counter "$testCounter" getValue)
    if [[ "$value" == "6" ]]; then
        pass "_native_daemon_dispatch incrementBy_ 5 increases value to 6"
    else
        fail "_native_daemon_dispatch incrementBy_ 5 increases value to 6" "6" "$value"
    fi
else
    skip "_native_daemon_dispatch incrementBy_" "Counter.dylib not available"
fi

# Test 11: State persisted to Bash env
echo "Test 11: State persisted to Bash env"
if [[ "$HAS_COUNTER_PLUGIN" == "true" ]]; then
    bashValue=$(echo "$(_env_get "$testCounter")" | jq -r '.value')
    if [[ "$bashValue" == "6" ]]; then
        pass "Updated value persisted to Bash env"
    else
        fail "Updated value persisted to Bash env" "6" "$bashValue"
    fi
else
    skip "Updated value persisted to Bash env" "Counter.dylib not available"
fi

# Test 12: _native_daemon_dispatch returns 200 for non-native class
echo "Test 12: fallback for non-native class"
_native_daemon_dispatch NonExistentClass "" foo 2>/dev/null
exitCode=$?
if [[ $exitCode -eq 200 ]]; then
    pass "Dispatch to non-native class returns 200 (fallback)"
else
    fail "Dispatch to non-native class returns 200 (fallback)" "200" "$exitCode"
fi

# Test 13: _native_daemon_stop stops daemon
echo "Test 13: _native_daemon_stop stops daemon"
_native_daemon_stop
sleep 0.5  # Give process time to exit
running=$(_native_daemon_is_running)
if [[ "$running" == "false" ]]; then
    pass "_native_daemon_stop stops daemon"
else
    fail "_native_daemon_stop stops daemon" "false" "$running"
fi

# Test 14: _native_daemon_reset cleans up
echo "Test 14: _native_daemon_reset cleans up"
_native_daemon_ensure >/dev/null 2>&1
_native_daemon_reset
if [[ ! -S "$_NATIVE_DAEMON_SOCKET" ]]; then
    pass "_native_daemon_reset removes socket"
else
    fail "_native_daemon_reset removes socket" "no socket" "socket exists"
fi

# Test 15: Daemon can restart after reset
echo "Test 15: Daemon can restart after reset"
if _native_daemon_ensure; then
    pass "Daemon restarts after reset"
else
    fail "Daemon restarts after reset" "exit 0" "exit $?"
fi

# Clean up
_native_daemon_reset 2>/dev/null || true

echo ""
echo "================================"
echo "Tests: $TESTS_RUN, Passed: $TESTS_PASSED, Failed: $TESTS_FAILED"

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
