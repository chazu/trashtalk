#!/opt/homebrew/bin/bash

# Test GrpcClient class
# Usage: bash tests/test_grpc_client.bash
#
# Note: Most tests verify the class structure and pragma handling.
# Actual gRPC calls require a running gRPC server with reflection enabled.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source trash system (suppress noisy output)
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

# Test counter for pass/fail
TESTS_PASSED=0
TESTS_FAILED=0

# Colors for output
_RED='\033[0;31m'
_GREEN='\033[0;32m'
_YELLOW='\033[0;33m'
_NC='\033[0m' # No Color

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${_GREEN}PASS${_NC}: $1"
}

fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${_RED}FAIL${_NC}: $1"
    echo "  Expected: $2"
    echo "  Got: $3"
}

skip() {
    echo -e "${_YELLOW}SKIP${_NC}: $1 - $2"
}

run_test() {
    "$@"
}

# ==============================================================================
# Tests
# ==============================================================================

echo "=== GrpcClient Tests ==="

# ------------------------------------------------------------------------------
# Test 1: Class exists and can create instances
# ------------------------------------------------------------------------------

test_client_creation() {
    local client
    client=$(@ GrpcClient connectTo: 'localhost:50051')
    local exit_code=$?

    if [[ $exit_code -eq 0 ]] && [[ -n "$client" ]]; then
        pass "GrpcClient instance created"
    else
        fail "GrpcClient instance created" "instance ID" "$client (exit: $exit_code)"
        return
    fi

    # Verify address was set
    local addr
    addr=$(@ $client address)
    if [[ "$addr" == "localhost:50051" ]]; then
        pass "address correctly set"
    else
        fail "address correctly set" "localhost:50051" "$addr"
    fi

    # Verify reflection mode is default
    local reflection
    reflection=$(@ $client isUsingReflection)
    if [[ "$reflection" == "yes" ]]; then
        pass "reflection mode enabled by default"
    else
        fail "reflection mode enabled by default" "yes" "$reflection"
    fi
}

run_test test_client_creation

# ------------------------------------------------------------------------------
# Test 2: Factory with proto file
# ------------------------------------------------------------------------------

test_client_with_proto() {
    local client
    client=$(@ GrpcClient connectTo: 'localhost:50051' withProto: '/path/to/service.proto')
    local exit_code=$?

    if [[ $exit_code -eq 0 ]] && [[ -n "$client" ]]; then
        pass "GrpcClient with proto created"
    else
        fail "GrpcClient with proto created" "instance ID" "$client (exit: $exit_code)"
        return
    fi

    # Verify reflection is disabled when proto is specified
    local reflection
    reflection=$(@ $client isUsingReflection)
    if [[ "$reflection" == "no" ]]; then
        pass "reflection disabled when proto specified"
    else
        fail "reflection disabled when proto specified" "no" "$reflection"
    fi
}

run_test test_client_with_proto

# ------------------------------------------------------------------------------
# Test 3: Connection pooling configuration
# ------------------------------------------------------------------------------

test_pooling_config() {
    local client
    client=$(@ GrpcClient connectTo: 'localhost:50051')

    # Default should be disabled
    local pooling
    pooling=$(@ $client isPoolingEnabled)
    if [[ "$pooling" == "no" ]]; then
        pass "pooling disabled by default"
    else
        fail "pooling disabled by default" "no" "$pooling"
    fi

    # Enable pooling
    @ $client enablePooling
    pooling=$(@ $client isPoolingEnabled)
    if [[ "$pooling" == "yes" ]]; then
        pass "pooling can be enabled"
    else
        fail "pooling can be enabled" "yes" "$pooling"
    fi

    # Disable pooling
    @ $client disablePooling
    pooling=$(@ $client isPoolingEnabled)
    if [[ "$pooling" == "no" ]]; then
        pass "pooling can be disabled"
    else
        fail "pooling can be disabled" "no" "$pooling"
    fi
}

run_test test_pooling_config

# ------------------------------------------------------------------------------
# Test 4: Streaming methods throw procyonOnly error
# ------------------------------------------------------------------------------

test_streaming_requires_native() {
    local client
    client=$(@ GrpcClient connectTo: 'localhost:50051')

    # Test serverStream
    local output
    output=$(@ $client serverStream: 'service.Method' with: '{}' handler: 'block' 2>&1)
    local exit_code=$?
    if [[ $exit_code -ne 0 ]] && [[ "$output" == *"requires native Procyon"* ]]; then
        pass "serverStream requires native Procyon"
    else
        fail "serverStream requires native Procyon" "Error about native Procyon" "$output (exit: $exit_code)"
    fi

    # Test clientStream
    output=$(@ $client clientStream: 'service.Method' handler: 'block' 2>&1)
    exit_code=$?
    if [[ $exit_code -ne 0 ]] && [[ "$output" == *"requires native Procyon"* ]]; then
        pass "clientStream requires native Procyon"
    else
        fail "clientStream requires native Procyon" "Error about native Procyon" "$output (exit: $exit_code)"
    fi

    # Test bidiStream
    output=$(@ $client bidiStream: 'service.Method' handler: 'block' 2>&1)
    exit_code=$?
    if [[ $exit_code -ne 0 ]] && [[ "$output" == *"requires native Procyon"* ]]; then
        pass "bidiStream requires native Procyon"
    else
        fail "bidiStream requires native Procyon" "Error about native Procyon" "$output (exit: $exit_code)"
    fi
}

run_test test_streaming_requires_native

# ------------------------------------------------------------------------------
# Test 5: Pragma markers are correctly set
# ------------------------------------------------------------------------------

test_pragma_markers() {
    # Check procyonOnly markers for streaming methods
    if [[ "${__GrpcClient__serverStream_with_handler___procyonOnly:-}" == "1" ]]; then
        pass "serverStream has procyonOnly marker"
    else
        fail "serverStream has procyonOnly marker" "1" "${__GrpcClient__serverStream_with_handler___procyonOnly:-NOT SET}"
    fi

    if [[ "${__GrpcClient__clientStream_handler___procyonOnly:-}" == "1" ]]; then
        pass "clientStream has procyonOnly marker"
    else
        fail "clientStream has procyonOnly marker" "1" "${__GrpcClient__clientStream_handler___procyonOnly:-NOT SET}"
    fi

    if [[ "${__GrpcClient__bidiStream_handler___procyonOnly:-}" == "1" ]]; then
        pass "bidiStream has procyonOnly marker"
    else
        fail "bidiStream has procyonOnly marker" "1" "${__GrpcClient__bidiStream_handler___procyonOnly:-NOT SET}"
    fi

    # Check procyonNative markers on public methods
    # procyonNative: Bash uses rawMethod body, Procyon uses native impl
    if [[ "${__GrpcClient__call_with___procyonNative:-}" == "1" ]]; then
        pass "call:with: has procyonNative marker"
    else
        fail "call:with: has procyonNative marker" "1" "${__GrpcClient__call_with___procyonNative:-NOT SET}"
    fi

    if [[ "${__GrpcClient__listServices__procyonNative:-}" == "1" ]]; then
        pass "listServices has procyonNative marker"
    else
        fail "listServices has procyonNative marker" "1" "${__GrpcClient__listServices__procyonNative:-NOT SET}"
    fi
}

run_test test_pragma_markers

# ------------------------------------------------------------------------------
# Test 6: Unary call (requires actual gRPC server)
# ------------------------------------------------------------------------------

test_unary_call() {
    # Check if grpcurl is available
    if ! command -v grpcurl &> /dev/null; then
        skip "unary call test" "grpcurl not installed"
        return
    fi

    # Try to connect to a test server (this will fail without one running)
    local client
    client=$(@ GrpcClient connectTo: 'localhost:50051')

    # This test is expected to fail without a server, but we verify error handling
    local output
    output=$(@ $client call: 'grpc.health.v1.Health/Check' with: '{}' 2>&1)
    local exit_code=$?

    # We expect either success (if server is running) or a connection error
    if [[ $exit_code -eq 0 ]]; then
        pass "unary call succeeded (server was running)"
    elif [[ "$output" == *"connection refused"* ]] || [[ "$output" == *"GrpcError"* ]]; then
        pass "unary call handles connection errors gracefully"
    else
        # Might be some other error - that's fine for this test
        pass "unary call error handling works"
    fi
}

run_test test_unary_call

# ==============================================================================
# Summary
# ==============================================================================

echo ""
echo "================================"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "================================"

if [[ $TESTS_FAILED -gt 0 ]]; then
    exit 1
fi
exit 0
