#!/opt/homebrew/bin/bash

# Integration tests for GrpcClient with actual gRPC server
# Usage: bash tests/test_grpc_integration.bash
#
# Prerequisites:
#   - grpcurl must be installed
#   - Optional: A gRPC server running on localhost:50051 with reflection enabled
#
# If no server is available, most tests will be skipped.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Source trash system (suppress noisy output)
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null

# Test counter for pass/fail/skip
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Colors for output
_RED='\033[0;31m'
_GREEN='\033[0;32m'
_YELLOW='\033[0;33m'
_NC='\033[0m' # No Color

# Test server configuration
GRPC_HOST="${GRPC_TEST_HOST:-localhost:50051}"

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
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
    echo -e "${_YELLOW}SKIP${_NC}: $1 - $2"
}

run_test() {
    "$@"
}

# Check if grpcurl is available
check_grpcurl() {
    if ! command -v grpcurl &> /dev/null; then
        echo "grpcurl not found. Install with: brew install grpcurl"
        exit 1
    fi
    pass "grpcurl is installed"
}

# Check if a gRPC server is reachable
check_server() {
    if grpcurl -plaintext "$GRPC_HOST" list &>/dev/null; then
        SERVER_AVAILABLE=1
        pass "gRPC server is reachable at $GRPC_HOST"
    else
        SERVER_AVAILABLE=0
        skip "gRPC server not reachable" "server tests will be skipped"
    fi
}

# ==============================================================================
# Tests
# ==============================================================================

echo "=== GrpcClient Integration Tests ==="
echo "Target server: $GRPC_HOST"
echo ""

run_test check_grpcurl
run_test check_server

# ------------------------------------------------------------------------------
# Test 1: Client creation and configuration
# ------------------------------------------------------------------------------

test_client_lifecycle() {
    echo ""
    echo "--- Client Lifecycle Tests ---"

    # Create client
    local client
    client=$(@ GrpcClient connectTo: "$GRPC_HOST")
    if [[ -n "$client" ]]; then
        pass "created client for $GRPC_HOST"
    else
        fail "created client for $GRPC_HOST" "instance ID" "$client"
        return
    fi

    # Check address
    local addr
    addr=$(@ $client address)
    if [[ "$addr" == "$GRPC_HOST" ]]; then
        pass "address correctly stored"
    else
        fail "address correctly stored" "$GRPC_HOST" "$addr"
    fi

    # Enable pooling
    @ $client enablePooling
    local pooling
    pooling=$(@ $client isPoolingEnabled)
    if [[ "$pooling" == "yes" ]]; then
        pass "connection pooling can be enabled"
    else
        fail "connection pooling can be enabled" "yes" "$pooling"
    fi

    # Disable pooling
    @ $client disablePooling
    pooling=$(@ $client isPoolingEnabled)
    if [[ "$pooling" == "no" ]]; then
        pass "connection pooling can be disabled"
    else
        fail "connection pooling can be disabled" "no" "$pooling"
    fi
}

run_test test_client_lifecycle

# ------------------------------------------------------------------------------
# Test 2: List services (requires running server)
# ------------------------------------------------------------------------------

test_list_services() {
    echo ""
    echo "--- Service Discovery Tests ---"

    if [[ "$SERVER_AVAILABLE" != "1" ]]; then
        skip "listServices" "no server available"
        return
    fi

    local client
    client=$(@ GrpcClient connectTo: "$GRPC_HOST")

    local services
    services=$(@ $client listServices 2>&1)
    local exit_code=$?

    if [[ $exit_code -eq 0 ]] && [[ -n "$services" ]]; then
        pass "listServices returns service list"
        echo "  Services found:"
        echo "$services" | sed 's/^/    /'
    else
        fail "listServices returns service list" "service list" "$services"
    fi
}

run_test test_list_services

# ------------------------------------------------------------------------------
# Test 3: Unary call (requires running server with health check)
# ------------------------------------------------------------------------------

test_unary_call() {
    echo ""
    echo "--- Unary Call Tests ---"

    if [[ "$SERVER_AVAILABLE" != "1" ]]; then
        skip "unary call" "no server available"
        return
    fi

    local client
    client=$(@ GrpcClient connectTo: "$GRPC_HOST")

    # Try health check if available
    local result
    result=$(@ $client call: 'grpc.health.v1.Health/Check' with: '{}' 2>&1)
    local exit_code=$?

    if [[ $exit_code -eq 0 ]]; then
        pass "health check unary call succeeded"
        echo "  Response: $result"
    else
        # Health check might not be available - that's okay
        skip "health check unary call" "health service may not be available"
    fi
}

run_test test_unary_call

# ------------------------------------------------------------------------------
# Test 4: Connection error handling
# ------------------------------------------------------------------------------

test_connection_errors() {
    echo ""
    echo "--- Error Handling Tests ---"

    local client
    client=$(@ GrpcClient connectTo: 'localhost:99999')

    # This should fail gracefully
    local result
    result=$(@ $client call: 'test.Service/Method' with: '{}' 2>&1)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]]; then
        pass "connection error handled gracefully"
    else
        fail "connection error handled gracefully" "error exit code" "exit code $exit_code"
    fi
}

run_test test_connection_errors

# ------------------------------------------------------------------------------
# Test 5: Streaming methods require native (procyonOnly)
# ------------------------------------------------------------------------------

test_streaming_requires_native() {
    echo ""
    echo "--- Streaming Pragma Tests ---"

    local client
    client=$(@ GrpcClient connectTo: "$GRPC_HOST")

    # Server streaming should fail with procyonOnly error
    local result
    result=$(@ $client serverStream: 'test.Service/Stream' with: '{}' handler: 'block123' 2>&1)
    local exit_code=$?

    if [[ $exit_code -ne 0 ]] && [[ "$result" == *"Procyon"* ]]; then
        pass "serverStream requires native Procyon"
    else
        fail "serverStream requires native Procyon" "Procyon error" "$result"
    fi

    # Client streaming should fail with procyonOnly error
    result=$(@ $client clientStream: 'test.Service/Stream' handler: 'block123' 2>&1)
    exit_code=$?

    if [[ $exit_code -ne 0 ]] && [[ "$result" == *"Procyon"* ]]; then
        pass "clientStream requires native Procyon"
    else
        fail "clientStream requires native Procyon" "Procyon error" "$result"
    fi

    # Bidi streaming should fail with procyonOnly error
    result=$(@ $client bidiStream: 'test.Service/Stream' handler: 'block123' 2>&1)
    exit_code=$?

    if [[ $exit_code -ne 0 ]] && [[ "$result" == *"Procyon"* ]]; then
        pass "bidiStream requires native Procyon"
    else
        fail "bidiStream requires native Procyon" "Procyon error" "$result"
    fi
}

run_test test_streaming_requires_native

# ==============================================================================
# Summary
# ==============================================================================

echo ""
echo "================================"
echo "Tests passed:  $TESTS_PASSED"
echo "Tests failed:  $TESTS_FAILED"
echo "Tests skipped: $TESTS_SKIPPED"
echo "================================"

if [[ $TESTS_FAILED -gt 0 ]]; then
    exit 1
fi
exit 0
