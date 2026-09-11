#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Explicit opt-in: a plaintext server with reflection and the health service.
# TRASH_TEST_GRPC_LIVE=1 GRPC_TEST_HOST=localhost:50051 bash tests/test_grpc_integration.bash
# The offline adapter contract is tested unconditionally in test_grpc_client.
if [[ "${TRASH_TEST_GRPC_LIVE:-}" != 1 ]]; then
    echo 'SKIP: live gRPC server test (set TRASH_TEST_GRPC_LIVE=1)'
    exit 0
fi
set -eo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
command -v grpcurl >/dev/null || { echo 'grpcurl is required' >&2; exit 1; }
source "$ROOT/lib/trash.bash"
client=$(@ GrpcClient connectTo: "${GRPC_TEST_HOST:-localhost:50051}")
services=$(@ "$client" listServices)
[[ -n "$services" ]]
methods=$(@ "$client" listMethods: grpc.health.v1.Health)
[[ -n "$methods" ]]
result=$(@ "$client" call: grpc.health.v1.Health/Check)
printf '%s' "$result" | jq -e '.status == "SERVING"' >/dev/null
if @ "$client" call: nonexistent.Service/Method; then
    echo 'FAIL: invalid RPC returned success' >&2; exit 1
fi
echo 'PASS: live reflection, health RPC, and invalid method rejection'
