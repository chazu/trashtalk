#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test the public API and its real process boundary without a network service.
set -eo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-grpc.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
mkdir "$scratch/bin"
cat > "$scratch/bin/grpcurl" <<'SH'
#!/usr/bin/env bash
printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]' > "$GRPC_ARGV"
printf '%s' "${GRPC_STDOUT:-ok}"
printf '%s' "${GRPC_STDERR:-}" >&2
exit "${GRPC_STATUS:-0}"
SH
chmod +x "$scratch/bin/grpcurl"
export PATH="$scratch/bin:$PATH" GRPC_ARGV="$scratch/argv.json"
source "$ROOT/lib/trash.bash"
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s expected %s got %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
}
argv_is() {
    jq -e --argjson expected "$1" '. == $expected' "$GRPC_ARGV" >/dev/null || exit 1
}
client=$(@ GrpcClient connectTo: 'localhost:50051')
check localhost:50051 @ "$client" address
payload='{"text":"a b; $(touch should-not-exist)"}'
check ok @ "$client" call: 'echo.Service/Echo' with: "$payload"
argv_is "$(jq -cn --arg payload "$payload" '["-plaintext","-d",$payload,"localhost:50051","echo.Service/Echo"]')"
check ok @ "$client" call: 'echo.Service/Echo'
argv_is '["-plaintext","-d","{}","localhost:50051","echo.Service/Echo"]'
check ok @ "$client" listServices
argv_is '["-plaintext","localhost:50051","list"]'
check ok @ "$client" listMethods: 'echo.Service'
argv_is '["-plaintext","localhost:50051","list","echo.Service"]'
@ "$client" enableTLS
check ok @ "$client" describe: 'echo.Service'
argv_is '["localhost:50051","describe","echo.Service"]'
@ "$client" enablePlaintext
check ok @ "$client" listServices
argv_is '["-plaintext","localhost:50051","list"]'
export GRPC_STATUS=17 GRPC_STDOUT=partial GRPC_STDERR='rpc failed'
status=0
@ "$client" call: 'echo.Service/Fail' >"$scratch/out" 2>"$scratch/err" || status=$?
check 17 printf '%s' "$status"
check partial cat "$scratch/out"
check 'rpc failed' cat "$scratch/err"
# No supported configuration or discovery request may swallow a child failure.
status=0
@ "$client" listServices >/dev/null 2>"$scratch/err" || status=$?
check 17 printf '%s' "$status"
# Hide executable discovery independently of any globally installed grpcurl.
_ensure_class_sourced Tools::Grpcurl
__Tools__Grpcurl__class__path() { printf ''; }
if @ "$client" listServices >"$scratch/out" 2>"$scratch/err"; then
    echo 'FAIL: missing grpcurl returned success' >&2; exit 1
fi
[[ ! -s "$scratch/out" ]]
rg -q 'grpcurl is not installed' "$scratch/err"
if @ GrpcClient connectTo: '' >"$scratch/out" 2>"$scratch/err"; then
    echo 'FAIL: empty address accepted' >&2; exit 1
fi
echo 'PASS: gRPC argv, TLS, discovery, stdout/stderr, child status, and validation'
