#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for Http, using a fake curl on PATH

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

FAKE_BIN=$(mktemp -d "${TMPDIR:-/tmp}/test_http_bin.XXXXXX")
trap 'rm -rf "$FAKE_BIN"' EXIT

# Writes $FAKE_CURL_BODY to the -o file and prints $FAKE_CURL_STATUS for -w.
cat > "$FAKE_BIN/curl" <<'EOF'
#!/usr/bin/env bash
out=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        -o) out="$2"; shift 2 ;;
        -w) shift 2 ;;
        *) shift ;;
    esac
done
[[ -n "$out" ]] && printf '%s' "$FAKE_CURL_BODY" > "$out"
printf '%s' "${FAKE_CURL_STATUS:-200}"
EOF
chmod +x "$FAKE_BIN/curl"
export PATH="$FAKE_BIN:$PATH"

PASSED=0
FAILED=0

assert_eq() {
    if [[ "$2" == "$3" ]]; then
        echo "  PASS: $1"
        ((PASSED++)) || true
    else
        echo "  FAIL: $1 (expected: $2, got: $3)"
        ((FAILED++)) || true
    fi
}

echo "=== Http Tests ==="
echo ""
echo "1. getFull: keeps the body a string"

full=$(FAKE_CURL_BODY='@/etc/hosts' @ Http getFull: 'http://example.test/')
assert_eq "body starting with @ is not read as a file" '"@/etc/hosts"' "$(jq -c '.body' <<< "$full")"
assert_eq "status is numeric" "200" "$(jq -c '.status' <<< "$full")"

full=$(FAKE_CURL_BODY='42' @ Http getFull: 'http://example.test/')
assert_eq "numeric body stays a string" '"42"' "$(jq -c '.body' <<< "$full")"

full=$(FAKE_CURL_BODY='two words
and a line' FAKE_CURL_STATUS=404 @ Http getFull: 'http://example.test/')
assert_eq "multi-line body survives" 'two words
and a line' "$(jq -r '.body' <<< "$full")"
assert_eq "error status is reported" "404" "$(jq -c '.status' <<< "$full")"

echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ "$FAILED" -eq 0 ]] && exit 0 || exit 1
