#!/usr/bin/env bash

set -uo pipefail
export LC_ALL=C

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
FAKE_CODEX_ARGV="$TEST_TMP/codex-argv.txt"
FAKE_CODEX_STDIN="$TEST_TMP/codex-stdin.txt"
FAKE_CODEX_CALLS="$TEST_TMP/codex-calls.txt"
FAKE_CODEX_API_KEY_STATE="$TEST_TMP/codex-api-key-state.txt"
FAKE_INPAGE_STDIN="$TEST_TMP/inpage-stdin.txt"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN"

cat > "$FAKE_BIN/codex" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$FAKE_CODEX_ARGV"
printf 'called\n' >> "$FAKE_CODEX_CALLS"

if [[ "${1:-}" == "--version" ]]; then
    echo 'codex-cli test-1.2.3'
    exit 0
fi

if [[ "${1:-}" == "login" && "${2:-}" == "status" ]]; then
    case "${FAKE_CODEX_AUTH_STATUS:-chatgpt}" in
        chatgpt) echo 'Logged in using ChatGPT' ;;
        api) echo 'Logged in using an API key' ;;
        missing) echo 'Not logged in' >&2; exit 1 ;;
    esac
    exit 0
fi

input=$(cat)
printf '%s' "$input" > "$FAKE_CODEX_STDIN"
printf '%s' "${CODEX_API_KEY-unset}" > "$FAKE_CODEX_API_KEY_STATE"

case "${FAKE_CODEX_SCENARIO:-success}" in
    success)
        printf 'codex answer line one\ncodex answer line two\n'
        ;;
    failure)
        echo 'authentication required' >&2
        exit 1
        ;;
    empty)
        exit 0
        ;;
esac
FAKE
chmod +x "$FAKE_BIN/codex"

cat > "$FAKE_BIN/inpage" <<'FAKE'
#!/usr/bin/env bash
cat > "$FAKE_INPAGE_STDIN"
jq -cn '{schema_version:1,outcome:"closed"}'
FAKE
chmod +x "$FAKE_BIN/inpage"

export FAKE_CODEX_ARGV FAKE_CODEX_STDIN FAKE_CODEX_CALLS FAKE_CODEX_API_KEY_STATE FAKE_INPAGE_STDIN
export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null
# trash.bash installs its own EXIT handler.
trap cleanup EXIT

PASSED=0
FAILED=0

pass() { echo "  PASS: $1"; ((PASSED++)) || true; }
fail() { echo "  FAIL: $1"; ((FAILED++)) || true; }

assert_eq() {
    if [[ "$2" == "$3" ]]; then
        pass "$1"
    else
        echo "    expected: $2"
        echo "    actual:   $3"
        fail "$1"
    fi
}

assert_true() {
    local name="$1"
    shift
    if "$@" >/dev/null; then pass "$name"; else fail "$name"; fi
}

echo "=== Codex Agent Integration Tests ==="

assert_eq "Codex version uses conventional flag" 'codex-cli test-1.2.3' "$(@ Tools::Codex version)"
assert_eq "Codex exposes authentication status" 'Logged in using ChatGPT' "$(@ Tools::Codex authStatus)"

export FAKE_CODEX_SCENARIO=success
export CODEX_API_KEY=billing-sentinel
context='{"question":"test"}'
result=$(@ Tools::Codex run: "$context" workingDirectory: "$TEST_TMP/work dir")
assert_eq "successful run identifies backend" 'codex' "$(printf '%s' "$result" | jq -r .backend)"
assert_eq "successful run outcome" 'success' "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "successful run preserves final answer" $'codex answer line one\ncodex answer line two' \
    "$(printf '%s' "$result" | jq -r .result.content)"
assert_eq "successful run preserves original exit" '0' "$(printf '%s' "$result" | jq -r .exit_code)"
assert_eq "context is passed exactly on stdin" "$context" "$(cat "$FAKE_CODEX_STDIN")"
assert_eq "API key is removed from subscription-backed run" 'unset' "$(cat "$FAKE_CODEX_API_KEY_STATE")"
assert_eq "Codex invocation starts with exec" 'exec' "$(sed -n '1p' "$FAKE_CODEX_ARGV")"
assert_true "Codex run is ephemeral" grep -Fxq -- '--ephemeral' "$FAKE_CODEX_ARGV"
assert_true "Codex ignores user tool configuration" grep -Fxq -- '--ignore-user-config' "$FAKE_CODEX_ARGV"
assert_true "Codex skips git requirement" grep -Fxq -- '--skip-git-repo-check' "$FAKE_CODEX_ARGV"
assert_true "Codex receives exact working directory" grep -Fxq -- "$TEST_TMP/work dir" "$FAKE_CODEX_ARGV"
assert_true "Codex reads prompt from stdin" grep -Fxq -- '-' "$FAKE_CODEX_ARGV"
assert_eq "Codex sandbox is read-only" 'read-only' "$(awk 'previous == "--sandbox" { print; exit } { previous=$0 }' "$FAKE_CODEX_ARGV")"
if grep -Fq -- '--dangerously-bypass-approvals-and-sandbox' "$FAKE_CODEX_ARGV"; then
    fail "Codex never bypasses its sandbox"
else
    pass "Codex never bypasses its sandbox"
fi

export FAKE_CODEX_AUTH_STATUS=api
before_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
result=$(@ Tools::Codex run: '{}' workingDirectory: "$TEST_TMP")
after_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
assert_eq "API-key login is rejected" 'configuration_error' "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "rejected login never starts exec" "$((before_calls + 1))" "$after_calls"
assert_true "rejected login explains ChatGPT requirement" grep -Fq -- 'authenticated with ChatGPT' \
    <(printf '%s' "$result" | jq -r .stderr)
export FAKE_CODEX_AUTH_STATUS=chatgpt

export FAKE_CODEX_SCENARIO=failure
result=$(@ Tools::Codex run: '{}' workingDirectory: "$TEST_TMP")
assert_eq "failed run preserves status" '1' "$(printf '%s' "$result" | jq -r .exit_code)"
assert_eq "failed run has process outcome" 'process_error' "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "failed run preserves diagnostics" 'authentication required' \
    "$(printf '%s' "$result" | jq -r '.stderr | rtrimstr("\n")')"
assert_eq "agent formats Codex failures" 'Codex execution error (exit 1): authentication required' \
    "$(@ Agent answerFromRun: "$result")"

export FAKE_CODEX_SCENARIO=empty
result=$(@ Tools::Codex run: '{}' workingDirectory: "$TEST_TMP")
assert_eq "empty successful output is rejected" 'invalid_output' "$(printf '%s' "$result" | jq -r .outcome)"

before_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
dry_result=$(@ CodexAgent dryRun: 'why failed?' workingDirectory: "$TEST_TMP/work dir" status: '7' lastResult: $'old\nresult')
after_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
assert_eq "dry run does not invoke Codex" "$before_calls" "$after_calls"
assert_eq "dry run is marked" 'true' "$(printf '%s' "$dry_result" | jq -r .result.dry_run)"
assert_eq "dry run context has question" 'why failed?' "$(printf '%s' "$dry_result" | jq -r '.result.stdin | fromjson | .question')"
assert_eq "dry run context has prior status" '7' "$(printf '%s' "$dry_result" | jq -r '.result.stdin | fromjson | .last_status')"

export TRASHTALK_AGENT_BACKEND=codex
export FAKE_CODEX_SCENARIO=success
result=$(@ Agent ask: 'selected?' workingDirectory: "$TEST_TMP/work dir" status: '9' lastResult: 'prior')
assert_eq "Agent selects Codex backend" 'codex' "$(printf '%s' "$result" | jq -r .backend)"
assert_eq "selected backend receives question" 'selected?' "$(jq -r .question "$FAKE_CODEX_STDIN")"

__='previous output'
false
@@ 'what happened?' > "$TEST_TMP/at-at-output.txt"
at_status=$?
assert_eq "@@ returns Codex success status" '0' "$at_status"
assert_eq "@@ captures previous status" '1' "$(jq -r .last_status "$FAKE_CODEX_STDIN")"
assert_eq "@@ presents Codex answer through inpage" $'codex answer line one\ncodex answer line two' "$(cat "$FAKE_INPAGE_STDIN")"

before_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
@@ --dry-run 'show context' > "$TEST_TMP/at-at-dry-output.txt"
at_status=$?
after_calls=$(wc -l < "$FAKE_CODEX_CALLS" | tr -d ' ')
assert_eq "@@ Codex dry run succeeds" '0' "$at_status"
assert_eq "@@ Codex dry run does not invoke Codex" "$before_calls" "$after_calls"
assert_true "@@ Codex dry run displays exact argv" grep -Fq -- '"--sandbox"' "$FAKE_INPAGE_STDIN"
assert_true "@@ Codex dry run displays context" grep -Fq -- 'show context' "$FAKE_INPAGE_STDIN"

PATH="/opt/homebrew/bin:/usr/bin:/bin"
hash -r
missing=$(@ Tools::Codex run: '{}' workingDirectory: "$TEST_TMP")
assert_eq "missing Codex has distinct outcome" 'missing_tool' "$(printf '%s' "$missing" | jq -r .outcome)"
assert_eq "missing Codex uses command-not-found status" '127' "$(printf '%s' "$missing" | jq -r .exit_code)"
ensure_output=$(@ Tools::Codex ensure 2>"$TEST_TMP/ensure-error.txt")
assert_eq "ensure does not silently install Codex" 'false' "$ensure_output"
assert_true "ensure provides login guidance" grep -Fq -- 'codex login' "$TEST_TMP/ensure-error.txt"
PATH="$ORIGINAL_PATH"
export PATH
hash -r

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
