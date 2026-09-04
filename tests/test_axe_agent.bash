#!/usr/bin/env bash

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
FAKE_AXE_ARGV="$TEST_TMP/axe-argv.txt"
FAKE_AXE_STDIN="$TEST_TMP/axe-stdin.txt"
FAKE_INPAGE_STDIN="$TEST_TMP/inpage-stdin.txt"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN"

cat > "$FAKE_BIN/axe" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$FAKE_AXE_ARGV"

if [[ "${1:-}" == "version" ]]; then
    echo 'axe version test-1.2.3'
    exit 0
fi

input=$(cat)
printf '%s' "$input" > "$FAKE_AXE_STDIN"

case "${FAKE_AXE_SCENARIO:-success}" in
    success)
        jq -cn --arg content $'answer line one\nanswer line two' \
            '{model:"test/model",content:$content,input_tokens:12,output_tokens:7,stop_reason:"end_turn",duration_ms:5,tool_calls:0,tool_call_details:[],refused:false,retry_attempts:0}'
        ;;
    dry-run)
        workdir=""
        while [[ $# -gt 0 ]]; do
            if [[ "$1" == "--workdir" ]]; then workdir="$2"; shift 2; else shift; fi
        done
        printf '=== Dry Run ===\nWorkdir:  %s\n--- User Message ---\n%s\n--- Tools ---\nlist_directory, read_file\n' "$workdir" "$input"
        ;;
    runtime)
        echo 'agent execution failed' >&2
        exit 1
        ;;
    configuration)
        echo 'agent not found in configured agents directory' >&2
        exit 2
        ;;
    provider)
        echo 'provider network timeout' >&2
        exit 3
        ;;
    budget)
        echo 'budget exceeded: used 21 of 20 tokens' >&2
        exit 4
        ;;
    malformed)
        echo 'not-json'
        ;;
esac
FAKE
chmod +x "$FAKE_BIN/axe"

cat > "$FAKE_BIN/inpage" <<'FAKE'
#!/usr/bin/env bash
cat > "$FAKE_INPAGE_STDIN"
jq -cn '{schema_version:1,outcome:"closed"}'
FAKE
chmod +x "$FAKE_BIN/inpage"

export FAKE_AXE_ARGV FAKE_AXE_STDIN FAKE_INPAGE_STDIN
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

run_scenario() {
    local scenario="$1"
    export FAKE_AXE_SCENARIO="$scenario"
    @ Tools::Axe run: 'trashtalk-readonly' input: '{"question":"test"}' workingDirectory: "$TEST_TMP/work dir"
}

echo "=== Axe Agent Integration Tests ==="

assert_eq "Axe version uses subcommand" 'axe version test-1.2.3' "$(@ Tools::Axe version)"

result=$(run_scenario success)
assert_eq "successful run outcome" 'success' "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "successful run preserves Axe content" $'answer line one\nanswer line two' \
    "$(printf '%s' "$result" | jq -r .result.content)"
assert_eq "successful run preserves original exit" '0' "$(printf '%s' "$result" | jq -r .exit_code)"
assert_true "normal run requests Axe JSON" grep -Fxq -- '--json' "$FAKE_AXE_ARGV"
assert_true "working directory is one exact argv" grep -Fxq -- "$TEST_TMP/work dir" "$FAKE_AXE_ARGV"
assert_true "project agent directory is explicit" grep -Fxq -- "$PROJECT_DIR/axe/agents" "$FAKE_AXE_ARGV"
assert_eq "input is passed exactly on stdin" '{"question":"test"}' "$(cat "$FAKE_AXE_STDIN")"

for mapping in 'runtime:1:runtime_error' 'configuration:2:configuration_error' \
    'provider:3:provider_error' 'budget:4:budget_exceeded'; do
    scenario=${mapping%%:*}
    rest=${mapping#*:}
    expected_code=${rest%%:*}
    expected_outcome=${rest#*:}
    result=$(run_scenario "$scenario")
    assert_eq "$scenario exit code preserved" "$expected_code" "$(printf '%s' "$result" | jq -r .exit_code)"
    assert_eq "$scenario outcome distinguished" "$expected_outcome" "$(printf '%s' "$result" | jq -r .outcome)"
done

result=$(run_scenario malformed)
assert_eq "malformed successful output is rejected" 'invalid_output' \
    "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "malformed output remains inspectable" 'not-json' \
    "$(printf '%s' "$result" | jq -r '.stdout | rtrimstr("\n")')"

export FAKE_AXE_SCENARIO=dry-run
dry_result=$(@ AxeAgent dryRun: 'why failed?' workingDirectory: "$TEST_TMP/work dir" status: '7' lastResult: $'old\nresult')
assert_eq "dry run succeeds without provider" 'success' "$(printf '%s' "$dry_result" | jq -r .outcome)"
assert_eq "dry run is marked in wrapper result" 'true' "$(printf '%s' "$dry_result" | jq -r .result.dry_run)"
assert_true "dry run uses Axe dry-run flag" grep -Fxq -- '--dry-run' "$FAKE_AXE_ARGV"
if grep -Fxq -- '--json' "$FAKE_AXE_ARGV"; then fail "dry run avoids Axe's context-dropping JSON mode"; else pass "dry run avoids Axe's context-dropping JSON mode"; fi
assert_eq "dry run context has question" 'why failed?' "$(jq -r .question "$FAKE_AXE_STDIN")"
assert_eq "dry run context has working directory" "$TEST_TMP/work dir" "$(jq -r .working_directory "$FAKE_AXE_STDIN")"
assert_eq "dry run context has status" '7' "$(jq -r .last_status "$FAKE_AXE_STDIN")"
assert_eq "dry run context has prior multiline result" $'old\nresult' "$(jq -r .last_result "$FAKE_AXE_STDIN")"
assert_true "dry run exposes only read-only tools" grep -Fq -- 'list_directory, read_file' <(printf '%s' "$dry_result" | jq -r .result.content)

export FAKE_AXE_SCENARIO=success
__=$'previous\noutput'
false
@@ 'what happened?' > "$TEST_TMP/at-at-output.txt"
at_status=$?
assert_eq "@@ returns Axe success status" '0' "$at_status"
assert_eq "@@ captures previous command status" '1' "$(jq -r .last_status "$FAKE_AXE_STDIN")"
assert_eq "@@ captures previous result" $'previous\noutput' "$(jq -r .last_result "$FAKE_AXE_STDIN")"
assert_eq "@@ presents final answer through inpage" $'answer line one\nanswer line two' "$(cat "$FAKE_INPAGE_STDIN")"
assert_true "@@ leaves final answer in scrollback" grep -Fq -- 'answer line one' "$TEST_TMP/at-at-output.txt"

export FAKE_AXE_SCENARIO=provider
@@ 'network?' > "$TEST_TMP/provider-output.txt"
at_status=$?
assert_eq "@@ preserves provider failure status" '3' "$at_status"
assert_true "@@ explains provider failure" grep -Fq -- 'Axe provider/network error (exit 3)' "$TEST_TMP/provider-output.txt"

PATH="/opt/homebrew/bin:/usr/bin:/bin"
hash -r
missing=$(@ Tools::Axe run: 'trashtalk-readonly' input: '{}' workingDirectory: "$TEST_TMP")
assert_eq "missing Axe has distinct outcome" 'missing_tool' "$(printf '%s' "$missing" | jq -r .outcome)"
assert_eq "missing Axe uses command-not-found status" '127' "$(printf '%s' "$missing" | jq -r .exit_code)"
ensure_output=$(@ Tools::Axe ensure 2>"$TEST_TMP/ensure-error.txt")
assert_eq "ensure does not silently install Axe" 'false' "$ensure_output"
assert_true "ensure provides installation guidance" grep -Fq -- 'Install it explicitly' "$TEST_TMP/ensure-error.txt"
PATH="$ORIGINAL_PATH"
export PATH
hash -r

assert_true "checked-in Axe agent exists" test -f "$PROJECT_DIR/axe/agents/trashtalk-readonly.toml"
assert_true "agent enables read-only file listing" grep -Fq -- 'tools = ["list_directory", "read_file"]' "$PROJECT_DIR/axe/agents/trashtalk-readonly.toml"
if grep -Eq 'write_file|edit_file|run_command|sub_agents[[:space:]]*=' "$PROJECT_DIR/axe/agents/trashtalk-readonly.toml"; then
    fail "agent enables no mutation, command, or delegation tools"
else
    pass "agent enables no mutation, command, or delegation tools"
fi

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
