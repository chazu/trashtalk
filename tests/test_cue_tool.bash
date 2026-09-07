#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

set -uo pipefail
export LC_ALL=C

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
FAKE_CUE_ARGV="$TEST_TMP/cue-argv.txt"
FAKE_CUE_STDIN="$TEST_TMP/cue-stdin.txt"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN"

# A fake cue that records exact argv and stdin, then plays a scenario.
cat > "$FAKE_BIN/cue" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$FAKE_CUE_ARGV"
: > "$FAKE_CUE_STDIN"
for arg in "$@"; do
    if [[ "$arg" == "-" ]]; then cat > "$FAKE_CUE_STDIN"; break; fi
done

if [[ "${1:-}" == "version" ]]; then
    printf 'cue version v0.14.0\n\n\t-compiler gc\n\tGOOS linux\n'
    exit 0
fi

case "${FAKE_CUE_SCENARIO:-success}" in
    success)
        case "${1:-}" in
            vet) ;;
            export) echo '{"name":"demo","port":8080}' ;;
            eval) printf 'name: "demo"\nport: int | *8080\n' ;;
            def) printf '#Config: {\n\tname: string\n}\n' ;;
            fmt) ;;
        esac
        ;;
    invalid)
        echo 'port: conflicting values 99999 and <=65535' >&2
        exit 1
        ;;
esac
FAKE
chmod +x "$FAKE_BIN/cue"

export FAKE_CUE_ARGV FAKE_CUE_STDIN
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

assert_argv() {
    local name="$1"
    shift
    local expected
    expected=$(printf '%s\n' "$@")
    assert_eq "$name" "$expected" "$(cat "$FAKE_CUE_ARGV")"
}

field() { printf '%s' "$1" | jq -r "$2"; }

echo "=== Tools::Cue Tests ==="

export FAKE_CUE_SCENARIO=success

assert_eq "version takes the first line of cue version" 'cue version v0.14.0' "$(@ Tools::Cue version)"

result=$(@ Tools::Cue vet: 'schema.cue')
assert_eq "vet success outcome" 'success' "$(field "$result" .outcome)"
assert_eq "vet result names the tool" 'cue' "$(field "$result" .tool)"
assert_eq "vet result names the command" 'vet' "$(field "$result" .command)"
assert_argv "vet passes one file as one argv entry" vet schema.cue

result=$(@ Tools::Cue vet: '["schema.cue","values with space.cue"]' concrete: true)
assert_argv "vet expands a JSON file array and -c" vet schema.cue 'values with space.cue' -c

result=$(@ Tools::Cue vet: 'schema.cue' json: '{"port": 80}')
assert_argv "vet json: reads stdin with a json qualifier" vet schema.cue 'json:' -
assert_eq "vet json: passes the document on stdin" '{"port": 80}' "$(cat "$FAKE_CUE_STDIN")"
assert_eq "isValid: json: is true on success" 'true' "$(@ Tools::Cue isValid: 'schema.cue' json: '{"port": 80}')"

result=$(@ Tools::Cue vet: 'schema.cue' yaml: $'port: 80\n')
assert_argv "vet yaml: uses a yaml qualifier" vet schema.cue 'yaml:' -

exported=$(@ Tools::Cue export: 'config.cue')
assert_eq "export returns JSON stdout" '{"name":"demo","port":8080}' "$exported"
assert_argv "export defaults to --out json" export config.cue --out json

exported=$(@ Tools::Cue export: 'config.cue' as: yaml)
assert_argv "export as: sets the output format" export config.cue --out yaml

exported=$(@ Tools::Cue export: 'config.cue' expression: 'server.port')
assert_argv "export expression: adds -e" export config.cue --out json -e server.port

unified=$(@ Tools::Cue unify: 'schema.cue' json: '{"name":"demo"}')
assert_argv "unify merges stdin json with schema files" export schema.cue 'json:' - --out json
assert_eq "unify passes the document on stdin" '{"name":"demo"}' "$(cat "$FAKE_CUE_STDIN")"
assert_eq "unify returns the exported document" '{"name":"demo","port":8080}' "$unified"

converted=$(@ Tools::Cue convert: $'name: demo\n' from: yaml to: json)
assert_argv "convert reads stdin in the source format" export 'yaml:' - --out json
assert_eq "convert passes the document on stdin" $'name: demo' "$(cat "$FAKE_CUE_STDIN")"

evaluated=$(@ Tools::Cue eval: 'config.cue')
assert_eq "eval returns CUE syntax" $'name: "demo"\nport: int | *8080' "$evaluated"
assert_argv "eval passes files" eval config.cue

evaluated=$(@ Tools::Cue eval: 'config.cue' expression: 'port')
assert_argv "eval expression: adds -e" eval config.cue -e port

defs=$(@ Tools::Cue def: 'schema.cue')
assert_argv "def passes files" def schema.cue
assert_eq "def returns definitions" $'#Config: {\n\tname: string\n}' "$defs"

result=$(@ Tools::Cue fmt: '["a.cue","b.cue"]')
assert_argv "fmt passes every file" fmt a.cue b.cue
assert_eq "fmt succeeds" 'success' "$(field "$result" .outcome)"

export FAKE_CUE_SCENARIO=invalid
result=$(@ Tools::Cue vet: 'schema.cue' json: '{"port": 99999}')
assert_eq "constraint failure is a validation_error" 'validation_error' "$(field "$result" .outcome)"
assert_eq "constraint failure preserves exit code" '1' "$(field "$result" .exit_code)"
assert_eq "constraint failure keeps cue diagnostics" 'port: conflicting values 99999 and <=65535' \
    "$(field "$result" '.stderr | rtrimstr("\n")')"
assert_eq "isValid: is false on failure" 'false' "$(@ Tools::Cue isValid: 'schema.cue')"

result=$(@ Tools::Cue exportResult: 'broken.cue')
assert_eq "export failure is an evaluation_error" 'evaluation_error' "$(field "$result" .outcome)"
exported=$(@ Tools::Cue export: 'broken.cue' 2>"$TEST_TMP/export-error.txt")
assert_eq "failed export projects to empty stdout" '' "$exported"
if grep -Fq 'conflicting values' "$TEST_TMP/export-error.txt"; then
    pass "failed export reports diagnostics on stderr"
else
    fail "failed export reports diagnostics on stderr"
fi

PATH="/usr/bin:/bin"
hash -r
missing=$(@ Tools::Cue vet: 'schema.cue')
assert_eq "missing cue has a distinct outcome" 'missing_tool' "$(field "$missing" .outcome)"
assert_eq "missing cue uses command-not-found status" '127' "$(field "$missing" .exit_code)"
if grep -Fq 'go install cuelang.org/go/cmd/cue@latest' <(field "$missing" .stderr); then
    pass "missing cue result carries install guidance"
else
    fail "missing cue result carries install guidance"
fi
assert_eq "missing cue version" 'not installed' "$(@ Tools::Cue version)"
PATH="$ORIGINAL_PATH"
export PATH
hash -r

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
