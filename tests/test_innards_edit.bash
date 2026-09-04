#!/usr/bin/env bash

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
TEST_CLASS="EditMilestone${BASHPID}"
TEST_PACKAGE="EditPackage${BASHPID}"
TEST_QUALIFIED="$TEST_PACKAGE::Widget"
SOURCE_FILE="$PROJECT_DIR/trash/user/${TEST_CLASS}.trash"
COMPILED_FILE="$PROJECT_DIR/trash/.compiled/${TEST_CLASS}"
NAMESPACE_DIR="$PROJECT_DIR/trash/user/$TEST_PACKAGE"
NAMESPACE_SOURCE="$NAMESPACE_DIR/Widget.trash"
NAMESPACE_COMPILED="$PROJECT_DIR/trash/.compiled/${TEST_PACKAGE}__Widget"
FAKE_BIN="$TEST_TMP/bin"
FAKE_INMACS="$FAKE_BIN/inmacs"
FAKE_EDITOR="$FAKE_BIN/fallback-editor"
FAKE_COUNT_FILE="$TEST_TMP/inmacs-count"
FAKE_ANNOTATIONS_CAPTURE="$TEST_TMP/annotations.json"
FAKE_ARGV_FILE="$TEST_TMP/argv.txt"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -f "$SOURCE_FILE" "$COMPILED_FILE" "$NAMESPACE_SOURCE" "$NAMESPACE_COMPILED"
    rmdir "$NAMESPACE_DIR" 2>/dev/null || true
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN" "$PROJECT_DIR/trash/user"

cat > "$FAKE_INMACS" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail

printf '%s\n' "$@" > "$FAKE_ARGV_FILE"
annotation_path=""
source_path=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --annotations)
            annotation_path="$2"
            shift 2
            ;;
        --line|--syntax|--tab-width)
            shift 2
            ;;
        --result-json)
            shift
            ;;
        *)
            source_path="$1"
            shift
            ;;
    esac
done

count=0
[[ -f "$FAKE_COUNT_FILE" ]] && count=$(cat "$FAKE_COUNT_FILE")
count=$((count + 1))
printf '%s' "$count" > "$FAKE_COUNT_FILE"

if [[ $count -gt 1 ]]; then
    [[ -n "$annotation_path" ]] && cp "$annotation_path" "$FAKE_ANNOTATIONS_CAPTURE"
    outcome="${FAKE_RETRY_OUTCOME:-cancelled}"
else
    outcome="${FAKE_OUTCOME:-unchanged}"
    case "${FAKE_SCENARIO:-}" in
        success)
            printf '\n' >> "$source_path"
            outcome="saved"
            ;;
        compile-failure)
            printf '\n  method: broken [\n' >> "$source_path"
            outcome="saved"
            ;;
        test-failure)
            printf '\n' >> "$source_path"
            outcome="saved"
            ;;
    esac
fi

jq -cn --arg outcome "$outcome" --arg path "$source_path" \
    '{schema_version:1,outcome:$outcome,path:$path,changed:($outcome == "saved"),cursor:{line:1,column:1},edit_count:0}'
FAKE
chmod +x "$FAKE_INMACS"
ln -s inmacs "$FAKE_BIN/inpage"

cat > "$FAKE_EDITOR" <<'FAKE'
#!/usr/bin/env bash
printf '\n# fallback edit\n' >> "$1"
FAKE
chmod +x "$FAKE_EDITOR"

export FAKE_COUNT_FILE FAKE_ANNOTATIONS_CAPTURE FAKE_ARGV_FILE
export PATH="$FAKE_BIN:$PATH"
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null
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

assert_not_contains() {
    if grep -q "$2" "$3"; then fail "$1"; else pass "$1"; fi
}

reset_fixture() {
    rm -f "$FAKE_COUNT_FILE" "$FAKE_ANNOTATIONS_CAPTURE" "$FAKE_ARGV_FILE" "$COMPILED_FILE"
    cat > "$SOURCE_FILE" <<EOF
$TEST_CLASS subclass: Object

  method: value [
    ^ 'ok'
  ]
EOF
}

echo "=== Innards Edit Integration Tests ==="

multiline_arg=$'line one\nline two'
argv_json=$(jq -cn --arg value "$multiline_arg" '["printf", "%s", $value]')
assert_eq "process boundary preserves embedded newlines" "$multiline_arg" "$(@ Tool runArgvJson: "$argv_json")"
stdin_argv='["/bin/cat"]'
assert_eq "process boundary preserves exact stdin" "$multiline_arg" \
    "$(@ Tool runArgvJson: "$stdin_argv" input: "$multiline_arg")"
capture_argv=$(jq -cn '["/bin/bash", "-c", "printf stdout-text; printf stderr-text >&2; exit 3"]')
capture=$(@ Tool captureArgvJson: "$capture_argv")
assert_eq "capturing process boundary preserves child status" "3" \
    "$(printf '%s' "$capture" | jq -r .exit_code)"
assert_eq "capturing process boundary keeps stdout separate" "stdout-text" \
    "$(printf '%s' "$capture" | jq -r .stdout)"
assert_eq "capturing process boundary keeps stderr separate" "stderr-text" \
    "$(printf '%s' "$capture" | jq -r .stderr)"
capture_stdin_argv=$(jq -cn '["/bin/bash", "-c", "cat; exit 5"]')
capture=$(@ Tool captureArgvJson: "$capture_stdin_argv" input: "$multiline_arg")
assert_eq "capturing stdin pipeline preserves child status" "5" \
    "$(printf '%s' "$capture" | jq -r .exit_code)"
assert_eq "capturing stdin pipeline preserves exact input" "$multiline_arg" \
    "$(printf '%s' "$capture" | jq -r .stdout)"
if @ Tool runArgvJson: '["printf", 3]' >/dev/null 2>&1; then
    fail "process boundary rejects non-string argv entries"
else
    pass "process boundary rejects non-string argv entries"
fi

reset_fixture
annotation_file="$TEST_TMP/input-annotations.json"
printf '%s\n' '{"schema_version":1,"annotations":[]}' > "$annotation_file"
export FAKE_OUTCOME=unchanged FAKE_SCENARIO=""
wrapper_result=$(@ Tools::Inmacs editFile: "$SOURCE_FILE" annotations: "$annotation_file" line: 7)
assert_eq "wrapper returns structured result" "unchanged" "$(printf '%s' "$wrapper_result" | jq -r .outcome)"
assert_true "wrapper passes result flag" grep -qx -- '--result-json' "$FAKE_ARGV_FILE"
assert_true "wrapper passes annotation flag" grep -qx -- '--annotations' "$FAKE_ARGV_FILE"
assert_true "wrapper passes requested line" grep -qx -- '7' "$FAKE_ARGV_FILE"
assert_true "wrapper passes two-space tab width" grep -qx -- '2' "$FAKE_ARGV_FILE"

rm -f "$FAKE_COUNT_FILE" "$FAKE_ARGV_FILE"
page_result=$(@ Tools::Inpage viewFile: "$SOURCE_FILE" annotations: "$annotation_file" line: 9)
assert_eq "pager wrapper returns structured result" "unchanged" "$(printf '%s' "$page_result" | jq -r .outcome)"
assert_true "pager wrapper passes result flag" grep -qx -- '--result-json' "$FAKE_ARGV_FILE"
assert_true "pager wrapper passes annotation flag" grep -qx -- '--annotations' "$FAKE_ARGV_FILE"
assert_true "pager wrapper passes requested line" grep -qx -- '9' "$FAKE_ARGV_FILE"
assert_true "pager wrapper passes source path" grep -Fxq -- "$SOURCE_FILE" "$FAKE_ARGV_FILE"

reset_fixture
export FAKE_OUTCOME=unchanged FAKE_SCENARIO=""
result=$(@ Trash edit: "$TEST_CLASS")
assert_eq "unchanged edit stops before compilation" "unchanged" "$(printf '%s\n' "$result" | tail -1)"
assert_true "unchanged edit leaves no artifact" test ! -e "$COMPILED_FILE"

reset_fixture
export FAKE_OUTCOME=discarded FAKE_SCENARIO=""
result=$(@ Trash edit: "$TEST_CLASS")
assert_eq "discarded edit is explicit" "discarded" "$(printf '%s\n' "$result" | tail -1)"
assert_true "discarded edit leaves no artifact" test ! -e "$COMPILED_FILE"

reset_fixture
export FAKE_OUTCOME=cancelled FAKE_SCENARIO=""
result=$(@ Trash edit: "$TEST_CLASS")
assert_eq "cancelled edit stops cleanly" "cancelled" "$(printf '%s\n' "$result" | tail -1)"

reset_fixture
export FAKE_SCENARIO=success FAKE_OUTCOME=saved
result=$(@ Trash edit: "$TEST_CLASS")
assert_eq "saved edit compiles, installs, reloads, and tests" "saved" "$(printf '%s\n' "$result" | tail -1)"
assert_true "successful edit installs valid Bash" bash -n "$COMPILED_FILE"
assert_not_contains "successful edit does not inject diagnostics" 'COMPILE ERRORS' "$SOURCE_FILE"

rm -f "$FAKE_COUNT_FILE" "$NAMESPACE_COMPILED"
mkdir -p "$NAMESPACE_DIR"
cat > "$NAMESPACE_SOURCE" <<EOF
package: $TEST_PACKAGE

Widget subclass: Object

  method: value [
    ^ 'namespaced'
  ]
EOF
export FAKE_SCENARIO=success FAKE_OUTCOME=saved
namespace_output="$TEST_TMP/namespace-output.txt"
@ Trash edit: "$TEST_QUALIFIED" > "$namespace_output"
result=$(cat "$namespace_output")
assert_eq "namespaced edit compiles, reloads, and tests" "saved" "$(printf '%s\n' "$result" | tail -1)"
assert_true "namespaced edit installs flattened artifact" test -f "$NAMESPACE_COMPILED"
namespace_instance=$(@ "$TEST_QUALIFIED" new)
assert_eq "namespaced artifact remains dispatchable" "namespaced" "$(@ "$namespace_instance" value)"
@ "$namespace_instance" destroy >/dev/null 2>&1 || true

reset_fixture
export FAKE_SCENARIO=compile-failure FAKE_OUTCOME=saved FAKE_RETRY_OUTCOME=cancelled
result=$(@ Trash edit: "$TEST_CLASS" 2>/dev/null)
assert_eq "compile failure reopens and can be cancelled" "cancelled" "$(printf '%s\n' "$result" | tail -1)"
assert_eq "compile failure invokes editor twice" "2" "$(cat "$FAKE_COUNT_FILE")"
assert_true "compile diagnostic is a valid annotation document" jq -e '.schema_version == 1 and .annotations[0].severity == "error"' "$FAKE_ANNOTATIONS_CAPTURE"
assert_not_contains "compile diagnostics stay out of source" 'COMPILE ERRORS' "$SOURCE_FILE"

reset_fixture
cat > "$SOURCE_FILE" <<EOF
$TEST_CLASS subclass: Object

  rawTestMethod: testFailure [
    _assert_eq 'actual' 'expected' 'intentional edit-loop failure'
  ]
EOF
export FAKE_SCENARIO=test-failure FAKE_OUTCOME=saved FAKE_RETRY_OUTCOME=cancelled
result=$(@ Trash edit: "$TEST_CLASS" 2>/dev/null)
assert_eq "test failure reopens and can be cancelled" "cancelled" "$(printf '%s\n' "$result" | tail -1)"
assert_true "test failure is identified in annotations" jq -e '.annotations[0].message | startswith("test:")' "$FAKE_ANNOTATIONS_CAPTURE"

invalid_bash="$TEST_TMP/invalid.bash"
printf '%s\n' 'if then' > "$invalid_bash"
validation=$(@ Trash validateCompiledCandidate: "$invalid_bash")
assert_eq "Bash syntax failure is structured" "false" "$(printf '%s' "$validation" | jq -r .ok)"
assert_eq "Bash syntax failure identifies its phase" "bash" "$(printf '%s' "$validation" | jq -r .phase)"
assert_true "invalid candidate is removed" test ! -e "$invalid_bash"

reset_fixture
mv "$FAKE_INMACS" "$FAKE_BIN/inmacs.hidden"
export PATH="/opt/homebrew/bin:/usr/bin:/bin"
export VISUAL="$FAKE_EDITOR"
fallback=$(@ Trash editFile: "$SOURCE_FILE" diagnostics: '' line: 1)
assert_eq "missing Innards uses editor fallback" "saved" "$(printf '%s' "$fallback" | jq -r .outcome)"
assert_true "fallback editor changed the source" grep -q 'fallback edit' "$SOURCE_FILE"
export PATH="$ORIGINAL_PATH"

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
