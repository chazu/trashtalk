#!/usr/bin/env bash

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
TEST_PACKAGE="BrowserMilestone${BASHPID}"
TEST_CLASS="$TEST_PACKAGE::Widget"
NAMESPACE_DIR="$PROJECT_DIR/trash/user/$TEST_PACKAGE"
SOURCE_FILE="$NAMESPACE_DIR/Widget.trash"
TRAIT_NAME="${TEST_PACKAGE}Trait"
TRAIT_FILE="$PROJECT_DIR/trash/traits/$TRAIT_NAME.trash"
FAKE_BIN="$TEST_TMP/bin"
CAPTURE_RECORDS="$TEST_TMP/picker-records.jsonl"
CAPTURE_PICKER_ARGV="$TEST_TMP/picker-argv.txt"
CAPTURE_EDITOR_ARGV="$TEST_TMP/editor-argv.txt"
CAPTURE_FZF_ROWS="$TEST_TMP/fzf-rows.txt"

cleanup() {
    rm -f "$SOURCE_FILE" "$TRAIT_FILE"
    rmdir "$NAMESPACE_DIR" 2>/dev/null || true
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN" "$NAMESPACE_DIR"

cat > "$SOURCE_FILE" <<EOF
package: $TEST_PACKAGE

Widget subclass: Object
  instanceVars: value:0
  classInstanceVars: tally:0

  method: at: index put: newValue [
    value := newValue.
    ^ value
  ]

  classMethod: from: first with: second [
    ^ first , second
  ]

  rawMethod: shellBoundary: input [
    printf '%s' "\$1"
  ]

  rawClassMethod: rawFactory [
    echo factory
  ]

  rawTestMethod: testKeywordMethod [
    true
  ]

  method: caller: index value: newValue [
    ^ @ self at: index put: newValue
  ]

  method: textOnly [
    ^ 'at:put:'
  ]
EOF

cat > "$TRAIT_FILE" <<EOF
$TRAIT_NAME trait

  method: traitValue [
    ^ 'trait'
  ]
EOF

cat > "$FAKE_BIN/inpick" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$CAPTURE_PICKER_ARGV"
records=$(cat)
printf '%s\n' "$records" > "$CAPTURE_RECORDS"
selection=$(printf '%s\n' "$records" | jq -c --arg id "${PICK_ID:-}" \
    'select($id == "" or .id == $id)' | head -1)
if [[ -z "$selection" ]]; then
    jq -cn '{schema_version:1,outcome:"cancelled",selection:null}'
    exit 130
fi
jq -cn --argjson selection "$selection" \
    '{schema_version:1,outcome:"selected",selection:$selection}'
FAKE
chmod +x "$FAKE_BIN/inpick"

cat > "$FAKE_BIN/inmacs" <<'FAKE'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$CAPTURE_EDITOR_ARGV"
source_path="${!#}"
jq -cn --arg path "$source_path" \
    '{schema_version:1,outcome:"unchanged",path:$path,changed:false,cursor:{line:1,column:1},edit_count:0}'
FAKE
chmod +x "$FAKE_BIN/inmacs"

cat > "$FAKE_BIN/fzf" <<'FAKE'
#!/usr/bin/env bash
tee "$CAPTURE_FZF_ROWS" | head -1
FAKE
chmod +x "$FAKE_BIN/fzf"

export CAPTURE_RECORDS CAPTURE_PICKER_ARGV CAPTURE_EDITOR_ARGV CAPTURE_FZF_ROWS
export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null
# trash.bash installs its own EXIT handler, so our fixture cleanup comes after it.
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

assert_jq() {
    local name="$1" file="$2" expression="$3"
    if jq -e "$expression" "$file" >/dev/null; then pass "$name"; else fail "$name"; fi
}

echo "=== Symbol and Instance Browser Tests ==="

records=$(@ Trash symbolRecords)
printf '%s\n' "$records" > "$TEST_TMP/symbols.jsonl"

assert_jq "namespaced class record" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS\" and .kind == \"class\")"
assert_jq "trait record" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TRAIT_NAME\" and .kind == \"trait\")"
assert_jq "instance variable record" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS.value\" and .kind == \"instance_variable\")"
assert_jq "class variable record" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS class.tally\" and .kind == \"class_variable\")"
assert_jq "multi-keyword instance method" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS>>at:put:\" and .kind == \"instance_method\" and .raw == false)"
assert_jq "multi-keyword class method" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS class>>from:with:\" and .kind == \"class_method\")"
assert_jq "raw instance method" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS>>shellBoundary:\" and .raw == true)"
assert_jq "raw class method" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS class>>rawFactory\" and .raw == true)"
assert_jq "test method" "$TEST_TMP/symbols.jsonl" \
    "select(.id == \"$TEST_CLASS test>>testKeywordMethod\" and .kind == \"test_method\")"

expected_line=$(grep -n 'method: at:' "$SOURCE_FILE" | cut -d: -f1)
actual_line=$(jq -r "select(.id == \"$TEST_CLASS>>at:put:\") | .line" "$TEST_TMP/symbols.jsonl")
assert_eq "method location is source-accurate" "$expected_line" "$actual_line"

implementors=$(@ Trash implementorsOf: 'at:put:')
assert_eq "implementors use complete selector" "1" \
    "$(printf '%s\n' "$implementors" | jq -s --arg owner "$TEST_CLASS" \
      '[.[] | select(.class_name == $owner and .selector == "at:put:")] | length')"

senders=$(@ Trash sendersOf: 'at:put:')
assert_eq "senders group keyword parts" "1" \
    "$(printf '%s\n' "$senders" | jq -s --arg owner "$TEST_CLASS" \
      '[.[] | select(.class_name == $owner and .selector == "caller:value:")] | length')"
partial_senders=$(@ Trash sendersOf: 'put:')
assert_eq "senders reject a trailing keyword fragment" "0" \
    "$(printf '%s\n' "$partial_senders" | jq -s --arg owner "$TEST_CLASS" \
      '[.[] | select(.class_name == $owner)] | length')"

export PICK_ID="$TEST_CLASS>>at:put:"
browse_result=$(@ Trash browseClass: "$TEST_CLASS")
assert_eq "browser opens selected source" "unchanged" "$(printf '%s\n' "$browse_result" | tail -1)"
assert_eq "browser passes exact selected line" "$expected_line" \
    "$(awk 'previous == "--line" {print; exit} {previous=$0}' "$CAPTURE_EDITOR_ARGV")"
assert_eq "browser passes exact selected path" "$SOURCE_FILE" \
    "$(tail -1 "$CAPTURE_EDITOR_ARGV")"
assert_jq "picker receives JSON records unchanged" "$CAPTURE_RECORDS" \
    "select(.id == \"$TEST_CLASS>>at:put:\" and .path == \"$SOURCE_FILE\")"

fallback_result=$(@ Tools::Inpick fallbackSelectRecords: \
    "$(jq -c "select(.id == \"$TEST_CLASS>>at:put:\")" "$TEST_TMP/symbols.jsonl")" \
    query: '' title: 'Fallback')
assert_eq "fzf fallback preserves selected JSON" "$TEST_CLASS>>at:put:" \
    "$(printf '%s' "$fallback_result" | jq -r '.selection.id')"

counter=$(@ Counter create)
instance_records=$(@ Trash instanceRecordsFor: Counter)
printf '%s\n' "$instance_records" > "$TEST_TMP/instances.jsonl"
assert_jq "persisted instance record includes object data" "$TEST_TMP/instances.jsonl" \
    "select(.id == \"$counter\" and .kind == \"instance\" and .class_name == \"Counter\" and .data.value == 0)"
assert_eq "instance label keeps class and a compact identity" \
    "Counter ${counter: -8}" \
    "$(printf '%s\n' "$instance_records" | jq -r --arg id "$counter" 'select(.id == $id) | .label')"
assert_eq "instance detail is a readable state summary" "value=0 | step=1" \
    "$(printf '%s\n' "$instance_records" | jq -r --arg id "$counter" 'select(.id == $id) | .detail')"

fallback_instance_result=$(@ Tools::Inpick fallbackSelectRecords: \
    "$(printf '%s\n' "$instance_records" | jq -c --arg id "$counter" 'select(.id == $id)')" \
    query: '' title: 'Instances: Counter')
assert_eq "fzf fallback puts compact identity before instance state" \
    $'Counter '"${counter: -8}"$'\tinstance\tvalue=0 | step=1' \
    "$(cut -f1-3 "$CAPTURE_FZF_ROWS")"
assert_eq "readable fallback preserves the full selected object id" "$counter" \
    "$(printf '%s' "$fallback_instance_result" | jq -r '.selection.object_id')"

export PICK_ID="$counter"
instance_result=$(@ Trash browseInstancesOf: Counter)
assert_eq "instance browser returns selected object" "$counter" \
    "$(printf '%s' "$instance_result" | jq -r '.selection.object_id')"

@ "$counter" delete >/dev/null 2>&1 || true

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
