#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
UI_LOG="$TEST_TMP/ui.jsonl"

cleanup() {
    [[ -n "${counter:-}" ]] && @ "$counter" delete >/dev/null 2>&1 || true
    rm -rf "$TEST_TMP"
}

# A scripted surface: records every frame it receives and replays the intents
# in $UI_SCRIPT, one per handler response, as the real inui would send them.
mkdir -p "$FAKE_BIN"
cat > "$FAKE_BIN/inui" <<'FAKE'
#!/usr/bin/env python3
import json, os, sys
log = open(os.environ['UI_LOG'], 'a')
def receive():
    line = sys.stdin.readline()
    log.write(line); log.flush()
    return json.loads(line)
receive()
for request_id, intent in enumerate(json.loads(os.environ.get('UI_SCRIPT', '[]')), 1):
    print(json.dumps(dict(schema_version=1, view='inspector', request_id=request_id, intent='action', **intent)), flush=True)
    while receive().get('type') != 'ack': pass
FAKE
chmod +x "$FAKE_BIN/inui"

export UI_LOG
export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
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
    if "$@" >/dev/null 2>&1; then pass "$name"; else fail "$name"; fi
}

echo "=== Object Inspector Tests ==="

counter=$(@ Counter create)

# Without a human at a terminal every entry point describes the object as text.
described=$(@ "$counter" inspect < /dev/null)
assert_eq "non-interactive inspect prints the textual description" \
    "a Counter|  id: $counter|  value: 0|  step: 1" "$(printf '%s' "$described" | paste -sd'|' -)"
assert_eq "Trash inspectObject: shares the inspect entry point" "$described" "$(@ Trash inspectObject: "$counter" < /dev/null)"
assert_fails() {
    local name="$1"
    shift
    if "$@" >/dev/null 2>&1; then fail "$name"; else pass "$name"; fi
}
assert_fails "a missing object is an error" @ UI::Inspector openObject: counter_missing

# Handler contract: drill to a leaf, stage an edit, and commit it.
record=$(@ UI::Inspector recordFor: "$counter")
context=$(jq -cn --argjson record "$record" '{record:$record,title:"Object inspector",paths:[[]],active:0,revision:0,editable:true}')
handle() { @ UI::Inspector handleFrame: "$1" context: "$context"; }
frame() { jq -cn --argjson id "$1" --arg widget "$2" --arg action "$3" --argjson value "$4" \
    '{schema_version:1,view:"inspector",request_id:$id,intent:"action",widget:$widget,action:$action,value:$value}'; }

result=$(handle "$(frame 1 'pane-[]' drill '{"key":"data"}')")
context=$(jq -c .context <<< "$result")
result=$(handle "$(frame 2 'pane-["data"]' drill '{"key":"value"}')")
context=$(jq -c .context <<< "$result")
assert_true "Enter on a scalar ivar opens its editor" jq -e \
    '.context.edit.path == ["data","value"] and (.frames[0].root.children[1].children[0] | .kind == "input" and .props.value == "0")' <<< "$result"

readonly_context=$(jq -c '.editable = false | .edit = null' <<< "$context")
assert_fails "records opened read-only never offer an editor" \
    @ UI::Inspector handleFrame: "$(frame 3 'pane-["data"]' drill '{"key":"value"}')" context: "$readonly_context"

result=$(handle "$(frame 4 apply apply '"seven x"')")
assert_true "a draft that is not JSON is rejected without a commit" jq -e \
    '.frames[1].ok == false and (.frames[1].message | test("JSON"))' <<< "$result"
assert_eq "rejected draft leaves state unchanged" "0" "$(@ "$counter" getValue)"

result=$(handle "$(frame 5 'edit-["data","value"]' apply '"7"')")
assert_true "an applied edit is acknowledged and closes the editor" jq -e \
    '.frames[1].ok == true and .context.edit == null and .context.record.data.value == 7' <<< "$result"
assert_eq "applied edit preserves the numeric type" "7" "$(@ "$counter" getValue)"

# The editor still holds the snapshot that showed value=0; meanwhile it changed.
@ "$counter" setValue: 3 >/dev/null
result=$(handle "$(frame 6 apply apply '"9"')")
assert_true "a concurrent change makes the edit stale and refreshes the pane" jq -e \
    '.frames[1].ok == false and (.frames[1].message | test("changed")) and .context.record.data.value == 3' <<< "$result"
assert_eq "stale edit cannot change object state" "3" "$(@ "$counter" getValue)"

# The real session bridge, with a terminal present, drives the same handler.
_trash_interactive_terminal() { true; }
export UI_SCRIPT='[{"widget":"pane-[]","action":"drill","value":{"key":"data"}},
  {"widget":"pane-[\"data\"]","action":"drill","value":{"key":"step"}},
  {"widget":"edit-[\"data\",\"step\"]","action":"apply","value":"5"}]'
: > "$UI_LOG"
@ "$counter" inspect
assert_true "interactive inspect opens the object inspector surface" jq -se \
    --arg id "$counter" '.[0].view == "inspector" and .[0].collections[0].rows[1].fields.text == ("object_id: " + ($id|tojson))' "$UI_LOG"
assert_true "surface edit is acknowledged" jq -se '.[-1].type == "ack" and .[-1].ok == true' "$UI_LOG"
assert_eq "surface edit commits through the proposal" "5" "$(@ "$counter" getStep)"

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
