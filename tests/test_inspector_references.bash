#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

# Object references in the inspector: ids in object state are resolved when a
# record is built, shown as links, and loaded only when drilled.

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
UI_LOG="$TEST_TMP/ui.jsonl"
MISSING_ID="counter_DEADBEEF-0000-0000-0000-000000000000"

cleanup() {
    rm -f "$PROJECT_DIR/trash/TestRefHolder.trash" "$PROJECT_DIR/trash/.compiled/TestRefHolder"
    rm -rf "$TEST_TMP"
}
trap cleanup EXIT

mkdir -p "$FAKE_BIN"
cat > "$PROJECT_DIR/trash/TestRefHolder.trash" <<'EOF'
TestRefHolder subclass: Object
  instanceVars: target:'' other:'' note:'plain_snake_value'

  method: target: anObject [
    target := anObject.
  ]

  method: other: anObject [
    other := anObject.
  ]
EOF
bash "$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/TestRefHolder.trash" \
    -o "$PROJECT_DIR/trash/.compiled/TestRefHolder" --check >/dev/null || exit 1

# A scripted surface: records every frame and replays $UI_SCRIPT intents.
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

assert_fails() {
    local name="$1"
    shift
    if "$@" >/dev/null 2>&1; then fail "$name"; else pass "$name"; fi
}

echo "=== Inspector References ==="

counter=$(@ Counter new)
inner=$(@ TestRefHolder new)
outer=$(@ TestRefHolder new)
@ "$inner" target: "$counter" >/dev/null
@ "$inner" other: "$MISSING_ID" >/dev/null
@ "$outer" target: "$inner" >/dev/null

assert_eq "references resolve to classes; an absent id-shaped value is null; plain text is ignored" \
    "{\"$counter\":\"Counter\",\"$MISSING_ID\":null}" "$(@ Runtime referencesIn: "{\"a\":\"$counter\",\"b\":[\"$MISSING_ID\",\"plain_snake_value\"],\"c\":3}")"
assert_eq "state without references has none" '{}' "$(@ Runtime referencesIn: '{"a":"x","b":[1,2]}')"

record=$(@ UI::Inspector recordFor: "$outer")
assert_true "a record carries the references in its state" jq -e --arg inner "$inner" \
    '.refs == {($inner): "TestRefHolder"}' <<< "$record"

context=$(jq -cn --argjson record "$record" '{record:$record,title:"Object inspector",paths:[[]],active:0,revision:0,editable:true}')
handle() { @ UI::Inspector handleFrame: "$1" context: "$context"; }
frame() { jq -cn --argjson id "$1" --arg widget "$2" --arg action "$3" --argjson value "$4" \
    '{schema_version:1,view:"inspector",request_id:$id,intent:"action",widget:$widget,action:$action,value:$value}'; }
data_rows() { jq -c '[.frames[0].collections[-1].rows[].fields.text]' <<< "$1"; }

result=$(handle "$(frame 1 'pane-[]' drill '{"key":"data"}')")
context=$(jq -c .context <<< "$result")
assert_true "a reference is shown as a drillable link" jq -e --arg inner "$inner" \
    '.frames[0].collections[1].rows | map(select(.key == "target"))[0].fields
     | .text == ("target: → TestRefHolder " + $inner) and .drillable == "true"' <<< "$result"
assert_true "id-shaped plain text stays plain" jq -e \
    '.frames[0].collections[1].rows | map(select(.key == "note"))[0].fields.text == "note: \"plain_snake_value\""' <<< "$result"
assert_true "records are not loaded before they are drilled" jq -e '.context.records == null' <<< "$result"

result=$(handle "$(frame 2 'pane-["data"]' drill '{"key":"target"}')")
context=$(jq -c .context <<< "$result")
inner_pane=$(jq -cn --arg inner "$inner" '["data","target",{ref:$inner},"data"]')
assert_true "drilling a link loads the referenced object" jq -e --arg inner "$inner" \
    '.context.records[$inner].class_name == "TestRefHolder" and .frames[1].ok == true' <<< "$result"
assert_true "the referenced object's pane is titled with its class and id" jq -e --arg inner "$inner" \
    '.frames[0].root.children[0].children[2].props.title == ("TestRefHolder " + $inner)' <<< "$result"
assert_eq "the referenced object's state, its own links, and a dangling id are shown" \
    "[\"target: → Counter $counter\",\"other: \\\"$MISSING_ID\\\" (missing object)\",\"note: \\\"plain_snake_value\\\"\"]" \
    "$(data_rows "$result")"
assert_true "a dangling reference is text: Enter edits it rather than opening a pane" jq -e \
    --argjson pane "$inner_pane" '.context.paths[-1] == $pane and .context.edit.path == ($pane + ["other"])' \
    <<< "$(handle "$(frame 3 "pane-$inner_pane" drill '{"key":"other"}')")"

result=$(handle "$(frame 4 "pane-$inner_pane" drill '{"key":"target"}')")
context=$(jq -c .context <<< "$result")
counter_pane=$(jq -cn --arg inner "$inner" --arg counter "$counter" '["data","target",{ref:$inner},"data","target",{ref:$counter},"data"]')
assert_eq "links can be followed through several objects" '["value: 0","step: 1"]' "$(data_rows "$result")"

result=$(handle "$(frame 5 "pane-$counter_pane" drill '{"key":"value"}')")
context=$(jq -c .context <<< "$result")
assert_true "a scalar in a referenced object opens its editor" jq -e \
    '.context.edit != null and (.frames[0].root.children[1].children[0].props | .title == "Edit value as JSON" and .value == "0")' <<< "$result"
result=$(handle "$(frame 6 apply apply '"4"')")
context=$(jq -c .context <<< "$result")
assert_true "the edit is applied to the referenced object" jq -e --arg counter "$counter" \
    '.frames[1].ok == true and .context.records[$counter].data.value == 4' <<< "$result"
assert_eq "the referenced object's state changed" "4" "$(@ "$counter" getValue)"
assert_eq "the root object's state is untouched" "$inner" "$(jq -r .context.record.data.target <<< "$result")"

# A reference that disappears after the record was built is reported, not shown.
ghost=$(@ Counter new)
@ "$outer" other: "$ghost" >/dev/null
record=$(@ UI::Inspector recordFor: "$outer")
context=$(jq -cn --argjson record "$record" '{record:$record,title:"Object inspector",paths:[[],["data"]],active:1,revision:0,editable:true}')
@ "$ghost" delete >/dev/null
sqlite3 "$SQLITE_JSON_DB" "DELETE FROM instances WHERE id = '$ghost';"
result=$(handle "$(frame 7 'pane-["data"]' drill '{"key":"other"}')")
assert_true "a reference deleted since display is refused with a message" jq -e --arg ghost "$ghost" \
    '.frames[1].ok == false and .frames[1].message == ("Object no longer exists: " + $ghost) and .context.active == 1' <<< "$result"

# The real session bridge drives the same handler, including record loads.
_trash_interactive_terminal() { true; }
export UI_SCRIPT='[{"widget":"pane-[\"data\"]","action":"drill","value":{"key":"target"}}]'
: > "$UI_LOG"
@ "$outer" inspect >/dev/null 2>&1
assert_true "the interactive inspector follows a link" jq -se --arg counter "$counter" \
    '[.[] | select(.type == "ack")] | all(.ok) and length == 1' "$UI_LOG"
assert_true "the linked object's links are shown in the surface" jq -se --arg counter "$counter" \
    '[.[] | select(.type == "init")][-1].collections[-1].rows[0].fields.text == ("target: → Counter " + $counter)' "$UI_LOG"

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
