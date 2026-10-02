#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

# Class instance variables at runtime (declared defaults, per-class values,
# inheritance, namespaces) and read-only class inspection.

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
UI_LOG="$TEST_TMP/ui.jsonl"
PACKAGE_DIR="$PROJECT_DIR/trash/ClassInspection"
FIXTURES=(TestRegistry TestSubRegistry ClassInspection/Thing)

cleanup() {
    local fixture
    for fixture in "${FIXTURES[@]}"; do
        rm -f "$PROJECT_DIR/trash/$fixture.trash" "$PROJECT_DIR/trash/.compiled/${fixture//\//__}"
    done
    rmdir "$PACKAGE_DIR" 2>/dev/null || true
    rm -rf "$TEST_TMP"
}
trap cleanup EXIT

mkdir -p "$FAKE_BIN" "$PACKAGE_DIR"

cat > "$PROJECT_DIR/trash/TestRegistry.trash" <<'EOF'
TestRegistry subclass: Object
  classInstanceVars: count:0 label:'main hall' note

  classMethod: bump [
    count := count + 1.
    ^ count
  ]

  classMethod: count [
    ^ count
  ]

  classMethod: label [
    ^ label
  ]

  classMethod: note [
    ^ note
  ]
EOF

cat > "$PROJECT_DIR/trash/TestSubRegistry.trash" <<'EOF'
TestSubRegistry subclass: TestRegistry
  classInstanceVars: extra:5

  classMethod: peek [
    ^ count
  ]

  classMethod: absorb [
    extra := extra + count.
    ^ extra
  ]
EOF

cat > "$PACKAGE_DIR/Thing.trash" <<'EOF'
package: ClassInspection

Thing subclass: Object
  classInstanceVars: hits:5

  classMethod: hits [
    ^ hits
  ]
EOF

for fixture in "${FIXTURES[@]}"; do
    bash "$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$PROJECT_DIR/trash/$fixture.trash" \
        -o "$PROJECT_DIR/trash/.compiled/${fixture//\//__}" --check >/dev/null || exit 1
done

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

echo "=== Class Instance Variables ==="

assert_eq "a numeric default is read before any assignment" "0" "$(@ TestRegistry count)"
assert_eq "a string default keeps its spaces" "main hall" "$(@ TestRegistry label)"
assert_eq "a variable without a default reads empty" "" "$(@ TestRegistry note)"
@ TestRegistry bump >/dev/null
assert_eq "assignment starts from the default" "2" "$(@ TestRegistry bump)"
assert_eq "assigned values persist across processes" "2" \
    "$(bash -c 'source "$1/lib/trash.bash" 2>/dev/null; @ TestRegistry count' _ "$PROJECT_DIR")"
assert_eq "a namespaced class reads its declared default" "5" "$(@ ClassInspection::Thing hits)"

assert_eq "a subclass reads the inherited default, not its superclass's value" "0" "$(@ TestSubRegistry count)"
assert_eq "a subclass keeps its own value" "1" "$(@ TestSubRegistry bump)"
assert_eq "the superclass value is unaffected" "2" "$(@ TestRegistry count)"
assert_eq "a subclass method reads an inherited class variable" "1" "$(@ TestSubRegistry peek)"
assert_eq "a subclass method combines own and inherited class variables" "6" "$(@ TestSubRegistry absorb)"

echo "=== Class State ==="

assert_eq "class state lists declared variables with current values" \
    '{"count":2,"label":"main hall","note":""}' "$(@ Runtime classStateFor: TestRegistry)"
assert_eq "subclass state lists inherited declarations first" \
    '{"count":1,"label":"main hall","note":"","extra":6}' "$(@ Runtime classStateFor: TestSubRegistry)"
assert_eq "a class without class instance variables has empty state" '{}' "$(@ Runtime classStateFor: Counter)"
assert_fails "an unknown class has no state" @ Runtime classStateFor: NoSuchClass

echo "=== Class Inspection ==="

assert_eq "a class prints as its name" "TestRegistry" "$(@ TestRegistry printString)"
counter=$(@ Counter new)
assert_eq "an object still prints with its id" "<Counter $counter>" "$(@ "$counter" printString)"
@ "$counter" delete >/dev/null 2>&1

described=$(@ TestRegistry describe)
assert_eq "a class describes its superclass and class state" \
    "class TestRegistry|  superclass: Object|  count: 2|  label: main hall|  note: " \
    "$(printf '%s' "$described" | paste -sd'|' -)"
assert_eq "non-interactive class inspect prints the description" "$described" "$(@ TestRegistry inspect < /dev/null)"
assert_eq "Trash inspectObject: accepts a class name" "$described" "$(@ Trash inspectObject: TestRegistry < /dev/null)"
assert_eq "a namespaced class describes itself" "class ClassInspection::Thing|  superclass: Object|  hits: 5" \
    "$(@ ClassInspection::Thing describe | paste -sd'|' -)"

record=$(@ UI::Inspector recordFor: TestRegistry)
assert_true "the class record carries name, superclass, and state" jq -e \
    '. == {schema_version:1,class_name:"TestRegistry",superclass:"Object",data:{count:2,label:"main hall",note:""},refs:{}}' <<< "$record"
assert_fails "an unknown name is still an error" @ UI::Inspector openObject: NoSuchClass

# With a terminal, inspect opens the inspector read-only: Enter on a class
# variable is refused rather than opening an editor.
_trash_interactive_terminal() { true; }
export UI_SCRIPT='[{"widget":"pane-[]","action":"drill","value":{"key":"data"}},
  {"widget":"pane-[\"data\"]","action":"drill","value":{"key":"count"}}]'
: > "$UI_LOG"
@ TestRegistry inspect >/dev/null 2>&1
assert_true "interactive class inspect opens the class inspector" jq -se \
    '.[0].view == "inspector" and .[0].root.children[0].children[0].props.title == "Class inspector"
     and (.[0].collections[0].rows | map(.fields.text) | index("class_name: \"TestRegistry\"") != null)' "$UI_LOG"
assert_true "class state can be drilled into" jq -se \
    '[.[] | select(.type == "init")][1].collections[1].rows | map(.fields.text) == ["count: 2","label: \"main hall\"","note: \"\""]' "$UI_LOG"
assert_true "class variables are not editable" jq -se \
    '[.[] | select(.type == "ack")][-1].ok == false' "$UI_LOG"
assert_eq "inspection leaves class state unchanged" "2" "$(@ TestRegistry count)"

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
