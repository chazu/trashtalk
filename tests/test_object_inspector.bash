#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
CAPTURE_INPUT="$TEST_TMP/inspection-input.json"
CAPTURE_ARGV="$TEST_TMP/inspection-argv.txt"

cleanup() {
    [[ -n "${counter:-}" ]] && @ "$counter" delete >/dev/null 2>&1 || true
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN"

cat > "$FAKE_BIN/ininspect" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$CAPTURE_ARGV"
input=$(cat)
printf '%s\n' "$input" > "$CAPTURE_INPUT"
object_id=$(printf '%s' "$input" | jq -r '.object_id')
class_name=$(printf '%s' "$input" | jq -r '.class_name')
base_data=$(printf '%s' "$input" | jq -c '.data')

case "${INSPECT_SCENARIO:-viewed}" in
    viewed)
        jq -cn --arg object_id "$object_id" --arg class_name "$class_name" \
            '{schema_version:1,outcome:"viewed",object_id:$object_id,class_name:$class_name,base_data:null,proposal:null}'
        ;;
    proposed)
        old_value=$(printf '%s' "$base_data" | jq -c '.value')
        jq -cn --arg object_id "$object_id" --arg class_name "$class_name" \
            --argjson base_data "$base_data" --argjson old_value "$old_value" \
            '{schema_version:1,outcome:"proposed",object_id:$object_id,class_name:$class_name,
              base_data:$base_data,proposal:{path:["value"],old_value:$old_value,new_value:7}}'
        ;;
    stale)
        jq -cn --arg object_id "$object_id" --arg class_name "$class_name" \
            '{schema_version:1,outcome:"proposed",object_id:$object_id,class_name:$class_name,
              base_data:{value:999},proposal:{path:["value"],old_value:999,new_value:7}}'
        ;;
    command)
        jq -cn --arg object_id "$object_id" --arg class_name "$class_name" \
            --argjson base_data "$base_data" \
            '{schema_version:1,outcome:"proposed",object_id:$object_id,class_name:$class_name,
              base_data:$base_data,proposal:{path:["value"],old_value:0,new_value:7,command:"echo owned"}}'
        ;;
    old-mismatch)
        jq -cn --arg object_id "$object_id" --arg class_name "$class_name" \
            --argjson base_data "$base_data" \
            '{schema_version:1,outcome:"proposed",object_id:$object_id,class_name:$class_name,
              base_data:$base_data,proposal:{path:["value"],old_value:999,new_value:7}}'
        ;;
esac
FAKE
chmod +x "$FAKE_BIN/ininspect"

export CAPTURE_INPUT CAPTURE_ARGV
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
    if "$@" >/dev/null; then pass "$name"; else fail "$name"; fi
}

echo "=== Object Inspector Tests ==="

counter=$(@ Counter create)

export INSPECT_SCENARIO=viewed
viewed=$(@ Trash inspectObject: "$counter")
assert_eq "view-only inspection returns structured outcome" "viewed" \
    "$(printf '%s' "$viewed" | jq -r '.outcome')"
assert_eq "view-only inspection does not change state" "0" "$(@ "$counter" getValue)"
assert_true "wrapper passes result-json" grep -qx -- '--result-json' "$CAPTURE_ARGV"
assert_true "wrapper passes an object title" grep -qx -- 'Object inspector' "$CAPTURE_ARGV"
assert_true "inspection input carries declared typed ivar data" jq -e \
    --arg id "$counter" '.schema_version == 1 and .object_id == $id and .class_name == "Counter" and .data == {value:0,step:1}' \
    "$CAPTURE_INPUT"

export INSPECT_SCENARIO=proposed
applied=$(@ "$counter" inspectInteractive)
assert_eq "interactive object convenience applies an explicit proposal" "applied" \
    "$(printf '%s' "$applied" | jq -r '.outcome')"
assert_eq "accepted inspector proposal preserves numeric type" "7" "$(@ "$counter" getValue)"

@ "$counter" setValue: 0 >/dev/null
export INSPECT_SCENARIO=stale
stale=$(@ Trash inspectObject: "$counter")
assert_eq "stale inspector proposal is rejected" "stale" \
    "$(printf '%s' "$stale" | jq -r '.outcome')"
assert_eq "stale proposal cannot change object state" "0" "$(@ "$counter" getValue)"

export INSPECT_SCENARIO=command
invalid=$(@ Trash inspectObject: "$counter")
assert_eq "inspector result schema rejects command fields" "invalid" \
    "$(printf '%s' "$invalid" | jq -r '.outcome')"
assert_eq "invalid proposal cannot change object state" "0" "$(@ "$counter" getValue)"

export INSPECT_SCENARIO=old-mismatch
invalid=$(@ Trash inspectObject: "$counter")
assert_eq "proposal old value must match its base path" "invalid" \
    "$(printf '%s' "$invalid" | jq -r '.outcome')"
assert_eq "old-value mismatch cannot change object state" "0" "$(@ "$counter" getValue)"

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
