#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
failed=0
check() { if "$@"; then printf 'PASS: %s\n' "$*"; else printf 'FAIL: %s\n' "$*"; failed=$((failed+1)); fi; }
cat > "$TRASHDIR/StateProbe.trash" <<'TRASH'
StateProbe subclass: Object
  instanceVars: first:1 second:2 empty bulk:'[]' payload:'{}'
  method: sum [ ^ first + second ]
  method: afterNestedWrite [
    | previous |
    previous := first + 0.
    @ self setFirst: 9.
    ^ first + second
  ]
  rawMethod: rawWrite [
    local data
    data=$(_env_get "$_RECEIVER")
    data=$(jq '.first=20' <<< "$data")
    _env_set "$_RECEIVER" "$data"
  ]
  method: afterRawWrite [ @ self rawWrite. ^ first + second ]
  method: shadow: first [ ^ first + second ]
  rawMethod: emptyBoundary [ _ivar empty; printf END ]
TRASH
cat > "$TRASHDIR/DefaultParent.trash" <<'TRASH'
DefaultParent subclass: Object
  instanceVars: inherited:7 overridden:1
TRASH
cat > "$TRASHDIR/DefaultChild.trash" <<'TRASH'
DefaultChild subclass: DefaultParent
  instanceVars: overridden:9 text:'quote and space' empty:'' neg:-3 fraction:-2.5 yes:'true' no:'false' array:'[]' dictionary:'{}' missing
TRASH
{
    printf 'ManyDefaults subclass: Object\n  instanceVars:'
    for ((i=0;i<40;i++)); do printf ' field%s:%s' "$i" "$i"; done
    printf '\n'
} > "$TRASHDIR/ManyDefaults.trash"
for name in Object Block Counter Array StateProbe DefaultParent DefaultChild ManyDefaults; do
    bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TRASHDIR/$name.trash" -o "$TRASHDIR/.compiled/$name" --check > /dev/null || exit 1
done
source "$TRASHTALK_DIR/lib/trash.bash" || exit 1
counter=$(@ Counter new)
probe=$(@ StateProbe new)
check test "$(@ "$probe" sum)" = 3
check test "$(@ "$probe" afterNestedWrite)" = 11
check test "$(@ "$probe" afterRawWrite)" = 22
check test "$(@ "$probe" shadow: 100)" = 102
@ "$probe" setEmpty: ''
check test "$(@ "$probe" emptyBoundary)" = $'\nEND'
# Direct sends and field writes from a sibling subshell retain visibility.
check test "$(send "$probe" sum)" = 22
(@ "$probe" setFirst: 30)
check test "$(@ "$probe" sum)" = 32
# Receiver resolution must not expand unread collections into Bash variables.
# Container getters still preserve the existing values and formatting.
@ "$probe" bulk: "$(jq -cn '[range(0;10000)]')"
@ "$probe" payload: '{"nested":["quote '\''",false,null,{"key":"value"}]}'
probe_data=$(_env_get "$probe")
decoded_state=$(_trash_json_get state "$probe_data" '')
check test "${#decoded_state}" -lt 512
check test "$(@ "$probe" sum)" = 32
check jq -e '. == [range(0;10000)]' <<< "$(@ "$probe" bulk)"
check test "$(@ "$probe" payload)" = "$(jq '.payload' <<< "$probe_data")"
child=$(@ DefaultChild new)
data=$(@ Runtime dataFor: "$child")
check jq -e '.inherited==7 and .overridden==9 and .text=="quote and space" and .empty==null and .neg == -3 and .fraction == -2.5 and .yes==true and .no==false and .array==[] and .dictionary=={} and .missing==null and (._vars|index("inherited")!=null)' <<< "$data"
check test "$(db_get "$child" | jq -c .)" = "$(jq -c . <<< "$data")"
# A failed persistent write must not publish an in-memory object.
( db_put() { return 1; }; _create_instance ManyDefaults failure_fixture >/dev/null 2>&1; test "$?" != 0 && test ! -e "$_ENV_DIR/failure_fixture" )
check test "$?" = 0
block=$(@ Block params: '["x"]' code: 'printf "%s\n" "$x"' captured: '{}')
block_argument="quote ' and newline
second line"
check test "$(@ "$block" valueWith: "$block_argument")" = "$block_argument"
@ "$block" code: 'return 7'
@ "$block" valueWith: ignored >/dev/null
check test "$?" = 7
@ "$block" code: 'printf "%s\n" "$x"'
# Captured receiver, two parameters, direct local mutation, and fresh metadata.
two=$(@ Block params: '["a","b"]' code: 'printf "%s:%s:%s\n" "$_RECEIVER" "$a" "$b"' captured: "{\"_RECEIVER\":\"$counter\"}")
check test "$(@ "$two" valueWith: one and: two)" = "$counter:one:two"
zero=$(@ Block params: '[]' code: 'local_value=changed' captured: '{}')
local_value=before
@ "$zero" value
check test "$local_value" = changed
calls="$TMPDIR/jq-calls"
jq() { printf 'jq\n' >> "$calls"; command jq "$@"; }
count() { : > "$calls"; "$@" > "$TMPDIR/result"; count_status=$?; count_result=$(wc -l < "$calls" | tr -d ' '); check test "$count_status" = 0; }
count @ "$counter" getValue
check test "$count_result" = 1
count @ "$counter" increment
check test "$count_result" = 2
count @ "$block" valueWith: hello
check test "$count_result" = 2
count @ ManyDefaults new
check test "$count_result" = 2
many=$(<"$TMPDIR/result")
check test "$(@ Runtime dataFor: "$many" | command jq '._vars|length')" = 40
# Accessor setup must not start a text-conversion process for every field.
tr() { printf 'tr\n' >> "$TMPDIR/accessor-processes"; command tr "$@"; }
: > "$TMPDIR/accessor-processes"
check _create_instance ManyDefaults accessor_fixture
check test ! -s "$TMPDIR/accessor-processes"
unset -f tr
# A map's callback now adds two jq processes per element rather than nine.
array=$(@ Array new)
@ "$array" setItems: '["a","b","c"]'
count @ "$array" collect: "$block"
check test "$count_result" -le 20
result=$(<"$TMPDIR/result")
check test "$(@ "$result" getItems | command jq -c .)" = '["a","b","c"]'
# Getter overrides retain public dispatch, including changes to Block itself.
cat >> "$TRASHDIR/Block.trash" <<'TRASH'
  method: code [ ^ 'printf override' ]
TRASH
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TRASHDIR/Block.trash" -o "$TRASHDIR/.compiled/Block" --check >/dev/null || exit 1
source "$TRASHDIR/.compiled/Block"
check test "$(@ "$block" valueWith: ignored)" = override
exit "$((failed > 0))"
