#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Exercise serialized values through compiled public messages, not code patterns.
set -uo pipefail
export LC_ALL=C
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
TEST_TMP=$(mktemp -d)
export TRASHDIR="$TEST_TMP/trash" SQLITE_JSON_DB="$TEST_TMP/instances.db"
mkdir -p "$TRASHDIR/.compiled"
cat >"$TEST_TMP/JsonValues.trash" <<'TRASH'
JsonValues subclass: Object
  classMethod: record: text raw: raw count: count [
    | result |
    result := #{version: 1 text: text data: (raw jsonValue)
                count: (count jsonValue) flags: #(true false null -1 -1.5)
                nested: #{items: #(text 'two words' '')}} asJson.
    ^ result
  ]
  classMethod: vector: text [
    ^ #(text '--flag' '' 'quote"slash\\') asJson
  ]
  classMethod: invalid: raw [
    ^ #{data: (raw jsonValue)} asJson
  ]
  classMethod: invalidAssignment: raw [
    | result |
    result := #{data: (raw jsonValue)} asJson.
    ^ 'must not reach this'
  ]
TRASH
"$ROOT/lib/jq-compiler/driver.bash" compile "$TEST_TMP/JsonValues.trash" >"$TRASHDIR/.compiled/JsonValues" || exit 1
source "$ROOT/lib/trash.bash"
trap '_env_cleanup; rm -rf "$TEST_TMP"' EXIT
failed=0
check() { if "$@"; then echo "PASS: $*"; else echo "FAIL: $*"; failed=$((failed+1)); fi; }
payload=$'quotes " \' \\ and\nnewlines $(touch should-not-exist) `false`'
raw='{"false":false,"null":null,"array":[1,"x"]}'
# Count actual serialization processes; a nested value must still take one jq.
jq() { printf 'jq\n' >>"$TEST_TMP/jq.calls"; command jq "$@"; }
@ JsonValues record: "$payload" raw: "$raw" count: 42 >"$TEST_TMP/result"
check test "$?" = 0
check test "$(wc -l <"$TEST_TMP/jq.calls" | tr -d ' ')" = 1
check command jq -e --arg text "$payload" --argjson raw "$raw" \
  '. == {version:1,text:$text,data:$raw,count:42,flags:[true,false,null,-1,-1.5],nested:{items:[$text,"two words",""]}}' "$TEST_TMP/result"
@ JsonValues vector: "$payload" >"$TEST_TMP/vector"
check command jq -e --arg text "$payload" '.[0] == $text and .[1] == "--flag" and .[2] == "" and length == 4' "$TEST_TMP/vector"
@ JsonValues invalid: 'not json' >"$TEST_TMP/invalid.out" 2>"$TEST_TMP/invalid.err"
check test "$?" != 0
check test ! -s "$TEST_TMP/invalid.out"
check test -s "$TEST_TMP/invalid.err"
@ JsonValues invalidAssignment: 'not json' >"$TEST_TMP/invalid.out" 2>"$TEST_TMP/invalid.err"
check test "$?" != 0
check test ! -s "$TEST_TMP/invalid.out"
check test "$(sqlite3 "$SQLITE_JSON_DB" 'SELECT count(*) FROM instances;')" = 0
exit "$((failed > 0))"
