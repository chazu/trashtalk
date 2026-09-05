#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
TEST_TMP=$(mktemp -d "$TMPDIR/json-test.XXXXXX")
cat > "$TRASHDIR/JsonReads.trash" <<'TRASH'
JsonReads subclass: Object
  classMethod: get: data path: path [ ^ data jsonAt: path ]
  classMethod: text: data path: path [ ^ data jsonTextAt: path ]
  classMethod: has: data path: path [ ^ data jsonHas: path ]
  classMethod: default: data path: path [ ^ data jsonAt: path ifAbsent: '42' ]
  classMethod: fail: data [
    | value |
    value := data jsonAt: 'missing'.
    ^ 'must not reach this'
  ]
  classMethod: unpack: data [
    data jsonUnpack: #('flag' 'empty' 'nothing' #('obj' 'a.b')) into: [:flag :empty :nothing :special |
      ^ #{flag: flag empty: empty nothing: nothing special: special} asJson
    ]
  ]
  classMethod: sum: data [
    | total |
    total := 0.
    data arrayEach: [:element | total := total + element].
    ^ total
  ]
  classMethod: find: data [
    data arrayEach: [:element | (element = 'stop') ifTrue: [^ element] ].
    ^ 'not found'
  ]
  classMethod: nested: data [
    | total |
    total := 0.
    data arrayEach: [:row | row arrayEach: [:value | total := total + value] ].
    ^ total
  ]
TRASH
"$ROOT/lib/jq-compiler/driver.bash" compile "$TRASHDIR/JsonReads.trash" --check > "$TRASHDIR/.compiled/JsonReads" || exit 1
source "$ROOT/lib/trash.bash" || exit 1
failed=0
check() { if "$@"; then echo "PASS: $*"; else echo "FAIL: $*"; failed=$((failed+1)); fi; }
data='{"flag":false,"empty":"","nothing":null,"obj":{"a.b":"quote '\'' slash \\ newline\n$(touch BAD)"}}'
check test "$(@ JsonReads get: "$data" path: flag)" = false
check test "$(@ JsonReads get: "$data" path: nothing)" = null
check test "$(@ JsonReads get: "$data" path: empty)" = '""'
check test "$(@ JsonReads has: "$data" path: nothing)" = true
check test "$(@ JsonReads has: "$data" path: missing)" = false
check test "$(@ JsonReads default: "$data" path: flag)" = false
check test "$(@ JsonReads default: "$data" path: nothing)" = null
check test "$(@ JsonReads default: "$data" path: missing)" = 42
check test "$(@ JsonReads get: '[1,false]' path: '[1]')" = false
@ JsonReads fail: '{}' > "$TEST_TMP/fail.out" 2> "$TEST_TMP/fail.err"
check test "$?" != 0
check test ! -s "$TEST_TMP/fail.out"
jq() { printf 'jq\n' >> "$TEST_TMP/calls"; command jq "$@"; }
@ JsonReads unpack: "$data" > "$TEST_TMP/unpacked"
check test "$?" = 0
check command jq -e '.flag == "false" and .empty == "" and .nothing == "null" and (.special | contains("$(touch BAD)"))' "$TEST_TMP/unpacked"
check test "$(wc -l < "$TEST_TMP/calls" | tr -d ' ')" = 2
check test ! -e BAD
check test "$(@ JsonReads sum: '[1,2,3]')" = 6
check test "$(@ JsonReads sum: '[]')" = 0
check test "$(@ JsonReads find: '["a","stop","b"]')" = stop
check test "$(@ JsonReads nested: '[[1,2],[3,4]]')" = 10
@ JsonReads unpack: '{}' > "$TEST_TMP/missing.out" 2> "$TEST_TMP/missing.err"
check test "$?" != 0
check test ! -s "$TEST_TMP/missing.out"
@ JsonReads get: '{} {}' path: flag > "$TEST_TMP/multiple.out" 2>/dev/null
check test "$?" != 0
@ JsonReads text: '{"x":"\u0000"}' path: x > "$TEST_TMP/nul.out" 2>/dev/null
check test "$?" != 0
exit "$((failed > 0))"
