#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
TEST_TMP=$(mktemp -d "$TMPDIR/json-test.XXXXXX")
cat > "$TRASHDIR/TraversalCallback.trash" <<'TRASH'
TraversalCallback subclass: Object
  classMethod: valueWith: value [ ^ value ]
  classMethod: valueWith: key and: value [ ^ value ]
TRASH
"$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TRASHDIR/TraversalCallback.trash" > "$TRASHDIR/.compiled/TraversalCallback" || exit 1
source "$TRASHTALK_DIR/lib/trash.bash"
failed=0
check() { if "$@"; then echo "PASS: $*"; else echo "FAIL: $*"; failed=$((failed+1)); fi; }
array=$(@ Array new)
@ "$array" setItems: '["","quote '\'' and $(touch BAD)","two\nlines",false,null,{"x":1}]'
mapped=$(@ "$array" collect: TraversalCallback)
check test "$(@ "$mapped" getItems | command jq -c .)" = '["","quote '\'' and $(touch BAD)","two\nlines","","","{\n  \"x\": 1\n}"]'
check test ! -e BAD
selected=$(@ "$array" select: TraversalCallback)
check test "$(@ "$selected" getItems | command jq -c .)" = '["quote '\'' and $(touch BAD)","two\nlines","{\n  \"x\": 1\n}"]'
dict=$(@ Dictionary new)
@ "$dict" setItems: '{"a":"one","b":"two\nlines","c":""}'
mapped=$(@ "$dict" collect: TraversalCallback)
check test "$(@ "$mapped" getItems | command jq -c .)" = '{"a":"one","b":"two\nlines","c":""}'
selected=$(@ "$dict" select: TraversalCallback)
check test "$(@ "$selected" getItems | command jq -c .)" = '{"a":"one","b":"two\nlines"}'
@ "$dict" setItems: '{"z":"first","a":"second"}'
check test "$(@ "$dict" valuesDo: TraversalCallback)" = $'first\nsecond'
check test "$(@ "$dict" keysDo: TraversalCallback)" = $'a\nz'
@ "$dict" setItems: '{"z":"contains\u0000nul","a":"ok"}'
check test "$(@ "$dict" keysDo: TraversalCallback)" = $'a\nz'
@ "$array" setItems: '[]'
mapped=$(@ "$array" collect: TraversalCallback)
check test "$(@ "$mapped" getItems | command jq -c .)" = '[]'
@ "$dict" setItems: '{}'
mapped=$(@ "$dict" collect: TraversalCallback)
check test "$(@ "$mapped" getItems | command jq -c .)" = '{}'

# Measure public collection calls with a constant class callback. jq use must
# be independent of element count: one decode and one result assembly.
jq() { printf 'jq\n' >> "$TEST_TMP/calls"; command jq "$@"; }
@ "$array" setItems: '["one"]'
: > "$TEST_TMP/calls"
@ "$array" collect: TraversalCallback > "$TEST_TMP/one"
small=$(wc -l < "$TEST_TMP/calls" | tr -d ' ')
values=$(command jq -cn '[range(0;25) | tostring]')
@ "$array" setItems: "$values"
: > "$TEST_TMP/calls"
@ "$array" collect: TraversalCallback > "$TEST_TMP/many"
large=$(wc -l < "$TEST_TMP/calls" | tr -d ' ')
check test "$small" = "$large"
check test "$(@ "$(<"$TEST_TMP/many")" getItems | command jq -c .)" = "$values"

# Editor diagnostics consume one decoder and one serializer.
: > "$TEST_TMP/calls"
@ Trash diagnosticsFromCompileResult: '{"message":"example","line":3,"phase":"parse"}' > "$TEST_TMP/diagnostic"
check test "$(wc -l < "$TEST_TMP/calls" | tr -d ' ')" = 2
check command jq -e '.annotations[0].line == 3 and .annotations[0].message == "parse: example"' "$TEST_TMP/diagnostic"
check test "$(@ String isJson: false)" = true
check test "$(@ String isJson: null)" = true
check test "$(@ String isJson: '{} {}')" = false
check test "$(@ String jsonAt: flag from: '{"flag":false}')" = false
check test "$(@ String jsonTextAt: text from: '{"text":"hello"}')" = hello
check test "$(@ String jsonHas: nil in: '{"nil":null}')" = true
check test "$(@ String jsonAt: absent from: '{}' ifAbsent: '42')" = 42
exit "$((failed > 0))"
