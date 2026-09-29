#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
@ Store put: field_fixture data: '{"class":"Object"}'
for value in 'a "quote"' 'a\backslash' $'line\nnext' '1e2' 'true' ''; do
    @ Store setField: field_fixture field: 'nested.value' value: "$value"
    data=$(@ Store getInstance: field_fixture)
    jq -e --arg v "$value" '.nested.value==$v' <<< "$data" >/dev/null
done
@ Store setField: field_fixture field: 'odd"key' value: text
jq -e '.["odd\"key"]=="text"' <<< "$(@ Store getInstance: field_fixture)" >/dev/null
for input in 001 -0; do
    @ Store setField: field_fixture field: count value: "$input"
    jq -e --arg input "$input" '.count == ($input|tonumber)' <<< "$(@ Store getInstance: field_fixture)" >/dev/null
done
@ Store setField: field_fixture field: count value: -42
jq -e '.count == -42' <<< "$(@ Store getInstance: field_fixture)" >/dev/null
before=$(@ Store getInstance: field_fixture)
# A jq path-type error must not replace the row with an empty value.
if @ Store setField: field_fixture field: count.invalid value: text 2>/dev/null; then exit 1; fi
[[ "$(@ Store getInstance: field_fixture)" == "$before" ]]
db_put() { return 1; }
if @ Store setField: field_fixture field: value value: text; then exit 1; fi
echo 'PASS: Store bound values, dotted paths, numeric contract, and failed writes'
