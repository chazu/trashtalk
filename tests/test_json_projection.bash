#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
rows=$'{"id":"one","v":[false,null,{"nested":"雪\\n\\t|"}]}\n{"id":"two","v":42}'
template='{"id":{"_at":"id"},"value":{"_at":"v"},"label":{"_concat":["row ",{"_at":"id"}]},"constant":{"_literal":{"_at":"unchanged"}}}'
actual=$(@ Json projectEach: "$rows" with: "$template")
jq -se 'length==2 and .[0].value==[false,null,{"nested":"雪\n\t|"}] and .[1].value==42 and .[1].label=="row two" and .[0].constant=={"_at":"unchanged"}' <<< "$actual" >/dev/null
[[ -z $(@ Json projectEach: '' with: "$template") ]]
# A later invalid field path must fail without emitting an earlier valid row.
if @ Json projectEach: $'{"v":{}}\n{"v":5}' with: '{"_at":["v","missing"]}' > "$tmp/partial" 2>/dev/null; then exit 1; fi
[[ ! -s "$tmp/partial" ]]
# Preview content is opaque and line breaks survive; output contains only records.
documents='{"path":"one.txt","text":"雪\n\nquote \" tab\tpipe|","record":{"id":"one"}}'
[[ $(@ Json writeDocuments: "$documents" in: "$tmp") == '{"id":"one"}' ]]
[[ $(cat "$tmp/one.txt") == $'雪\n\nquote " tab\tpipe|' ]]
if @ Json writeDocuments: $'{"path":"ok.txt","text":"ok","record":{}}\n{"path":"../bad","text":"bad","record":{}}' in: "$tmp" >/dev/null 2>&1; then exit 1; fi
[[ ! -e "$tmp/ok.txt" ]]
# Compare batched projection to the existing scalar public helpers on varied state.
snapshots='{"a":{"class":"Counter","_vars":["z","a","missing","n"],"z":false,"a":{"雪":"line\n|"},"n":null},"b":{"_vars":["a","z"],"a":[1,"x"],"z":"é"},"c":{},"d":null}'
projections=$(@ String jsonInstanceProjections: "$snapshots")
while IFS= read -r item; do
    id=$(jq -r .id <<< "$item")
    raw=$(jq -c --arg id "$id" '.[$id]' <<< "$snapshots")
    view=$(@ String jsonInstanceView: "$raw")
    columns=$(@ String jsonInstanceColumns: "$raw")
    detail=$(@ String jsonInstanceSummary: "$view")
    jq -e --argjson view "$view" --argjson columns "$columns" --arg detail "$detail" '.data==$view and .columns==$columns and .detail==$detail' <<< "$item" >/dev/null
done <<< "$projections"
printf 'PASS: typed projections, opaque values, order, atomic validation and scalar parity\n'
