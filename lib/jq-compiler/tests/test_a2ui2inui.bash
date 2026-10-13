#!/usr/bin/env bash
# A2UI (a2ui.org) -> inui frame mapping. The A2UI message stream is folded into
# one inui "init" frame; data bindings bake into props, ids become keys.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
MAP="$ROOT/lib/a2ui2inui.jq"
pass=0 fail=0
check() { local label=$1 actual=$2 expected=$3
  if [[ $actual == "$expected" ]]; then pass=$((pass+1))
  else fail=$((fail+1)); echo "FAIL: $label: got [$actual] want [$expected]"; fi }

# Minimal stream: surface, a column, a bound text, a data model.
mini=$(cat <<'EOF'
{"messages":[
 {"version":"v1.0","createSurface":{"surfaceId":"s","catalogId":"c","sendDataModel":true}},
 {"version":"v1.0","updateComponents":{"surfaceId":"s","components":[
   {"id":"root","component":"Column","children":["t"]},
   {"id":"t","component":"Text","text":{"@path":"/greeting"}}]}},
 {"version":"v1.0","updateDataModel":{"surfaceId":"s","value":{"greeting":"hi"}}}
]}
EOF
)
out=$(jq -s --arg view v -f "$MAP" <<<"$mini")
check "envelope" "$(jq -c '{schema_version,view,type}' <<<"$out")" '{"schema_version":1,"view":"v","type":"init"}'
check "root kind" "$(jq -r '.root.kind' <<<"$out")" panel
check "child resolves @path" "$(jq -r '.root.children[0].props.text' <<<"$out")" hi
check "root key" "$(jq -r '.root.key' <<<"$out")" root

# A stream without createSurface is rejected with a clean error object.
out=$(jq -s --arg view v -f "$MAP" <<<'{"messages":[{"version":"v1.0","updateComponents":{"surfaceId":"s","components":[{"id":"root","component":"Card"}]}}]}')
check "missing surface" "$(jq -r '.error | length > 0' <<<"$out")" true

echo "a2ui2inui: $pass passed, $fail failed"
(( fail == 0 ))
