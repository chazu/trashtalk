#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
host="$tmp/host"
directory="$tmp/run"
real_bash=$(command -v bash)
mkdir -p "$host/bin" "$host/processes" "$directory" "$tmp/runtime"
cp "$root/tests/fixtures/jcode-api.py" "$tmp/jcode"
chmod +x "$tmp/jcode"
printf '#!%s\nexec %q %q shell %q %q "$@"\n' "$real_bash" "$real_bash" \
    "$root/lib/jcode-processes.bash" "$host" "$real_bash" > "$host/bin/bash"
chmod +x "$host/bin/bash"
printf 'idle' > "$host/fixture-state"
printf 'jcode-fixture-session\n' > "$directory/conversation"
export JCODE_TEST_GATE="$tmp/gate"
jq -cn --arg executable "$tmp/jcode" --arg home "$host" --arg runtime "$tmp/runtime" \
    --arg workspace "$tmp" --arg bash "$real_bash" \
    '{executable:$executable,home:$home,runtime:$runtime,workspace:$workspace,bash:$bash,model:"fixture",ref:"jcode-fixture-session"}' > "$directory/jcode.json"
# An earlier failed stop has closed admission to model-launched Bash jobs.
# The recovery control bridge must still connect and confirm native idle.
touch "$host/stopping"
if ! "$real_bash" "$root/lib/jcode-api.bash" stop "$directory" > "$tmp/control.log" 2> "$tmp/error.log"; then
    cat "$tmp/error.log"
    echo 'FAIL: stop retry could not connect while tool admission was closed'
    exit 1
fi
jq -se 'any(.req=="cancel" and .session_id=="jcode-fixture-session")' "$host/fixture-calls.jsonl" >/dev/null
[[ -z $(ls -A "$host/processes") ]] || { echo 'FAIL: control bridge registered as a model tool'; exit 1; }
echo 'PASS: stop retry connects through closed tool admission without registering itself'
