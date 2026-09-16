#!/usr/bin/env bash
# Exercise the actual bridge adapter and native socket cwd verification together.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
scratch=$(mktemp -d /tmp/tt-workspace.XXXXXX)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/workspace with spaces"
cp "$root/tests/fixtures/jcode-api.py" "$scratch/jcode"
chmod +x "$scratch/jcode"
workspace=$(cd "$scratch/workspace with spaces" && pwd -P)
export JCODE_TEST_GATE="$scratch/gate"
touch "$JCODE_TEST_GATE"
for mode in workspace-ok workspace-wrong workspace-busy workspace-attach-wrong; do
    directory="$scratch/$mode"
    mkdir -p "$directory/home" "$directory/runtime"
    jq -cn --arg executable "$scratch/jcode" \
        --arg home "$directory/home" --arg runtime "$directory/runtime" \
        --arg workspace "$workspace" --arg bash "$(command -v bash)" \
        '{executable:$executable,home:$home,runtime:$runtime,workspace:$workspace,bash:$bash,model:"fixture",ref:""}' > "$directory/jcode.json"
    printf 'Workspace qualification\n' > "$directory/prompt.txt"
    status=0
    JCODE_TEST_MODE="$mode" timeout 40 bash "$root/lib/jcode-api.bash" run "$directory" \
        > "$directory/events" 2> "$directory/err" || status=$?
    if [[ "$mode" == workspace-ok ]]; then
        [[ "$status" == 0 && -s "$directory/completed" ]] || { cat "$directory/err"; exit 1; }
        jq -e --arg dir "$workspace" '.verified and .execution_workspace==$dir and .session_id=="jcode-fixture-session"' "$directory/workspace-control.json" >/dev/null
        jq -se 'any(.native_type=="input_shell") and any(.req=="send_message")' "$directory/home/fixture-calls.jsonl" >/dev/null
        echo 'PASS: missing attachment cwd requires a verified native workspace before input'
    else
        [[ "$status" != 0 && "$status" != 124 && "$status" != 137 ]] || { cat "$directory/err"; exit 1; }
        case "$mode" in
            workspace-wrong) expected='did not apply the requested execution directory' ;;
            workspace-busy) expected='target is busy or changed' ;;
            workspace-attach-wrong) expected='attachment has the wrong execution directory' ;;
        esac
        rg -q "$expected" "$directory/err"
        [[ ! -e "$directory/send-intent" && ! -e "$directory/completed" ]]
        jq -se 'all(.req!="send_message")' "$directory/home/fixture-calls.jsonl" >/dev/null
        echo "PASS: $mode rejects model input"
    fi
done
