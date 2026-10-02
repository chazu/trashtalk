#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=pre-send-owner
export JCODE_HOME="$tmp/auth" CODEX_HOME="$tmp/no-codex" JCODE_TEST_GATE="$tmp/gate"
unset TRASHTALK_RUN_TOKEN TRASHTALK_GUSGUS_PROFILE
mkdir -p "$tmp/bin" "$JCODE_HOME" "$tmp/workspace"
printf '{}\n' > "$JCODE_HOME/openai-auth.json"
cp "$root/tests/fixtures/jcode-api.py" "$tmp/bin/jcode"
chmod +x "$tmp/bin/jcode"
export PATH="$tmp/bin:$PATH"
trap 'rm -rf "$tmp"' EXIT
touch "$JCODE_TEST_GATE"
db_init
check() { [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }; printf 'PASS: %s\n' "$1"; }
settle() {
    for i in {1..100}; do
        @ Agent::Worker tickSession: "$session" >/dev/null
        [[ -n "$(@ "$session" activeRun)" ]] || return 0
        sleep .1
    done
    echo 'FAIL: direct turn did not settle'; exit 1
}
session=$(@ Gusgus sessionFor: "$tmp/workspace")
export JCODE_TEST_MODE=refuse-model
if @ "$session" input: 'first turn' > "$tmp/ack" 2> "$tmp/error"; then
    echo 'FAIL: rejected model accepted input'; exit 1
fi
check 'composer receives the model rejection' true "$(grep -q 'Unsupported OpenAI model fixture-model' "$tmp/ack" "$tmp/error" && echo true || echo false)"
settle
run=$(@ Store findByClass: Agent::Run where: "json_extract(data,'$.session')='$session'" orderBy: 'created_at DESC' limit: 1)
directory="$TRASHTALK_RUN_DIR/$run"
host=$(jq -r .home "$directory/jcode.json")
check 'model rejection fails its run' failed "$(@ "$run" state)"
check 'pre-send failure leaves session open' open "$(@ "$session" lifecycleState)"
check 'unsaved native reference is not published to session' '' "$(@ "$session" lastConversationRef)"
check 'unsaved native reference is not published to run' '' "$(@ "$run" externalConversationRef)"
check 'pre-send failure records no send intent' false "$([[ -e "$directory/send-intent" ]] && echo true || echo false)"
check 'pre-send failure has no persisted history' false "$([[ -d "$host/sessions" ]] && echo true || echo false)"
export JCODE_TEST_MODE=require-catalog
check 'retry is accepted directly' 'Input sent directly to the session' "$(@ "$session" input: 'retry after configuration correction')"
check 'private model catalog is refreshed before model selection' true "$([[ -s "$host/fixture-catalog-ready" ]] && echo true || echo false)"
settle
check 'retry retains its now durable conversation' jcode-fixture-session "$(@ "$session" lastConversationRef)"
check 'only retry sends model input' 1 "$(jq -s '[.[]|select(.req=="send_message")]|length' "$host/fixture-calls.jsonl")"
export JCODE_TEST_MODE=refuse-model
if @ "$session" input: 'reject on existing conversation' >/dev/null 2>&1; then
    echo 'FAIL: rejected model accepted existing conversation input'; exit 1
fi
settle
check 'pre-send failure preserves existing native history' jcode-fixture-session "$(@ "$session" lastConversationRef)"
unset JCODE_TEST_MODE
check 'existing conversation can retry directly' 'Input sent directly to the session' "$(@ "$session" input: 'continue retained history')"
settle
check 'existing history is never replaced on retry' 2 "$(jq -s '[.[]|select(.req=="create_session")]|length' "$host/fixture-calls.jsonl")"
check 'direct retry creates no inbox message' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Message';")"
echo 'Jcode pre-send rejection checks passed'
