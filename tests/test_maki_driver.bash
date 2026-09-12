#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=maki-tester
export TRASHTALK_GUSGUS_PROFILE=maki
unset TRASHTALK_MAKI_MODEL
export MAKI_TEST_LOG="$tmp/log"
mkdir "$tmp/bin" "$MAKI_TEST_LOG" "$tmp/workspace with spaces"
cat > "$tmp/bin/maki" <<'MAKI'
#!/usr/bin/env bash
if [[ "$1" == auth ]]; then
    [[ "${MAKI_TEST_AUTH:-oauth}" == oauth ]] && echo 'openai         OpenAI (oauth)' || echo 'openai OpenAI (not authenticated)'
    exit 0
fi
[[ -z "${OPENAI_API_KEY:-}" && -z "${CODEX_API_KEY:-}" && -z "${OPENROUTER_API_KEY:-}" ]] || exit 91
id=${TRASHTALK_RUN_TOKEN%%:*}
printf '%s\n' "$@" > "$MAKI_TEST_LOG/$id.argv"
pwd > "$MAKI_TEST_LOG/$id.cwd"
cp "$XDG_CONFIG_HOME/maki/init.lua" "$MAKI_TEST_LOG/$id.config"
input=$(cat)
printf '%s\n' "$input" > "$MAKI_TEST_LOG/$id.input"
prompt=$(printf '%s' "$input" | jq -er 'select(.type=="user") | .message.content') || exit 92
echo '{"type":"system","subtype":"init","session_id":"maki-fixture-session"}'
if [[ "${MAKI_TEST_MODE:-success}" == error ]]; then
    echo '{"type":"result","subtype":"error_during_execution","is_error":true,"result":"fixture provider failure"}'
    exit 0
fi
if [[ "${MAKI_TEST_MODE:-success}" == wait ]]; then
    touch "$MAKI_TEST_LOG/$id.waiting"
    while :; do sleep 1; done
fi
ts="$TRASHTALK_RUN_DIR/$id/trash-send"
for message in $(printf '%s\n' "$prompt" | sed -n 's/^Message: //p'); do
    inbox=$("$ts" Inbox named: "$("$ts" "$message" to)") || exit
    "$ts" "$inbox" show: "$message" >/dev/null || exit
done
"$ts" AgentRun result: 'Maki fixture reply' >/dev/null || exit
for delivery in $(printf '%s\n' "$prompt" | sed -n 's/^--- delivery //p'); do
    "$ts" AgentRun settle: "$delivery" >/dev/null || exit
done
echo '{"type":"result","subtype":"success","is_error":false,"session_id":"maki-fixture-session","result":"done"}'
MAKI
chmod +x "$tmp/bin/maki"
export PATH="$tmp/bin:$PATH" OPENAI_API_KEY=fixture CODEX_API_KEY=fixture OPENROUTER_API_KEY=fixture
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
contains() { [[ "$3" == *"$2"* ]] || { echo "FAIL: $1 missing $2"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
field() { db_get "$1" | jq -r --arg f "$2" '.[$f] // empty'; }
settle() {
    local i
    for i in {1..100}; do
        @ AgentWorker tickSession: "$session" >/dev/null 2>&1
        [[ -n "$(@ "$session" activeRun)" ]] || return 0
        sleep 0.1
    done
    echo 'FAIL: harness did not finish'; exit 1
}
check 'Gusgus honors the explicit Maki profile' maki "$(@ Gusgus profile)"
check 'worker resolves Maki driver' MakiDriver "$(@ AgentWorker driverFor: maki)"
check 'legacy profiles still resolve Codex' CodexDriver "$(@ AgentWorker driverFor: assistant-low-power)"
session=$(@ Gusgus sessionFor: "$tmp/workspace with spaces")
msg=$(@ Inbox send: FIRST_MAKI_SECRET to: "session:$session" from: maki-tester)
run=$(@ AgentWorker tickSession: "$session")
settle
check 'Maki launch completes through worker' succeeded "$(field "$run" state)"
check 'Maki reads message contents from Inbox' read "$(field "$msg" status)"
check 'Maki notification omits the message body' 0 "$(grep -c FIRST_MAKI_SECRET "$MAKI_TEST_LOG/$run.input")"
check 'new session snapshots Maki profile' maki "$(field "$session" backendProfile)"
check 'workspace argv is preserved' "$(cd "$tmp/workspace with spaces" && pwd -P)" "$(cat "$MAKI_TEST_LOG/$run.cwd")"
contains 'explicit Terra model' 'openai/gpt-5.6-terra' "$(cat "$MAKI_TEST_LOG/$run.argv")"
contains 'SDK input enabled' 'stream-json' "$(cat "$MAKI_TEST_LOG/$run.argv")"
contains 'medium uses supported standard configuration' 'always_thinking="medium"' "$(tr -d ' ' < "$MAKI_TEST_LOG/$run.config")"
check 'session remembers Maki reference' maki-fixture-session "$(field "$session" lastConversationRef)"
inbox=$(@ Inbox named: maki-tester)
reply=$(@ "$inbox" unread)
check 'Maki answer reaches inbox' 'Maki fixture reply' "$(@ "$reply" body)"
check 'answer stays in thread' "$msg" "$(@ "$reply" replyTo)"
msg2=$(@ "$reply" reply: again)
run2=$(@ AgentWorker tickSession: "$session")
settle
check 'resumed Maki run completes' succeeded "$(field "$run2" state)"
contains 'resume passes exact Maki session id' $'--session\nmaki-fixture-session' "$(cat "$MAKI_TEST_LOG/$run2.argv")"
check 'every delivery processed' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='AgentDelivery' AND json_extract(data,'$.state')!='processed';")"
export MAKI_TEST_MODE=error
@ Inbox send: fail to: "session:$session" from: maki-tester >/dev/null
run3=$(@ AgentWorker tickSession: "$session")
settle
check 'error result with exit zero still fails' failed "$(field "$run3" state)"
check 'provider error retained' 'fixture provider failure' "$(field "$run3" error)"
check 'failed execution requires review' 1 "$(@ "$session" stalledCount)"

# Preflight failure records diagnostics without launching a process.
export MAKI_TEST_AUTH=missing
mapfile -t started < <(@ AgentRun startFor: "$session" profile: maki)
authrun=${started[0]}
@ MakiDriver launch: "$MAKI_TEST_LOG/$run.input" run: "$authrun" token: "${started[1]}" >/dev/null 2>&1; status=$?
check 'missing OAuth prevents launch' 1 "$status"
check 'auth failure stays before launch' starting "$(field "$authrun" state)"
contains 'auth failure gives login command' 'loginToProvider: openai' "$(@ MakiDriver errorFor: "$authrun")"
unset MAKI_TEST_AUTH
mkdir -p "$tmp/workspace with spaces/.maki"
for file in init.lua mcp.toml .env; do
    touch "$tmp/workspace with spaces/.maki/$file"
    @ MakiDriver launch: "$MAKI_TEST_LOG/$run.input" run: "$authrun" token: "${started[1]}" >/dev/null 2>&1; status=$?
    check "project $file prevents ambient extensions" 1 "$status"
    contains "project $file diagnostic" ".maki/$file" "$(@ MakiDriver errorFor: "$authrun")"
    rm "$tmp/workspace with spaces/.maki/$file"
done
printf '%s\n' '{"type":"result","subtype":"success","is_error":false}' '{"type":"result","subtype":"error_during_execution","is_error":true}' > "$(field "$run3" outputLog)"
check 'last failed result overrides earlier success' false "$(@ MakiDriver resultSeenFor: "$run3")"
printf '%s\n' '{"type":"result","subtype":"error_during_execution","is_error":true,"result":"earlier failure"}' '{"type":"result","subtype":"success","is_error":false}' > "$(field "$run3" outputLog)"
check 'last success determines protocol result' true "$(@ MakiDriver resultSeenFor: "$run3")"
check 'earlier result does not contaminate final diagnostics' '' "$(@ MakiDriver errorFor: "$run3")"

# The same queue/stop contract works without native prompt steering.
rm -rf "$tmp/workspace with spaces/.maki"
mkdir "$tmp/stop-workspace"
export MAKI_TEST_MODE=wait
@ "$authrun" transitionTo: failed >/dev/null
session=$(@ Gusgus fresh: "$tmp/stop-workspace")
busy_message=$(@ Inbox send: 'long Maki task' to: "session:$session" from: maki-tester)
busy=$(@ AgentWorker tickSession: "$session")
for i in {1..100}; do [[ -e "$MAKI_TEST_LOG/$busy.waiting" ]] && break; sleep .1; done
check 'Maki work is active' true "$(@ "$busy" isProcessAlive)"
@ Inbox send: 'queued Maki followup' to: "session:$session" from: maki-tester >/dev/null
check 'busy Maki does not launch overlapping work' '' "$(@ AgentWorker tickSession: "$session")"
check 'Maki followup stays queued' 1 "$(@ "$session" pendingCount)"
check 'common stop interrupts active Maki' interrupted "$(@ "$busy" stop)"
check 'Maki process is confirmed stopped' false "$(@ "$busy" isProcessAlive)"
check 'Maki queue remains paused after stop' paused "$(field "$session" lifecycleState)"
check 'Maki stopped delivery needs review' 1 "$(@ "$session" stalledCount)"
check 'queued Maki input retained' 1 "$(@ "$session" pendingCount)"
echo "=== $passed Maki driver checks passed ==="
