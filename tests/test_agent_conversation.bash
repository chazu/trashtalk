#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
trap 'echo "FAIL: line $LINENO: $BASH_COMMAND" >&2' ERR
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=conversation-owner JCODE_TEST_IDLE_LATE=1
export JCODE_HOME="$tmp/auth" CODEX_HOME="$tmp/no-codex" JCODE_TEST_GATE="$tmp/gate"
unset TRASHTALK_RUN_TOKEN TRASHTALK_GUSGUS_PROFILE
mkdir -p "$tmp/bin" "$JCODE_HOME" "$tmp/workspace"
printf '{}\n' > "$JCODE_HOME/openai-auth.json"
cp "$root/tests/fixtures/jcode-api.py" "$tmp/bin/jcode"
chmod +x "$tmp/bin/jcode"
export PATH="$tmp/bin:$PATH"
cleanup() {
    for pf in "$TRASHTALK_RUN_DIR"/*/pid; do
        [[ -s "$pf" ]] || continue
        read -r pid < "$pf"
        kill -TERM -- "-$pid" 2>/dev/null || true
    done
    rm -rf "$tmp"
}
trap cleanup EXIT
db_init
check() { [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }; printf 'PASS: %s\n' "$1"; }
count() { _db_sql "SELECT count(*) FROM instances WHERE class='$1';"; }
settle() {
    for i in {1..100}; do
        @ AgentWorker tickSession: "$session" >/dev/null
        [[ -n "$(@ "$session" activeRun)" ]] || return 0
        sleep .1
    done
    cat "$TRASHTALK_RUN_DIR"/*/stderr.log
    echo 'FAIL: direct turn did not settle'; exit 1
}
session=$(@ Gusgus sessionFor: "$tmp/workspace")
body=$'literal $(touch unexpected); "quotes"\n日本語\n'
# A routine worker tick can outlast four 100ms retries. Exercise the public
# input path under a real OS lock, then require exactly one native send.
source "$root/lib/process-lock.bash"
(
    function @() { touch "$tmp/lock-ready"; sleep 1; }
    _trash_with_process_lock "$SQLITE_JSON_DB.worker.lock" fixture hold ''
) &
lock_holder=$!
for i in {1..200}; do
    [[ ! -f "$tmp/lock-ready" ]] || break
    sleep .01
done
[[ -f "$tmp/lock-ready" ]] || { echo 'FAIL: lock holder did not start'; exit 1; }
ack=$(@ "$session" input: "$body")
wait "$lock_holder"
check 'native acceptance returned to human' 'Input sent directly to the session' "$ack"
run=$(@ "$session" activeRun)
directory="$TRASHTALK_RUN_DIR/$run"
host=$(jq -r .home "$directory/jcode.json")
check 'worker lock contention sends input exactly once' 1 "$(jq -s '[.[]|select(.req=="send_message")]|length' "$host/fixture-calls.jsonl")"
check 'direct turn creates no Message' 0 "$(count Message)"
check 'direct turn creates no delivery' 0 "$(count AgentDelivery)"
check 'direct turn keeps a managed run' conversation "$(@ "$run" purpose)"
check 'native user content preserves all bytes' true "$(jq -s --arg body "$body" 'any(.req=="send_message" and .content==$body and (.system_reminder|contains("Reply directly")))' "$host/fixture-calls.jsonl")"
check 'run launcher retains session authority' "$run" "$("$directory/trash-send" AgentRun current)"
ack=$(@ "$session" input: $'follow-up\n')
check 'busy input is acknowledged by native API' 'Input sent directly to the session at its next safe point' "$ack"
check 'busy input uses same run' "$run" "$(@ "$session" activeRun)"
check 'busy input uses soft interrupt on exact conversation' true "$(jq -s 'any(.req=="soft_interrupt" and .session_id=="jcode-fixture-session" and .content=="follow-up\n")' "$host/fixture-calls.jsonl")"
snapshot=$(@ AgentTranscript snapshotFor: "$session" limit: 400)
check 'view contains literal direct user input' true "$(jq --arg body "$body" 'any(.entries[]; .kind=="user" and .text==$body)' <<< "$snapshot")"
check 'view contains streaming assistant reply' true "$(jq 'any(.entries[]; .text=="Steering received\n")' <<< "$snapshot")"
check 'direct input still creates no Message' 0 "$(count Message)"
touch "$host/refuse-input"
context=$(jq -cn --arg session "$session" '{session:$session,window:400}')
result=$(@ AgentFocus handleFrame: '{"schema_version":1,"request_id":1,"intent":"send_message","body":"refused text"}' context: "$context")
check 'native rejection reaches composer as failure' false "$(jq -r .frame.ok <<< "$result")"
check 'native refusal diagnostic reaches composer' true "$(jq '.frame.message|contains("fixture input refused")' <<< "$result")"
check 'rejection creates no fallback mail' 0 "$(count Message)"
rm "$host/refuse-input"
if TRASHTALK_USER=someone-else @ "$session" input: forbidden >/dev/null 2>&1; then echo 'FAIL: foreign owner admitted'; exit 1; fi
if TRASHTALK_RUN_TOKEN=bad @ "$session" input: forbidden >/dev/null 2>&1; then echo 'FAIL: agent admitted to human composer'; exit 1; fi
touch "$JCODE_TEST_GATE"
settle
check 'completion waits for native idle after turn_done' true "$(jq -s '[.[]|select(.req=="attach_session")]|length>=2' "$host/fixture-calls.jsonl")"
check 'direct turn settles without deliveries' succeeded "$(@ "$run" state)"
check 'completed turn revokes launcher' '' "$("$directory/trash-send" AgentRun current 2>/dev/null || true)"
ack=$(@ "$session" input: 'second direct turn')
settle
check 'idle input resumes native conversation' jcode-fixture-session "$(@ "$session" lastConversationRef)"
check 'only one provider conversation created' 1 "$(jq -s '[.[]|select(.req=="create_session")]|length' "$host/fixture-calls.jsonl")"
check 'native user turns stay outside inbox' 0 "$(count Message)"
snapshot=$(@ AgentTranscript snapshotFor: "$session" limit: 400)
check 'reopened view retains both turns' true "$(jq --arg body "$body" 'any(.entries[];.text==$body) and any(.entries[];.text=="second direct turn")' <<< "$snapshot")"
# Provider failure still belongs to the original request after steering has
# advanced the adapter's control request ID. It cannot be silently ignored.
rm "$JCODE_TEST_GATE"
touch "$host/fail-after-input"
@ "$session" input: 'will fail after live input' >/dev/null
failed_run=$(@ "$session" activeRun)
@ "$session" input: 'steer before provider failure' >/dev/null
touch "$JCODE_TEST_GATE"
for i in {1..100}; do
    @ AgentWorker tickSession: "$session" >/dev/null
    [[ $(db_get "$failed_run" | jq -r .state) != recovering ]] || break
    sleep .1
done
check 'provider error from original request remains visible after steering' recovering "$(db_get "$failed_run" | jq -r .state)"
check 'lost direct turn is never replayed through mail' 0 "$(count Message)"
check 'failed direct turn retains exact stop control' interrupted "$(@ "$failed_run" stop)"
rm "$host/fail-after-input"
if @ "$session" input: forbidden >/dev/null 2>&1; then echo 'FAIL: paused input admitted'; exit 1; fi
@ "$session" close >/dev/null
if @ "$session" input: forbidden >/dev/null 2>&1; then echo 'FAIL: historical input admitted'; exit 1; fi
echo 'Direct conversation checks passed'
