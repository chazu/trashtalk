#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs with spaces"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=jcode-owner
unset TRASHTALK_GUSGUS_PROFILE
export JCODE_TEST_GATE="$tmp/gate" JCODE_HOME="$tmp/source-auth" CODEX_HOME="$tmp/no-codex"
mkdir -p "$tmp/bin" "$JCODE_HOME" "$tmp/workspace with spaces"
# Interactive shells may source Trashtalk without exporting its optional root.
export HOME="$tmp/home"
mkdir -p "$HOME"
ln -s "$root" "$HOME/.trashtalk"
unset TRASHTALK_DIR
printf '{}\n' > "$JCODE_HOME/openai-auth.json"
cp "$root/tests/fixtures/jcode-api.py" "$tmp/bin/jcode"
chmod +x "$tmp/bin/jcode"
export PATH="$tmp/bin:$PATH" OPENAI_API_KEY=fixture CODEX_API_KEY=fixture OPENROUTER_API_KEY=fixture
cleanup() {
    for config in "$TRASHTALK_RUN_DIR"/*/jcode.json; do
        [[ ! -s "$config" ]] || bash "$root/lib/jcode-processes.bash" stop "$(jq -r .home "$config")" >/dev/null 2>&1 || true
    done
    for pf in "$TRASHTALK_RUN_DIR"/*/pid; do
        [[ -s "$pf" ]] || continue
        read -r pid < "$pf"
        kill -TERM -- "-$pid" 2>/dev/null || true
    done
    for path in "$TRASHTALK_RUN_DIR"/hosts/*/jcode/runtime.path; do
        [[ ! -s "$path" ]] || rmdir "$(cat "$path")" 2>/dev/null || true
    done
    rm -rf "$tmp"
}
trap cleanup EXIT
db_init
passed=0
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 expected=$2 got=$3"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
field() { db_get "$1" | jq -r --arg f "$2" '.[$f] // empty'; }
await_file() { for i in {1..100}; do [[ ! -s "$1" ]] || return; sleep .1; done; echo "FAIL: missing $1"; exit 1; }
settle() {
    for i in {1..100}; do
        @ AgentWorker tickSession: "$session" >/dev/null 2>&1
        [[ -n "$(@ "$session" activeRun)" ]] || return
        sleep .1
    done
    echo 'FAIL: Jcode run did not settle'; cat "$TRASHTALK_RUN_DIR"/*/stderr.log; exit 1
}
check 'common profile resolves Jcode' JcodeDriver "$(@ AgentWorker driverFor: jcode)"
check 'Gusgus defaults to Jcode' jcode "$(@ Gusgus profile)"
doctor=$(@ Trash doctor 2>&1)
check 'doctor checks the default Jcode harness' true "$([[ "$doctor" == *'Jcode found'* ]] && echo true || echo false)"
check 'doctor does not install an unselected Maki harness' false "$([[ "$doctor" == *'Maki '* ]] && echo true || echo false)"
session=$(@ Gusgus sessionFor: "$tmp/workspace with spaces")
check 'new session snapshots Jcode by default' jcode "$(field "$session" backendProfile)"
msg=$(@ Inbox send: FIRST_SECRET to: "session:$session" from: jcode-owner)
run=$(@ AgentWorker tickSession: "$session")
directory="$TRASHTALK_RUN_DIR/$run"
await_file "$directory/conversation"
host=$(jq -r .home "$directory/jcode.json")
for i in {1..100}; do [[ $(cat "$host/fixture-state") == processing ]] && break; sleep .1; done
check 'ack and unrelated completion leave run active' running "$(field "$run" state)"
msg2=$(@ Inbox send: SECOND_SECRET to: "session:$session" from: jcode-owner)
check 'busy session gets no overlapping run' '' "$(@ AgentWorker tickSession: "$session")"
check 'second message stays queued' 1 "$(@ "$session" pendingCount)"
check 'direct steering can join a run processing inbox mail' 'Input sent directly to the session at its next safe point' "$(@ "$session" input: 'direct steering while reading inbox')"
check 'steering adds no pending delivery' 1 "$(@ "$session" pendingCount)"
touch "$JCODE_TEST_GATE"
settle
check 'first native run succeeded' succeeded "$(field "$run" state)"
check 'same native conversation retained' jcode-fixture-session "$(field "$session" lastConversationRef)"
check 'exactly two queued prompts sent' 2 "$(jq -s '[.[] | select(.req=="send_message")] | length' "$host/fixture-calls.jsonl")"
check 'native session only created once' 1 "$(jq -s '[.[] | select(.req=="create_session")] | length' "$host/fixture-calls.jsonl")"
check 'first message actually read from Inbox' read "$(field "$msg" status)"
check 'second message actually read from Inbox' read "$(field "$msg2" status)"
check 'deliveries settled through common API' 0 "$(@ "$session" pendingCount)"
owner=$(@ Inbox named: jcode-owner)
replies=$(@ "$owner" unread)
check 'two attributed replies' 2 "$(printf '%s\n' "$replies" | wc -l | tr -d ' ')"
for reply in $replies; do
    original=$(@ "$reply" replyTo)
    check 'reply scoped to its delivery' "fixture got: $(@ "$original" body)" "$(@ "$reply" body)"
    check 'sender derived from run' "session:$session" "$(@ "$reply" from)"
done
check 'completed launcher loses authority' '' "$("$directory/trash-send" AgentRun current 2>/dev/null)"
check 'notification contains no message body' 0 "$(grep -c FIRST_SECRET "$directory/prompt.txt")"

# Public context maintenance keeps the session, provider reference, and queue.
context_result=$(@ "$session" compact)
check 'compaction reports a background run' true "$([[ "$context_result" == 'Compacting context in the background'* ]] && echo true || echo false)"
settle
check 'compaction targets existing native session once' 1 "$(jq -s '[.[]|select(.req=="compact" and .session_id=="jcode-fixture-session")]|length' "$host/fixture-calls.jsonl")"
check 'compaction keeps its bridge alive through asynchronous summary' true "$(jq -s 'any(.req=="ping")' "$host/fixture-calls.jsonl")"
check 'compaction preserves logical provider reference' jcode-fixture-session "$(field "$session" lastConversationRef)"
check 'compaction sends no new delivery' 0 "$(@ "$session" pendingCount)"
checkpoint=$(rg --files "$TRASHTALK_RUN_DIR" | rg 'context-before.json$' | head -1)
check 'compaction retains the original history checkpoint' 1 "$(jq '.messages|length' "$checkpoint")"
# Refusal has no model work to settle; it must remain a visible failed run.
touch "$host/refuse-compact"
@ "$session" compact >/dev/null
compact_run=$(@ "$session" activeRun)
for i in {1..100}; do
    @ AgentWorker tickSession: "$session" >/dev/null 2>&1
    [[ $(field "$compact_run" state) != running ]] && break
    sleep .1
done
check 'native compaction refusal is visible' failed "$(field "$compact_run" state)"
check 'refused compaction does not pause new work' open "$(field "$session" lifecycleState)"
rm "$host/refuse-compact"
touch "$host/lose-compact"
@ "$session" compact >/dev/null
compact_run=$(@ "$session" activeRun)
for i in {1..100}; do
    @ AgentWorker tickSession: "$session" >/dev/null 2>&1
    [[ $(field "$compact_run" state) != running ]] && break
    sleep .1
done
check 'lost compaction observer becomes a failed maintenance run' failed "$(field "$compact_run" state)"
check 'lost compaction observer leaves conversation available' open "$(field "$session" lifecycleState)"
rm "$host/lose-compact"

# A connection can disappear while a resident daemon still owns the prompt.
export JCODE_TEST_MODE=lost
@ Inbox send: 'long task' to: "session:$session" from: jcode-owner >/dev/null
lost=$(@ AgentWorker tickSession: "$session")
for i in {1..100}; do
    @ AgentWorker tickSession: "$session" >/dev/null
    [[ $(field "$lost" state) == recovering ]] && break
    sleep .1
done
check 'lost connection retains exact recoverable run' recovering "$(field "$lost" state)"
check 'lost connection pauses new dispatch' paused "$(field "$session" lifecycleState)"
work_pid=$(cat "$host/fixture-work.pid")
check 'native tool survives adapter connection loss' true "$(kill -0 "$work_pid" 2>/dev/null && echo true || echo false)"
check 'lost run launcher revoked' '' "$("$TRASHTALK_RUN_DIR/$lost/trash-send" AgentRun current 2>/dev/null)"
@ Inbox send: 'still queued' to: "session:$session" from: jcode-owner >/dev/null
@ AgentWorker tickSession: "$session" >/dev/null
check 'lost prompt never replayed' 3 "$(jq -s '[.[] | select(.req=="send_message")] | length' "$host/fixture-calls.jsonl")"

# Agent stop authority is checked against the authenticated caller and owner.
identity=$(@ AgentIdentity named: supervisor)
@ "$identity" owner: jcode-owner
@ "$identity" save
role=$(@ AgentRole define: supervisor revision: 1 capabilities: '["agent.stop"]' workspacePolicy: '[]' runBudget: '{}')
actor_session=$(@ AgentSession openFor: "$identity" archetype: "$(@ "$session" archetype)" role: "$role" workspace: "$tmp" profile: shell)
mapfile -t started < <(@ AgentRun startFor: "$actor_session" profile: shell)
actor=${started[0]} actor_token=${started[1]}
@ "$actor" transitionTo: running >/dev/null
check 'invalid supplied token rejects stop' '' "$(TRASHTALK_RUN_TOKEN=invalid @ "$lost" stop 2>/dev/null)"
@ "$identity" owner: someone-else
@ "$identity" save
TRASHTALK_RUN_TOKEN="$actor_token" @ AgentRun stop: "$lost" >/dev/null 2>&1; rc=$?
check 'cross-owner stop rejected' 1 "$rc"
@ "$identity" owner: jcode-owner
@ "$identity" save
touch "$host/refuse-stop"
TRASHTALK_RUN_TOKEN="$actor_token" @ AgentRun stop: "$lost" >/dev/null 2>&1; rc=$?
check 'cancel acknowledgment alone cannot confirm stop' 1 "$rc"
check 'unconfirmed cancellation retains run' recovering "$(field "$lost" state)"
rm "$host/refuse-stop"
check 'authorized agent cancels native work after adapter loss' interrupted "$(TRASHTALK_RUN_TOKEN="$actor_token" @ AgentRun stop: "$lost")"
check 'native session observed idle' idle "$(cat "$host/fixture-state")"
check 'stop also kills native foreground tool' true "$([[ -z "$(ps -p "$work_pid" -o stat=)" || "$(ps -p "$work_pid" -o stat=)" == *Z* ]] && echo true || echo false)"
check 'stopped session remains paused' paused "$(field "$session" lifecycleState)"
check 'unsettled work preserved for review' 1 "$(@ "$session" stalledCount)"
receipt=$(@ Store findByClass: AgentDelivery where: "json_extract(data,'$.run')='$lost'" orderBy: 'created_at ASC' limit: 1)
check 'stopped delivery cache agrees with durable receipt' uncertain "$(@ "$receipt" state)"
check 'later inbox message remains queued' 1 "$(@ "$session" pendingCount)"
check 'repeat stop is idempotent' already-stopped "$(@ "$lost" stop)"
@ AgentWorker tickSession: "$session" >/dev/null
check 'stop does not cause automatic restart' '' "$(@ "$session" activeRun)"
check 'other agent keeps its authority' "$actor" "$(TRASHTALK_RUN_TOKEN="$actor_token" @ AgentRun current)"

# Explicit review/resume may launch replacement work; an old stop cannot hit it.
uncertain=$(@ Store findByClass: AgentDelivery where: "json_extract(data,'$.run')='$lost' AND json_extract(data,'$.state')='uncertain'" orderBy: 'created_at ASC' limit: 1)
@ "$session" skip: "$uncertain" note: 'reviewed interrupted fixture' >/dev/null
@ "$session" resume >/dev/null
unset JCODE_TEST_MODE
rm "$JCODE_TEST_GATE"
replacement=$(@ AgentWorker tickSession: "$session")
await_file "$TRASHTALK_RUN_DIR/$replacement/conversation"
for i in {1..100}; do [[ $(cat "$host/fixture-state") == processing ]] && break; sleep .1; done
check 'stale stop is harmless after replacement' already-stopped "$(@ "$lost" stop)"
check 'replacement remains active' "$replacement" "$(@ "$session" activeRun)"
check 'replacement native work remains processing' processing "$(cat "$host/fixture-state")"

denyrole=$(@ AgentRole define: observer revision: 1 capabilities: '["inbox.read"]' workspacePolicy: '[]' runBudget: '{}')
observer_identity=$(@ AgentIdentity named: observer)
@ "$observer_identity" owner: jcode-owner
@ "$observer_identity" save
denysession=$(@ AgentSession openFor: "$observer_identity" archetype: "$(@ "$session" archetype)" role: "$denyrole" workspace: "$tmp" profile: shell)
mapfile -t denied < <(@ AgentRun startFor: "$denysession" profile: shell)
@ "${denied[0]}" transitionTo: running >/dev/null
TRASHTALK_RUN_TOKEN="${denied[1]}" @ AgentRun stop: "$replacement" >/dev/null 2>&1; rc=$?
check 'same-owner agent without agent.stop is rejected' 1 "$rc"
check 'user termination cancels resident work as well as adapter' terminated "$(@ "$session" terminate)"
check 'termination confirmed native idle' idle "$(cat "$host/fixture-state")"
check 'terminated replacement token is revoked' '' "$("$TRASHTALK_RUN_DIR/$replacement/trash-send" AgentRun current 2>/dev/null)"
echo "=== $passed Jcode driver checks passed ==="
