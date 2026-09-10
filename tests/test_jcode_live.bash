#!/usr/bin/env bash
# Opt-in subscription-backed acceptance; isolated Inbox, workspace and daemon.
if [[ "${TRASHTALK_TEST_JCODE_LIVE:-}" != 1 ]]; then
    echo 'SKIP: set TRASHTALK_TEST_JCODE_LIVE=1 for authenticated Jcode acceptance'
    exit 0
fi
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=jcode-live-test
mkdir "$tmp/workspace"
cleanup() {
    for config in "$tmp"/runs/*/jcode.json; do
        [[ -s "$config" ]] || continue
        home=$(jq -r .home "$config") runtime=$(jq -r .runtime "$config") executable=$(jq -r .executable "$config")
        bash "$root/lib/jcode-processes.bash" stop "$home" >/dev/null 2>&1 || true
        env JCODE_HOME="$home" JCODE_RUNTIME_DIR="$runtime" JCODE_SOCKET="$runtime/jcode.sock" \
            "$executable" --no-update --quiet --socket "$runtime/jcode.sock" server stop --force >/dev/null 2>&1 && rm -rf "$runtime"
    done
    if [[ "${TRASH_TEST_KEEP:-}" == 1 ]]; then echo "Live artifacts: $tmp"; else rm -rf "$tmp"; fi
}
trap cleanup EXIT
db_init
identity=$(@ AgentIdentity named: jcode-live)
@ "$identity" owner: jcode-live-test
@ "$identity" save
arch=$(@ AgentArchetype define: jcode-live revision: 1 instructions: 'This is an integration test. Follow each inbox message precisely. Use only the run-specific Trashtalk launcher in the current notification. Read the Inbox messages before answering. Complete each delivery before ending your prompt.' profile: jcode)
role=$(@ AgentRole define: jcode-live revision: 1 capabilities: '["inbox.read","message.send"]' workspacePolicy: '[]' runBudget: '{"retryLimit":1}')
session=$(@ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$tmp/workspace" profile: jcode)
first=$(@ Inbox send: 'Reply with exactly JCODE_FIRST_OK, then settle this delivery.' to: "session:$session" from: jcode-live-test)
run=$(@ AgentWorker tickSession: "$session")
second=$(@ Inbox send: 'Reply with exactly JCODE_SECOND_OK, then settle this delivery.' to: "session:$session" from: jcode-live-test)
for attempt in {1..180}; do
    @ AgentWorker tickSession: "$session" >/dev/null 2>&1
    life=$(@ "$session" lifecycleState)
    [[ "$life" == open ]] || break
    [[ -n "$(@ "$session" activeRun)" || "$(@ "$session" pendingCount)" != 0 ]] || break
    sleep 1
done
failed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; else echo "FAIL: $1 expected=$2 got=$3"; failed=1; fi; }
check 'live first run succeeded' succeeded "$(@ "$run" state)"
check 'live first message read' read "$(@ "$first" status)"
check 'live second message read' read "$(@ "$second" status)"
check 'live no stalled work' 0 "$(@ "$session" stalledCount)"
check 'live queue drained' 0 "$(@ "$session" pendingCount)"
owner=$(@ Inbox named: jcode-live-test)
bodies=$(for reply in $(@ "$owner" unread); do @ "$reply" body; done)
check 'live replies delivered through Inbox' $'JCODE_FIRST_OK\nJCODE_SECOND_OK' "$bodies"
check 'live completed launcher revoked' '' "$("$TRASHTALK_RUN_DIR/$run/trash-send" AgentRun current 2>/dev/null)"
if [[ "$failed" == 0 ]]; then
    # Stop actual resident work, not merely the adapter connection.
    instruction="Use the bash tool in the foreground to run exactly this command: echo \$\$ > '$tmp/workspace/work.pid'; touch '$tmp/workspace/work-started'; sleep 120; touch '$tmp/workspace/work-finished'. After it finishes reply SLEPT and settle. Do not background it."
    @ Inbox send: "$instruction" to: "session:$session" from: jcode-live-test >/dev/null
    busy=$(@ AgentWorker tickSession: "$session")
    for attempt in {1..90}; do
        [[ ! -f "$tmp/workspace/work-started" ]] || break
        sleep 1
    done
    check 'live long tool actually started' true "$([[ -f "$tmp/workspace/work-started" ]] && echo true || echo false)"
    check 'live native work stopped' interrupted "$(@ "$busy" stop)"
    check 'live session paused after stop' paused "$(@ "$session" lifecycleState)"
    check 'live adapter has stopped' false "$(@ "$busy" isProcessAlive)"
    if [[ -s "$tmp/workspace/work.pid" ]]; then
        work_pid=$(cat "$tmp/workspace/work.pid")
        for attempt in {1..50}; do
            work_state=$(ps -p "$work_pid" -o stat= 2>/dev/null)
            [[ -n "$work_state" && "$work_state" != *Z* ]] || break
            sleep .1
        done
        check 'live foreground tool has stopped' true "$([[ -z "$work_state" || "$work_state" == *Z* ]] && echo true || echo false)"
        check 'live stopped tool did not finish its remaining work' false "$([[ -e "$tmp/workspace/work-finished" ]] && echo true || echo false)"
    fi
    native_ref=$(@ "$session" lastConversationRef)
    stopped_delivery=$(@ Store findByClass: AgentDelivery where: "json_extract(data,'$.run')='$busy' AND json_extract(data,'$.state')='uncertain'" orderBy: 'created_at ASC' limit: 1)
    @ "$session" skip: "$stopped_delivery" note: 'integration test stop reviewed' >/dev/null
    @ "$session" resume >/dev/null
    @ Inbox send: 'The previous sleep was intentionally stopped. Reply with exactly JCODE_RESUMED_OK and settle this delivery.' to: "session:$session" from: jcode-live-test >/dev/null
    resumed=$(@ AgentWorker tickSession: "$session")
    for attempt in {1..90}; do
        @ AgentWorker tickSession: "$session" >/dev/null 2>&1
        [[ -n "$(@ "$session" activeRun)" ]] || break
        [[ "$(@ "$session" lifecycleState)" == open ]] || break
        sleep 1
    done
    check 'live conversation resumes after daemon stop' succeeded "$(@ "$resumed" state)"
    check 'live resume retains native conversation identity' "$native_ref" "$(@ "$session" lastConversationRef)"
fi
if [[ "$failed" != 0 ]]; then
    for log in "$tmp"/runs/*/stderr.log; do [[ ! -f "$log" ]] || tail -c 3000 "$log"; done
fi
exit "$failed"
