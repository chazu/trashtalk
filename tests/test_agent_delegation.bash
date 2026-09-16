#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
cleanup() {
    local rc=$?
    if [[ "$rc" != 0 ]]; then
        for log in "$tmp"/runs/*/stderr.log; do [[ ! -f "$log" ]] || tail -20 "$log" >&2; done
    fi
    if [[ "${TRASH_TEST_KEEP:-}" == 1 ]]; then printf 'Delegation artifacts: %s\n' "$tmp" >&2; else rm -rf "$tmp"; fi
}
trap cleanup EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_USER=delegation-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
settle_session() {
    local i
    for ((i=0;i<100;i++)); do
        must @ Agent::Worker tickSession: "$1" >/dev/null
        [[ -z "$(@ "$1" activeRun)" ]] && return 0
        sleep .1
    done
    echo 'FAIL: session did not settle' >&2
    return 1
}

coordinator=$(must @ Gusgus sessionFor: "$root")
mapfile -t pair < <(@ Agent::Run startFor: "$coordinator" profile: shell)
parent=${pair[0]}; token=${pair[1]}
must @ "$parent" transitionTo: running >/dev/null
must @ Agent::Queue ensureSchema
_db_sql "CREATE TRIGGER reject_delegation_publication BEFORE INSERT ON agent_outbox
 WHEN NEW.message_id LIKE 'message_assignment_%_work_%' BEGIN SELECT RAISE(ABORT,'fixture rollback'); END;"
export TRASHTALK_RUN_TOKEN="$token"
reject 'failed publication cannot launch partial work' @ Agent::Run delegate: 'Explain the fixture' criteria: 'Record fixture evidence' key: fixture
check 'failed publication leaves no Assignment' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Assignment';")"
check 'failed publication leaves no specialist identity' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Agent::Identity' AND json_extract(data,'$.handle')='specialist';")"
_db_sql 'DROP TRIGGER reject_delegation_publication;'
a=$(must @ Agent::Run delegate: 'Explain the fixture' criteria: 'Record fixture evidence' key: fixture)
check 'retry returns the same assignment' "$a" "$(@ Agent::Run delegate: 'Explain the fixture' criteria: 'Record fixture evidence' key: fixture)"
reject 'same key cannot hide changed work' @ Agent::Run delegate: changed criteria: 'Record fixture evidence' key: fixture
reject 'one open child per conversation' @ Agent::Run delegate: another criteria: evidence key: second
unset TRASHTALK_RUN_TOKEN
child=$(field "$a" .currentSession)
delivery=$(field "$a" .delivery)
notice=$(field "$a" .statusMessage)
check 'work is durably queued for dispatch' automatic "$(field "$delivery" .dispatchMode)"
check 'responsibility belongs to specialist identity' "$(field "$child" .identity)" "$(field "$a" .assignee)"
check 'requesting conversation is retained' "$coordinator" "$(field "$a" .requesterSession)"
check 'requesting run is retained for direct input' "$parent" "$(field "$a" .originRun)"
check 'queued status is visible without Gusgus replying' queued "$(@ "$a" snapshot | jq -r .activity)"
check 'one inbox status belongs to human' delegation-owner "$(field "$notice" .to)"
check 'worker has one work delivery' "$delivery" "$(@ "$child" pendingDeliveries)"

export DELEGATION_GATE="$tmp/release" DELEGATION_MARK="$tmp/started" DELEGATION_MODE=complete
cat > "$tmp/agent.bash" <<'SH'
set -euo pipefail
cat > "$DELEGATION_MARK.prompt"
source "$TRASHTALK_DIR/lib/trash.bash"
# Deterministic concurrent supervisor update at every transaction boundary.
# Assignment authorization must fence real authority, without conflicting on
# heartbeat bookkeeping. No production hook is installed by this fixture.
_store_tx_before_commit() {
    @ "${TRASHTALK_RUN_TOKEN%%:*}" markHeartbeat >/dev/null
}
a=$(@ Trash currentAssignment)
@ "$a" progress: 'Fixture evidence recorded' >/dev/null
if @ Agent::Run delegate: recursive criteria: bad key: child >/dev/null 2>&1; then exit 88; fi
if @ Agent::Run settle: "$(@ "$a" delivery)" >/dev/null 2>&1; then exit 89; fi
printf '%s\n' "$a" > "$DELEGATION_MARK"
if [[ "$DELEGATION_MODE" == question ]]; then
    @ "$a" ask: 'Which fixture variant?' > "$DELEGATION_MARK.question"
elif [[ "$DELEGATION_MODE" == silent ]]; then
    exit 0
else
    for ((i=0;i<300;i++)); do [[ -f "$DELEGATION_GATE" ]] && break; sleep .1; done
    [[ -f "$DELEGATION_GATE" ]]
    @ "$a" complete: 'Explained: fixture evidence verified' >/dev/null
    for delivery in $(sed -n 's/^--- delivery //p' "$DELEGATION_MARK.prompt"); do
        if [[ -z "$(@ "$delivery" assignment)" ]]; then @ Agent::Run settle: "$delivery" >/dev/null; fi
    done
fi
SH
export TRASHTALK_SHELL_DRIVER="bash '$tmp/agent.bash'"
must @ Agent::Worker tickSession: "$child" >/dev/null
check 'actual launched run is reported' running "$(@ "$a" snapshot | jq -r .activity)"
check 'same inbox item now names actual run' true "$(field "$notice" .body | awk '/Run: agentrun_/ {yes=1} END {print yes ? "true" : "false"}')"
for ((i=0;i<150;i++)); do [[ -f "$DELEGATION_MARK" ]] && break; sleep .1; done
check 'agent can use currentAssignment and fenced progress' "$a" "$(cat "$DELEGATION_MARK")"
check 'generated prompt teaches completion protocol' true "$(awk '/Complete this work:/ {yes=1} END {print yes ? "true" : "false"}' "$DELEGATION_MARK.prompt")"
touch "$DELEGATION_GATE"
must settle_session "$child"
check 'explicit completion records outcome' completed "$(field "$a" .state)"
check 'completion settles only assignment delivery' processed "$(field "$delivery" .state)"
check 'outcome updates the existing inbox item' "$notice" "$(field "$a" .statusMessage)"
check 'human sees the outcome while coordinator remains busy' true "$(field "$notice" .body | awk '/Explained: fixture evidence verified/ {yes=1} END {print yes ? "true" : "false"}')"
outcome=$(field "$a" .resultMessage)
check 'outcome is routed back to requesting conversation' "session:$coordinator" "$(field "$outcome" .to)"
check 'completion notification is pending on coordinator' 1 "$(@ "$coordinator" pendingCount)"
must @ "$notice" markRead >/dev/null
must @ Agent::Delegation reconcile >/dev/null
check 'reconciliation does not resurrect read status' read "$(field "$notice" .status)"
export TRASHTALK_RUN_TOKEN="$token"
check 'completed request retry does not launch again' "$a" "$(@ Agent::Run delegate: 'Explain the fixture' criteria: 'Record fixture evidence' key: fixture)"

# Questions are durable and ordinary replies resume exactly this assignment.
b=$(must @ Agent::Run delegate: 'Choose the fixture' criteria: 'Record the choice' key: question)
unset TRASHTALK_RUN_TOKEN
export DELEGATION_MODE=question
rm -f "$DELEGATION_MARK"
must settle_session "$child"
q=$(cat "$DELEGATION_MARK.question")
check 'question leaves work open' open "$(field "$b" .state)"
check 'question is recorded on assignment' "$b" "$(field "$q" .assignment)"
check 'work waits for its own answer' 'waiting on question' "$(@ "$b" snapshot | jq -r .activity)"
must @ "$child" close >/dev/null
identity=$(field "$child" .identity)
arch=$(field "$child" .archetype)
role=$(field "$child" .role)
child=$(must @ Agent::Session openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
must @ "$b" workIn: "$child" >/dev/null
answer=$(must @ "$q" reply: 'Use the small fixture')
export DELEGATION_MODE=complete
must settle_session "$child"
check 'answer is attached to the exact question' "$answer" "$(@ "$q" answerId)"
check 'answer resumes and completes same responsibility' completed "$(field "$b" .state)"
check 'continuation retains both sessions' 2 "$(field "$b" '.history | length')"
check 'continuation records both runs' 2 "$(field "$b" '[.history[].runs | length] | add')"

# A provider exiting zero is not an Assignment outcome.
export TRASHTALK_RUN_TOKEN="$token"
c=$(must @ Agent::Run delegate: 'Do not infer success' criteria: 'Explicit result required' key: silent)
unset TRASHTALK_RUN_TOKEN
export DELEGATION_MODE=silent
must settle_session "$child"
check 'silent process success leaves assignment open' open "$(field "$c" .state)"
check 'unsettled work is visible for review' 'needs review' "$(@ "$c" snapshot | jq -r .activity)"
check 'same status item reports failure to finish' alert "$(field "$(field "$c" .statusMessage)" .kind)"

echo "=== $passed delegation checks passed ==="
