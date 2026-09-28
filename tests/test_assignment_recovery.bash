#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=recovery-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
coordinator=$(must @ Gusgus sessionFor: "$root")
mapfile -t pair < <(@ Agent::Run startFor: "$coordinator" profile: shell)
parent=${pair[0]}; token=${pair[1]}
must @ "$parent" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
a=$(must @ Agent::Run delegate: 'Recover bounded work' criteria: evidence key: bounded)
child=$(field "$a" .currentSession)
d=$(field "$a" .delivery)
# Exercise the actual domain claim/finish seam without spawning a model. The
# worker subprocess journey is covered in test_agent_delegation.bash.
stall() {
    unset TRASHTALK_RUN_TOKEN
    mapfile -t worker_pair < <(@ Agent::Run startFor: "$child" profile: shell)
    worker=${worker_pair[0]}; worker_token=${worker_pair[1]}
    must @ "$worker" transitionTo: running >/dev/null
    check 'worker claims current generation' true "$(@ Agent::Delivery claim: "$d" run: "$worker")"
    must @ "$d" transitionTo: uncertain >/dev/null
    must @ "$worker" finishWith: unsettled outcome: '{"stop_reason":"unfinished"}' error: '' >/dev/null
    export TRASHTALK_RUN_TOKEN="$token"
}
stall
# Legacy objects missing the added allowance receive the documented default.
_db_sql "UPDATE instances SET data=json_remove(data,'$.continuationAllowance') WHERE id='$a';"
reject 'agent cannot raise its own allowance' @ "$a" allowContinuations: 5 reason: more
export TRASHTALK_RUN_TOKEN=invalid
reject 'invalid token cannot become operator authority' @ "$a" continue: again afterDelivery: "$d" key: invalid
export TRASHTALK_RUN_TOKEN="$worker_token"
reject 'expired worker cannot recover its assignment' @ "$a" continue: again afterDelivery: "$d" key: worker
unset TRASHTALK_RUN_TOKEN
export TRASHTALK_USER=other-owner
reject 'foreign human cannot continue assignment' @ "$a" continue: again afterDelivery: "$d" key: foreign
export TRASHTALK_USER=recovery-owner
reject 'human cannot bypass allowance through session requeue' @ "$child" requeue: "$d"
export TRASHTALK_RUN_TOKEN="$token"
# A resident run in recovering must fence continuation even if delivery is uncertain.
_db_sql "UPDATE instances SET data=json_set(data,'$.state','recovering') WHERE id='$worker';"
reject 'recovering resident run prevents overlap' @ "$a" continue: again afterDelivery: "$d" key: active
_db_sql "UPDATE instances SET data=json_set(data,'$.state','unsettled') WHERE id='$worker';"
role=$(field "$child" .role)
@ Store patch: "$role" with: '{"capabilities":[]}' >/dev/null
reject 'revoked specialist role blocks continuation' @ "$a" continue: again afterDelivery: "$d" key: revoked
@ Store patch: "$role" with: '{"capabilities":["inbox.read","message.send","question.ask","assignment.work"]}' >/dev/null
# Publication failure must roll back supersession, count, generation and receipt.
_db_sql "CREATE TRIGGER reject_recovery BEFORE INSERT ON agent_outbox WHEN NEW.message_id='message_${a}_work_2' BEGIN SELECT RAISE(ABORT,'fixture recovery rollback'); END;"
reject 'outbox failure rolls back entire continuation' @ "$a" continue: again afterDelivery: "$d" key: first
check 'rollback reached the injected publication failure' true "$(rg -q 'fixture recovery rollback' "$tmp/rejected" && echo true || echo false)"
check 'rollback retains uncertain delivery' uncertain "$(field "$d" .state)"
check 'rollback retains generation' 1 "$(field "$a" .generation)"
check 'rollback consumes no allowance' 0 "$(@ "$a" continuationCount)"
_db_sql 'DROP TRIGGER reject_recovery;'
# Two processes stage the same receipt before either commits. Store replay may
# acknowledge the winner, but must never publish a second attempt.
_store_tx_before_commit() {
    touch "$tmp/ready-$BASHPID"
    local i
    for ((i=0;i<2000;i++)); do [[ ! -f "$tmp/release" ]] || return 0; sleep .01; done
    return 1
}
(@ "$a" continue: again afterDelivery: "$d" key: first >"$tmp/one" 2>"$tmp/one.err") & p1=$!
(@ "$a" continue: again afterDelivery: "$d" key: first >"$tmp/two" 2>"$tmp/two.err") & p2=$!
for ((i=0;i<2000;i++)); do ready=("$tmp"/ready-*); ((${#ready[@]} == 2)) && break; sleep .01; done
touch "$tmp/release"
must wait "$p1"; must wait "$p2"
unset -f _store_tx_before_commit
check 'both continuations staged before committing' 2 "${#ready[@]}"
next=$(cat "$tmp/one")
check 'concurrent duplicate returns the same delivery' "$next" "$(cat "$tmp/two")"
check 'concurrent duplicate consumes one continuation' 1 "$(@ "$a" continuationCount)"
check 'one generation published' 2 "$(field "$a" .generation)"
reject 'same key cannot hide a changed reason' @ "$a" continue: changed afterDelivery: "$d" key: first
reject 'different key cannot recover stale delivery' @ "$a" continue: again afterDelivery: "$d" key: stale
check 'old attempt stays recorded' 1 "$(field "$d" .attempts)"
d=$next
stall
d=$(must @ "$a" continue: again afterDelivery: "$d" key: second)
stall
d=$(must @ "$a" continue: again afterDelivery: "$d" key: third)
stall
reject 'fourth continuation hits the total allowance' @ "$a" continue: again afterDelivery: "$d" key: fourth
check 'exhaustion retains current delivery' uncertain "$(field "$d" .state)"
check 'count is not reset across generations' 3 "$(@ "$a" continuationCount)"
unset TRASHTALK_RUN_TOKEN
reject 'allowance must be numeric' @ "$a" allowContinuations: garbage reason: more
reject 'allowance cannot be lowered' @ "$a" allowContinuations: 2 reason: more
must @ "$a" allowContinuations: 4 reason: 'Approve one additional continuation' >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
next=$(must @ "$a" continue: again afterDelivery: "$d" key: fourth)
check 'owner grant permits exactly the additional continuation' 4 "$(@ "$a" continuationCount)"
check 'fresh context selected without deleting conversation history' true "$(field "$next" .freshConversation)"
# A new active run appearing after staging must invalidate the negative query.
d=$next
stall
unset TRASHTALK_RUN_TOKEN
must @ "$a" allowContinuations: 5 reason: 'Test concurrent run fence' >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
_store_tx_before_commit() {
    _db_sql "INSERT INTO instances(id,data) VALUES('agentrun_race',json_object('class','Agent::Run','session','$child','state','running'));"
}
reject 'concurrent run start fences staged continuation' @ "$a" continue: again afterDelivery: "$d" key: race
unset -f _store_tx_before_commit
check 'race consumes no continuation' 4 "$(@ "$a" continuationCount)"
check 'race leaves current attempt intact' uncertain "$(field "$d" .state)"
db_delete agentrun_race
# Unanswered questions cannot be bypassed through continuation.
unset TRASHTALK_RUN_TOKEN
q=$(must @ "$a" ask: 'Which remaining implementation?')
export TRASHTALK_RUN_TOKEN="$token"
reject 'unanswered question prevents continuation' @ "$a" continue: again afterDelivery: "$d" key: question
unset TRASHTALK_RUN_TOKEN
must @ "$q" reply: 'The original criteria' >/dev/null
# Ordinary requeue still works for the owner and retains its attempt counter.
ordinary=$(must @ Agent::Delivery new)
@ "$ordinary" session: "$child"
@ "$ordinary" state: uncertain
@ "$ordinary" attempts: 7
@ "$ordinary" save
export TRASHTALK_USER=foreign-owner
reject 'foreign owner cannot requeue ordinary delivery' @ "$child" requeue: "$ordinary"
export TRASHTALK_USER=recovery-owner
check 'owner can requeue ordinary delivery' pending "$(@ "$child" requeue: "$ordinary")"
check 'ordinary retry preserves attempts' 7 "$(field "$ordinary" .attempts)"
# A different live coordinator of the same owner has no authority over this work.
other=$(must @ Agent::Identity named: other-coordinator)
@ "$other" owner: recovery-owner
@ "$other" save
other_session=$(must @ Agent::Session openFor: "$other" archetype: "$(@ "$coordinator" archetype)" role: "$(@ "$coordinator" role)" workspace: "$root" profile: shell)
mapfile -t other_pair < <(@ Agent::Run startFor: "$other_session" profile: shell)
must @ "${other_pair[0]}" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN="${other_pair[1]}"
reject 'foreign live coordinator cannot continue assignment' @ "$a" continue: again afterDelivery: "$d" key: foreign-coordinator
unset TRASHTALK_RUN_TOKEN
# Coordinator authority also requires current membership, not just an old token.
export TRASHTALK_RUN_TOKEN="$token"
must @ "$coordinator" pause >/dev/null
reject 'paused coordinator cannot recover old assignments' @ "$a" continue: again afterDelivery: "$d" key: closed
unset TRASHTALK_RUN_TOKEN
echo "=== $passed assignment recovery checks passed ==="
