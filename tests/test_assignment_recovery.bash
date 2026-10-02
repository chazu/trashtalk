#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_USER=recovery-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID TRASHTALK_ASSIGNMENT_ATTEMPTS
db_init
passed=0
source tests/helpers/check.bash
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
settle() {
    local i
    for ((i=0;i<200;i++)); do
        must @ Agent::Worker tickSession: "$1" >/dev/null
        [[ -z "$(@ "$1" activeRun)" ]] && return 0
        sleep .1
    done
    echo 'FAIL: session did not settle' >&2
    exit 1
}

# The specialist fixture: silent ends the turn without completing, complete
# finishes the Assignment, question asks and waits. Each prompt is kept.
export RECOVERY_MODE=silent RECOVERY_PROMPTS="$tmp/prompts"
mkdir -p "$RECOVERY_PROMPTS"
cat > "$tmp/agent.bash" <<'SH'
set -euo pipefail
prompt="$RECOVERY_PROMPTS/${TRASHTALK_RUN_TOKEN%%:*}"
cat > "$prompt"
source "$TRASHTALK_DIR/lib/trash.bash"
a=$(@ Trash currentAssignment)
case "$RECOVERY_MODE" in
    silent) exit 0 ;;
    complete) @ "$a" complete: 'Finished after resuming' >/dev/null ;;
    question) @ "$a" ask: 'Which variant?' >/dev/null ;;
esac
# Ordinary deliveries batched with the Assignment, such as an answer, are settled.
for delivery in $(sed -n 's/^--- delivery //p' "$prompt"); do
    [[ -n "$(@ "$delivery" assignment)" ]] || @ Agent::Run settle: "$delivery" >/dev/null
done
SH
export TRASHTALK_SHELL_DRIVER="bash '$tmp/agent.bash'"
prompt_of() { cat "$RECOVERY_PROMPTS/$1"; }

coordinator=$(must @ Gusgus sessionFor: "$root")
mapfile -t pair < <(@ Agent::Run startFor: "$coordinator" profile: shell)
parent=${pair[0]}; token=${pair[1]}
must @ "$parent" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
a=$(must @ Agent::Run delegate: 'Long work' criteria: evidence key: long)
unset TRASHTALK_RUN_TOKEN
child=$(field "$a" .currentSession)
d=$(field "$a" .delivery)

# Unfinished turns resume the same delivery automatically, up to the limit.
settle "$child"
check 'default limit is four turns' 4 "$(@ Assignment attemptLimit)"
check 'every turn ran on the same delivery' 4 "$(field "$a" '.history[-1].runs | length')"
check 'attempts are counted on the delivery' 4 "$(field "$d" .attempts)"
check 'no new generation is published' 1 "$(field "$a" .generation)"
check 'exhausted work becomes uncertain' uncertain "$(field "$d" .state)"
check 'exhausted work needs review' 'needs review' "$(@ "$a" snapshot | jq -r .activity)"
check 'status item alerts the owner' alert "$(field "$(field "$a" .statusMessage)" .kind)"
check 'owner receives one stall alert' 1 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Message' AND json_extract(data,'$.to')='recovery-owner' AND json_extract(data,'$.from')='worker';")"
check 'coordinator receives no recovery notification' 0 "$(@ "$coordinator" pendingCount)"
check 'show reports attempts' true "$(@ "$a" show | rg -q 'Attempts: 4 of 4' && echo true || echo false)"
runs=$(field "$a" '.history[-1].runs[]')
first=$(head -1 <<<"$runs"); second=$(sed -n 2p <<<"$runs")
check 'first turn is not told it resumed' false "$(prompt_of "$first" | rg -q 'This is turn' && echo true || echo false)"
check 'later turns are told to inspect earlier work' true "$(prompt_of "$second" | rg -q 'This is turn 2' && echo true || echo false)"
check 'prompt names the turn limit and ask: as the way to stop' true "$(prompt_of "$first" | rg -q 'at most 4 turns' && echo true || echo false)"
check 'no continuation protocol is taught' false "$(prompt_of "$first" | rg -q 'afterDelivery' && echo true || echo false)"

# retry is the one recovery verb; it is guarded by authority, live runs and questions.
export TRASHTALK_USER=foreign-owner
reject 'foreign human cannot retry' @ "$a" retry
export TRASHTALK_USER=recovery-owner
other=$(must @ Agent::Identity named: other-coordinator)
@ "$other" owner: recovery-owner
@ "$other" save
other_session=$(must @ Agent::Session openFor: "$other" archetype: "$(@ "$coordinator" archetype)" role: "$(@ "$coordinator" role)" workspace: "$root" profile: shell)
mapfile -t other_pair < <(@ Agent::Run startFor: "$other_session" profile: shell)
must @ "${other_pair[0]}" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN="${other_pair[1]}"
reject 'another coordinator identity cannot retry' @ "$a" retry
unset TRASHTALK_RUN_TOKEN
last=$(field "$d" .run)
_db_sql "UPDATE instances SET data=json_set(data,'$.state','recovering') WHERE id='$last';"
reject 'a live run fences retry' @ "$a" retry
check 'live run is the stated reason' true "$(rg -q 'Stop the Assignment run' "$tmp/rejected" && echo true || echo false)"
_db_sql "UPDATE instances SET data=json_set(data,'$.state','unsettled') WHERE id='$last';"
q=$(must @ "$a" ask: 'Is the remaining work still wanted?')
reject 'an unanswered question fences retry' @ "$a" retry
check 'question is the stated reason' true "$(rg -q 'Answer the Assignment question' "$tmp/rejected" && echo true || echo false)"
must @ "$q" reply: 'Yes' >/dev/null
reject 'session requeue cannot bypass the Assignment' @ "$child" requeue: "$d"
check 'owner retry requeues the same delivery' "$d" "$(@ "$a" retry)"
check 'retry restores a fresh set of turns' 0 "$(field "$d" .attempts)"
check 'retry is journaled' retried "$(field "$a" '.events[-1].kind')"
reject 'pending work cannot be retried twice' @ "$a" retry
export RECOVERY_MODE=complete
settle "$child"
check 'retried work completes' completed "$(field "$a" .state)"
check 'completion settles the same delivery' processed "$(field "$d" .state)"
check 'the answer rode along with its Assignment' 0 "$(@ "$child" stalledCount)"

# A requesting coordinator may retry too, and a question stops automatic turns.
export TRASHTALK_ASSIGNMENT_ATTEMPTS=1 RECOVERY_MODE=silent TRASHTALK_RUN_TOKEN="$token"
b=$(must @ Agent::Run delegate: 'Short budget' criteria: evidence key: short)
unset TRASHTALK_RUN_TOKEN
settle "$child"
bd=$(field "$b" .delivery)
check 'configured limit stops after one turn' 1 "$(field "$bd" .attempts)"
check 'short budget needs review' uncertain "$(field "$bd" .state)"
export RECOVERY_MODE=question TRASHTALK_ASSIGNMENT_ATTEMPTS=3 TRASHTALK_RUN_TOKEN="$token"
check 'coordinator retry requeues its delegated work' "$bd" "$(@ "$b" retry)"
unset TRASHTALK_RUN_TOKEN
settle "$child"
check 'a question blocks instead of resuming' blocked "$(field "$bd" .state)"
check 'the question ends automatic turns and resets the count' 0 "$(field "$bd" .attempts)"
check 'blocked work is waiting on the question' 'waiting on question' "$(@ "$b" snapshot | jq -r .activity)"
# The answer is consumed with its Assignment and restarts the turn count.
export RECOVERY_MODE=silent
bq=$(@ "$b" snapshot | jq -r '.questions[-1].message')
must @ "$bq" reply: 'The small variant' >/dev/null
settle "$child"
check 'answered work gets a fresh set of turns' 5 "$(field "$b" '.history[-1].runs | length')"
check 'answered work needs review after those turns' uncertain "$(field "$bd" .state)"
answer=$(@ "$bq" answerId)
answer_delivery=$(_db_sql "SELECT d.id FROM instances d, json_each(json_extract(d.data,'$.messageIds')) j WHERE d.class='Agent::Delivery' AND j.value='$answer';")
check 'the answer delivery settles with its Assignment' processed "$(field "$answer_delivery" .state)"
check 'the answer does not stall the session' 0 "$(@ "$child" stalledOrdinaryCount)"
check 'the failure reason survives in the resume note' true "$(field "$bd" .lastError | rg -q 'Turn' && echo true || echo false)"

# A stalled Assignment does not hold back other work in the same session.
export TRASHTALK_ASSIGNMENT_ATTEMPTS=1 RECOVERY_MODE=silent TRASHTALK_RUN_TOKEN="$token"
c=$(must @ Agent::Run delegate: 'Stalls' criteria: evidence key: stalls)
unset TRASHTALK_RUN_TOKEN
must @ "$b" cancel: 'Superseded' >/dev/null
settle "$child"
check 'stalled Assignment needs review' uncertain "$(field "$(field "$c" .delivery)" .state)"
export RECOVERY_MODE=complete TRASHTALK_RUN_TOKEN="$token"
e=$(must @ Agent::Run delegate: 'Queued behind a stall' criteria: evidence key: behind)
unset TRASHTALK_RUN_TOKEN
settle "$child"
check 'queued work runs past a stalled Assignment' completed "$(field "$e" .state)"

# An explicit stop is a human decision: no automatic resume.
export TRASHTALK_ASSIGNMENT_ATTEMPTS=4 TRASHTALK_RUN_TOKEN="$token"
f=$(must @ Agent::Run delegate: 'Stopped by hand' criteria: evidence key: stopped)
unset TRASHTALK_RUN_TOKEN
fd=$(field "$f" .delivery)
mapfile -t wpair < <(@ Agent::Run startFor: "$child" profile: shell)
must @ "${wpair[0]}" transitionTo: running >/dev/null
check 'fixture claims the stopped work' true "$(@ Agent::Delivery claim: "$fd" run: "${wpair[0]}")"
check 'stop interrupts the run' interrupted "$(@ "${wpair[0]}" stop)"
check 'stop pauses the specialist session' paused "$(field "$child" .lifecycleState)"
settle "$child"
check 'stopped work is not resumed automatically' uncertain "$(field "$fd" .state)"
check 'stopped work keeps one attempt' 1 "$(field "$fd" .attempts)"
check 'retry after a stop requeues the work' "$fd" "$(@ "$f" retry)"
check 'retry reopens the session the stop paused' open "$(field "$child" .lifecycleState)"
must @ "$f" cancel: 'Done with this fixture' >/dev/null
must @ "$child" pause >/dev/null
reject 'retry refuses a session a person paused' @ "$c" retry
check 'the refusal says how to proceed' true "$(rg -q 'resume it, or cancel' "$tmp/rejected" && echo true || echo false)"
must @ "$child" resume >/dev/null

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
echo "=== $passed assignment recovery checks passed ==="
