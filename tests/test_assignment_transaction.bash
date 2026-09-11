#!/usr/bin/env bash
# Regression journey against production Store transactions and Assignment DSL.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
fixture="$root/tests/fixtures/assignment-transaction"
cp "$fixture/TransactionFaults.trash" "$root/trash/"
make -s single CLASS=TransactionFaults >"$TMPDIR/transaction-fault-build.log" 2>&1 || {
    cat "$TMPDIR/transaction-fault-build.log"; exit 1;
}
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=assignment-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_NATIVE=1
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
@ AgentQueue ensureSchema
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected.out" 2>"$tmp/rejected.err"; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
new_work() {
    local a
    a=$(@ Assignment draft: "$1" in: "$root") || return
    @ "$a" origin: "$origin" >/dev/null || return
    @ "$a" assignTo: "$identity" >/dev/null || return
    @ "$a" workIn: "$session" >/dev/null || return
    echo "$a"
}
# No conflict replay here: test the primitive separately from the public API's
# opt-in read-only acknowledgement of a concurrently completed operation.
complete() {
    local args
    args=$(jq -cn --arg outcome "$2" '{outcome:$outcome,cancel:"false"}')
    _store_transaction false "$1" finishWithin: "$args"
}
unpublished() {
    check "$2: Assignment open" open "$(field "$1" .state)"
    check "$2: delivery pending" pending "$(field "$(field "$1" .delivery)" .state)"
    check "$2: no Message draft" 0 "$(_db_sql "SELECT count(*) FROM instances WHERE id='message_${1}_outcome';")"
    check "$2: no outbox entry" 0 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE message_id='message_${1}_outcome';")"
}

identity=$(must @ AgentIdentity named: proof-specialist)
@ "$identity" owner: assignment-owner
@ "$identity" save
arch=$(must @ AgentArchetype define: proof-specialist revision: 1 instructions: 'Manual proof only.' profile: shell)
role=$(must @ AgentRole define: proof-specialist revision: 1 capabilities: '["inbox.read","message.send","assignment.work"]' workspacePolicy: '[]' runBudget: '{}')
session=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
requester=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
origin=$(must @ Inbox send: 'Please investigate' to: assignment-owner from: "session:$requester")
a=$(must new_work 'Prove atomic completion')
delivery=$(field "$a" .delivery)
unrelated=$(must new_work 'Leave this work alone')
check 'warm caller Assignment cache' open "$(@ "$a" state)"
check 'warm caller delivery cache' pending "$(@ "$delivery" state)"
body='Evidence: single quote '\''; literal $(false), `false`, and a newline
remain message data.'

_store_tx_before_commit() {
    [[ $(field "$a" .state) == open && $(field "$delivery" .state) == pending ]] || return 1
    [[ $(_db_sql "SELECT count(*) FROM instances WHERE id='message_${a}_outcome';") == 0 ]] || return 1
    [[ $(_db_sql "SELECT count(*) FROM agent_outbox WHERE message_id='message_${a}_outcome';") == 0 ]] || return 1
    [[ $(SQLITE_JSON_DB="$_STORE_BARRIER_TX/work.db" field "$a" .state) == completed ]] || return 1
    printf '%s\n' observed > "$tmp/invisible"
}
proof_started=$EPOCHREALTIME
proof_objects=$(_db_sql 'SELECT count(*) FROM instances;')
result=$(must complete "$a" "$body")
proof_elapsed=$(awk -v start="$proof_started" -v end="$EPOCHREALTIME" 'BEGIN {printf "%.3f", end-start}')
printf 'First completion: %ss, %s snapshot objects (illustrative, not a benchmark)\n' "$proof_elapsed" "$proof_objects"
unset -f _store_tx_before_commit
check 'result is released only after commit' "$a" "$result"
check 'staged writes invisible to live reader' observed "$(cat "$tmp/invisible")"
check 'caller Assignment cache invalidated after captured completion' completed "$(@ "$a" state)"
check 'caller delivery cache invalidated' processed "$(@ "$delivery" state)"
msg=$(field "$a" .resultMessage)
check 'message uses regular Message class' Message "$(field "$msg" .class)"
check 'message uses ordinary unread default' unread "$(field "$msg" .status)"
check 'message preserves hostile-looking multiline data' "$body" "$(field "$msg" .body)"
check 'result routed to originating requester' "session:$requester" "$(field "$msg" .to)"
check 'result preserves thread' "$(field "$origin" .thread)" "$(field "$msg" .thread)"
check 'result replies to exact origin' "$origin" "$(field "$msg" .replyTo)"
check 'result has manual dispatch metadata' manual "$(field "$msg" .dispatchMode)"
check 'outbox is durably routed without waking harness' "$requester" "$(_db_sql "SELECT session FROM agent_outbox WHERE message_id='$msg';")"
check 'unrelated delivery unaffected' pending "$(field "$(field "$unrelated" .delivery)" .state)"
check 'normal Inbox can read committed result' "$msg" "$(@ "$(@ "$requester" inboxObject)" unread)"
check 'repeat completion returns same Assignment' "$a" "$(must complete "$a" "$body")"
check 'repeat completion appends no event' 1 "$(field "$a" '.events | length')"
reject 'conflicting completion rejected' complete "$a" different

b=$(must new_work 'Roll back after state updates')
bd=$(field "$b" .delivery)
check 'warm cache before rollback' open "$(@ "$b" state)"
# This trigger lives only on the LIVE database. Its error proves updates were
# applied before publication failed; connection close must roll all of it back.
_db_sql "CREATE TRIGGER proof_fail_outbox BEFORE INSERT ON agent_outbox WHEN NEW.message_id='message_${b}_outcome' BEGIN
 SELECT CASE WHEN (SELECT json_extract(data,'$.state') FROM instances WHERE id='$b')='completed'
 AND (SELECT json_extract(data,'$.state') FROM instances WHERE id='$bd')='processed'
 THEN RAISE(ABORT,'injected after both state updates') ELSE RAISE(ABORT,'wrong write order') END; END;"
reject 'live outbox failure rejects commit' complete "$b" done
check 'failure really occurs after both updates' true "$(rg -q 'injected after both state updates' "$tmp/rejected.err" && echo true || echo false)"
check 'failed commit leaks no result' '' "$(cat "$tmp/rejected.out")"
unpublished "$b" rollback
check 'aborted staging never pollutes caller cache' open "$(@ "$b" state)"
_db_sql 'DROP TRIGGER proof_fail_outbox;'
must complete "$b" done >/dev/null

c=$(must new_work 'Reject a swallowed failure')
reject 'failed captured message poisons successful later work' _store_transaction false TransactionFaults swallowedFailure: "$c" outcome: done
check 'swallowed error releases no result' '' "$(cat "$tmp/rejected.out")"
unpublished "$c" 'sticky abort'
reject 'nested transaction poisons the outer operation' _store_transaction false TransactionFaults nested: "$c" outcome: done
unpublished "$c" 'nested abort'
reject 'unsupported object deletion cannot escape staging' _store_transaction false Store deleteInstance: "$c"
check 'unsupported deletion preserves original object' open "$(field "$c" .state)"
_store_tx_before_commit() { kill -TERM "$BASHPID"; }
reject 'termination before commit discards staged writes' complete "$c" done
unset -f _store_tx_before_commit
check 'terminated operation releases no result' '' "$(cat "$tmp/rejected.out")"
unpublished "$c" termination

# A concurrent record update is preserved; retry recomputes from fresh state.
_store_tx_before_commit() { @ "$c" progress: 'Concurrent evidence' >/dev/null; }
reject 'concurrent Assignment update rejects commit' complete "$c" done
unset -f _store_tx_before_commit
unpublished "$c" 'record conflict'
check 'conflicting writer keeps its progress' 'Concurrent evidence' "$(field "$c" '.events[0].body')"
must complete "$c" done >/dev/null
check 'explicit retry preserves progress and completion' 2 "$(field "$c" '.events | length')"

# New rows invalidate negative queries: no previously read row needs to change.
d=$(must new_work 'Reject a new active run')
_store_tx_before_commit() {
    _db_sql "INSERT INTO instances(id,data) VALUES('agentrun_phantom',json_object('class','AgentRun','session','$session','state','running'));"
}
reject 'new active run after staging rejects commit' complete "$d" done
unset -f _store_tx_before_commit
unpublished "$d" 'query conflict'
check 'concurrent inserted run survives rejection' running "$(field agentrun_phantom .state)"
reject 'fresh attempt sees active run and rejects in DSL' complete "$d" done
db_delete agentrun_phantom

q=$(must @ "$d" ask: 'Which branch?')
reject 'unanswered question blocks completion in DSL' complete "$d" done
must @ "$q" reply: main >/dev/null
_store_tx_before_commit() { _db_sql "UPDATE agent_questions SET answer_id='' WHERE message_id='$q';"; }
reject 'queue-only concurrent change rejects commit' complete "$d" done
unset -f _store_tx_before_commit
check 'question remains unanswered after conflict' '' "$(@ "$q" answerId)"
must @ "$q" reply: main >/dev/null
must complete "$d" done >/dev/null

# Both independent Bash processes must stage before either may commit.
e=$(must new_work 'Race the same result')
_store_tx_before_commit() {
    touch "$tmp/ready-$BASHPID"
    local attempt
    for ((attempt=0; attempt<1000; attempt++)); do
        [[ ! -f "$tmp/release" ]] || return 0
        sleep 0.01
    done
    return 1
}
(complete "$e" done >"$tmp/race1.out" 2>"$tmp/race1.err"; echo "$?" >"$tmp/race1.status") & p1=$!
(complete "$e" done >"$tmp/race2.out" 2>"$tmp/race2.err"; echo "$?" >"$tmp/race2.status") & p2=$!
for ((attempt=0; attempt<1000; attempt++)); do
    ready=("$tmp"/ready-*)
    ((${#ready[@]} == 2)) && break
    sleep 0.01
done
touch "$tmp/release"
must wait "$p1"; must wait "$p2"
unset -f _store_tx_before_commit
check 'both competing completions reached commit barrier' 2 "${#ready[@]}"
check 'exactly one concurrent transaction commits' 1 "$(awk '$0==0 {n++} END {print n+0}' "$tmp/race1.status" "$tmp/race2.status")"
check 'loser releases no premature result' "$e" "$(cat "$tmp/race1.out" "$tmp/race2.out")"
check 'race creates exactly one completion event' 1 "$(field "$e" '.events | length')"
check 'explicit replay after conflict succeeds' "$e" "$(must complete "$e" done)"

# Same completion DSL also enforces live agent token/capability/delivery fences.
f=$(must new_work 'Worker completion')
fd=$(field "$f" .delivery)
mapfile -t pair < <(@ AgentRun startFor: "$session" profile: shell)
run=${pair[0]}; token=${pair[1]}
must @ "$run" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
reject 'worker cannot complete unclaimed work' complete "$f" done
check 'worker claims selected delivery' true "$(@ AgentDelivery claim: "$fd" run: "$run")"
export TRASHTALK_RUN_TOKEN=invalid
reject 'invalid token cannot become human authority' complete "$f" done
export TRASHTALK_RUN_TOKEN="$token"
_store_tx_before_commit() { @ Store patch: "$role" with: '{"capabilities":[]}' >/dev/null; }
reject 'revocation during completion rejects commit' complete "$f" done
unset -f _store_tx_before_commit
check 'revocation leaves work open' open "$(field "$f" .state)"
check 'revocation leaves exact delivery offered' offered "$(field "$fd" .state)"
reject 'fresh revoked worker rejected in DSL' complete "$f" done
@ Store patch: "$role" with: '{"capabilities":["inbox.read","message.send","assignment.work"]}' >/dev/null
must complete "$f" done >/dev/null
check 'worker result keeps session attribution' "session:$session" "$(field "$(field "$f" .resultMessage)" .from)"
check 'worker result records exact run' "$run" "$(field "$f" '.events[-1].run')"
check 'worker completion is idempotent while still authorized' "$f" "$(must complete "$f" done)"
must @ "$run" finishWith: succeeded outcome: '{}' error: '' >/dev/null
reject 'finished token cannot replay completion' complete "$f" done
unset TRASHTALK_RUN_TOKEN

# Supported operations are shared with ordinary Persistable objects.
object=$(must @ TransactionFaults new)
check 'generic transaction reads its own persisted write' 3 "$(must @ Store transaction: "$object" sending: 'incrementWithin:' with: 3)"
check 'structured query preserves numeric values' "$object" "$(must @ Store transaction: TransactionFaults sending: 'matchingWithin:' with: '{"value":3}' | jq -r '.[0]')"
check 'structured query distinguishes text from numbers' '[]' "$(must @ Store transaction: TransactionFaults sending: 'matchingWithin:' with: '{"value":"3"}')"
check 'generic committed state is durable' 3 "$(field "$object" .value)"
reject 'failure after save discards ordinary object writes' @ Store transaction: TransactionFaults sending: 'failAfterSave:' with: "$object"
check 'ordinary object rollback preserves previous value' 3 "$(field "$object" .value)"
reject 'raw SQL is unsupported inside the transaction' @ Store transaction: Store sending: 'executeBatch:' with: 'DELETE FROM instances;'
check 'raw SQL rejection preserves existing work' completed "$(field "$a" .state)"
reject 'raw query cannot silently read an incomplete private store' @ Store transaction: Store sending: 'query:' with: '1=1'
reject 'constructor cannot replace an existing id' _store_transaction false Runtime create: TransactionFaults id: "$object"
check 'rejected constructor preserves object' 3 "$(field "$object" .value)"
args=$(jq -cn --arg object "$object" --arg path "$tmp/hints" '{object:$object,path:$path}')
_store_tx_before_commit() { return 1; }
reject 'aborted transaction discards notification hint' @ Store transaction: TransactionFaults sending: 'saveAndNotify:' with: "$args"
unset -f _store_tx_before_commit
check 'no hint escaped aborted transaction' false "$([[ -e "$tmp/hints" ]] && echo true || echo false)"
check 'committed callback transaction returns value' 4 "$(must @ Store transaction: TransactionFaults sending: 'saveAndNotify:' with: "$args")"
check 'post-commit hint sees committed state exactly once' 4 "$(cat "$tmp/hints")"

source "$fixture/concurrency.bash"

check 'transaction directories cleaned' 0 "$(find "$TMPDIR" -maxdepth 1 -type d -name 'trash-store-tx.*' | wc -l | tr -d ' ')"
echo "Assignment transactions: $passed checks passed"
