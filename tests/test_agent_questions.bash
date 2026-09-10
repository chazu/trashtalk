#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=question-owner TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
state() { db_get "$1" | jq -r .state; }
start_run() {
    local pair
    mapfile -t pair < <(@ AgentRun startFor: "$session" profile: shell)
    run=${pair[0]}
    export TRASHTALK_RUN_TOKEN=${pair[1]}
    @ "$run" transitionTo: running >/dev/null
}
new_delivery() {
    local m
    m=$(@ Inbox send: "$1" to: "session:$session" from: question-owner)
    @ AgentDelivery forSession: "$session" messages: "$(jq -cn --arg m "$m" '[$m]')"
}
session=$(@ Gusgus sessionFor: "$root")
d1=$(new_delivery first)
start_run
@ AgentDelivery claim: "$d1" run: "$run" >/dev/null
q1=$(@ AgentRun askUser: 'Which branch?')
@ "$run" finishWith: waiting_for_user outcome: '{}' error: '' >/dev/null
d2=$(new_delivery second)
start_run
@ AgentDelivery claim: "$d2" run: "$run" >/dev/null
q2=$(@ AgentRun askUser: 'Which test?')
@ "$run" finishWith: waiting_for_user outcome: '{}' error: '' >/dev/null
unrelated=$(@ Inbox send: 'another task' to: "session:$session" from: question-owner)
check 'unrelated incoming message leaves first question blocked' blocked "$(state "$d1")"
check 'unrelated incoming message leaves second question blocked' blocked "$(state "$d2")"
@ "$q1" markRead >/dev/null
@ "$q2" archive >/dev/null
check 'reading a question does not resume work' blocked "$(state "$d1")"
check 'archiving a question does not resume work' blocked "$(state "$d2")"
a1=$(@ "$q1" reply: 'main')
check 'reply resumes only its original delivery' pending "$(state "$d1")"
check 'reply does not resume another question' blocked "$(state "$d2")"
a2=$(@ "$q2" reply: 'integration')
check 'archived question can still be answered explicitly' pending "$(state "$d2")"
check 'question retains its delivery links' "[\"$d1\"]" "$(@ "$q1" blockingDeliveryIds)"
check 'question records its first answer independently of unread state' "$a1" "$(@ "$q1" answerId)"

# Two independent questions in one run, plus a second prerequisite for d3.
d3=$(new_delivery third); d4=$(new_delivery fourth)
start_run
@ AgentDelivery claim: "$d3" run: "$run" >/dev/null
@ AgentDelivery claim: "$d4" run: "$run" >/dev/null
q3=$(@ AgentRun askUser: 'Branch for third?' forDelivery: "$d3")
check 'targeted question leaves other held work offered' offered "$(state "$d4")"
q4=$(@ AgentRun askUser: 'Branch for fourth?' forDelivery: "$d4")
q5=$(@ AgentRun askUser: 'Also choose the third test' forDelivery: "$d3")
check 'targeted question publishes only its delivery' "[\"$d3\"]" "$(@ "$q3" blockingDeliveryIds)"
@ "$run" finishWith: waiting_for_user outcome: '{}' error: '' >/dev/null
wrong=$(@ Message to: "session:$session" from: someone-else subject: wrong body: wrong kind: note)
@ "$wrong" replyTo: "$q3"
inbox=$(@ "$session" inboxObject)
@ "$inbox" deliver: "$wrong" >/dev/null
check 'a reply from a different recipient cannot answer the question' '' "$(@ "$q3" answerId)"
same_thread=$(@ Message to: "session:$session" from: question-owner subject: topic body: topic kind: note)
@ "$same_thread" thread: "$(@ "$q3" thread)"
@ "$inbox" deliver: "$same_thread" >/dev/null
check 'thread membership alone does not answer the question' blocked "$(state "$d3")"
a3=$(@ "$q3" reply: main)
check 'delivery waits for its other unanswered question' blocked "$(state "$d3")"
check 'independent question remains blocked' blocked "$(state "$d4")"
@ "$q5" reply: unit >/dev/null
check 'answering every prerequisite resumes the delivery' pending "$(state "$d3")"

# Crash between reply persistence and routing, followed by a failed atomic route.
a4=$(@ Message to: "session:$session" from: question-owner subject: answer body: main kind: note)
@ "$a4" replyTo: "$q4"
@ "$inbox" prepare: "$a4"
@ AgentQueue persist: "$a4"
check 'persisting an unrouted reply leaves delivery blocked' blocked "$(state "$d4")"
_db_sql "CREATE TRIGGER reject_unblock BEFORE UPDATE ON instances WHEN OLD.id='$d4' AND json_extract(NEW.data,'$.state')='pending' BEGIN SELECT RAISE(ABORT,'fixture failure'); END;"
@ AgentWorker routePending >/dev/null 2>&1; rc=$?
check 'failed routing reports failure' 1 "$rc"
check 'failed routing does not record an answer' '' "$(@ "$q4" answerId)"
check 'failed routing preserves pending outbox obligation' '' "$(_db_sql "SELECT session FROM agent_outbox WHERE message_id='$a4';")"
_db_sql 'DROP TRIGGER reject_unblock;'
@ AgentWorker routePending >/dev/null
check 'routing replay recovers the matching answer' "$a4" "$(@ "$q4" answerId)"
check 'routing replay resumes linked delivery' pending "$(state "$d4")"

# Old answers, including duplicate outbox routing, cannot release new blockers.
start_run
@ AgentDelivery claim: "$d3" run: "$run" >/dev/null
q6=$(@ AgentRun askUser: 'A new question after resume' forDelivery: "$d3")
@ "$q3" reply: duplicate >/dev/null
_db_sql "UPDATE agent_outbox SET session='' WHERE message_id='$a3';"
@ AgentWorker routePending >/dev/null
check 'old answer replay does not unblock a later run' blocked "$(state "$d3")"
check 'old question retains its original answer' "$a3" "$(@ "$q3" answerId)"
@ "$q3" archive >/dev/null
check 'archiving a stale message cannot overwrite recorded answer' "$a3" "$(@ "$q3" answerId)"

# Question publication and blocking also roll back as one transaction.
before=$(_db_sql "SELECT count(*) FROM agent_questions;")
bad=$(@ AgentRun askUser: 'wrong delivery' forDelivery: "$d4" 2>/dev/null); rc=$?
check 'cannot block a delivery not held by the current run' 1 "$rc"
check 'rejected question returns no message id' '' "$bad"
check 'rejected question does not publish links' "$before" "$(_db_sql 'SELECT count(*) FROM agent_questions;')"
d5=$(new_delivery fifth)
@ AgentDelivery claim: "$d5" run: "$run" >/dev/null
before=$(_db_sql "SELECT count(*) FROM instances WHERE class='Message' AND json_extract(data,'$.kind')='question';")
_db_sql "CREATE TRIGGER reject_question BEFORE INSERT ON agent_questions BEGIN SELECT RAISE(ABORT,'fixture failure'); END;"
bad=$(@ AgentRun askUser: 'atomic question' forDelivery: "$d5" 2>/dev/null); rc=$?
check 'question transaction failure is propagated' 1 "$rc"
check 'failed question publishes no id' '' "$bad"
check 'failed question leaves delivery offered' offered "$(state "$d5")"
check 'failed question never appears in the inbox' "$before" "$(_db_sql "SELECT count(*) FROM instances WHERE class='Message' AND json_extract(data,'$.kind')='question';")"
_db_sql 'DROP TRIGGER reject_question;'
echo "=== $passed question checks passed ==="
