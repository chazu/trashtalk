#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=assignment-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
new_session() { @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell; }
new_assignment() { local a; a=$(@ Assignment draft: "$1" in: "$root") || return 1; @ "$a" assignTo: "$identity" >/dev/null || return 1; @ "$a" workIn: "$session" >/dev/null || return 1; echo "$a"; }
start_run() {
    local pair
    mapfile -t pair < <(@ AgentRun startFor: "$1" profile: shell)
    run=${pair[0]}; token=${pair[1]}
    must @ "$run" transitionTo: running >/dev/null
}

identity=$(must @ AgentIdentity named: assignment-specialist)
@ "$identity" owner: assignment-owner
@ "$identity" save
arch=$(must @ AgentArchetype define: assignment-specialist revision: 1 instructions: 'Use Assignment and read inbox messages.' profile: shell)
role=$(must @ AgentRole define: assignment-specialist revision: 1 capabilities: '["inbox.read","message.send","assignment.work"]' workspacePolicy: '[]' runBudget: '{}')
session=$(must new_session)
foreign=$(must @ AgentIdentity named: foreign)
foreign_session=$(must @ AgentSession openFor: "$foreign" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)

reject 'empty objective rejected' @ Assignment draft: ' ' in: "$root"
a=$(must @ Assignment draft: 'Explain the failing integration test' in: "$root")
b=$(must @ Assignment draft: 'Explain the failing integration test' in: "$root")
check 'another draft is deliberately distinct' false "$([[ "$a" == "$b" ]] && echo true || echo false)"
check 'draft belongs to the local human' assignment-owner "$(field "$a" .requester)"
check 'unassigned is derived activity' unassigned "$(@ "$a" snapshot | jq -r .activity)"
must @ "$a" criteria: 'Identify the cause and record evidence.' >/dev/null
must @ "$a" issueReference: 'TKT-123' >/dev/null
reject 'assignee must be an identity instance' @ "$a" assignTo: assignment-specialist
reject 'session is not an assignee' @ "$a" assignTo: "$session"
must @ "$a" assignTo: "$identity" >/dev/null
must @ "$a" assignTo: "$identity" >/dev/null
reject 'automatic identity reassignment is excluded' @ "$a" assignTo: "$foreign"
reject 'foreign identity session rejected' @ "$a" workIn: "$foreign_session"
wrong_ws=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$tmp" profile: shell)
reject 'workspace mismatch rejected' @ "$a" workIn: "$wrong_ws"

# Work publication must be all-or-nothing even at its final outbox write.
_db_sql "CREATE TRIGGER reject_assignment_work BEFORE INSERT ON agent_outbox WHEN NEW.message_id='message_${a}_work_1' BEGIN SELECT RAISE(ABORT,'fixture publication failure'); END;"
reject 'failed work outbox rolls back selection' @ "$a" workIn: "$session"
check 'failed selection leaves no generation' 0 "$(field "$a" .generation)"
check 'failed selection leaves no Message draft' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE id='message_${a}_work_1';")"
_db_sql 'DROP TRIGGER reject_assignment_work;'
must @ "$a" workIn: "$session" >/dev/null
delivery=$(field "$a" .delivery)
must @ "$a" workIn: "$session" >/dev/null
check 'repeated workIn publishes once' 1 "$(field "$a" '.history | length')"
work_body=$(field "message_${a}_work_1" .body)
check 'work message keeps actual line breaks' true "$([[ "$work_body" == *$'\nCompletion criteria: '* ]] && echo true || echo false)"
check 'work is held for manual use' manual "$(field "$delivery" .dispatchMode)"
check 'worker pending list excludes held work' '' "$(@ AgentDelivery pendingFor: "$session")"
must @ AgentWorker tickSession: "$session" >/dev/null
check 'worker tick launches no assignment run' '' "$(@ "$session" activeRun)"
check 'held work is visible through inbox messages' "message_${a}_work_1" "$(@ "$session" inboxObject | while read -r inbox; do @ "$inbox" unread; done)"
reject 'criteria cannot silently change published work' @ "$a" criteria: changed
reject 'human currentAssignment requires explicit selection' @ Trash currentAssignment
export TRASHTALK_ASSIGNMENT_ID="$a"
check 'human resolves explicit current assignment' "$a" "$(@ Trash currentAssignment)"
unset TRASHTALK_ASSIGNMENT_ID
must @ "$a" progress: 'Reproduced the failure; fixture expires at midnight.' >/dev/null
q=$(must @ "$a" ask: 'Include the integration suite?')
check 'human question is attributed to the operator' assignment-owner "$(field "$q" .from)"
check 'question blocks only assignment work' blocked "$(field "$delivery" .state)"
must @ "$q" markRead >/dev/null
must @ "$q" archive >/dev/null
check 'read/archive does not answer question' '' "$(@ "$q" answerId)"
reject 'unanswered question prevents completion' @ "$a" complete: premature
must @ "$session" close >/dev/null
check 'closing session leaves assignment open' open "$(field "$a" .state)"
second=$(must new_session)
must @ "$a" workIn: "$second" >/dev/null
check 'replacement retains both participations' 2 "$(field "$a" '.history | length')"
check 'replacement retains progress and question' 2 "$(field "$a" '.events | length')"
check 'old work is explicitly superseded' skipped "$(field "$delivery" .state)"
answer=$(must @ "$q" reply: 'Yes, include it.')
check 'ordinary reply answers the retained question' "$answer" "$(@ "$q" answerId)"
check 'question history exposes answer handle' "$answer" "$(@ "$a" snapshot | jq -r '.questions[0].answer')"
must @ "$a" complete: 'The fixture has an expired credential; reproduced in integration.' >/dev/null
outcome=$(field "$a" .resultMessage)
check 'completion records outcome lifecycle' completed "$(field "$a" .state)"
check 'completion settles selected delivery' processed "$(field "$(field "$a" .delivery)" .state)"
check 'completion preserves opaque issue reference' TKT-123 "$(field "$a" .externalIssue)"
must @ "$a" complete: 'The fixture has an expired credential; reproduced in integration.' >/dev/null
check 'completion replay retains outcome message' "$outcome" "$(field "$a" .resultMessage)"
reject 'conflicting completion rejected' @ "$a" complete: 'Different outcome'
reject 'completed assignment cannot be cancelled' @ "$a" cancel: changed
shown=$(must @ "$a" show)
check 'DSL presentation includes completed lifecycle' true "$([[ "$shown" == *'State: completed (completed)'* ]] && echo true || echo false)"
check 'DSL presentation includes participation and outcome' true "$([[ "$shown" == *'Participation 1:'* && "$shown" == *"Outcome message: $outcome"* ]] && echo true || echo false)"
check 'fresh shell sees durable completed outcome' completed "$(TRASH_SESSION_ID=assignment-fresh bash -c 'source "$TRASHTALK_DIR/lib/trash.bash"; @ "$1" snapshot' _ "$a" | jq -r .state)"

# Controlled run fixture: no live model. A run must hold this exact delivery.
session="$second"
c=$(must new_assignment 'Inspect a controlled run')
cdelivery=$(field "$c" .delivery)
start_run "$session"
first_run="$run"; first_token="$token"
export TRASHTALK_RUN_TOKEN="$token"
reject 'unclaimed assignment is not current' @ Trash currentAssignment
reject 'unclaimed work cannot be completed' @ "$c" complete: unclaimed
check 'explicit fixture claim succeeds' true "$(@ AgentDelivery claim: "$cdelivery" run: "$run")"
check 'run resolves exact held assignment' "$c" "$(@ Trash currentAssignment)"
check 'claim records participating run' "$run" "$(field "$c" '.history[0].runs[0]')"
must @ "$c" progress: 'Controlled run observed the failure.' >/dev/null
export TRASHTALK_RUN_TOKEN=invalid
reject 'invalid token cannot become human authority' @ "$c" complete: forged
export TRASHTALK_RUN_TOKEN=''
reject 'present empty token cannot become human authority' @ "$c" complete: forged
export TRASHTALK_RUN_TOKEN="$first_token"
reject 'run cannot create delegated drafts in manual slice' @ Assignment draft: child in: "$root"
q2=$(must @ "$c" ask: 'Which branch?')
check 'agent question has session attribution' "session:$session" "$(field "$q2" .from)"
unset TRASHTALK_RUN_TOKEN
reject 'active execution prevents opening competing session' new_session
reject 'operator cannot complete over active execution' @ "$c" complete: premature
must @ "$first_run" finishWith: waiting_for_user outcome: '{}' error: '' >/dev/null
wrong=$(must @ Message to: "session:$session" from: someone-else subject: answer body: wrong kind: note)
@ "$wrong" replyTo: "$q2"
@ "$(@ "$session" inboxObject)" deliver: "$wrong" >/dev/null
check 'wrong sender cannot answer assignment question' '' "$(@ "$q2" answerId)"
answer2=$(must @ "$q2" reply: main)
check 'agent question is answered by ordinary reply' "$answer2" "$(@ "$q2" answerId)"
check 'manual answer has no pending routing obligation' "$session" "$(_db_sql "SELECT session FROM agent_outbox WHERE message_id='$answer2';")"
followup=$(must @ "$answer2" reply: 'Acknowledged; continuing the manual walkthrough.')
followup_reply=$(must @ "$followup" reply: 'One more detail for this exchange.')
check 'manual boundary survives subsequent replies' manual "$(field "$followup_reply" .dispatchMode)"
check 'subsequent manual reply stays out of automatic routing' 0 "$(@ AgentQueue pending | awk -v id="$followup_reply" '$0==id {n++} END {print n+0}')"
must @ "$session" close >/dev/null
third=$(must new_session)
must @ "$c" workIn: "$third" >/dev/null
export TRASHTALK_RUN_TOKEN="$first_token"
reject 'finished predecessor token rejected' @ "$c" complete: stale
# Even a falsely revived predecessor remains fenced by session and generation.
reject 'superseded run cannot be resurrected' @ Store patch: "$first_run" with: '{"state":"running"}'
reject 'superseded running predecessor cannot complete' @ "$c" complete: stale
@ Store patch: "$first_run" with: '{"state":"waiting_for_user"}' >/dev/null
unset TRASHTALK_RUN_TOKEN
start_run "$third"
export TRASHTALK_RUN_TOKEN="$token"
new_delivery=$(field "$c" .delivery)
check 'replacement run claims only new delivery' true "$(@ AgentDelivery claim: "$new_delivery" run: "$run")"
check 'superseded delivery cannot be reclaimed' false "$(@ AgentDelivery claim: "$cdelivery" run: "$run")"
unrelated_msg=$(must @ Inbox send: unrelated to: "session:$third" from: assignment-owner)
unrelated=$(must @ AgentDelivery forSession: "$third" messages: "[\"$unrelated_msg\"]")
must @ AgentDelivery claim: "$unrelated" run: "$run" >/dev/null

# Failure at outcome insertion must not leave completion or settlement behind.
_db_sql "CREATE TRIGGER reject_assignment_outcome BEFORE INSERT ON instances WHEN NEW.id='message_${c}_outcome' BEGIN SELECT RAISE(ABORT,'fixture outcome failure'); END;"
reject 'failed outcome publication rolls back completion' @ "$c" complete: 'Evidence recorded.'
check 'failed completion leaves work open' open "$(field "$c" .state)"
check 'failed completion leaves delivery offered' offered "$(field "$new_delivery" .state)"
check 'failed completion leaves no outcome reference' '' "$(field "$c" .resultMessage)"
_db_sql 'DROP TRIGGER reject_assignment_outcome;'
must @ "$c" complete: 'Evidence recorded.' >/dev/null
must @ "$c" complete: 'Evidence recorded.' >/dev/null
check 'result attributed to selected worker session' "session:$third" "$(field "$(field "$c" .resultMessage)" .from)"
check 'completion does not settle unrelated delivery' offered "$(field "$unrelated" .state)"
must @ "$run" finishWith: succeeded outcome: '{}' error: '' >/dev/null
unset TRASHTALK_RUN_TOKEN

# Requester session and original conversation survive outcome routing.
second="$third"
origin_identity=$(must @ AgentIdentity named: conversation-requester)
origin_session=$(must @ AgentSession openFor: "$origin_identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
origin=$(must @ Inbox send: 'Investigate for me' to: assignment-owner from: "session:$origin_session")
r=$(must @ Assignment draft: 'Keep the conversation link' in: "$root")
must @ "$r" origin: "$origin" >/dev/null
must @ "$r" assignTo: "$identity" >/dev/null
must @ "$r" workIn: "$second" >/dev/null
check 'agent requester records identity' "$origin_identity" "$(field "$r" .requester)"
check 'requester session is separate context' "$origin_session" "$(field "$r" .requesterSession)"
must @ "$r" complete: 'Reported, without accepting or merging work.' >/dev/null
rmid=$(field "$r" .resultMessage)
check 'result goes to original requester session' "session:$origin_session" "$(field "$rmid" .to)"
check 'result preserves original thread' "$(field "$origin" .thread)" "$(field "$rmid" .thread)"
check 'result references original request' "$origin" "$(field "$rmid" .replyTo)"

# Independent processes race on selection and completion, using one store.
race=$(must @ Assignment draft: 'Concurrent replay' in: "$root")
must @ "$race" assignTo: "$identity" >/dev/null
(@ "$race" workIn: "$second" >"$tmp/select1") & p1=$!
(@ "$race" workIn: "$second" >"$tmp/select2") & p2=$!
must wait "$p1"; must wait "$p2"
check 'concurrent selection has one participation' 1 "$(field "$race" '.history | length')"
(@ "$race" complete: done >"$tmp/complete1") & p1=$!
(@ "$race" complete: done >"$tmp/complete2") & p2=$!
must wait "$p1"; must wait "$p2"
check 'concurrent completion has one event' 1 "$(field "$race" '.events | length')"

cancelled=$(must @ Assignment draft: 'No longer needed' in: "$root")
must @ "$cancelled" cancel: 'Request withdrawn.' >/dev/null
must @ "$cancelled" cancel: 'Request withdrawn.' >/dev/null
check 'cancellation is distinct lifecycle' cancelled "$(field "$cancelled" .state)"
reject 'cancelled work cannot complete' @ "$cancelled" complete: done

# Role/owner gates and ambiguous run context. Both assignments are deliberately
# selected before starting a run; currentAssignment must never choose the first.
session="$second"
x=$(must new_assignment 'First explicitly selected work')
y=$(must new_assignment 'Second explicitly selected work')
xd=$(field "$x" .delivery); yd=$(field "$y" .delivery)
export TRASHTALK_USER=another-operator
reject 'another human cannot mutate the assignment' @ "$x" progress: forged
export TRASHTALK_USER=assignment-owner
start_run "$session"
export TRASHTALK_RUN_TOKEN="$token"
export TRASHTALK_RUN_TOKEN=invalid
reject 'invalid token cannot claim assignment work' @ AgentDelivery claim: "$xd" run: "$run"
export TRASHTALK_RUN_TOKEN="$token"
@ Store patch: "$role" with: '{"capabilities":[]}' >/dev/null
check 'role without assignment capability cannot claim' false "$(@ AgentDelivery claim: "$xd" run: "$run")"
@ Store patch: "$role" with: '{"capabilities":["inbox.read","message.send","assignment.work"]}' >/dev/null
check 'first assignment claimed' true "$(@ AgentDelivery claim: "$xd" run: "$run")"
check 'second assignment claimed' true "$(@ AgentDelivery claim: "$yd" run: "$run")"
reject 'multiple held assignments require explicit selection' @ Trash currentAssignment
export TRASHTALK_ASSIGNMENT_ID="$y"
check 'explicit context resolves ambiguity' "$y" "$(@ Trash currentAssignment)"
unset TRASHTALK_ASSIGNMENT_ID
@ Store patch: "$role" with: '{"capabilities":[]}' >/dev/null
reject 'revoked assignment capability prevents progress' @ "$x" progress: forged
@ Store patch: "$role" with: '{"capabilities":["inbox.read","message.send","assignment.work"]}' >/dev/null

before_messages=$(_db_sql "SELECT count(*) FROM instances WHERE class='Message';")
_db_sql "CREATE TRIGGER reject_assignment_question BEFORE INSERT ON agent_questions BEGIN SELECT RAISE(ABORT,'fixture question failure'); END;"
reject 'question-link failure rolls back publication' @ "$x" ask: 'This must roll back'
check 'failed question leaves no Message draft' "$before_messages" "$(_db_sql "SELECT count(*) FROM instances WHERE class='Message';")"
check 'failed question leaves no history event' 0 "$(field "$x" '.events | length')"
check 'failed question does not block delivery' offered "$(field "$xd" .state)"
_db_sql 'DROP TRIGGER reject_assignment_question;'
qx=$(must @ "$x" ask: 'Confirm the branch for the retry')
check 'question leaves another assignment offered' offered "$(field "$yd" .state)"
must @ "$y" complete: 'Second work reported.' >/dev/null
old_run="$run"; old_token="$token"
must @ "$run" finishWith: waiting_for_user outcome: '{}' error: '' >/dev/null
unset TRASHTALK_RUN_TOKEN
must @ "$qx" reply: main >/dev/null
start_run "$session"
export TRASHTALK_RUN_TOKEN="$token"
check 'new run resumes answered work in same session' true "$(@ AgentDelivery claim: "$xd" run: "$run")"
check 'same-session retry keeps both run references' 2 "$(field "$x" '.history[0].runs | length')"
reject 'predecessor cannot become active beside replacement' @ Store patch: "$old_run" with: '{"state":"running"}'
export TRASHTALK_RUN_TOKEN="$old_token"
reject 'old run cannot complete after same-session handoff' @ "$x" complete: stale
reject 'agent cannot claim on behalf of another run' @ AgentDelivery claim: "$xd" run: "$run"
@ Store patch: "$old_run" with: '{"state":"waiting_for_user"}' >/dev/null
export TRASHTALK_RUN_TOKEN="$token"
must @ "$x" complete: 'Retry evidence recorded.' >/dev/null
must @ "$run" finishWith: succeeded outcome: '{}' error: '' >/dev/null
unset TRASHTALK_RUN_TOKEN

# A stopped/uncertain effect requires explicit review before continuation.
uncertain=$(must new_assignment 'Review an interrupted effect')
ud=$(field "$uncertain" .delivery)
start_run "$session"
check 'operator fixture claims work' true "$(@ AgentDelivery claim: "$ud" run: "$run")"
must @ "$ud" transitionTo: uncertain >/dev/null
must @ "$run" finishWith: unsettled outcome: '{}' error: 'Needs review' >/dev/null
check 'uncertainty is derived review activity' 'needs review' "$(@ "$uncertain" snapshot | jq -r .activity)"
must @ "$session" close >/dev/null
third=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
second="$third"
reject 'uncertainty blocks replacement' @ "$uncertain" workIn: "$third"
reject 'uncertainty blocks inferred completion' @ "$uncertain" complete: done
must @ "$session" skip: "$ud" note: 'Reviewed effects; safe to continue manually.' >/dev/null
must @ "$uncertain" workIn: "$third" >/dev/null
check 'reviewed work can continue in another session' "$third" "$(field "$uncertain" .currentSession)"

# The final routing obligation is part of completion, including agent requesters.
rr=$(must @ Assignment draft: 'Atomic requester notification' in: "$root")
must @ "$rr" origin: "$origin" >/dev/null
must @ "$rr" assignTo: "$identity" >/dev/null
must @ "$rr" workIn: "$second" >/dev/null
_db_sql "CREATE TRIGGER reject_assignment_result_outbox BEFORE INSERT ON agent_outbox WHEN NEW.message_id='message_${rr}_outcome' BEGIN SELECT RAISE(ABORT,'fixture result outbox failure'); END;"
reject 'result outbox failure rolls back all completion records' @ "$rr" complete: 'Atomic result'
check 'result outbox failure retains open assignment' open "$(field "$rr" .state)"
check 'result outbox failure retains pending delivery' pending "$(field "$(field "$rr" .delivery)" .state)"
check 'result outbox failure removes staged message' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE id='message_${rr}_outcome';")"
_db_sql 'DROP TRIGGER reject_assignment_result_outbox;'
must @ "$rr" complete: 'Atomic result' >/dev/null

# An identity-addressed requester must not accidentally activate a harness when
# it happens to have exactly one eligible session.
requester=$(must @ AgentIdentity named: assignment-requester)
requester_session=$(must @ AgentSession openFor: "$requester" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
requester_inbox=$(@ "$requester" inbox)
identity_origin=$(must @ Inbox send: 'A request from an identity inbox' to: assignment-owner from: "$requester_inbox")
ir=$(must @ Assignment draft: 'Reply to the requester identity' in: "$root")
must @ "$ir" origin: "$identity_origin" >/dev/null
check 'identity-addressed requester resolves to identity object' "$requester" "$(field "$ir" .requester)"
must @ "$ir" assignTo: "$identity" >/dev/null
must @ "$ir" workIn: "$second" >/dev/null
must @ "$ir" complete: 'Ready for manual review.' >/dev/null
im=$(field "$ir" .resultMessage)
check 'identity result uses the originating inbox' "$requester_inbox" "$(field "$im" .to)"
check 'identity result is excluded from automatic routing' 0 "$(@ AgentQueue pending | awk -v id="$im" '$0==id {n++} END {print n+0}')"
must @ AgentWorker deliverMessage: "$im" toInbox: "$requester_inbox" >/dev/null
must @ AgentWorker tickSession: "$requester_session" >/dev/null
check 'direct routing cannot create automatic work from held result' '' "$(@ "$requester_session" pendingDeliveries)"
check 'identity requester has no automatic run' '' "$(@ "$requester_session" activeRun)"
echo "Assignment: $passed checks passed"
