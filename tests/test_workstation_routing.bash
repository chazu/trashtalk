#!/usr/bin/env bash
# Phase 2 guarded routing: dry-run admission (2A), explicit one-attention
# delegation (2B), opt-in automatic routing with loop controls (2C), and
# attention-to-conversation operations (2D). No model; ShellDriver plays the agent.
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
trap 'echo "FAIL: exit at line $LINENO"' ERR
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_RUN_DIR="$TMPDIR/runs"
unset TRASHTALK_RUN_TOKEN
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
honker_available || { echo 'SKIP: Honker not installed'; exit 0; }
honker_bootstrap
passed=0
check() { if [[ "$2" == "$3" ]]; then passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$TMPDIR/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else passed=$((passed+1)); fi; }
count() { @ Store countByClass: "$1"; }
data() { @ Store getInstance: "$1"; }
outbox() { _db_sql 'SELECT count(*) FROM agent_outbox;'; }
field() { data "$1" | jq -r "$2"; }
status() { @ "$1" routingStatus | jq -r "$2"; }
mkdir -p "$TMPDIR/ws" "$TMPDIR/runs"
ws=$(cd "$TMPDIR/ws" && pwd -P)
digest=$(@ WorkstationSchema digest)
sub=$(jq -c --arg d "$digest" '.adapterKind="command-receipt"|.streamName="workstation.command-receipts.v1"|.schemaDigest=$d|.filter={exitNot:0}|.debounceSeconds=3600' schemas/workstation/v1/fixtures/EventSubscription.valid.json)
subscription=$(must @ EventSubscription createFrom: "$sub")
consumer=$(@ CommandReceiptSourceAdapter consumerFor: "$sub")
fixture=$(jq -c --arg ws "$ws" '.workspace=$ws' schemas/workstation/v1/fixtures/CommandReceipt.valid.json)
publish() { @ CommandReceipt publish: "$(jq -c --arg l "$1" '.commandLabel=$l|.display.title=$l' <<<"$fixture")" >/dev/null; }
publish_origin() { @ CommandReceipt publish: "$(jq -c --arg l "$1" --arg run "$2" '.commandLabel=$l|.display.title=$l|.origin={producer:"agent-run",run:$run}' <<<"$fixture")" >/dev/null; }
tick() { @ WorkstationWorker tick >/dev/null || { echo 'FAIL: worker tick'; exit 1; }; }
attention_for() { @ Store findByClass: Attention where: "json_extract(data,'\$.groupKey')!='' AND EXISTS(SELECT 1 FROM instances m WHERE m.class='Message' AND m.id=json_extract(instances.data,'\$.message') AND json_extract(m.data,'\$.subject')='$1')" orderBy: 'created_at DESC' limit: 1; }
inbox=$(@ Inbox named: local-user)

# ---- 2A: target configuration and dry-run admission -------------------------
publish 'unit tests'; tick
a=$(attention_for 'unit tests'); root=$(field "$a" .message)
check 'attention records the canonical receipt workspace' "$ws" "$(field "$a" .workspace)"
check 'human receipts have lineage depth zero' 0 "$(field "$a" .lineageDepth)"
check 'no target is a structured reason' no-target "$(status "$root" .reason)"
check 'next action asks for configuration' configure-target "$(status "$root" .nextAction)"
reject 'automatic delegation needs a target' @ "$subscription" enableAutomaticDelegation: 'I accept' reason: early
identity=$(must @ AgentIdentity named: routing-specialist)
@ "$identity" owner: local-user; @ "$identity" save
foreign=$(must @ AgentIdentity named: foreign-specialist)
@ "$foreign" owner: someone-else; @ "$foreign" save
reject 'a foreign identity cannot be a target' @ "$subscription" target: "$foreign" reason: nope
reject 'a session is not an identity' @ "$subscription" target: "$root" reason: nope
must @ "$subscription" target: "$identity" reason: 'route failures' >/dev/null
check 'target revision is audited' 2 "$(field "$subscription" .revision)"
check 'default delegation mode is manual' manual "$(@ "$subscription" delegationMode)"
check 'target without a session' no-session "$(status "$root" .reason)"
arch=$(must @ Gusgus archetype)
role=$(must @ AgentRole define: routing-specialist revision: 1 capabilities: '["inbox.read","message.send"]' workspacePolicy: '[]' runBudget: '{}')
session=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$ws" profile: shell)
check 'eligible with the current workspace session' eligible "$(status "$root" .status)"
check 'dry run resolves the exact session' "$session" "$(status "$root" .session)"
check 'execution workspace is the receipt cwd' "$ws" "$(status "$root" .executionWorkspace)"
check 'next action is delegate' delegate "$(status "$root" .nextAction)"
@ "$session" pause >/dev/null
check 'paused session is a reason' session-paused "$(status "$root" .reason)"
@ "$session" resume >/dev/null
@ Store patch: "$role" with: '{"workspacePolicy":["/elsewhere"]}' >/dev/null
check 'workspace policy is enforced' workspace-unauthorized "$(status "$root" .reason)"
@ Store patch: "$role" with: '{"workspacePolicy":[]}' >/dev/null
@ Store patch: "$role" with: '{"capabilities":["message.send"]}' >/dev/null
check 'receive capability is required' capability-missing "$(status "$root" .reason)"
@ Store patch: "$role" with: '{"capabilities":["inbox.read","message.send"]}' >/dev/null
@ Store patch: "$role" with: '{"recipientPolicy":["someone-else"]}' >/dev/null
check 'recipient policy is enforced' recipient-denied "$(status "$root" .reason)"
@ Store patch: "$role" with: '{"recipientPolicy":[]}' >/dev/null
@ Store patch: "$role" with: '{"messageBudget":{"count":1}}' >/dev/null
held=$(must @ Inbox send: 'hold' to: "session:$session" from: local-user)
check 'budget counts unsettled deliveries' budget-exhausted "$(status "$root" .reason)"
@ Store patch: "$role" with: '{"messageBudget":{"count":0}}' >/dev/null
for d in $(@ Store findByClass: AgentDelivery matching: "{\"session\":\"$session\"}"); do @ Store patch: "$d" with: '{"state":"processed"}' >/dev/null; done
_db_sql "DELETE FROM agent_outbox WHERE message_id='$held';"
@ "$identity" enabled: false; @ "$identity" save
check 'disabled target is a reason' target-disabled "$(status "$root" .reason)"
@ "$identity" enabled: true; @ "$identity" save
check 'dry runs are eligible again' eligible "$(status "$root" .status)"
# Lineage: receipts produced by the target identity are never routed back to it.
mapfile -t pair < <(@ AgentRun startFor: "$session" profile: shell)
run=${pair[0]}; token=${pair[1]}
must @ "$run" transitionTo: running >/dev/null
publish_origin 'agent tests' "$run"; tick
b=$(attention_for 'agent tests')
check 'origin run gives lineage depth one' 1 "$(field "$b" .lineageDepth)"
check 'target-produced receipt is rejected' lineage-target "$(status "$(field "$b" .message)" .reason)"
outcome=$(jq -cn --arg ws "$ws" '{workspace:$ws,commandLabel:"token tests",exitCode:3,startedAt:"2026-09-14T15:00:00Z",finishedAt:"2026-09-14T15:00:01Z",summary:"Command exited with status 3"}')
TRASHTALK_RUN_TOKEN="$token" @ CommandReceipt publishOutcome: "$outcome" >/dev/null
tick
check 'run token stamps the receipt origin' "$run" "$(field "$(attention_for 'token tests')" .origin.run)"
TRASHTALK_RUN_TOKEN="$run:bogus" @ CommandReceipt publishOutcome: "$(jq -c '.commandLabel="bogus tests"' <<<"$outcome")" >/dev/null
tick
check 'an invalid token yields a human receipt' null "$(field "$(attention_for 'bogus tests')" .origin)"
@ "$run" finishWith: succeeded outcome: "{}" error: "" >/dev/null || { echo "finish failed: $(@ "$run" state)"; @ "$run" finishWith: succeeded outcome: "{}" error: ""; exit 1; }
other=$(must @ AgentIdentity named: other-specialist)
@ "$other" owner: local-user; @ "$other" save
other_session=$(must @ AgentSession openFor: "$other" archetype: "$arch" role: "$role" workspace: "$ws" profile: shell)
mapfile -t pair < <(@ AgentRun startFor: "$other_session" profile: shell)
other_run=${pair[0]}
publish_origin 'other tests' "$other_run"; tick
c=$(attention_for 'other tests'); c_root=$(field "$c" .message)
check 'another identity within the depth limit is eligible' eligible "$(status "$c_root" .status)"
must @ "$subscription" lineageLimit: 0 reason: 'strict' >/dev/null
check 'lineage depth limit is enforced' lineage-depth "$(status "$c_root" .reason)"
must @ "$subscription" lineageLimit: 1 reason: 'default' >/dev/null
must @ "$other_run" transitionTo: failed >/dev/null
check 'dry runs publish nothing' "0 0" "$(echo "$(outbox) $(count AgentDelivery | jq --argjson h 1 '. - $h')")"
check 'dry runs create no runs beyond fixtures' 2 "$(count AgentRun)"

# ---- 2B: explicit one-attention delegation ----------------------------------
msg=$(must @ "$root" delegateAttention)
check 'delegation message id is deterministic' "message_${a}_delegation_1" "$msg"
check 'message goes to the resolved session' "session:$session" "$(field "$msg" .to)"
check 'message is sent by the owner' local-user "$(field "$msg" .from)"
check 'message carries the receipt cwd' "$ws" "$(field "$msg" .executionWorkspace)"
check 'message carries lineage depth' 1 "$(field "$msg" .attentionLineageDepth)"
check 'message carries causal coordinates' 1 "$(field "$msg" .attentionFirstCoordinate.offset)"
delivery="agentdelivery_${a}_delegation_1"
check 'delivery is pending automatic' 'pending automatic' "$(field "$delivery" '"\(.state) \(.dispatchMode)"')"
check 'delivery records the execution workspace' "$ws" "$(field "$delivery" .executionWorkspace)"
check 'outbox row is assigned to the session' "$session" "$(_db_sql "SELECT session FROM agent_outbox WHERE message_id='$msg';")"
check 'attention links the delegation' "$msg $session $identity 1" "$(field "$a" '"\(.delegatedMessage) \(.delegatedSession) \(.delegatedIdentity) \(.delegationRevision)"')"
check 'status reports delegated with links' "delegated $delivery pending focus" "$(status "$root" '"\(.status) \(.delivery) \(.deliveryState) \(.nextAction)"')"
before=$(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Attention','Message','AgentDelivery') ORDER BY id;")
check 'a repeated click returns the same publication' "$msg" "$(must @ "$root" delegateAttention)"
check 'direct routing replay is inert' "$msg" "$(must @ WorkstationRouting delegate: "$a")"
check 'repeats change nothing durable' "$before" "$(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Attention','Message','AgentDelivery') ORDER BY id;")"
check 'one outbox row' 1 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE message_id='$msg';")"
publish 'unit tests'; tick
check 'appended events keep one delegation' "2 $msg" "$(field "$a" '"\(.eventCount) \(.delegatedMessage)"')"
check 'root message shows the assignment' true "$([[ $(field "$root" .body) == *"Delegated to session $session"* ]] && echo true || echo false)"
# Changed admission publishes nothing.
@ "$session" pause >/dev/null
reject 'ineligible attention cannot be delegated' @ "$c_root" delegateAttention
check 'rejected delegation publishes nothing' '' "$(field "$c" .delegatedMessage)"
@ "$session" resume >/dev/null
# Delegated work reaches the existing session with the receipt cwd (ShellDriver).
export TRASHTALK_SHELL_DRIVER='prompt=$(cat); ids=$(printf "%s\n" "$prompt" | sed -n "s/^--- delivery //p"); ts="$TRASHTALK_DIR/bin/trash-send"; "$ts" AgentRun result: "cause: $PWD" >/dev/null; for id in $ids; do "$ts" AgentRun settle: "$id" >/dev/null; done'
for i in $(seq 1 100); do @ AgentWorker tickSession: "$session" >/dev/null 2>&1; [[ -n "$(@ "$session" activeRun)" ]] || break; sleep 0.2; done
check 'delivery was processed by the existing session' processed "$(field "$delivery" .state)"
work=$(field "$delivery" .run)
check 'run executed in the receipt workspace' "$ws" "$(field "$work" .executionWorkspace)"
check 'status links the run' "$work succeeded" "$(status "$root" '"\(.run) \(.runState)"')"
reply=$(@ Store findByClass: Message where: "json_extract(data,'\$.thread')='$msg' AND json_extract(data,'\$.to')='local-user'" orderBy: 'created_at DESC' limit: 1)
check 'the agent answered into the owner thread' "cause: $ws" "$(field "$reply" .body)"
unset TRASHTALK_SHELL_DRIVER

# ---- 2C: opt-in automatic routing and loop controls -------------------------
publish 'manual tests'; tick
m=$(attention_for 'manual tests')
check 'default stays manual' '' "$(field "$m" .delegatedMessage)"
must @ "$subscription" enableAutomaticDelegation: 'I understand delegated work runs without review' reason: 'opt in' >/dev/null
reject 'confirmation cannot be blank' @ "$subscription" enableAutomaticDelegation: ' ' reason: again
publish 'paused dispatch'; tick
p=$(attention_for 'paused dispatch')
check 'paused dispatch withholds automatic routing' 'automatic routing withheld: dispatch-paused' "$(field "$p" .routingNote)"
must @ "$subscription" resumeDispatch: 'route automatically' >/dev/null
deliveries=$(count AgentDelivery)
publish 'auto tests'; tick
f=$(attention_for 'auto tests')
check 'accepted group is delegated automatically' "message_${f}_delegation_1 $session" "$(field "$f" '"\(.delegatedMessage) \(.delegatedSession)"')"
check 'exactly one delivery' $((deliveries+1)) "$(count AgentDelivery)"
@ "$consumer" ack: $(( $(@ "$consumer" offset) - 1 )) >/dev/null
snapshot=$(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Attention','Message','AgentDelivery') ORDER BY id;")
tick
check 'restart replay creates no second delegation' "$snapshot" "$(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Attention','Message','AgentDelivery') ORDER BY id;")"
publish 'auto tests'; tick
check 'grouped failures append without a fresh prompt' "2 $((deliveries+1))" "$(echo "$(field "$f" .eventCount) $(count AgentDelivery)")"
check 'automatic delegation reaches the outbox once' 1 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE message_id='message_${f}_delegation_1';")"
# Session replacement: the withheld group is retried on its next event.
@ "$session" close >/dev/null
publish 'replaced tests'; tick
g=$(attention_for 'replaced tests')
check 'no current session withholds routing visibly' 'automatic routing withheld: no-session' "$(field "$g" .routingNote)"
session2=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$ws" profile: shell)
publish 'replaced tests'; tick
check 'a later event routes to the replacement session' "$session2" "$(field "$g" .delegatedSession)"
check 'withheld note clears on delegation' '' "$(field "$g" .routingNote)"
# Loop controls: target-produced and too-deep receipts stay local.
mapfile -t pair < <(@ AgentRun startFor: "$session2" profile: shell)
loop_run=${pair[0]}
publish_origin 'loop tests' "$loop_run"; tick
h=$(attention_for 'loop tests')
check 'recursive origin is withheld' 'automatic routing withheld: lineage-target' "$(field "$h" .routingNote)"
check 'recursive origin publishes nothing' '' "$(field "$h" .delegatedMessage)"
must @ "$loop_run" transitionTo: failed >/dev/null
must @ "$subscription" lineageLimit: 0 reason: 'strict' >/dev/null
mapfile -t pair < <(@ AgentRun startFor: "$other_session" profile: shell)
deep_run=${pair[0]}
publish_origin 'deep tests' "$deep_run"; tick
check 'lineage depth limit withholds routing' 'automatic routing withheld: lineage-depth' "$(field "$(attention_for 'deep tests')" .routingNote)"
must @ "$deep_run" transitionTo: failed >/dev/null
must @ "$subscription" disableAutomaticDelegation: 'opt out' >/dev/null
publish 'manual again'; tick
check 'disabling restores manual routing' '' "$(field "$(attention_for 'manual again')" .delegatedMessage)"
must @ "$(field "$m" .message)" acknowledgeAttention >/dev/null
must @ "$subscription" enableAutomaticDelegation: 'I understand delegated work runs without review' reason: 'opt in again' >/dev/null
publish 'manual tests'; tick
check 'acknowledged groups are never routed automatically' 'acknowledged ' "$(field "$m" '"\(.state) \(.delegatedMessage)"')"
must @ "$subscription" disableAutomaticDelegation: 'opt out again' >/dev/null

# ---- 2D: attention-to-conversation operations ------------------------------
status "$root" '.' | jq -e '.schema_version==1 and .targetHandle=="routing-specialist" and .receiptWorkspace!="" and (.firstCoordinate.offset|type)=="number" and .message!="" and .delivery!="" and .run!=""' >/dev/null || { echo 'FAIL: status projection'; status "$root" '.'; exit 1; }
@ AgentFocus human >/dev/null
saved_focus=$(declare -f __AgentFocus__class__open_)
__AgentFocus__class__open_() { printf 'focused %s\n' "$1"; }
check 'focus attaches to the recorded session' "focused $session" "$(must @ "$root" focusDelegatedAttention)"
check 'focus changes no lifecycle' closed "$(field "$session" .lifecycleState)"
reject 'focus without a delegation is rejected' @ "$(field "$m" .message)" focusDelegatedAttention
eval "$saved_focus"
# Redelegation after replacement: prior unstarted work is skipped, new revision publishes.
must @ "$root" redelegateAttention: 'session replaced' >/dev/null
check 'redelegation clears the assignment but keeps the revision' '|1' "$(field "$a" '"\(.delegatedMessage)|\(.delegationRevision)"')"
check 'redelegation records a note' 'redelegated: session replaced' "$(field "$a" .routingNote)"
check 'redelegation leaves finished work alone' processed "$(field "$delivery" .state)"
msg2=$(must @ "$root" delegateAttention)
check 'redelegation publishes the next revision' "message_${a}_delegation_2 $session2" "$(echo "$msg2 $(field "$msg2" .to | sed 's/^session://')")"
check 'previous delivery id is retained' 2 "$(_db_sql "SELECT count(*) FROM instances WHERE class='AgentDelivery' AND id LIKE 'agentdelivery_${a}_delegation_%';")"
must @ "$root" redelegateAttention: 'again' >/dev/null
check 'unstarted work is skipped on redelegation' skipped "$(field "agentdelivery_${a}_delegation_2" .state)"
reject 'redelegating an undelegated attention is rejected' @ "$root" redelegateAttention: 'twice'
# No routing path created identities or sessions beyond the fixtures.
check 'no sessions were created by routing' 3 "$(count AgentSession)"
check 'no identities were created by routing' 3 "$(count AgentIdentity)"
check 'no assignments were created' 0 "$(count Assignment)"
echo "PASS: $passed checks; dry-run admission, explicit delegation, automatic routing with loop controls, and conversation operations"
