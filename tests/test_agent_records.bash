#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for the agent domain records:
# AgentIdentity, AgentArchetype, AgentRole, AgentSession, AgentDelivery, AgentRun

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

export SQLITE_JSON_DB="/tmp/test_agent_records_$$.db"
db_init

PASSED=0
FAILED=0
SCRATCH_DIRS=()
agent_workspace_root=$(mktemp -d)
agent_workspace_root=$(cd "$agent_workspace_root" && pwd -P)
SCRATCH_DIRS+=("$agent_workspace_root")
mkdir -p "$agent_workspace_root"/{ws,t1,t2,term}

pass() {
    echo "  PASS: $1"
    ((PASSED++)) || true
}

fail() {
    echo "  FAIL: $1 (expected: $2, got: $3)"
    ((FAILED++)) || true
}

assert_eq() {
    [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"
}

assert_contains() {
    [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"
}

assert_nonempty() {
    [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" ""
}

assert_empty() {
    [[ -z "$2" ]] && pass "$1" || fail "$1" "empty" "$2"
}

line_count() {
    if [[ -z "$1" ]]; then echo 0; else printf '%s\n' "$1" | wc -l | tr -d ' '; fi
}

sha256_of() {
    printf '%s' "$1" | shasum -a 256 | cut -d' ' -f1
}

cleanup() {
    rm -f "$SQLITE_JSON_DB" 2>/dev/null
    for d in "${SCRATCH_DIRS[@]}"; do rm -rf "$d"; done
}
trap cleanup EXIT

unset TRASHTALK_RUN_TOKEN
export TRASHTALK_USER="owner_default"
# Inbox deliver: hands session:/agent: messages to AgentWorker, which records a
# delivery and (unless disabled) ticks the session and launches a run.
export TRASHTALK_NO_AUTOTICK=1

echo "=== Agent Records Tests ==="
echo ""

# ==========================================
echo "1. AgentIdentity named: finds or creates, and creates its inbox"
# ==========================================

identity=$(@ AgentIdentity named: 'gusgus')
assert_nonempty "identity created" "$identity"
assert_contains "identity id has class prefix" "agentidentity_" "$identity"
again=$(@ AgentIdentity named: 'gusgus')
assert_eq "same handle returns same identity" "$identity" "$again"
assert_eq "handle stored" "gusgus" "$(@ $identity handle)"
assert_eq "inbox name is agent:<handle>" "agent:gusgus" "$(@ $identity inbox)"
assert_nonempty "created stamped" "$(@ $identity created)"
assert_eq "enabled defaults to true" "true" "$(@ $identity enabled)"
assert_eq "findByHandle: finds it" "$identity" "$(@ AgentIdentity findByHandle: 'gusgus')"
assert_empty "findByHandle: misses unknown handle" "$(@ AgentIdentity findByHandle: 'nobody')"
inbox_ids=$(@ Store findByClass: Inbox where: "json_extract(data, '\$.name') = 'agent:gusgus'" orderBy: 'rowid ASC' limit: 10)
assert_eq "agent inbox record exists" "1" "$(line_count "$inbox_ids")"
assert_eq "inboxObject is that inbox" "$inbox_ids" "$(@ $identity inboxObject)"
bad=$(@ AgentIdentity named: 'bad handle;' 2>/dev/null)
rc=$?
assert_eq "invalid handle returns non-zero" "1" "$rc"
assert_empty "invalid handle yields no id" "$bad"
@ $identity owner: 'bob'
@ $identity save
assert_eq "owner persisted" "bob" "$(@ $identity owner)"

# ==========================================
echo ""
echo "2. AgentArchetype define: is idempotent and hashes instructions"
# ==========================================

instructions='You are a helpful assistant.'
archetype=$(@ AgentArchetype define: 'assistant' revision: '1' instructions: "$instructions" profile: 'shell')
assert_nonempty "archetype created" "$archetype"
assert_eq "archetype hash is sha256 of instructions" "$(sha256_of "$instructions")" "$(@ $archetype instructionsHash)"
assert_eq "archetype profile stored" "shell" "$(@ $archetype suggestedBackendProfile)"
assert_nonempty "archetype created stamped" "$(@ $archetype created)"
dup=$(@ AgentArchetype define: 'assistant' revision: '1' instructions: 'different text' profile: 'other')
assert_eq "define: returns existing id for same name+revision" "$archetype" "$dup"
assert_eq "existing archetype unchanged" "$instructions" "$(@ $archetype instructions)"
assert_eq "named:revision: finds it" "$archetype" "$(@ AgentArchetype named: 'assistant' revision: '1')"
assert_empty "named:revision: misses other revision" "$(@ AgentArchetype named: 'assistant' revision: '2')"
rev2=$(@ AgentArchetype define: 'assistant' revision: '2' instructions: 'v2' profile: 'shell')
[[ "$rev2" != "$archetype" ]] && pass "new revision is a new record" || fail "new revision" "distinct" "same"

# ==========================================
echo ""
echo "3. AgentRole define: idempotency, retryLimit, allowsWorkspace:"
# ==========================================

role=$(@ AgentRole define: 'assistant' revision: '1' capabilities: '["inbox.read","message.send"]' workspacePolicy: '[]' runBudget: '{"turns":0,"usd":0,"retryLimit":3}')
assert_nonempty "role created" "$role"
assert_eq "define: returns existing role" "$role" "$(@ AgentRole define: 'assistant' revision: '1' capabilities: '[]' workspacePolicy: '["/x"]' runBudget: '{}')"
assert_eq "named:revision: finds role" "$role" "$(@ AgentRole named: 'assistant' revision: '1')"
assert_eq "retryLimit from runBudget" "3" "$(@ $role retryLimit)"
assert_eq "empty workspacePolicy allows any path" "true" "$(@ $role allowsWorkspace: /anywhere/at/all)"

strict=$(@ AgentRole define: 'strict' revision: '1' capabilities: '[]' workspacePolicy: '["/opt/work","/srv/other"]' runBudget: '{}')
assert_eq "retryLimit defaults to 2" "2" "$(@ $strict retryLimit)"
assert_eq "prefix match allows" "true" "$(@ $strict allowsWorkspace: /opt/work/project)"
assert_eq "second prefix allows" "true" "$(@ $strict allowsWorkspace: /srv/other)"
assert_eq "non-matching path denied" "false" "$(@ $strict allowsWorkspace: /home/x)"
badrole=$(@ AgentRole define: 'broken' revision: '1' capabilities: 'nope' workspacePolicy: '[]' runBudget: '{}' 2>/dev/null)
rc=$?
assert_eq "invalid policy JSON rejected" "1" "$rc"

# ==========================================
echo ""
echo "4. AgentSession openFor: snapshots revisions and creates its inbox"
# ==========================================

session=$(@ AgentSession openFor: $identity archetype: $archetype role: $role workspace: ${agent_workspace_root}/ws profile: 'shell')
assert_nonempty "session created" "$session"
assert_contains "session id has class prefix" "agentsession_" "$session"
assert_eq "identity stored" "$identity" "$(@ $session identity)"
assert_eq "archetype stored" "$archetype" "$(@ $session archetype)"
assert_eq "archetype revision snapshot" "1" "$(@ $session archetypeRevision)"
assert_eq "instructions hash snapshot" "$(@ $archetype instructionsHash)" "$(@ $session instructionsHash)"
assert_eq "role revision snapshot" "1" "$(@ $session roleRevision)"
assert_eq "profile stored" "shell" "$(@ $session backendProfile)"
assert_eq "workspace stored" "${agent_workspace_root}/ws" "$(@ $session workspace)"
assert_eq "lifecycle open" "open" "$(@ $session lifecycleState)"
assert_eq "execution policy defaults to single" "single" "$(@ $session executionPolicy)"
assert_eq "session inbox name" "session:$session" "$(@ $session inbox)"
assert_nonempty "created stamped" "$(@ $session created)"
assert_nonempty "lastActivityAt stamped" "$(@ $session lastActivityAt)"
sinbox=$(@ Store findByClass: Inbox where: "json_extract(data, '\$.name') = 'session:$session'" orderBy: 'rowid ASC' limit: 10)
assert_eq "session inbox record exists" "1" "$(line_count "$sinbox")"
assert_eq "inboxObject is that inbox" "$sinbox" "$(@ $session inboxObject)"

denied=$(@ AgentSession openFor: $identity archetype: $archetype role: $strict workspace: /home/x profile: 'shell' 2>/dev/null)
rc=$?
assert_eq "openFor: rejects disallowed workspace" "1" "$rc"
assert_empty "no session for disallowed workspace" "$denied"

found=$(@ AgentSession findFor: $identity workspace: ${agent_workspace_root}/ws)
assert_eq "findFor: finds the open session" "$session" "$found"
assert_empty "findFor: misses other workspace" "$(@ AgentSession findFor: $identity workspace: /elsewhere)"
assert_contains "summary has handle and state" "$session gusgus open ws=${agent_workspace_root}/ws pending=0 run=-" "$(@ $session summary)"

before=$(@ $session lastActivityAt)
@ $session touch >/dev/null
assert_nonempty "touch keeps lastActivityAt" "$(@ $session lastActivityAt)"

# ==========================================
echo ""
echo "5. AgentSession transition table"
# ==========================================

assert_eq "open -> paused" "paused" "$(@ $session pause)"
assert_eq "paused -> open" "open" "$(@ $session resume)"
assert_eq "open -> closed" "closed" "$(@ $session close)"
assert_eq "closed -> open (reopen)" "open" "$(@ $session reopen)"
@ $session pause >/dev/null
assert_eq "paused -> closed" "closed" "$(@ $session close)"
@ $session reopen >/dev/null
assert_eq "findFor: still sees reopened session" "$session" "$(@ AgentSession findFor: $identity workspace: ${agent_workspace_root}/ws)"

# Rejected edges
out=$(@ $session resume 2>/dev/null); rc=$?
assert_eq "open -> open (resume) rejected rc" "1" "$rc"
assert_eq "state unchanged after rejected resume" "open" "$(@ $session lifecycleState)"
out=$(@ $session reopen 2>/dev/null); rc=$?
assert_eq "open -> open (reopen) rejected rc" "1" "$rc"
@ $session close >/dev/null
out=$(@ $session pause 2>/dev/null); rc=$?
assert_eq "closed -> paused rejected rc" "1" "$rc"
assert_eq "state unchanged after rejected pause" "closed" "$(@ $session lifecycleState)"
out=$(@ $session close 2>/dev/null); rc=$?
assert_eq "closed -> closed rejected rc" "1" "$rc"
out=$(@ $session transitionTo: 'bogus' 2>/dev/null); rc=$?
assert_eq "unknown state rejected rc" "1" "$rc"
assert_eq "state unchanged after unknown state" "closed" "$(@ $session lifecycleState)"
assert_empty "findFor: excludes closed session" "$(@ AgentSession findFor: $identity workspace: ${agent_workspace_root}/ws)"

# terminated from each live state, and terminal
term1=$(@ AgentSession openFor: $identity archetype: $archetype role: $role workspace: ${agent_workspace_root}/t1 profile: 'shell')
assert_eq "open -> terminated" "terminated" "$(@ $term1 terminate)"
term2=$(@ AgentSession openFor: $identity archetype: $archetype role: $role workspace: ${agent_workspace_root}/t2 profile: 'shell')
@ $term2 pause >/dev/null
assert_eq "paused -> terminated" "terminated" "$(@ $term2 terminate)"
assert_eq "closed -> terminated" "terminated" "$(@ $session terminate)"
out=$(@ $session reopen 2>/dev/null); rc=$?
assert_eq "terminated -> open rejected rc" "1" "$rc"
assert_eq "terminated is terminal" "terminated" "$(@ $session lifecycleState)"

# ==========================================
echo ""
echo "6. AgentSession workspaceFor:"
# ==========================================

repo=$(mktemp -d)
SCRATCH_DIRS+=("$repo")
git -C "$repo" init -q
mkdir -p "$repo/a/b"
expected_root=$(git -C "$repo/a/b" rev-parse --show-toplevel)
assert_eq "git subdirectory resolves to repo root" "$expected_root" "$(@ AgentSession workspaceFor: "$repo/a/b")"
plain=$(mktemp -d)
SCRATCH_DIRS+=("$plain")
mkdir -p "$plain/deep"
expected_plain=$(cd "$plain/deep" && pwd -P)
assert_eq "non-repo dir resolves to its real path" "$expected_plain" "$(@ AgentSession workspaceFor: "$plain/deep")"
out=$(@ AgentSession workspaceFor: "$plain/missing" 2>/dev/null); rc=$?
assert_eq "missing dir rejected rc" "1" "$rc"

# ==========================================
echo ""
echo "7. AgentDelivery forSession: is idempotent by key"
# ==========================================

session=$(@ AgentSession openFor: $identity archetype: $archetype role: $role workspace: ${agent_workspace_root}/ws profile: 'shell')
m1=$(@ Inbox send: 'please help' to: "session:$session" from: 'alice' subject: 'help' kind: 'question')
hooked=$(@ AgentDelivery pendingFor: $session)
assert_eq "Inbox hook records one delivery for the message" "1" "$(line_count "$hooked")"
d1=$(@ AgentDelivery forSession: $session messages: "[\"$m1\"]")
assert_nonempty "delivery created" "$d1"
assert_eq "forSession: resolves to the hook-created delivery" "$hooked" "$d1"
assert_contains "delivery id has class prefix" "agentdelivery_" "$d1"
assert_eq "delivery session" "$session" "$(@ $d1 session)"
assert_eq "delivery state pending" "pending" "$(@ $d1 state)"
assert_eq "attempts starts at 0" "0" "$(@ $d1 attempts)"
expected_key="$session:$(sha256_of "[\"$m1\"]" | cut -c1-16)"
assert_eq "delivery key is session:sha16" "$expected_key" "$(@ $d1 deliveryKey)"
assert_eq "same messages return same delivery" "$d1" "$(@ AgentDelivery forSession: $session messages: "[\"$m1\"]")"
assert_eq "forKey: finds it" "$d1" "$(@ AgentDelivery forKey: "$expected_key")"
assert_empty "forKey: misses unknown key" "$(@ AgentDelivery forKey: 'nope')"
assert_eq "messageIdList" "$m1" "$(@ $d1 messageIdList)"
assert_eq "senderOfFirstMessage" "alice" "$(@ $d1 senderOfFirstMessage)"
assert_eq "pendingFor: lists it" "$d1" "$(@ AgentDelivery pendingFor: $session)"
assert_eq "session pendingDeliveries" "$d1" "$(@ $session pendingDeliveries)"
assert_contains "summary counts pending" "pending=1" "$(@ $session summary)"
out=$(@ AgentDelivery forSession: $session messages: 'not json' 2>/dev/null); rc=$?
assert_eq "non-array messages rejected rc" "1" "$rc"

m2=$(@ Inbox send: 'second' to: "session:$session" from: 'carol' subject: 'two' kind: 'note')
d2=$(@ AgentDelivery forSession: $session messages: "[\"$m2\"]")
[[ "$d2" != "$d1" ]] && pass "different messages make a different delivery" || fail "distinct delivery" "distinct" "same"
pending=$(@ AgentDelivery pendingFor: $session)
assert_eq "two pending" "2" "$(line_count "$pending")"
assert_eq "pending is oldest first" "$d1" "$(printf '%s\n' "$pending" | head -1)"

# ==========================================
echo ""
echo "8. AgentRun startFor: prints id and token; hash matches"
# ==========================================

mapfile -t lines < <(@ AgentRun startFor: $session profile: 'shell')
run="${lines[0]}"
token="${lines[1]}"
assert_eq "startFor: prints two lines" "2" "${#lines[@]}"
assert_contains "run id has class prefix" "agentrun_" "$run"
assert_eq "token starts with run id" "$run" "${token%%:*}"
secret="${token#*:}"
assert_nonempty "token has a secret" "$secret"
assert_eq "stored hash is sha256 of secret" "$(sha256_of "$secret")" "$(@ $run capabilityTokenHash)"
assert_eq "secret is not stored" "0" "$(db_get "$run" | grep -c "$secret")"
assert_eq "run state starting" "starting" "$(@ $run state)"
assert_eq "run session" "$session" "$(@ $run session)"
assert_eq "run snapshots archetype revision" "1" "$(@ $run archetypeRevision)"
assert_eq "run snapshots role revision" "1" "$(@ $run roleRevision)"
assert_eq "run profile" "shell" "$(@ $run backendProfile)"
assert_nonempty "run started stamped" "$(@ $run started)"
assert_eq "session activeRun sees starting run" "$run" "$(@ $session activeRun)"
assert_contains "summary shows run" "run=$run" "$(@ $session summary)"
assert_eq "isProcessAlive false with no pid" "false" "$(@ $run isProcessAlive)"
@ $run processPid: $$
assert_eq "isProcessAlive true for live pid" "true" "$(@ $run isProcessAlive)"

# ==========================================
echo ""
echo "9. AgentDelivery claim:run: is a one-shot compare-and-set"
# ==========================================

assert_eq "first claim succeeds" "true" "$(@ AgentDelivery claim: $d1 run: $run)"
assert_eq "second claim fails" "false" "$(@ AgentDelivery claim: $d1 run: $run)"
assert_eq "claimed state offered" "offered" "$(@ $d1 state)"
assert_eq "claimed run" "$run" "$(@ $d1 run)"
assert_eq "attempts incremented once" "1" "$(@ $d1 attempts)"
assert_eq "offeredFor: lists claimed delivery" "$d1" "$(@ AgentDelivery offeredFor: $run)"
assert_eq "pendingFor: no longer lists it" "$d2" "$(@ AgentDelivery pendingFor: $session)"

# ==========================================
echo ""
echo "10. AgentDelivery transitions"
# ==========================================

out=$(@ $d1 transitionTo: 'skipped' 2>/dev/null); rc=$?
assert_eq "offered -> skipped rejected rc" "1" "$rc"
assert_eq "state unchanged" "offered" "$(@ $d1 state)"
assert_eq "offered -> blocked" "blocked" "$(@ $d1 transitionTo: 'blocked')"
out=$(@ $d1 transitionTo: 'offered' 2>/dev/null); rc=$?
assert_eq "blocked -> offered rejected rc" "1" "$rc"
assert_eq "blocked -> pending" "pending" "$(@ $d1 transitionTo: 'pending')"
assert_eq "re-claim after pending" "true" "$(@ AgentDelivery claim: $d1 run: $run)"
assert_eq "attempts now 2" "2" "$(@ $d1 attempts)"
assert_eq "offered -> failed" "failed" "$(@ $d1 transitionTo: 'failed')"
out=$(@ $d1 transitionTo: 'processed' 2>/dev/null); rc=$?
assert_eq "failed -> processed rejected rc" "1" "$rc"
assert_eq "failed -> pending" "pending" "$(@ $d1 transitionTo: 'pending')"
out=$(@ $d1 transitionTo: 'processed' 2>/dev/null); rc=$?
assert_eq "pending -> processed rejected rc" "1" "$rc"
assert_eq "still pending" "pending" "$(@ $d1 state)"

# uncertain path and skip:note:
d3=$(@ AgentDelivery forSession: $session messages: "[\"$m1\",\"$m2\"]")
assert_eq "batch delivery lists both ids" "2" "$(line_count "$(@ $d3 messageIdList)")"
@ AgentDelivery claim: $d3 run: $run >/dev/null
assert_eq "offered -> uncertain" "uncertain" "$(@ $d3 transitionTo: 'uncertain')"
out=$(@ $session skip: $d3 note: '' 2>/dev/null); rc=$?
assert_eq "skip: requires a note rc" "1" "$rc"
assert_eq "skip: without note leaves state" "uncertain" "$(@ $d3 state)"
out=$(@ $session skip: $d1 note: 'nope' 2>/dev/null); rc=$?
assert_eq "skip: rejects pending delivery rc" "1" "$rc"
assert_eq "skip: uncertain -> skipped" "skipped" "$(@ $session skip: $d3 note: 'giving up')"
assert_eq "skip stores note in lastError" "giving up" "$(@ $d3 lastError)"
skipnote=$(@ Store findByClass: Message where: "json_extract(data, '\$.to') = 'bob' AND json_extract(data, '\$.from') = 'worker'" orderBy: 'rowid ASC' limit: 10)
assert_eq "skip sends a note to the identity owner's inbox" "1" "$(line_count "$skipnote")"
assert_contains "skip note names the delivery" "skipped $d3" "$(@ $skipnote body)"
assert_eq "skip note kind" "note" "$(@ $skipnote kind)"
assert_eq "skip note did not create a session delivery" "$d1
$d2" "$(@ AgentDelivery pendingFor: $session)"

# ==========================================
echo ""
echo "11. AgentRun current"
# ==========================================

unset TRASHTALK_RUN_TOKEN
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails with no env" "1" "$rc"
assert_empty "current prints nothing with no env" "$out"

export TRASHTALK_RUN_TOKEN="$run:wrongsecret"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails with wrong secret" "1" "$rc"
assert_empty "current prints nothing with wrong secret" "$out"

export TRASHTALK_RUN_TOKEN="$token"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails while run is starting" "1" "$rc"

assert_eq "starting -> running" "running" "$(@ $run transitionTo: 'running')"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current succeeds when running" "0" "$rc"
assert_eq "current prints the run id" "$run" "$out"

export TRASHTALK_RUN_TOKEN="not_a_run:abc"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails for unknown run" "1" "$rc"
export TRASHTALK_RUN_TOKEN="$token"

# terminated session
tsession=$(@ AgentSession openFor: $identity archetype: $archetype role: $role workspace: ${agent_workspace_root}/term profile: 'shell')
mapfile -t tlines < <(@ AgentRun startFor: $tsession profile: 'shell')
trun="${tlines[0]}"
@ $trun transitionTo: 'running' >/dev/null
@ $tsession terminate >/dev/null
saved_token="$TRASHTALK_RUN_TOKEN"
export TRASHTALK_RUN_TOKEN="${tlines[1]}"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails when session is terminated" "1" "$rc"
assert_empty "current prints nothing for terminated session" "$out"
export TRASHTALK_RUN_TOKEN="$saved_token"

# ==========================================
echo ""
echo "12. settle:"
# ==========================================

@ AgentDelivery claim: $d1 run: $run >/dev/null
assert_eq "d1 offered again" "offered" "$(@ $d1 state)"
out=$(@ AgentRun settle: $d2 2>/dev/null); rc=$?
assert_eq "settle: rejects a delivery not offered to this run" "1" "$rc"
assert_eq "unoffered delivery unchanged" "pending" "$(@ $d2 state)"
assert_eq "settle: processes an offered delivery" "processed" "$(@ AgentRun settle: $d1)"
assert_eq "settled state" "processed" "$(@ $d1 state)"
out=$(@ AgentRun settle: $d1 2>/dev/null); rc=$?
assert_eq "settle: twice is rejected" "1" "$rc"

# ==========================================
echo ""
echo "13. result: replies in-thread to the sender"
# ==========================================

@ AgentDelivery claim: $d2 run: $run >/dev/null
alice=$(@ Inbox named: 'alice')
carol=$(@ Inbox named: 'carol')
assert_eq "carol inbox empty before result" "0" "$(@ $carol unreadCount)"
reply=$(@ AgentRun result: 'here is the answer')
assert_nonempty "result: prints message id" "$reply"
assert_eq "reply lands in sender inbox" "carol" "$(@ $reply to)"
assert_eq "reply is from session:<id>" "session:$session" "$(@ $reply from)"
assert_eq "reply kind result" "result" "$(@ $reply kind)"
assert_eq "reply keeps subject" "two" "$(@ $reply subject)"
assert_eq "reply body" "here is the answer" "$(@ $reply body)"
assert_eq "reply thread is original thread" "$(@ $m2 thread)" "$(@ $reply thread)"
assert_eq "reply replyTo is original message" "$m2" "$(@ $reply replyTo)"
assert_eq "carol has one unread" "1" "$(@ $carol unreadCount)"
assert_eq "alice got nothing (delivery already settled)" "0" "$(@ $alice unreadCount)"
assert_eq "result: leaves delivery offered" "offered" "$(@ $d2 state)"

# ==========================================
echo ""
echo "14. askUser: goes to the owner and blocks offered deliveries"
# ==========================================

bob=$(@ Inbox named: 'bob')
q=$(@ AgentRun askUser: 'which branch?')
assert_nonempty "askUser: prints message id" "$q"
assert_eq "question lands in owner inbox" "bob" "$(@ $q to)"
assert_eq "question from session:<id>" "session:$session" "$(@ $q from)"
assert_eq "question kind" "question" "$(@ $q kind)"
assert_eq "question subject" "question" "$(@ $q subject)"
assert_eq "question inherits thread of first offered message" "$(@ $m2 thread)" "$(@ $q thread)"
assert_eq "offered delivery is now blocked" "blocked" "$(@ $d2 state)"
assert_eq "bob questions lists it" "$q" "$(@ $bob questions)"
assert_eq "blocked delivery still counts as held" "$d2" "$(@ AgentDelivery offeredFor: $run)"
assert_eq "settle:note: processes a blocked delivery" "processed" "$(@ AgentRun settle: $d2 note: 'answered')"
assert_eq "settle note stored" "answered" "$(@ $d2 note)"

# result: with no held delivery goes to the owner
fallback=$(@ AgentRun result: 'nothing held')
assert_eq "result: without deliveries goes to owner" "bob" "$(@ $fallback to)"
assert_eq "fallback kind result" "result" "$(@ $fallback kind)"

# ==========================================
echo ""
echo "15. send:to: and send:to:key: dedupe"
# ==========================================

n1=$(@ AgentRun send: 'fyi' to: 'alice')
assert_eq "send:to: lands in address" "alice" "$(@ $n1 to)"
assert_eq "send:to: from session" "session:$session" "$(@ $n1 from)"
assert_eq "send:to: kind note" "note" "$(@ $n1 kind)"

k1=$(@ AgentRun send: 'progress 1' to: 'alice' key: 'progress')
k2=$(@ AgentRun send: 'progress 2' to: 'alice' key: 'progress')
assert_nonempty "keyed send returns id" "$k1"
assert_eq "same key returns the same message" "$k1" "$k2"
assert_eq "keyed subject" "key:progress" "$(@ $k1 subject)"
assert_eq "first body kept" "progress 1" "$(@ $k1 body)"
k3=$(@ AgentRun send: 'other' to: 'alice' key: 'other')
[[ "$k3" != "$k1" ]] && pass "different key sends a new message" || fail "different key" "distinct" "same"
assert_eq "alice unread count" "3" "$(@ $alice unreadCount)"

listing=$(@ AgentRun inbox)
assert_contains "inbox prints session inbox listing" "Inbox session:$session" "$listing"

# ==========================================
echo ""
echo "16. AgentRun finishWith: and heartbeat"
# ==========================================

@ $run markHeartbeat >/dev/null
assert_nonempty "heartbeat stamped" "$(@ $run heartbeatAt)"
out=$(@ $run finishWith: 'starting' outcome: 'x' error: '' 2>/dev/null); rc=$?
assert_eq "finishWith: rejects a bad transition rc" "1" "$rc"
assert_eq "state unchanged after rejected finish" "running" "$(@ $run state)"
assert_empty "outcome untouched after rejected finish" "$(@ $run outcome)"
assert_eq "running -> recovering" "recovering" "$(@ $run transitionTo: 'recovering')"
assert_eq "finishWith: succeeded" "succeeded" "$(@ $run finishWith: 'succeeded' outcome: 'done' error: '')"
assert_eq "outcome stored" "done" "$(@ $run outcome)"
assert_nonempty "finished stamped" "$(@ $run finished)"
assert_empty "activeRun empty once finished" "$(@ $session activeRun)"
out=$(@ AgentRun current 2>/dev/null); rc=$?
assert_eq "current fails once run finished" "1" "$rc"
out=$(@ $run transitionTo: 'running' 2>/dev/null); rc=$?
assert_eq "succeeded is terminal" "1" "$rc"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ $FAILED -eq 0 ]]
