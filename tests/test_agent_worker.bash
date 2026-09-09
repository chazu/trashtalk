#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# End-to-end test of the headless session loop over the ShellDriver:
# @@ / Gusgus chat: -> Inbox deliver: -> AgentWorker tick -> detached process ->
# trash-send AgentRun result:/settle: -> reply in the owner's inbox thread.
# No model is involved; TRASHTALK_SHELL_DRIVER plays the agent.

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export TRASHTALK_DIR
source "$TRASHTALK_DIR/lib/trash.bash"

export SQLITE_JSON_DB="/tmp/test_agent_worker_$$.db"
export TRASHTALK_RUN_DIR="/tmp/test_agent_worker_run_$$"
export TRASHTALK_USER=tester
export TRASHTALK_GUSGUS_PROFILE=shell
db_init
honker_available && honker_bootstrap

PASSED=0
FAILED=0

pass() { echo "  PASS: $1"; ((PASSED++)) || true; }
fail() { echo "  FAIL: $1 (expected: $2, got: $3)"; ((FAILED++)) || true; }
assert_eq() { [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"; }
assert_contains() { [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"; }
assert_nonempty() { [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" ""; }
assert_empty() { [[ -z "$2" ]] && pass "$1" || fail "$1" "empty" "$2"; }
line_count() { if [[ -z "$1" ]]; then echo 0; else printf '%s\n' "$1" | grep -c .; fi; }

cleanup() {
    rm -f "$SQLITE_JSON_DB"
    rm -rf "$TRASHTALK_RUN_DIR"
}
trap cleanup EXIT

# Tick the session until its active run has finished (or a timeout elapses).
settle_session() {
    local session="$1" i
    for i in $(seq 1 100); do
        @ AgentWorker tickSession: "$session" >/dev/null 2>&1
        [[ -z "$(@ "$session" activeRun)" ]] && return 0
        sleep 0.2
    done
    return 1
}

# The stand-in agent: answer with a count of the messages it saw, then settle.
AGENT_OK='prompt=$(cat); ids=$(printf "%s\n" "$prompt" | sed -n "s/^--- delivery //p"); ts="$TRASHTALK_DIR/bin/trash-send"; "$ts" AgentRun result: "pong: $(printf "%s\n" "$prompt" | grep -c "^From:")" >/dev/null; for id in $ids; do "$ts" AgentRun settle: "$id" >/dev/null; done'
AGENT_NO_SETTLE='prompt=$(cat); "$TRASHTALK_DIR/bin/trash-send" AgentRun result: "forgot to settle" >/dev/null'
AGENT_CRASH='cat >/dev/null; exit 1'
AGENT_ASK='prompt=$(cat); "$TRASHTALK_DIR/bin/trash-send" AgentRun askUser: "A or B?" >/dev/null'

echo "=== AgentWorker end-to-end (ShellDriver) ==="
echo ""

# ==========================================
echo "1. chat: creates a Gusgus session and a delivery, and launches a run"
# ==========================================

export TRASHTALK_SHELL_DRIVER="$AGENT_OK"
msg=$(@ Gusgus chat: 'hello there' workingDirectory: "$TRASHTALK_DIR" status: 0 lastResult: '' 2>/dev/null)
assert_nonempty "chat returns a message id" "$msg"
session=$(@ Gusgus sessionFor: "$TRASHTALK_DIR")
assert_nonempty "session exists for the workspace" "$session"
assert_eq "session is open" "open" "$(@ $session lifecycleState)"
assert_eq "session profile is shell" "shell" "$(@ $session backendProfile)"
assert_eq "message went to the session inbox" "$(@ $session inbox)" "$(@ $msg to)"
assert_eq "message is from the owner" "tester" "$(@ $msg from)"

settle_session "$session" && pass "run finished" || fail "run finished" "no active run" "$(@ $session activeRun)"
run=$(@ Store findByClass: AgentRun where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at ASC' limit: 1)
assert_nonempty "a run was recorded" "$run"
assert_eq "run succeeded" "succeeded" "$(@ $run state)"
assert_eq "run recorded the shell conversation ref" "shell-$run" "$(@ $run externalConversationRef)"
assert_eq "session remembers the conversation ref" "shell-$run" "$(@ $session lastConversationRef)"
delivery=$(@ Store findByClass: AgentDelivery where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at ASC' limit: 1)
assert_eq "forKey: finds the same delivery" "$delivery" "$(@ AgentDelivery forKey: "$(@ $delivery deliveryKey)")"
assert_nonempty "a delivery was recorded" "$delivery"
assert_eq "delivery processed by the agent" "processed" "$(@ $delivery state)"
assert_eq "delivery attempts is 1" "1" "$(@ $delivery attempts)"

owner_inbox=$(@ Inbox named: tester)
unread=$(@ $owner_inbox unread)
assert_eq "owner has one unread message" "1" "$(line_count "$unread")"
reply="$unread"
assert_eq "reply kind is result" "result" "$(@ $reply kind)"
assert_eq "reply is from the session" "session:$session" "$(@ $reply from)"
assert_eq "reply is in the question's thread" "$msg" "$(@ $reply thread)"
assert_eq "reply points at the question" "$msg" "$(@ $reply replyTo)"
assert_contains "reply body carries the agent's answer" "pong: 1" "$(@ $reply body)"
assert_contains "prompt included the context line" "working_directory=" "$(cat "$(@ $run promptFile)")"
assert_contains "prompt named the delivery" "--- delivery $delivery" "$(cat "$(@ $run promptFile)")"

# ==========================================
echo ""
echo "2. replying to the result resumes the same session"
# ==========================================

@ $owner_inbox readAll >/dev/null
followup=$(@ $reply reply: 'and again?')
assert_eq "reply landed in the session inbox" "session:$session" "$(@ $followup to)"
settle_session "$session" && pass "second run finished" || fail "second run finished" "no active run" "$(@ $session activeRun)"
runs=$(@ Store findByClass: AgentRun where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at ASC' limit: 10)
assert_eq "two runs for the session" "2" "$(line_count "$runs")"
run2=$(printf '%s\n' "$runs" | tail -n 1)
assert_eq "second run succeeded" "succeeded" "$(@ $run2 state)"
unread=$(@ $owner_inbox unread)
assert_eq "owner has a new unread reply" "1" "$(line_count "$unread")"
assert_eq "second reply stays in the original thread" "$msg" "$(@ $unread thread)"
assert_eq "thread has four messages" "4" "$(line_count "$(@ $owner_inbox thread: $msg)")"
@ $owner_inbox readAll >/dev/null

# ==========================================
echo ""
echo "3. a process that exits without settling leaves the delivery uncertain"
# ==========================================

export TRASHTALK_SHELL_DRIVER="$AGENT_NO_SETTLE"
msg3=$(@ Gusgus chat: 'third' workingDirectory: "$TRASHTALK_DIR" status: 0 lastResult: '' 2>/dev/null)
settle_session "$session" && pass "third run finished" || fail "third run finished" "no active run" "$(@ $session activeRun)"
run3=$(@ Store findByClass: AgentRun where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at DESC, rowid DESC' limit: 1)
assert_eq "run is unsettled" "unsettled" "$(@ $run3 state)"
d3=$(@ AgentDelivery offeredFor: "$run3")
assert_empty "no delivery still offered on the run" "$d3"
d3=$(@ Store findByClass: AgentDelivery where: "json_extract(data, '\$.run') = '$run3'" orderBy: 'created_at ASC' limit: 1)
assert_eq "delivery is uncertain" "uncertain" "$(@ $d3 state)"
unread=$(@ $owner_inbox unread)
assert_eq "owner got the result and an alert" "2" "$(line_count "$unread")"
kinds=$(for m in $unread; do @ $m kind; done | sort | tr '\n' ' ')
assert_eq "kinds are alert and result" "alert result " "$kinds"
@ $owner_inbox readAll >/dev/null

# ==========================================
echo ""
@ $session skip: "$d3" note: "reviewed the unsettled test result" >/dev/null
@ $owner_inbox readAll >/dev/null

echo "4. a failure before process launch retries once, then stalls with an alert; a human skips it"
# ==========================================

export TRASHTALK_SHELL_DRIVER=""
msg4=$(@ Gusgus chat: 'fourth' workingDirectory: "$TRASHTALK_DIR" status: 0 lastResult: '' 2>/dev/null)
d4=$(@ Store findByClass: AgentDelivery where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at DESC, rowid DESC' limit: 1)
# First attempt: missing driver command -> run failed -> delivery back to pending.
for i in $(seq 1 50); do
    [[ -z "$(@ $session activeRun)" ]] && break
    sleep 0.2
done
@ AgentWorker tickSession: "$session" >/dev/null 2>&1   # reconcile attempt 1 AND launch attempt 2
sleep 0.5
attempts_after_first=$(@ $d4 attempts)
[[ "$attempts_after_first" == 1 || "$attempts_after_first" == 2 ]] && pass "delivery was retried" || fail "delivery was retried" "1 or 2" "$attempts_after_first"
settle_session "$session" && pass "retry run finished" || fail "retry run finished" "no active run" "$(@ $session activeRun)"
# Reconcile attempt 2 (settle_session ticks until no active run; one more tick makes sure).
@ AgentWorker tickSession: "$session" >/dev/null 2>&1
assert_eq "delivery attempts is 2" "2" "$(@ $d4 attempts)"
assert_eq "delivery is failed" "failed" "$(@ $d4 state)"
assert_empty "no new run after the retry limit" "$(@ $session activeRun)"
unread=$(@ $owner_inbox unread)
assert_contains "owner was alerted about the stall" "stalled after 2 attempts" "$(for m in $unread; do @ $m body; done)"
@ $owner_inbox readAll >/dev/null

skipped=$(@ $session skip: "$d4" note: '' 2>/dev/null)
assert_eq "skip without a note is rejected" "failed" "$(@ $d4 state)"
@ $session skip: "$d4" note: 'test input was broken' >/dev/null
assert_eq "human skip marks the delivery skipped" "skipped" "$(@ $d4 state)"

# ==========================================
echo ""
echo "5. a blocking question blocks the delivery; the user's reply unblocks it"
# ==========================================

export TRASHTALK_SHELL_DRIVER="$AGENT_ASK"
msg5=$(@ Gusgus chat: 'fifth' workingDirectory: "$TRASHTALK_DIR" status: 0 lastResult: '' 2>/dev/null)
settle_session "$session" && pass "asking run finished" || fail "asking run finished" "no active run" "$(@ $session activeRun)"
run5=$(@ Store findByClass: AgentRun where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at DESC, rowid DESC' limit: 1)
assert_eq "run is waiting for the user" "waiting_for_user" "$(@ $run5 state)"
d5=$(@ Store findByClass: AgentDelivery where: "json_extract(data, '\$.run') = '$run5'" orderBy: 'created_at ASC' limit: 1)
assert_eq "delivery is blocked" "blocked" "$(@ $d5 state)"
question=$(@ $owner_inbox questions)
assert_eq "owner has one question" "1" "$(line_count "$question")"
assert_eq "question is from the session" "session:$session" "$(@ $question from)"

export TRASHTALK_SHELL_DRIVER="$AGENT_OK"
answer=$(@ $question reply: 'B')
settle_session "$session" && pass "resumed run finished" || fail "resumed run finished" "no active run" "$(@ $session activeRun)"
assert_eq "blocked delivery was processed after the reply" "processed" "$(@ $d5 state)"
run6=$(@ Store findByClass: AgentRun where: "json_extract(data, '\$.session') = '$session'" orderBy: 'created_at DESC, rowid DESC' limit: 1)
assert_eq "resumed run succeeded" "succeeded" "$(@ $run6 state)"
assert_contains "agent saw both the original and the answer" "pong: 2" "$(for m in $(@ $owner_inbox unread); do @ $m body; done)"
@ $owner_inbox readAll >/dev/null

# ==========================================
echo ""
echo "6. TRASHTALK_NO_AUTOTICK records the delivery without launching"
# ==========================================

export TRASHTALK_NO_AUTOTICK=1
msg6=$(@ Gusgus chat: 'sixth' workingDirectory: "$TRASHTALK_DIR" status: 0 lastResult: '' 2>/dev/null)
assert_empty "no run launched" "$(@ $session activeRun)"
assert_eq "one pending delivery" "1" "$(line_count "$(@ $session pendingDeliveries)")"
unset TRASHTALK_NO_AUTOTICK
started=$(@ AgentWorker tick)
assert_nonempty "tick launches the pending work" "$started"
settle_session "$session" && pass "ticked run finished" || fail "ticked run finished" "no active run" "$(@ $session activeRun)"
assert_eq "no pending deliveries remain" "0" "$(line_count "$(@ $session pendingDeliveries)")"
@ $owner_inbox readAll >/dev/null

# ==========================================
echo ""
echo "7. @@ sends to Gusgus; --fresh opens a new session"
# ==========================================

cd "$TRASHTALK_DIR"
out=$(@@ 'via at-at' 2>/dev/null)
assert_nonempty "@@ prints a message id" "$out"
assert_eq "@@ message went to the session" "session:$session" "$(@ $out to)"
settle_session "$session" >/dev/null
fresh=$(@@ --fresh 'new topic' 2>/dev/null)
session2=$(@ Gusgus sessionFor: "$TRASHTALK_DIR")
[[ "$session2" != "$session" ]] && pass "--fresh opened a different session" || fail "--fresh opened a different session" "different" "same"
assert_eq "old session is closed" "closed" "$(@ $session lifecycleState)"
assert_eq "fresh message went to the new session" "session:$session2" "$(@ $fresh to)"
settle_session "$session2" >/dev/null
assert_contains "summary shows the identity" "gusgus" "$(@ $session2 summary)"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
[[ $FAILED -eq 0 ]]
