#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for Inbox and Message
# Storage/query/reply tests run without honker; listener tests skip without it.

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

export SQLITE_JSON_DB="/tmp/test_inbox_$$.db"
db_init
honker_available && honker_bootstrap

PASSED=0
FAILED=0
LISTENER_PIDS=()

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

line_count() {
    if [[ -z "$1" ]]; then echo 0; else printf '%s\n' "$1" | wc -l | tr -d ' '; fi
}

cleanup() {
    for pid in "${LISTENER_PIDS[@]}"; do
        kill "$pid" 2>/dev/null
    done
    rm -f "$SQLITE_JSON_DB" /tmp/inbox_test_* 2>/dev/null
}
trap cleanup EXIT

echo "=== Inbox Tests ==="
echo ""

# ==========================================
echo "1. named: finds or creates"
# ==========================================

inbox=$(@ Inbox named: 'alice')
assert_nonempty "inbox created" "$inbox"
assert_contains "inbox id has class prefix" "inbox_" "$inbox"
again=$(@ Inbox named: 'alice')
assert_eq "same name returns same instance" "$inbox" "$again"
assert_eq "name stored" "alice" "$(@ $inbox name)"
other=$(@ Inbox named: 'maki:abc-1.2')
[[ "$other" != "$inbox" ]] && pass "different name is a different inbox" || fail "different inbox" "distinct" "same"

# ==========================================
echo ""
echo "2. invalid names are rejected"
# ==========================================

bad=$(@ Inbox named: 'bad name; drop table' 2>/dev/null)
rc=$?
assert_eq "invalid name returns non-zero" "1" "$rc"
assert_eq "invalid name yields no id" "" "$bad"
bad=$(@ Inbox named: '' 2>/dev/null)
assert_eq "empty name yields no id" "" "$bad"

# ==========================================
echo ""
echo "3. send: delivers a stamped message"
# ==========================================

msg=$(@ Inbox send: 'tests pass' to: 'alice' from: 'maki' subject: 'done' kind: 'result')
assert_nonempty "send returns message id" "$msg"
assert_contains "message id has class prefix" "message_" "$msg"
assert_eq "to set" "alice" "$(@ $msg to)"
assert_eq "from set" "maki" "$(@ $msg from)"
assert_eq "subject set" "done" "$(@ $msg subject)"
assert_eq "body set" "tests pass" "$(@ $msg body)"
assert_eq "kind set" "result" "$(@ $msg kind)"
assert_eq "status unread" "unread" "$(@ $msg status)"
assert_eq "isUnread true" "true" "$(@ $msg isUnread)"
assert_nonempty "created stamped" "$(@ $msg created)"
assert_eq "fresh message threads to itself" "$msg" "$(@ $msg thread)"
assert_eq "replyTo empty" "" "$(@ $msg replyTo)"
assert_nonempty "message persisted" "$(db_get "$msg")"

# ==========================================
echo ""
echo "4. unread / unreadCount / list"
# ==========================================

assert_eq "one unread" "1" "$(@ $inbox unreadCount)"
assert_eq "unread lists the message" "$msg" "$(@ $inbox unread)"

msg2=$(@ Inbox send: 'second' to: 'alice' from: 'cron')
assert_eq "two unread" "2" "$(@ $inbox unreadCount)"
assert_eq "default kind is note" "note" "$(@ $msg2 kind)"
assert_eq "default subject empty" "" "$(@ $msg2 subject)"
unread=$(@ $inbox unread)
assert_eq "unread is oldest first" "$msg" "$(printf '%s\n' "$unread" | head -1)"

listing=$(@ $inbox list)
assert_contains "list has header count" "alice (2 unread)" "$listing"
assert_contains "list shows first id" "$msg" "$listing"
assert_contains "list shows kind" "[result]" "$listing"
assert_contains "list shows sender/recipient" "maki -> alice: done" "$listing"
assert_contains "list falls back to body when no subject" "cron -> alice: second" "$listing"

# ==========================================
echo ""
echo "5. show: prints and marks read"
# ==========================================

shown=$(@ $inbox show: $msg)
assert_contains "show prints body" "tests pass" "$shown"
assert_contains "show prints from" "From:    maki" "$shown"
assert_eq "shown message is read" "read" "$(@ $msg status)"
assert_nonempty "readAt stamped" "$(@ $msg readAt)"
assert_eq "one unread after show" "1" "$(@ $inbox unreadCount)"

# ==========================================
echo ""
echo "6. questions / ask:"
# ==========================================

q=$(@ Inbox ask: 'ok to force-push?' to: 'alice' from: 'maki')
assert_eq "ask sets kind question" "question" "$(@ $q kind)"
assert_eq "questions returns only the question" "$q" "$(@ $inbox questions)"
assert_eq "two unread now" "2" "$(@ $inbox unreadCount)"

# ==========================================
echo ""
echo "7. reply: lands in sender inbox, same thread"
# ==========================================

reply=$(@ $q reply: 'yes')
assert_nonempty "reply returns id" "$reply"
assert_eq "reply addressed to original sender" "maki" "$(@ $reply to)"
assert_eq "reply from original recipient" "alice" "$(@ $reply from)"
assert_eq "reply body" "yes" "$(@ $reply body)"
assert_eq "reply keeps thread" "$q" "$(@ $reply thread)"
assert_eq "reply points at question" "$q" "$(@ $reply replyTo)"

maki=$(@ Inbox named: 'maki')
assert_eq "reply is unread in maki inbox" "$reply" "$(@ $maki unread)"
assert_eq "reply did not land in alice inbox" "2" "$(@ $inbox unreadCount)"

thread=$(@ $inbox thread: $q)
assert_eq "thread has two messages" "2" "$(line_count "$thread")"
assert_eq "thread is oldest first" "$q" "$(printf '%s\n' "$thread" | head -1)"
assert_eq "thread ends with reply" "$reply" "$(printf '%s\n' "$thread" | tail -1)"

# ==========================================
echo ""
echo "8. readAll / archive / messages"
# ==========================================

@ $inbox readAll
assert_eq "readAll clears unread" "0" "$(@ $inbox unreadCount)"
assert_eq "question now read" "read" "$(@ $q status)"
assert_contains "list reports none" "no unread messages" "$(@ $inbox list)"

@ $msg2 archive
assert_eq "archive sets status" "archived" "$(@ $msg2 status)"
recent=$(@ $inbox messages)
assert_eq "messages excludes archived" "2" "$(line_count "$recent")"
assert_eq "messages is newest first" "$q" "$(printf '%s\n' "$recent" | head -1)"
assert_eq "messages: honours limit" "1" "$(line_count "$(@ $inbox messages: 1)")"

# ==========================================
echo ""
echo "9. names"
# ==========================================

names=$(@ Inbox names)
assert_contains "names lists alice" "alice" "$names"
assert_contains "names lists maki" "maki" "$names"

# ==========================================
echo ""
echo "10. onMessage: listener"
# ==========================================

if honker_available; then
    RESULT_FILE="/tmp/inbox_test_$$"
    rm -f "$RESULT_FILE"
    handler=$(@ Block params: '["payload"]' code: "echo \"\$payload\" >> $RESULT_FILE" captured: '{}')

    pid=$(@ $inbox onMessage: $handler)
    LISTENER_PIDS+=("$pid")
    assert_nonempty "onMessage returns pid" "$pid"
    assert_eq "isListening true" "true" "$(@ $inbox isListening)"
    sleep 0.1

    alert=$(@ Inbox alert: 'disk 95%' to: 'alice' from: 'cron')
    sleep 0.3

    if [[ -f "$RESULT_FILE" ]]; then
        payload=$(cat "$RESULT_FILE")
        assert_contains "listener received message body" "disk 95%" "$payload"
        assert_eq "payload carries message class" "Message" "$(jq -r '.class' <<<"$payload")"
        assert_eq "payload kind is alert" "alert" "$(jq -r '.kind' <<<"$payload")"
    else
        fail "listener received message" "file" "missing"
    fi

    # Messages to other inboxes do not wake this listener
    rm -f "$RESULT_FILE"
    @ Inbox send: 'elsewhere' to: 'maki' from: 'cron' >/dev/null
    sleep 0.2
    [[ ! -f "$RESULT_FILE" ]] && pass "listener ignores other inboxes" || fail "listener ignores other inboxes" "no file" "$(cat "$RESULT_FILE")"

    @ $inbox stopListening
    assert_eq "isListening false after stop" "false" "$(@ $inbox isListening)"
    kill -0 "$pid" 2>/dev/null && fail "listener process stopped" "dead" "alive" || pass "listener process stopped"
    assert_eq "alert still stored after listener stop" "unread" "$(@ $alert status)"
else
    echo "  SKIP: honker extension not installed (listener tests)"
    out=$(@ $inbox onMessage: 'block_none' 2>&1)
    assert_contains "onMessage warns without honker" "requires the honker extension" "$out"
fi

# ==========================================
echo ""
echo "11. destroy keeps messages"
# ==========================================

scratch=$(@ Inbox named: 'scratch')
smsg=$(@ Inbox send: 'keep me' to: 'scratch' from: 'test')
@ $scratch destroy
assert_eq "inbox record gone" "" "$(db_get "$scratch" 2>/dev/null)"
assert_eq "message survives inbox destroy" "keep me" "$(@ $smsg body)"

# ==========================================
echo ""
echo "=== Results ==="
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

[[ $FAILED -eq 0 ]]
