#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Inbox browse: pick a message, read its thread, reply or archive, driven by
# fake inpick / inpage / inmacs binaries on PATH (no terminal needed).

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export TRASHTALK_DIR
source "$TRASHTALK_DIR/lib/trash.bash"

TEST_TMP=$(mktemp -d "${TMPDIR:-/tmp}/trash-inbox-browse.XXXXXX")
FAKE_BIN="$TEST_TMP/bin"
mkdir -p "$FAKE_BIN"
export PICK_QUEUE="$TEST_TMP/pick-queue"
export CAPTURE_RECORDS="$TEST_TMP/records.jsonl"
export CAPTURE_PICKER_ARGV="$TEST_TMP/picker-argv"
export CAPTURE_PAGER_TEXT="$TEST_TMP/pager-text"
export CAPTURE_EDITOR_ARGV="$TEST_TMP/editor-argv"
export FAKE_REPLY_TEXT="thanks, that fixed it"
export TRASHTALK_NO_AUTOTICK=1

# inpick: pop the next id from PICK_QUEUE; an empty or missing id cancels.
cat > "$FAKE_BIN/inpick" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" >> "$CAPTURE_PICKER_ARGV"
records=$(cat)
printf '%s\n' "$records" >> "$CAPTURE_RECORDS"
hook=''
while (( $# )); do
    if [[ "$1" == --preview-hook ]]; then hook="$2"; shift; fi
    shift
done
if [[ -n "$hook" && -n "${FAKE_PREVIEW_ID:-}" ]]; then
    printf '%s\n' "$records" | jq -c --arg id "$FAKE_PREVIEW_ID" 'select(.id == $id)' | "$hook" > "$CAPTURE_PREVIEW_DISPLAY" || exit 1
fi
pick=""
if [[ -s "$PICK_QUEUE" ]]; then
    pick=$(head -n 1 "$PICK_QUEUE")
    tail -n +2 "$PICK_QUEUE" > "$PICK_QUEUE.next" && mv "$PICK_QUEUE.next" "$PICK_QUEUE"
fi
action=""
if [[ "$pick" == archive:* ]]; then
    action=archive
    pick="${pick#archive:}"
fi
selection=""
if [[ -n "$pick" ]]; then
    selection=$(printf '%s\n' "$records" | jq -c --arg id "$pick" 'select(.id == $id)' | head -1)
fi
if [[ -z "$selection" ]]; then
    jq -cn '{schema_version:1,outcome:"cancelled",selection:null}'
    exit 130
fi
jq -cn --argjson selection "$selection" --arg action "$action" \
    '{schema_version:1,outcome:"selected",selection:$selection} + (if $action == "" then {} else {action:$action} end)'
FAKE
chmod +x "$FAKE_BIN/inpick"

# inpage: swallow the text, report unchanged.
cat > "$FAKE_BIN/inpage" <<'FAKE'
#!/usr/bin/env bash
cat >> "$CAPTURE_PAGER_TEXT"
jq -cn '{schema_version:1,outcome:"unchanged",path:"",changed:false,cursor:{line:1,column:1},edit_count:0}'
FAKE
chmod +x "$FAKE_BIN/inpage"

# inmacs: write FAKE_REPLY_TEXT to the -o path and report saved (or cancelled
# when FAKE_EDITOR_OUTCOME says so).
cat > "$FAKE_BIN/inmacs" <<'FAKE'
#!/usr/bin/env bash
printf '%s\n' "$@" >> "$CAPTURE_EDITOR_ARGV"
cat >/dev/null
out=""
prev=""
for a in "$@"; do
    if [[ "$prev" == "-o" ]]; then out="$a"; fi
    prev="$a"
done
outcome="${FAKE_EDITOR_OUTCOME:-saved}"
if [[ "$outcome" == saved && -n "$out" ]]; then
    printf '%s\n' "$FAKE_REPLY_TEXT" > "$out"
fi
jq -cn --arg outcome "$outcome" --arg path "$out" \
    '{schema_version:1,outcome:$outcome,path:$path,changed:($outcome == "saved"),cursor:{line:1,column:1},edit_count:1}'
FAKE
chmod +x "$FAKE_BIN/inmacs"

export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
db_init

PASSED=0
FAILED=0
pass() { echo "  PASS: $1"; ((PASSED++)) || true; }
fail() { echo "  FAIL: $1 (expected: $2, got: $3)"; ((FAILED++)) || true; }
assert_eq() { [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"; }
assert_contains() { [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"; }
line_count() { if [[ -z "$1" ]]; then echo 0; else printf '%s\n' "$1" | grep -c .; fi; }
cleanup() { rm -rf "$TEST_TMP"; }
trap cleanup EXIT

echo "=== Inbox browse ==="
echo ""

inbox=$(@ Inbox named: tester)
q1=$(@ Inbox ask: 'ok to force-push?' to: tester from: alice)
n1=$(@ Inbox send: 'build finished' to: tester from: 'session:agentsession_x' subject: done kind: result)

# ==========================================
echo "1. picker records describe the inbox, newest first, with previews"
# ==========================================

: > "$PICK_QUEUE"
outcome=$(@ $inbox browse)
assert_eq "empty queue cancels the loop" "cancelled" "$outcome"
records=$(cat "$CAPTURE_RECORDS")
assert_eq "one record per message" "2" "$(line_count "$records")"
assert_eq "newest message first" "$n1" "$(printf '%s\n' "$records" | head -1 | jq -r .id)"
assert_eq "content-first label" "ok to force-push?" "$(printf '%s\n' "$records" | jq -r 'select(.id == "'"$q1"'") | .label')"
assert_contains "unread and question markers in compact prefix" "●? alice" "$(printf '%s\n' "$records" | jq -r 'select(.id == "'"$q1"'") | .display.prefix')"
assert_eq "record kind is the message kind" "result" "$(printf '%s\n' "$records" | head -1 | jq -r .kind)"
assert_contains "hidden search text carries status" "unread" "$(printf '%s\n' "$records" | head -1 | jq -r .display.search_text)"
assert_eq "preview path is the message file" "$n1.txt" "$(printf '%s\n' "$records" | head -1 | jq -r .path)"
assert_contains "picker was given the preview root" "--root" "$(cat "$CAPTURE_PICKER_ARGV")"
assert_contains "picker title names the inbox" "Inbox tester" "$(cat "$CAPTURE_PICKER_ARGV")"
assert_eq "rendering unvisited previews does not read them" "2" "$(@ $inbox unreadCount)"

export FAKE_PREVIEW_ID="$n1" CAPTURE_PREVIEW_DISPLAY="$TEST_TMP/preview-display"
outcome=$(@ $inbox browse)
assert_eq "preview is read even when picker is cancelled" "read" "$(@ $n1 status)"
assert_eq "unvisited message remains unread" "unread" "$(@ $q1 status)"
assert_eq "preview updates the unread count" "1" "$(@ $inbox unreadCount)"
assert_eq "preview removes unread dot from displayed row" "false" "$(jq '.prefix | contains("●")' "$CAPTURE_PREVIEW_DISPLAY")"
seen_at=$(@ $n1 readAt)
assert_eq "preview records a read timestamp" "true" "$([[ -n "$seen_at" ]] && echo true || echo false)"
@ $inbox browse >/dev/null
assert_eq "repeat preview keeps first read timestamp" "$seen_at" "$(@ $n1 readAt)"
unset FAKE_PREVIEW_ID

# ==========================================
echo ""
echo "2. selecting a message marks it read; view shows the thread; reply sends into the thread"
# ==========================================

: > "$CAPTURE_RECORDS"; : > "$CAPTURE_PICKER_ARGV"; : > "$CAPTURE_PAGER_TEXT"; : > "$CAPTURE_EDITOR_ARGV"
printf '%s\n' "$q1" "view" "details" "reply" > "$PICK_QUEUE"
outcome=$(@ $inbox browse 2>/dev/null)
assert_eq "loop ends when the queue runs out" "cancelled" "$outcome"
assert_contains "action picker offered view" '"id":"view"' "$(cat "$CAPTURE_RECORDS")"
assert_eq "selected message marked read" "read" "$(@ $q1 status)"
assert_contains "pager showed the question body" "ok to force-push?" "$(cat "$CAPTURE_PAGER_TEXT")"
assert_contains "action picker offered reply" '"id":"reply"' "$(cat "$CAPTURE_RECORDS")"
assert_contains "details action exposes original message id" "$q1" "$(cat "$CAPTURE_PAGER_TEXT")"
assert_contains "details action exposes full metadata" 'From:    alice' "$(cat "$CAPTURE_PAGER_TEXT")"
assert_eq "every picker record carries path, line, and column" "0" "$(jq -c 'select((.path|type) != "string" or (.line|type) != "number" or (.column|type) != "number")' "$CAPTURE_RECORDS" | grep -c .)"
assert_eq "action records preview the selected message" "$q1.txt" "$(jq -r 'select(.id == "reply") | .path' "$CAPTURE_RECORDS" | head -1)"
assert_eq "pickers ran: list, actions after view and details, list again" "5" "$(grep -c 'From alice\|Inbox tester' "$CAPTURE_PICKER_ARGV")"
assert_contains "editor opened with an output path" "-o" "$(cat "$CAPTURE_EDITOR_ARGV")"
assert_contains "editor title names the sender" "Reply to alice" "$(cat "$CAPTURE_EDITOR_ARGV")"
alice=$(@ Inbox named: alice)
reply=$(@ $alice unread)
assert_eq "alice received one reply" "1" "$(line_count "$reply")"
assert_eq "reply body is the composed text" "$FAKE_REPLY_TEXT" "$(@ $reply body)"
assert_eq "reply stays in the question's thread" "$q1" "$(@ $reply thread)"
assert_eq "reply points at the question" "$q1" "$(@ $reply replyTo)"
assert_eq "reply is from the inbox owner" "tester" "$(@ $reply from)"

# ==========================================
echo ""
echo "3. a cancelled editor sends nothing; archive hides the message"
# ==========================================

export FAKE_EDITOR_OUTCOME=cancelled
printf '%s\n%s\n' "$n1" "reply" > "$PICK_QUEUE"
@ $inbox browse >/dev/null 2>&1
session_inbox=$(@ Inbox named: 'session:agentsession_x')
assert_eq "no reply sent when the editor is cancelled" "0" "$(line_count "$(@ $session_inbox unread)")"
unset FAKE_EDITOR_OUTCOME

printf '%s\n%s\n' "$n1" "archive" > "$PICK_QUEUE"
@ $inbox browse >/dev/null 2>&1
assert_eq "archive action archives the message" "archived" "$(@ $n1 status)"
: > "$CAPTURE_RECORDS"; : > "$PICK_QUEUE"
@ $inbox browse >/dev/null 2>&1
assert_eq "archived message no longer listed" "1" "$(line_count "$(cat "$CAPTURE_RECORDS")")"

# ==========================================
echo ""
echo "4. browse the instance returned by Trash userInbox"
# ==========================================

: > "$CAPTURE_PICKER_ARGV"; : > "$PICK_QUEUE"
user_inbox=$(TRASHTALK_USER=tester @ Trash userInbox)
@ "$user_inbox" browse >/dev/null 2>&1
assert_contains "userInbox browse opened the user's inbox" "Inbox tester" "$(cat "$CAPTURE_PICKER_ARGV")"

echo ""
echo "5. Ctrl-D archives directly from the message list and preserves the thread"

direct=$(@ Inbox send: 'hide this message' to: tester from: alice)
: > "$CAPTURE_RECORDS"; : > "$CAPTURE_PICKER_ARGV"
printf 'archive:%s\n' "$direct" > "$PICK_QUEUE"
@ $inbox browse >/dev/null 2>&1
assert_eq "direct archive persists" "archived" "$(@ $direct status)"
assert_contains "list enables the archive key" $'--ctrl-d-action\narchive' "$(cat "$CAPTURE_PICKER_ARGV")"
assert_eq "direct archive never opens the action menu" "0" "$(grep -c 'From alice' "$CAPTURE_PICKER_ARGV")"
assert_eq "archived message only appears in the initial list" "1" "$(jq -r --arg id "$direct" 'select(.id == $id) | .id' "$CAPTURE_RECORDS" | grep -c .)"
assert_eq "archive preserves the message in its thread" "$direct" "$(@ $inbox thread: "$direct")"
assert_eq "archive does not mark an unread message read" "" "$(@ $direct readAt)"

echo ""
echo "6. fzf fallback returns the same archive action"
cat > "$FAKE_BIN/fzf" <<'FAKE'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$CAPTURE_PICKER_ARGV"
printf 'ctrl-d\n'
head -n 1
FAKE
chmod +x "$FAKE_BIN/fzf"
record=$(jq -cn --arg id "$q1" '{schema_version:1,id:$id,path:"message.txt",line:1,column:1,label:"question",kind:"question"}')
result=$(@ Tools::Inpick fallbackSelectRecords: "$record" query: '' title: Inbox controlDAction: archive)
assert_eq "fallback reports archive" "archive" "$(printf '%s' "$result" | jq -r .action)"
assert_eq "fallback preserves message identity" "$q1" "$(printf '%s' "$result" | jq -r .selection.id)"
assert_contains "fallback binds Ctrl-D" '--expect=ctrl-d' "$(cat "$CAPTURE_PICKER_ARGV")"

echo ""
echo "7. reading a thread marks its unread messages without restoring archived ones"
thread_reply=$(@ $q1 reply: 'a later reply')
thread_archived=$(@ $q1 reply: 'archived history')
@ $thread_archived archive
@ $inbox readThreadOf: "$q1" >/dev/null
assert_eq "opening the thread reads its other messages" "read" "$(@ $thread_reply status)"
assert_eq "reading preserves archived history" "archived" "$(@ $thread_archived status)"
assert_eq "thread does not reset original read time" "$seen_at" "$(@ $n1 readAt)"

echo ""
echo "8. the preview hook rejects a message outside the offered records"
foreign=$(@ Inbox send: 'not in this picker' to: another-inbox from: alice)
printf '%s\n' "$record" > "$TEST_TMP/records.jsonl"
@ $inbox writePreviewHookIn: "$TEST_TMP"
jq -cn --arg id "$foreign" '{id:$id}' | "$TEST_TMP/preview-hook" >/dev/null 2>&1
hook_status=$?
assert_eq "foreign preview rejected" "true" "$([[ "$hook_status" != 0 ]] && echo true || echo false)"
assert_eq "foreign message remains unread" "unread" "$(@ $foreign status)"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
[[ $FAILED -eq 0 ]]
