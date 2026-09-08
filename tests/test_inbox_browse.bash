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
pick=""
if [[ -s "$PICK_QUEUE" ]]; then
    pick=$(head -n 1 "$PICK_QUEUE")
    tail -n +2 "$PICK_QUEUE" > "$PICK_QUEUE.next" && mv "$PICK_QUEUE.next" "$PICK_QUEUE"
fi
selection=""
if [[ -n "$pick" ]]; then
    selection=$(printf '%s\n' "$records" | jq -c --arg id "$pick" 'select(.id == $id)' | head -1)
fi
if [[ -z "$selection" ]]; then
    jq -cn '{schema_version:1,outcome:"cancelled",selection:null}'
    exit 130
fi
jq -cn --argjson selection "$selection" '{schema_version:1,outcome:"selected",selection:$selection}'
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
assert_contains "unread marker and kind in label" "* [question] alice: ok to force-push?" "$(printf '%s\n' "$records" | jq -r 'select(.id == "'"$q1"'") | .label')"
assert_eq "record kind is the message kind" "result" "$(printf '%s\n' "$records" | head -1 | jq -r .kind)"
assert_contains "detail carries status" "unread" "$(printf '%s\n' "$records" | head -1 | jq -r .detail)"
assert_eq "preview path is the message file" "$n1.txt" "$(printf '%s\n' "$records" | head -1 | jq -r .path)"
assert_contains "picker was given the preview root" "--root" "$(cat "$CAPTURE_PICKER_ARGV")"
assert_contains "picker title names the inbox" "Inbox tester" "$(cat "$CAPTURE_PICKER_ARGV")"

# ==========================================
echo ""
echo "2. selecting a message shows its thread and marks it read; reply sends into the thread"
# ==========================================

: > "$CAPTURE_RECORDS"; : > "$CAPTURE_PICKER_ARGV"; : > "$CAPTURE_PAGER_TEXT"; : > "$CAPTURE_EDITOR_ARGV"
printf '%s\n%s\n' "$q1" "reply" > "$PICK_QUEUE"
outcome=$(@ $inbox browse 2>/dev/null)
assert_eq "loop ends when the queue runs out" "cancelled" "$outcome"
assert_eq "selected message marked read" "read" "$(@ $q1 status)"
assert_contains "pager showed the question body" "ok to force-push?" "$(cat "$CAPTURE_PAGER_TEXT")"
assert_contains "action picker offered reply" '"id":"reply"' "$(cat "$CAPTURE_RECORDS")"
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
echo "4. class-side browse uses the current user's inbox"
# ==========================================

: > "$CAPTURE_PICKER_ARGV"; : > "$PICK_QUEUE"
TRASHTALK_USER=tester @ Inbox browse >/dev/null 2>&1
assert_contains "class browse opened the user's inbox" "Inbox tester" "$(cat "$CAPTURE_PICKER_ARGV")"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
[[ $FAILED -eq 0 ]]
