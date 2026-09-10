#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=browser-owner TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
export PICKS="$tmp/picks" RECORDS="$tmp/records" PAGES="$tmp/pages" TICKS="$tmp/ticks" PICK_ARGS="$tmp/pick-args"
mkdir "$tmp/bin"
cat > "$tmp/bin/inpick" <<'PICK'
#!/usr/bin/env bash
records=$(cat)
printf '%s\n' "$*" >> "$PICK_ARGS"
printf '%s\n' "$records" >> "$RECORDS"
choice=$(head -n 1 "$PICKS")
tail -n +2 "$PICKS" > "$PICKS.next"; mv "$PICKS.next" "$PICKS"
if [[ -z "$choice" ]]; then echo '{"outcome":"cancelled"}'; exit 130; fi
action=''
if [[ "$choice" == terminate:* ]]; then action=terminate; choice=${choice#terminate:}; fi
printf '%s\n' "$records" | jq -sc --arg id "$choice" --arg action "$action" '{schema_version:1,outcome:"selected",action:$action,selection:(map(select(.id==$id))[0])}'
PICK
cat > "$tmp/bin/inpage" <<'PAGE'
#!/usr/bin/env bash
cat >> "$PAGES"
echo '{"schema_version":1,"outcome":"unchanged"}'
PAGE
chmod +x "$tmp/bin/"*
export PATH="$tmp/bin:$PATH"
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
contains() { [[ "$3" == *"$2"* ]] || { echo "FAIL: $1 missing $2"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
session=$(@ Gusgus sessionFor: "$root")
msg=$(@ Inbox send: 'please inspect this' to: "session:$session" from: browser-owner)
mapfile -t lines < <(@ AgentRun startFor: "$session" profile: shell)
run=${lines[0]}
@ "$run" transitionTo: running >/dev/null
@ "$run" finishWith: failed outcome: '{}' error: 'fixture provider error' >/dev/null
# Browser dispatch still crosses the public worker selector; spy on that boundary
# so this UI contract test doesn't also launch a harness.
@ AgentWorker handlesInbox: test >/dev/null
__AgentWorker__class__tickSession_() { printf '%s\n' "$1" >> "$TICKS"; }
records=$(@ AgentBrowser sessionRecordsIn: "$tmp")
contains 'session record includes workspace' "$root" "$records"
contains 'session record includes queued count' 'pending=1' "$records"
contains 'session details include provider failure' 'fixture provider error' "$(@ "$session" details)"
printf '%s\n' "$session" details messages runs "$run" back '' > "$PICKS"
result=$(@ AgentSession browse)
check 'browser stdout contains only dismissal outcome' dismissed "$result"
contains 'pager shows session message body' 'please inspect this' "$(cat "$PAGES")"
contains 'pager shows run failure' 'fixture provider error' "$(cat "$PAGES")"
check 'opening the conversation marks its message read' read "$(@ "$msg" status)"
check 'reading does not settle queued delivery' 1 "$(@ "$session" pendingCount)"
check 'dismiss leaves lifecycle open' open "$(@ "$session" lifecycleState)"
check 'read-only browser never ticks or controls execution' false "$([[ -e "$TICKS" ]] && echo true || echo false)"
check 'temporary preview dirs removed' 0 "$(find "$TMPDIR" -maxdepth 1 -type d -name 'trash-sessions.*' | wc -l | tr -d ' ')"

# Pause, select a failed delivery, explicitly confirm retry, then dismiss.
delivery=$(@ "$session" pendingDeliveries)
@ AgentDelivery claim: "$delivery" run: "$run" >/dev/null
@ "$delivery" transitionTo: failed >/dev/null
printf '%s\n' "$session" pause retry "$delivery" retry back '' > "$PICKS"
@ AgentSession browse >/dev/null
check 'pause is applied on the session instance' paused "$(@ "$session" lifecycleState)"
check 'confirmed retry returns the delivery to pending' pending "$(@ "$delivery" state)"
check 'confirmed retry resets attempts' 0 "$(@ "$delivery" attempts)"
contains 'retry requests a tick for the selected session' "$session" "$(cat "$TICKS")"
printf '%s\n' "$session" resume back '' > "$PICKS"
@ AgentSession browse >/dev/null
check 'resume is applied on the session instance' open "$(@ "$session" lifecycleState)"

# Cancel the retry confirmation and reject a forged picker id.
@ AgentDelivery claim: "$delivery" run: "$run" >/dev/null
@ "$delivery" transitionTo: uncertain >/dev/null
printf '%s\n' "$session" retry "$delivery" '' back '' > "$PICKS"
@ AgentSession browse >/dev/null
check 'cancelled confirmation preserves uncertain state' uncertain "$(@ "$delivery" state)"
check 'unoffered picker id is rejected' '' "$(@ AgentBrowser selectedFrom: '{"outcome":"selected","selection":{"id":"forged"}}' records: "$records")"

# Ctrl-D is an explicit intent for the selected session, with a safe default.
check 'termination confirmation defaults to Cancel' cancel "$(@ AgentBrowser terminationRecordsAt: session.txt | head -n 1 | jq -r .id)"
printf '%s\n' "terminate:$session" cancel '' > "$PICKS"
@ AgentSession browse >/dev/null
contains 'session list binds Ctrl-D to terminate' '--ctrl-d-action terminate' "$(cat "$PICK_ARGS")"
check 'Cancel preserves the session' open "$(@ "$session" lifecycleState)"
printf '%s\n' "terminate:$session" '' '' > "$PICKS"
@ AgentSession browse >/dev/null
check 'Escape preserves the session' open "$(@ "$session" lifecycleState)"
other=$(@ AgentSession new)
printf '%s\n' "terminate:$session" terminate '' > "$PICKS"
@ AgentSession browse >/dev/null
@ "$session" reload >/dev/null
check 'confirmed Ctrl-D terminates selected session' terminated "$(@ "$session" lifecycleState)"
check 'another session stays open' open "$(@ "$other" lifecycleState)"
check 'refreshed picker excludes terminated session' "$other" "$(tail -n 1 "$RECORDS" | jq -r .id)"
records=$(@ AgentBrowser sessionRecordsIn: "$tmp")
check 'reopening list excludes terminated session' 0 "$(printf '%s\n' "$records" | jq -s --arg id "$session" '[.[] | select(.id == $id)] | length')"
check 'termination retains original message' 'please inspect this' "$(@ "$msg" body)"
printf '%s\n' details '' > "$PICKS"
check 'terminated session remains directly browsable by id' dismissed "$(@ "$session" browse)"
contains 'direct browsing shows terminal state' 'Lifecycle: terminated' "$(cat "$PAGES")"

# A public control failure must be visible in the pager, not silently dismissed.
@ AgentBrowser terminationRecordsAt: session.txt >/dev/null
__AgentBrowser__class__terminateResultFor_() { echo 'fixture harness has not stopped'; return 1; }
printf '%s\n' "terminate:$other" terminate '' > "$PICKS"
@ AgentSession browse >/dev/null
contains 'termination failure is shown' 'fixture harness has not stopped' "$(cat "$PAGES")"
echo "=== $passed browser checks passed ==="
