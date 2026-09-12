#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs" TRASHTALK_USER=focus-owner TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
unset TRASHTALK_RUN_TOKEN
cleanup() {
    if [[ -n ${pid:-} ]]; then kill -TERM -- "-$pid" 2>/dev/null || true; fi
    rm -rf "$tmp"
}
trap cleanup EXIT
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; passed=$((passed+1)); else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
field() { jq -r "$2" <<< "$1"; }
mkdir -p "$tmp/bin" "$tmp/workspace"
session=$(@ Gusgus sessionFor: "$tmp/workspace")
identity=$(@ "$session" identity)
message=$(@ Inbox send: 'initial question' to: "agent:gusgus" from: focus-owner)
mapfile -t started < <(@ AgentRun startFor: "$session" profile: shell)
run=${started[0]}
directory="$TRASHTALK_RUN_DIR/$run"
mkdir -p "$directory"
argv=$(jq -cn --arg bash "$BASH" '[$bash,"-c","printf \"first output\\n\"; sleep 180"]')
pid=$(@ Tool detachArgvJson: "$argv" stdinFile: /dev/null dir: "$directory")
@ "$run" processPid: "$pid"
@ "$run" pidFile: "$directory/pid"
@ "$run" exitFile: "$directory/exit"
@ "$run" outputLog: "$directory/stdout.log"
@ "$run" errorLog: "$directory/stderr.log"
@ "$run" save
@ "$run" transitionTo: running >/dev/null
reply=$(@ Inbox send: 'origin reply' to: focus-owner from: "session:$session")
snapshot=$(@ AgentTranscript snapshotFor: "$session" limit: 400) || exit 1
check 'snapshot identifies its pinned session' "$session" "$(field "$snapshot" .session.id)"
check 'snapshot names the identity' gusgus "$(field "$snapshot" .session.title)"
check 'snapshot retains active run' "$run" "$(field "$snapshot" .session.run_id)"
check 'snapshot includes identity-addressed mail via delivery' true "$(jq -r --arg id "$message" '.entries|any(.id==$id)' <<< "$snapshot")"
check 'snapshot excludes native plain output' false "$(field "$snapshot" '.entries|any(.text=="first output")')"
check 'snapshot never exposes capability hashes' false "$(field "$snapshot" 'tostring|contains("capabilityTokenHash")')"
check 'snapshot alone does not mark mail read' unread "$(@ "$message" status)"
check 'snapshot never settles deliveries' 1 "$(@ "$session" pendingCount)"

# Exercise the actual exact-argv duplex bridge through the public focus method.
export FOCUS_CAPTURE="$tmp/capture.jsonl"
cat > "$tmp/bin/inagent" <<'PY'
#!/usr/bin/env python3
import sys,json,os
first=json.loads(sys.stdin.readline())
with open(os.environ['FOCUS_CAPTURE'],'w') as log: log.write(json.dumps(first)+'\n')
if os.environ.get('FOCUS_CLOSE_EARLY'):
    # Exit during an acknowledgement, as a real UI can after a final read event.
    print(json.dumps({'schema_version':1,'request_id':1,'intent':'load_older'}),flush=True)
    sys.stdin.close()
    sys.exit(0)
if os.environ.get('FOCUS_VIEW_ONLY'):
    if os.environ['FOCUS_VIEW_ONLY']=='live':
        with open(os.environ['FOCUS_NATIVE_OUTPUT'],'a') as log:log.write('new live native output\n')
        for line in sys.stdin:
            frame=json.loads(line)
            if frame.get('type')=='snapshot' and any(e['text']=='new live native output' for e in frame['entries']):break
    print(json.dumps({'schema_version':1,'request_id':1,'intent':'dismiss'}),flush=True)
    sys.exit(0)
request={'schema_version':1,'request_id':1,'intent':'send_message','body':'literal $(touch unexpected); "quotes"\nsecond line\n'}
print(json.dumps(request),flush=True)
for line in sys.stdin:
    frame=json.loads(line)
    with open(os.environ['FOCUS_CAPTURE'],'a') as log: log.write(json.dumps(frame)+'\n')
    if frame.get('type')=='ack' and frame.get('request_id')==1:
        assert frame['ok'], frame
        print(json.dumps(request),flush=True) # retransmission must not redeliver
        break
for line in sys.stdin:
    frame=json.loads(line)
    if frame.get('type')=='ack':
        print(json.dumps({'schema_version':1,'request_id':2,'intent':'dismiss'}),flush=True)
        break
PY
chmod +x "$tmp/bin/inagent"
export PATH="$tmp/bin:$PATH"
check 'attach/detach returns a clean outcome' dismissed "$(@ "$session" focus)"
check 'detach leaves the real detached process alive' true "$(@ "$run" isProcessAlive)"
check 'detach leaves session lifecycle alone' open "$(@ "$session" lifecycleState)"
check 'composer delivered once through inbox despite retransmission' 2 "$(@ "$session" pendingCount)"
sent=$(jq -r 'select(.type=="ack")|.result' "$FOCUS_CAPTURE" | head -1)
check 'composer preserves literal text and trailing newline' true "$(db_get "$sent" | jq '.body == "literal $(touch unexpected); \"quotes\"\nsecond line\n"')"
check 'composer attributes the human' focus-owner "$(@ "$sent" from)"
check 'composer targets exactly the attached session' "session:$session" "$(@ "$sent" to)"
export FOCUS_CLOSE_EARLY=1
check 'closing during an acknowledgement returns dismissed' dismissed "$(@ "$session" focus 2> "$tmp/early-close-errors")"
check 'closing the UI emits no broken-pipe diagnostic' '' "$(cat "$tmp/early-close-errors")"
check 'closing during an acknowledgement leaves the process alive' true "$(@ "$run" isProcessAlive)"
unset FOCUS_CLOSE_EARLY
export FOCUS_VIEW_ONLY=1
check 'attached view is limited to durable chat messages' dismissed "$(@ "$session" focus)"
export FOCUS_VIEW_ONLY=1 FOCUS_PICK_ID=session FOCUS_PICK_RECORDS="$tmp/picker-records"
cat > "$tmp/bin/inpick" <<'PICK'
#!/usr/bin/env bash
cat > "$FOCUS_PICK_RECORDS"
jq -sc --arg id "$FOCUS_PICK_ID" '{outcome:"selected",selection:(map(select(.id==$id))[0])}' "$FOCUS_PICK_RECORDS"
PICK
chmod +x "$tmp/bin/inpick"
inbox=$(@ Trash userInbox)
check 'message menu offers a session jump' session "$(@ "$inbox" pickActionFor: "$reply" in: "$tmp")"
check 'message jump opens the exact current session' dismissed "$(@ AgentBrowser focusSenderOf: "$reply" in: "$tmp")"
check 'message jump passes the session to the applet' "$session" "$(jq -r .session.id "$FOCUS_CAPTURE")"

context=$(jq -cn --arg session "$session" '{session:$session,window:400}')
# A separate process changes lifecycle while this shell retains an older cache.
ignored=$(@ Store patch: "$session" with: '{"lifecycleState":"closed"}')
frame='{"schema_version":1,"request_id":19,"intent":"send_message","body":"must not send into a closed session"}'
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'send refreshes lifecycle changed by another process' false "$(field "$result" .frame.ok)"
check 'rejected send does not create a delivery' 2 "$(@ "$session" pendingCount)"
@ "$session" reopen >/dev/null
frame=$(jq -cn --arg id "$message" '{schema_version:1,request_id:20,intent:"mark_viewed",message_ids:[$id]}')
before_order=$(@ AgentTranscript recordsFor: "$session" limit: 400 | jq -r --arg id "$message" '.rows[]|select(.id==$id)|.seq')
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'displayed message intent marks it read' read "$(@ "$message" status)"
after_order=$(@ AgentTranscript recordsFor: "$session" limit: 400 | jq -r --arg id "$message" '.rows[]|select(.id==$id)|.seq')
check 'marking read preserves historical ordering' "$before_order" "$after_order"
check 'displayed message does not acknowledge processing' 2 "$(@ "$session" pendingCount)"
foreign=$(@ Inbox send: 'unrelated' to: someone-else from: someone-else)
frame=$(jq -cn --arg id "$foreign" '{schema_version:1,request_id:21,intent:"mark_viewed",message_ids:[$id]}')
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'unrelated message cannot be marked through this view' false "$(field "$result" .frame.ok)"
check 'unrelated message stays unread' unread "$(@ "$foreign" status)"
frame='{"schema_version":1,"request_id":3,"intent":"load_older"}'
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'explicit earlier-history intent expands bounded window' 800 "$(field "$result" .context.window)"
if @ AgentFocus handleFrame: '{"schema_version":1,"request_id":4,"intent":"eval","body":"bad"}' context: "$context" >/dev/null 2>&1; then echo 'FAIL: arbitrary intent accepted'; exit 1; fi
passed=$((passed+1))
check 'message resolves its originating current session' "$session" "$(@ "$reply" senderSessions)"
ignored=$(@ "$session" close)
next=$(@ Gusgus sessionFor: "$tmp/workspace")
check 'message jumps to replacement in the same workspace' "$next" "$(@ "$reply" senderSessions)"
other=$(@ Gusgus sessionFor: "$tmp")
check 'different workspace does not steal origin jump' "$next" "$(@ "$reply" senderSessions)"
extra=$(@ AgentSession openFor: "$identity" archetype: "$(@ "$next" archetype)" role: "$(@ "$next" role)" workspace: "$(@ "$next" workspace)" profile: shell)
export FOCUS_PICK_ID="$extra"
@ AgentBrowser focusSenderOf: "$reply" in: "$tmp" >/dev/null
check 'ambiguous replacement sessions use the picker selection' "$extra" "$(jq -r .session.id "$FOCUS_CAPTURE")"
check 'replacement picker excludes other workspaces' false "$(jq -s --arg id "$other" 'any(.id==$id)' "$FOCUS_PICK_RECORDS")"
@ "$extra" close >/dev/null
@ "$next" close >/dev/null
check 'no current session retains originating history' "$session" "$(@ "$reply" senderSessions)"
unknown=$(@ Message to: focus-owner from: 'session:not-real' subject: '' body: '' kind: note)
check 'missing origin offers no fabricated destination' '' "$(@ "$unknown" senderSessions)"
export FOCUS_PICK_ID=session
check 'unknown sender menu omits an unusable jump' back "$(@ "$inbox" pickActionFor: "$unknown" in: "$tmp")"
@ "$session" reopen >/dev/null
frame=$(jq -cn --arg run invalid '{schema_version:1,request_id:5,intent:"interrupt_run",run_id:$run}')
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'foreign/stale run is rejected' false "$(field "$result" .frame.ok)"
check 'foreign stop leaves active run alive' true "$(@ "$run" isProcessAlive)"
frame=$(jq -cn --arg run "$run" '{schema_version:1,request_id:6,intent:"interrupt_run",run_id:$run}')
result=$(@ AgentFocus handleFrame: "$frame" context: "$context")
check 'explicit stop succeeds through the worker' true "$(field "$result" .frame.ok)"
check 'explicit stop actually stops the native process' false "$(@ "$run" isProcessAlive)"
check 'explicit stop retains existing pause semantics' paused "$(@ "$session" lifecycleState)"
printf '%d agent focus checks passed\n' "$passed"
