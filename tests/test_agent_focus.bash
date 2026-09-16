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
mapfile -t started < <(@ Agent::Run startFor: "$session" profile: shell)
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
snapshot=$(@ Agent::Transcript snapshotFor: "$session" limit: 400) || exit 1
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
if os.environ.get('FOCUS_TRANSIENT_AUTH_FAILURE'):
    # Force exactly one or two idle frame authorization failures, then prove
    # that the bridge is still alive by completing a normal request.
    import sqlite3,time
    db=os.environ['SQLITE_JSON_DB']; identity=os.environ['FOCUS_IDENTITY']
    con=sqlite3.connect(db)
    con.execute("UPDATE instances SET data=json_set(data,'$.enabled','false') WHERE id=?",(identity,)); con.commit()
    time.sleep(1.3)
    con.execute("UPDATE instances SET data=json_set(data,'$.enabled','true') WHERE id=?",(identity,)); con.commit(); con.close()
    print(json.dumps({'schema_version':1,'request_id':1,'intent':'load_older'}),flush=True)
    for line in sys.stdin:
        frame=json.loads(line)
        if frame.get('type')=='ack' and frame.get('request_id')==1:
            open(os.environ['FOCUS_RETRY_ACK'],'w').write('ack')
            print(json.dumps({'schema_version':1,'request_id':2,'intent':'dismiss'}),flush=True)
            break
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
        assert not frame['ok'], frame
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
check 'unsupported driver never falls back to inbox on retransmission' 1 "$(@ "$session" pendingCount)"
export FOCUS_CLOSE_EARLY=1
check 'closing during an acknowledgement returns dismissed' dismissed "$(@ "$session" focus 2> "$tmp/early-close-errors")"
check 'closing the UI emits no broken-pipe diagnostic' '' "$(cat "$tmp/early-close-errors")"
check 'closing during an acknowledgement leaves the process alive' true "$(@ "$run" isProcessAlive)"
unset FOCUS_CLOSE_EARLY
export FOCUS_TRANSIENT_AUTH_FAILURE=1 FOCUS_IDENTITY="$identity" FOCUS_RETRY_ACK="$tmp/retry-ack"
check 'transient refresh authorization failure does not dismiss the view' dismissed "$(@ "$session" focus)"
check 'view accepts a request after transient refresh failure' ack "$(cat "$tmp/retry-ack")"
unset FOCUS_TRANSIENT_AUTH_FAILURE FOCUS_IDENTITY FOCUS_RETRY_ACK
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
check 'message jump opens the exact current session' dismissed "$(@ Agent::Browser focusSenderOf: "$reply" in: "$tmp")"
check 'message jump passes the session to the applet' "$session" "$(jq -r .session.id "$FOCUS_CAPTURE")"

context=$(jq -cn --arg session "$session" '{session:$session,window:400}')
# A retained view continues to refer to the same active conversation.
frame=$(jq -cn --arg id "$message" '{schema_version:1,request_id:20,intent:"mark_viewed",message_ids:[$id]}')
before_order=$(@ Agent::Transcript recordsFor: "$session" limit: 400 | jq -r --arg id "$message" '.rows[]|select(.id==$id)|.seq')
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'displayed message intent marks it read' read "$(@ "$message" status)"
after_order=$(@ Agent::Transcript recordsFor: "$session" limit: 400 | jq -r --arg id "$message" '.rows[]|select(.id==$id)|.seq')
check 'marking read preserves historical ordering' "$before_order" "$after_order"
check 'displayed message does not acknowledge processing' 1 "$(@ "$session" pendingCount)"
foreign=$(@ Inbox send: 'unrelated' to: someone-else from: someone-else)
frame=$(jq -cn --arg id "$foreign" '{schema_version:1,request_id:21,intent:"mark_viewed",message_ids:[$id]}')
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'unrelated message cannot be marked through this view' false "$(field "$result" .frame.ok)"
check 'unrelated message stays unread' unread "$(@ "$foreign" status)"
frame='{"schema_version":1,"request_id":3,"intent":"load_older"}'
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'explicit earlier-history intent expands bounded window' 800 "$(field "$result" .context.window)"
# The idle poll probe: identical state yields the same token, and anything the
# frame authorizes or projects changes it, so a skipped refresh is never stale.
token=$(@ Agent::Focus changeTokenFor: "$context")
check 'change token is a digest' 64 "${#token}"
check 'change token is stable while nothing changed' "$token" "$(@ Agent::Focus changeTokenFor: "$context")"
check 'change token follows the view context' true "$([[ $(@ Agent::Focus changeTokenFor: "$(field "$result" .context)") != "$token" ]] && echo true)"
printf 'later native output\n' >> "$directory/stdout.log"
after_log=$(@ Agent::Focus changeTokenFor: "$context")
check 'change token follows run log growth' true "$([[ $after_log != "$token" ]] && echo true)"
later=$(@ Inbox send: 'later note' to: focus-owner from: "session:$session")
after_mail=$(@ Agent::Focus changeTokenFor: "$context")
check 'change token follows session mail' true "$([[ $after_mail != "$after_log" ]] && echo true)"
@ "$later" markViewed >/dev/null
check 'change token follows message status' true "$([[ $(@ Agent::Focus changeTokenFor: "$context") != "$after_mail" ]] && echo true)"
if @ Agent::Focus changeTokenFor: '{"window":400}' >/dev/null 2>&1; then echo 'FAIL: token without a session'; exit 1; fi
passed=$((passed+1))
if @ Agent::Focus handleFrame: '{"schema_version":1,"request_id":4,"intent":"eval","body":"bad"}' context: "$context" >/dev/null 2>&1; then echo 'FAIL: arbitrary intent accepted'; exit 1; fi
passed=$((passed+1))
check 'message resolves its originating current session' "$session" "$(@ "$reply" senderSessions)"
# Stop the real run before exercising closed/replacement history.
frame=$(jq -cn --arg run invalid '{schema_version:1,request_id:5,intent:"interrupt_run",run_id:$run}')
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'foreign/stale run is rejected' false "$(field "$result" .frame.ok)"
check 'foreign stop leaves active run alive' true "$(@ "$run" isProcessAlive)"
frame=$(jq -cn --arg run "$run" '{schema_version:1,request_id:6,intent:"interrupt_run",run_id:$run}')
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'explicit stop succeeds through the worker' true "$(field "$result" .frame.ok)"
check 'explicit stop actually stops the native process' false "$(@ "$run" isProcessAlive)"
check 'explicit stop retains existing pause semantics' paused "$(@ "$session" lifecycleState)"
ignored=$(@ "$session" close)
frame='{"schema_version":1,"request_id":19,"intent":"send_message","body":"must not send into a closed session"}'
result=$(@ Agent::Focus handleFrame: "$frame" context: "$context")
check 'send refreshes lifecycle changed by another process' false "$(field "$result" .frame.ok)"
check 'rejected send does not create a delivery' 1 "$(@ "$session" pendingCount)"
next=$(@ Gusgus sessionFor: "$tmp/workspace")
check 'message jumps to replacement conversation' "$next" "$(@ "$reply" senderSessions)"
other=$(@ Gusgus sessionFor: "$tmp")
check 'another directory resolves the same current session' "$next" "$other"
@ Agent::Browser focusSenderOf: "$reply" in: "$tmp" >/dev/null
check 'old message attaches to current identity conversation' "$next" "$(jq -r .session.id "$FOCUS_CAPTURE")"
check 'focusCurrent attaches without creating a session' dismissed "$(@ Gusgus focusCurrent)"
check 'focusCurrent selects the replacement' "$next" "$(jq -r .session.id "$FOCUS_CAPTURE")"
@ "$next" close >/dev/null
check 'no current session retains originating history' "$session" "$(@ "$reply" senderSessions)"
unknown=$(@ Message to: focus-owner from: 'session:not-real' subject: '' body: '' kind: note)
check 'missing origin offers no fabricated destination' '' "$(@ "$unknown" senderSessions)"
export FOCUS_PICK_ID=session
check 'unknown sender menu omits an unusable jump' back "$(@ "$inbox" pickActionFor: "$unknown" in: "$tmp")"
printf '%d agent focus checks passed\n' "$passed"
