#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=chad-tester
export TRASHTALK_GUSGUS_PROFILE=chad
unset TRASHTALK_CHAD_MODEL TRASHTALK_CHAD_THINK_BUDGET TRASHTALK_CHAD_SANDBOX CHAD_SESSION_DIR CHAD_NO_SEATBELT
export CHAD_TEST_LOG="$tmp/log"
mkdir "$tmp/bin" "$CHAD_TEST_LOG" "$tmp/workspace with spaces"
cat > "$tmp/bin/chad" <<'CHAD'
#!/usr/bin/env bash
if [[ "$1" == --version ]]; then echo 'chad 0.0.0-fixture'; exit 0; fi
id=${TRASHTALK_RUN_TOKEN%%:*}
printf '%s\n' "$@" > "$CHAD_TEST_LOG/$id.argv"
pwd > "$CHAD_TEST_LOG/$id.cwd"
printf '%s|%s|%s\n' "${CHAD_SESSION_DIR:-}" "${CHAD_NO_SEATBELT:-}" "${CHAD_AUTO_CONTINUE:-}" > "$CHAD_TEST_LOG/$id.env"
prompt=${!#}
printf '%s\n' "$prompt" > "$CHAD_TEST_LOG/$id.input"
store="$CHAD_SESSION_DIR/fixturehash"
mkdir -p "$store"
sid="2026100$((RANDOM % 9 + 1))-120000-$id"
printf '{"messages":[]}\n' > "$store/$sid.json"
printf '{"sessions":{}}\n' > "$store/index.json"
case "${CHAD_TEST_MODE:-success}" in
    notdone) echo 'guard stop' >&2; exit 1 ;;
    nochange|nochange_unsettled)
        ts="$TRASHTALK_RUN_DIR/$id/trash-send"
        if [[ "$CHAD_TEST_MODE" == nochange ]]; then
            "$ts" Agent::Run result: 'Chad reply, no file change' >/dev/null || exit
            for delivery in $(printf '%s\n' "$prompt" | sed -n 's/^--- delivery //p'); do
                "$ts" Agent::Run settle: "$delivery" >/dev/null || exit
            done
        fi
        echo '  [stopped: the model called done, but no change passed a check]' >&2
        exit 1 ;;
    interrupted) exit 130 ;;
    convo_gate)
        sleep 1
        printf '{"messages":[{"role":"user","content":"q"},{"role":"assistant","content":"(Thinking: plan)\\n</think>\\n\\nFixture answer"}]}\n' > "$store/$sid.json"
        echo '  [stopped: the model said it was finished, but no change passed a check]' >&2
        echo "[stopped: the turn ended without applying a verified change - say 'continue' to resume]"
        exit 1 ;;
    convo_clean) sleep 1; echo 'Clean stdout answer'; exit 0 ;;
    wait) touch "$CHAD_TEST_LOG/$id.waiting"; while :; do sleep 1; done ;;
esac
ts="$TRASHTALK_RUN_DIR/$id/trash-send"
for message in $(printf '%s\n' "$prompt" | sed -n 's/^Message: //p'); do
    inbox=$("$ts" Inbox named: "$("$ts" "$message" to)") || exit
    "$ts" "$inbox" show: "$message" >/dev/null || exit
done
"$ts" Agent::Run result: 'Chad fixture reply' >/dev/null || exit
for delivery in $(printf '%s\n' "$prompt" | sed -n 's/^--- delivery //p'); do
    "$ts" Agent::Run settle: "$delivery" >/dev/null || exit
done
echo 'done'
CHAD
chmod +x "$tmp/bin/chad"
export PATH="$tmp/bin:$PATH"
db_init
passed=0
source tests/helpers/check.bash
field() { db_get "$1" | jq -r --arg f "$2" '.[$f] // empty'; }
settle() {
    local i
    for i in {1..100}; do
        @ Agent::Worker tickSession: "$session" >/dev/null 2>&1
        [[ -n "$(@ "$session" activeRun)" ]] || return 0
        sleep 0.1
    done
    echo 'FAIL: harness did not finish'; exit 1
}
check 'Gusgus honors the explicit chad profile' chad "$(@ Gusgus profile)"
check 'worker resolves chad driver' Agent::ChadDriver "$(@ Agent::Worker driverFor: chad)"
check 'chad defaults to its own model' '' "$(@ Config at: 'chad.model')"
check 'sandbox is off by default' off "$(@ Config at: 'chad.sandbox')"
session=$(@ Gusgus sessionFor: "$tmp/workspace with spaces")
msg=$(@ Inbox send: FIRST_CHAD_SECRET to: "session:$session" from: chad-tester)
run=$(@ Agent::Worker tickSession: "$session")
settle
check 'chad launch completes through worker' succeeded "$(field "$run" state)"
check 'chad reads message contents from Inbox' read "$(field "$msg" status)"
check 'chad prompt omits the message body' 0 "$(grep -c FIRST_CHAD_SECRET "$CHAD_TEST_LOG/$run.input")"
check 'new session snapshots chad profile' chad "$(field "$session" backendProfile)"
check 'workspace is the working directory' "$(cd "$tmp/workspace with spaces" && pwd -P)" "$(cat "$CHAD_TEST_LOG/$run.cwd")"
contains 'prompt follows the option terminator' $'--yolo\n--\n' "$(cat "$CHAD_TEST_LOG/$run.argv")"
check 'unset model leaves chad default' 0 "$(grep -c -- --model "$CHAD_TEST_LOG/$run.argv")"
check 'unset think budget omits the flag' 0 "$(grep -c -- --think-budget "$CHAD_TEST_LOG/$run.argv")"
check 'fresh run does not continue' 0 "$(grep -c -- --continue "$CHAD_TEST_LOG/$run.argv")"
check 'store is per session and seatbelt is off' "$tmp/runs/chad-sessions/$session|1|0" "$(cat "$CHAD_TEST_LOG/$run.env")"
check 'store is private' 700 "$(stat -f %Lp "$tmp/runs/chad-sessions/$session" 2>/dev/null || stat -c %a "$tmp/runs/chad-sessions/$session")"
ref=$(field "$session" lastConversationRef)
contains 'session remembers the newest chad conversation' "-$run" "$ref"
inbox=$(@ Inbox named: chad-tester)
reply=$(@ "$inbox" unread)
check 'chad answer reaches inbox' 'Chad fixture reply' "$(@ "$reply" body)"
check 'answer stays in thread' "$msg" "$(@ "$reply" replyTo)"
msg2=$(@ "$reply" reply: again)
export TRASHTALK_CHAD_MODEL=mlx-community/fixture TRASHTALK_CHAD_THINK_BUDGET=256 TRASHTALK_CHAD_SANDBOX=on
run2=$(@ Agent::Worker tickSession: "$session")
settle
check 'resumed chad run completes' succeeded "$(field "$run2" state)"
contains 'resume continues the latest conversation' $'--yolo\n' "$(cat "$CHAD_TEST_LOG/$run2.argv")"
check 'resume passes --continue' 1 "$(grep -c -- --continue "$CHAD_TEST_LOG/$run2.argv")"
contains 'configured model reaches chad' $'--model\nmlx-community/fixture' "$(cat "$CHAD_TEST_LOG/$run2.argv")"
contains 'think budget reaches chad' $'--think-budget\n256' "$(cat "$CHAD_TEST_LOG/$run2.argv")"
check 'sandbox on leaves seatbelt enabled' "$tmp/runs/chad-sessions/$session||0" "$(cat "$CHAD_TEST_LOG/$run2.env")"
check 'both runs share one session store' 2 "$(ls "$tmp/runs/chad-sessions/$session"/fixturehash/2*.json | wc -l | tr -d ' ')"
unset TRASHTALK_CHAD_MODEL TRASHTALK_CHAD_THINK_BUDGET TRASHTALK_CHAD_SANDBOX
check 'a zero think budget is not passed' 0 "$(@ Agent::ChadDriver argvFor: /bin/chad workspace: /w store: /s model: '' think: 0 sandbox: off promptFile: /p ref: '' | grep -c think-budget)"
check 'every delivery processed' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Agent::Delivery' AND json_extract(data,'$.state')!='processed';")"

# Exit status classifies the run; a clean exit is the only result.
export CHAD_TEST_MODE=notdone
@ Inbox send: stall to: "session:$session" from: chad-tester >/dev/null
run3=$(@ Agent::Worker tickSession: "$session")
settle
check 'exit 1 fails the run' failed "$(field "$run3" state)"
contains 'exit 1 is named as a guard stop' 'stopped before finishing' "$(field "$run3" error)"
contains 'exit 1 keeps the stderr tail' 'guard stop' "$(field "$run3" error)"
check 'failed run requires review' 1 "$(@ "$session" stalledCount)"
check 'exit 1 is not a result' false "$(@ Agent::ChadDriver resultSeenFor: "$run3")"
echo 130 > "$(field "$run3" exitFile)"
contains 'exit 130 is named as an interrupt' 'interrupted' "$(@ Agent::ChadDriver errorFor: "$run3")"
echo 0 > "$(field "$run3" exitFile)"
check 'exit 0 is a result' true "$(@ Agent::ChadDriver resultSeenFor: "$run3")"

# chad's no-empty-diff gate rejects a reply-only turn as "no change passed a
# check". That is a result when the deliveries were settled, never when not.
mkdir "$tmp/nochange-workspace"
session=$(@ Gusgus fresh: "$tmp/nochange-workspace")
export CHAD_TEST_MODE=nochange
@ Inbox send: reply-only to: "session:$session" from: chad-tester >/dev/null
run4=$(@ Agent::Worker tickSession: "$session")
settle
check 'reply-only turn rejected by the no-change gate is a result' true "$(@ Agent::ChadDriver resultSeenFor: "$run4")"
check 'settled reply-only turn succeeds' succeeded "$(field "$run4" state)"
export CHAD_TEST_MODE=nochange_unsettled
session=$(@ Gusgus fresh: "$tmp/nochange-workspace")
@ Inbox send: unanswered to: "session:$session" from: chad-tester >/dev/null
run5=$(@ Agent::Worker tickSession: "$session")
settle
check 'no-change stop is still a result for the worker to judge' true "$(@ Agent::ChadDriver resultSeenFor: "$run5")"
check 'unsettled no-change turn is not a success' true "$([[ "$(field "$run5" state)" != succeeded ]] && echo true)"
echo 1 > "$(field "$run3" exitFile)"
check 'a guard stop stays a failure' false "$(@ Agent::ChadDriver resultSeenFor: "$run3")"
unset CHAD_TEST_MODE

# Direct conversation input launches without a live-input driver and projects
# both sides into the run's conversation log. The no-change gate leaves only a
# stop notice on stdout, so that answer comes from chad's saved conversation.
mkdir "$tmp/convo-workspace"
session=$(@ Gusgus fresh: "$tmp/convo-workspace")
export CHAD_TEST_MODE=convo_gate
check 'idle conversation input is acknowledged' 'Input sent directly to the session' "$(@ "$session" input: 'hello chad' 2>&1)"
convo=$(@ "$session" activeRun)
convo_dir="$TRASHTALK_RUN_DIR/$convo"
for i in {1..100}; do [[ -f "$convo_dir/exit" ]] && break; sleep .1; done
check 'conversation run is a conversation' conversation "$(field "$convo" purpose)"
@ Agent::Worker reconcileRun: "$convo" >/dev/null
@ Agent::ChadDriver outcomeOf: "$convo" >/dev/null
check 'conversation run succeeds' succeeded "$(field "$convo" state)"
check 'user text is projected' 'hello chad' "$(jq -rs '[.[]|select(.kind=="user")][0].text' "$convo_dir/conversation.jsonl")"
check 'gate stop projects the saved answer' 'Fixture answer' "$(jq -rs '[.[]|select(.kind=="assistant_delta")][0].text' "$convo_dir/conversation.jsonl")"
check 'answer is projected once' 1 "$(jq -s '[.[]|select(.kind=="assistant_delta")]|length' "$convo_dir/conversation.jsonl")"
session=$(@ Gusgus fresh: "$tmp/convo-workspace")
export CHAD_TEST_MODE=convo_clean
@ "$session" input: 'again' >/dev/null 2>&1
convo=$(@ "$session" activeRun)
convo_dir="$TRASHTALK_RUN_DIR/$convo"
for i in {1..100}; do [[ -f "$convo_dir/exit" ]] && break; sleep .1; done
@ Agent::Worker reconcileRun: "$convo" >/dev/null
check 'clean exit projects stdout' 'Clean stdout answer' "$(jq -rs '[.[]|select(.kind=="assistant_delta")][0].text' "$convo_dir/conversation.jsonl")"
unset CHAD_TEST_MODE

# A missing executable records a diagnostic without launching.
mapfile -t started < <(@ Agent::Run startFor: "$session" profile: chad)
missing=${started[0]}
no_chad_path=$(IFS=:; for dir in $PATH; do [[ -x "$dir/chad" ]] || printf '%s:' "$dir"; done)
PATH=${no_chad_path%:} @ Agent::ChadDriver launch: "$CHAD_TEST_LOG/$run.input" run: "$missing" token: "${started[1]}" >/dev/null 2>&1; status=$?
check 'missing chad prevents launch' 1 "$status"
check 'launch failure stays before launch' starting "$(field "$missing" state)"
contains 'missing chad gives install guidance' 'chad is not installed' "$(@ Agent::ChadDriver errorFor: "$missing")"

# The queue and stop contract work without native prompt steering.
mkdir "$tmp/stop-workspace"
export CHAD_TEST_MODE=wait
@ "$missing" transitionTo: failed >/dev/null
session=$(@ Gusgus fresh: "$tmp/stop-workspace")
@ Inbox send: 'long chad task' to: "session:$session" from: chad-tester >/dev/null
busy=$(@ Agent::Worker tickSession: "$session")
for i in {1..100}; do [[ -e "$CHAD_TEST_LOG/$busy.waiting" ]] && break; sleep .1; done
check 'chad work is active' true "$(@ "$busy" isProcessAlive)"
@ Inbox send: 'queued chad followup' to: "session:$session" from: chad-tester >/dev/null
check 'busy chad does not launch overlapping work' '' "$(@ Agent::Worker tickSession: "$session")"
check 'chad followup stays queued' 1 "$(@ "$session" pendingCount)"
check 'common stop interrupts active chad' interrupted "$(@ "$busy" stop)"
check 'chad process is confirmed stopped' false "$(@ "$busy" isProcessAlive)"
check 'chad queue remains paused after stop' paused "$(field "$session" lifecycleState)"
check 'queued chad input retained' 1 "$(@ "$session" pendingCount)"
echo "=== $passed chad driver checks passed ==="
