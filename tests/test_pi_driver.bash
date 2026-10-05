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
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=pi-tester
export TRASHTALK_GUSGUS_PROFILE=pi
unset TRASHTALK_PI_MODEL TRASHTALK_PI_EXCLUDE_TOOLS TRASHTALK_PI_EXTENSION_PATHS
export PI_TEST_LOG="$tmp/log"
mkdir "$tmp/bin" "$PI_TEST_LOG" "$tmp/workspace with spaces"
cat > "$tmp/bin/pi" <<'PI'
#!/usr/bin/env bash
if [[ "$1" == --version ]]; then echo 'pi 0.0.0-fixture'; exit 0; fi
id=${TRASHTALK_RUN_TOKEN%%:*}
printf '%s\n' "$@" > "$PI_TEST_LOG/$id.argv"
pwd > "$PI_TEST_LOG/$id.cwd"
prompt=$(cat)
printf '%s\n' "$prompt" > "$PI_TEST_LOG/$id.input"
echo '{"type":"session","version":3,"id":"pi-fixture-session","cwd":"."}'
echo '{"type":"agent_start"}'
if [[ "${PI_TEST_MODE:-success}" == error ]]; then
    echo '{"type":"message_end","message":{"role":"assistant","stopReason":"error","errorMessage":"fixture provider failure","content":[]}}'
    echo '{"type":"agent_end","messages":[],"willRetry":false}'
    exit 0
fi
if [[ "${PI_TEST_MODE:-success}" == wait ]]; then
    touch "$PI_TEST_LOG/$id.waiting"
    while :; do sleep 1; done
fi
ts="$TRASHTALK_RUN_DIR/$id/trash-send"
for message in $(printf '%s\n' "$prompt" | sed -n 's/^Message: //p'); do
    inbox=$("$ts" Inbox named: "$("$ts" "$message" to)") || exit
    "$ts" "$inbox" show: "$message" >/dev/null || exit
done
"$ts" Agent::Run result: 'Pi fixture reply' >/dev/null || exit
for delivery in $(printf '%s\n' "$prompt" | sed -n 's/^--- delivery //p'); do
    "$ts" Agent::Run settle: "$delivery" >/dev/null || exit
done
echo '{"type":"message_end","message":{"role":"assistant","stopReason":"stop","content":[{"type":"text","text":"done"}]}}'
echo '{"type":"agent_end","messages":[],"willRetry":false}'
PI
chmod +x "$tmp/bin/pi"
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
check 'Gusgus honors the explicit pi profile' pi "$(@ Gusgus profile)"
check 'worker resolves pi driver' Agent::PiDriver "$(@ Agent::Worker driverFor: pi)"
check 'legacy profiles still resolve Codex' Agent::CodexDriver "$(@ Agent::Worker driverFor: assistant-low-power)"
check 'pi defaults to its own model' '' "$(@ Config at: 'pi.model')"
session=$(@ Gusgus sessionFor: "$tmp/workspace with spaces")
msg=$(@ Inbox send: FIRST_PI_SECRET to: "session:$session" from: pi-tester)
run=$(@ Agent::Worker tickSession: "$session")
settle
check 'pi launch completes through worker' succeeded "$(field "$run" state)"
check 'pi reads message contents from Inbox' read "$(field "$msg" status)"
check 'pi notification omits the message body' 0 "$(grep -c FIRST_PI_SECRET "$PI_TEST_LOG/$run.input")"
check 'new session snapshots pi profile' pi "$(field "$session" backendProfile)"
check 'workspace argv is preserved' "$(cd "$tmp/workspace with spaces" && pwd -P)" "$(cat "$PI_TEST_LOG/$run.cwd")"
contains 'json print mode' $'--mode\njson\n--print' "$(cat "$PI_TEST_LOG/$run.argv")"
check 'unset model leaves pi default' 0 "$(grep -c -- --model "$PI_TEST_LOG/$run.argv")"
contains 'background tools are excluded by default' $'--exclude-tools\nbg_delegate,bg_result,bg_run,bg_run_pi_attested,bg_status,bg_logs,bg_kill' "$(cat "$PI_TEST_LOG/$run.argv")"
check 'extensions are discovered by default' 0 "$(grep -c -- --no-extensions "$PI_TEST_LOG/$run.argv")"
check 'fresh run starts a new pi session' 0 "$(grep -c -- --session-id "$PI_TEST_LOG/$run.argv")"
check 'session remembers pi reference' pi-fixture-session "$(field "$session" lastConversationRef)"
inbox=$(@ Inbox named: pi-tester)
reply=$(@ "$inbox" unread)
check 'pi answer reaches inbox' 'Pi fixture reply' "$(@ "$reply" body)"
check 'answer stays in thread' "$msg" "$(@ "$reply" replyTo)"
msg2=$(@ "$reply" reply: again)
export TRASHTALK_PI_MODEL=omlx/fixture TRASHTALK_PI_EXCLUDE_TOOLS=custom_tool TRASHTALK_PI_EXTENSION_PATHS='/a b/one.ts, /two.js'
run2=$(@ Agent::Worker tickSession: "$session")
settle
check 'resumed pi run completes' succeeded "$(field "$run2" state)"
contains 'resume passes exact pi session id' $'--session-id\npi-fixture-session' "$(cat "$PI_TEST_LOG/$run2.argv")"
contains 'configured model reaches pi' $'--model\nomlx/fixture' "$(cat "$PI_TEST_LOG/$run2.argv")"
contains 'excluded tools are configurable' $'--exclude-tools\ncustom_tool' "$(cat "$PI_TEST_LOG/$run2.argv")"
contains 'extension paths switch to an allowlist' $'--no-extensions\n-e\n/a b/one.ts\n-e\n/two.js' "$(cat "$PI_TEST_LOG/$run2.argv")"
unset TRASHTALK_PI_MODEL TRASHTALK_PI_EXCLUDE_TOOLS TRASHTALK_PI_EXTENSION_PATHS
export TRASHTALK_PI_EXCLUDE_TOOLS=
check 'empty exclusion list omits the flag' 0 "$(@ Agent::PiDriver argvFor: /bin/pi workspace: /w model: '' exclude: '' paths: '' ref: '' | grep -c exclude-tools)"
unset TRASHTALK_PI_EXCLUDE_TOOLS
check 'every delivery processed' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Agent::Delivery' AND json_extract(data,'$.state')!='processed';")"
export PI_TEST_MODE=error
@ Inbox send: fail to: "session:$session" from: pi-tester >/dev/null
run3=$(@ Agent::Worker tickSession: "$session")
settle
check 'error stop with exit zero still fails' failed "$(field "$run3" state)"
check 'provider error retained' 'fixture provider failure' "$(field "$run3" error)"
check 'failed execution requires review' 1 "$(@ "$session" stalledCount)"

# Result classification reads the last turn: a retry that succeeds counts.
log=$(field "$run3" outputLog)
printf '%s\n' \
    '{"type":"message_end","message":{"role":"assistant","stopReason":"error","errorMessage":"earlier"}}' \
    '{"type":"agent_end","willRetry":true}' \
    '{"type":"message_end","message":{"role":"assistant","stopReason":"stop"}}' \
    '{"type":"agent_end","willRetry":false}' > "$log"
check 'successful retry determines protocol result' true "$(@ Agent::PiDriver resultSeenFor: "$run3")"
check 'earlier failure does not contaminate diagnostics' '' "$(@ Agent::PiDriver errorFor: "$run3")"
printf '%s\n' '{"type":"session","id":"x"}' > "$log"
check 'a log without agent_end is not a result' false "$(@ Agent::PiDriver resultSeenFor: "$run3")"
printf '%s\n' '{"type":"message_end","message":{"role":"assistant","stopReason":"stop"}}' '{"type":"agent_end","willRetry":true}' > "$log"
check 'a pending retry is not a result' false "$(@ Agent::PiDriver resultSeenFor: "$run3")"

# A missing executable records a diagnostic without launching.
mapfile -t started < <(@ Agent::Run startFor: "$session" profile: pi)
missing=${started[0]}
PATH=/usr/bin:/bin @ Agent::PiDriver launch: "$PI_TEST_LOG/$run.input" run: "$missing" token: "${started[1]}" >/dev/null 2>&1; status=$?
check 'missing pi prevents launch' 1 "$status"
check 'launch failure stays before launch' starting "$(field "$missing" state)"
contains 'missing pi gives install guidance' 'pi is not installed' "$(@ Agent::PiDriver errorFor: "$missing")"

# The same queue/stop contract works without native prompt steering.
mkdir "$tmp/stop-workspace"
export PI_TEST_MODE=wait
@ "$missing" transitionTo: failed >/dev/null
session=$(@ Gusgus fresh: "$tmp/stop-workspace")
@ Inbox send: 'long pi task' to: "session:$session" from: pi-tester >/dev/null
busy=$(@ Agent::Worker tickSession: "$session")
for i in {1..100}; do [[ -e "$PI_TEST_LOG/$busy.waiting" ]] && break; sleep .1; done
check 'pi work is active' true "$(@ "$busy" isProcessAlive)"
@ Inbox send: 'queued pi followup' to: "session:$session" from: pi-tester >/dev/null
check 'busy pi does not launch overlapping work' '' "$(@ Agent::Worker tickSession: "$session")"
check 'pi followup stays queued' 1 "$(@ "$session" pendingCount)"
check 'common stop interrupts active pi' interrupted "$(@ "$busy" stop)"
check 'pi process is confirmed stopped' false "$(@ "$busy" isProcessAlive)"
check 'pi queue remains paused after stop' paused "$(field "$session" lifecycleState)"
check 'pi stopped delivery needs review' 1 "$(@ "$session" stalledCount)"
check 'queued pi input retained' 1 "$(@ "$session" pendingCount)"
echo "=== $passed pi driver checks passed ==="
