#!/usr/bin/env bash
# Opt-in: two real OAuth-backed model turns, isolated from the user's inbox.
if [[ "${TRASHTALK_TEST_MAKI_LIVE:-0}" != 1 ]]; then
    echo 'SKIP: set TRASHTALK_TEST_MAKI_LIVE=1 for authenticated Maki acceptance'
    exit 0
fi
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
export TRASHTALK_RUN_DIR="$TMPDIR/maki-live-runs"
export TRASHTALK_NO_AUTOTICK=1 TRASHTALK_USER=maki-live-tester
export TRASHTALK_GUSGUS_PROFILE=maki
unset TRASHTALK_MAKI_MODEL
mkdir -p "$TMPDIR/workspace"
db_init
session=$(@ Gusgus sessionFor: "$TMPDIR/workspace")
cleanup() { @ "$session" terminate >/dev/null 2>&1 || true; }
trap cleanup EXIT
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 expected=$2 got=$3"; exit 1; }; echo "PASS: $1"; }
finish() {
    local deadline=$((SECONDS+240))
    while (( SECONDS < deadline )); do
        @ AgentWorker tickSession: "$session" >/dev/null 2>&1
        [[ -n "$(@ "$session" activeRun)" ]] || return 0
        sleep 1
    done
    echo 'FAIL: timed out waiting for Maki'; exit 1
}
marker="maki-memory-$RANDOM-$RANDOM"
@ Inbox send: "Remember this marker for my next message: $marker. Send exactly 'remembered' using AgentRun result:, then settle this delivery. No file work is needed." to: "session:$session" from: maki-live-tester >/dev/null
run=$(@ AgentWorker tickSession: "$session")
[[ -n "$run" ]] || { echo 'FAIL: first launch'; exit 1; }
finish
check 'real Maki initial run succeeds' succeeded "$(@ "$run" state)"
ref=$(@ "$session" lastConversationRef)
[[ -n "$ref" ]] || { echo 'FAIL: missing Maki conversation reference'; exit 1; }
inbox=$(@ Inbox named: maki-live-tester)
reply=$(@ "$inbox" unread)
check 'real reply reaches inbox' remembered "$(@ "$reply" body)"
@ "$reply" markRead >/dev/null
@ "$reply" reply: 'What marker did I ask you to remember? Send only that marker through AgentRun result:, then settle this delivery.' >/dev/null
run2=$(@ AgentWorker tickSession: "$session")
[[ -n "$run2" ]] || { echo 'FAIL: resume launch'; exit 1; }
finish
check 'real Maki resumed run succeeds' succeeded "$(@ "$run2" state)"
check 'same Maki conversation resumes' "$ref" "$(@ "$session" lastConversationRef)"
reply2=$(@ "$inbox" unread)
check 'resumed Maki remembers earlier turn' "$marker" "$(@ "$reply2" body)"
check 'all deliveries settled' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE class='AgentDelivery' AND json_extract(data,'$.state')!='processed';")"
echo '=== Maki live launch, reply, memory, resume, and settlement passed ==='
