#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export TRASHTALK_DIR="$root"
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs"
export TRASHTALK_USER=termination-owner TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
export STARTED="$tmp/started" TOKEN_FILE="$tmp/token"
cleanup() {
    for pf in "$tmp"/runs/*/pid; do
        [[ -f "$pf" ]] || continue
        read -r pid < "$pf"
        kill -TERM -- "-$pid" 2>/dev/null || true
    done
    rm -rf "$tmp"
}
trap cleanup EXIT
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
contains() { [[ "$3" == *"$2"* ]] || { echo "FAIL: $1 missing $2"; exit 1; }; echo "PASS: $1"; passed=$((passed+1)); }
field() { db_get "$1" | jq -r --arg field "$2" '.[$field] // empty'; }
export TRASHTALK_SHELL_DRIVER='cat >/dev/null; trap "exit 0" TERM; printf "%s" "$TRASHTALK_RUN_TOKEN" > "$TOKEN_FILE"; echo started > "$STARTED"; while :; do sleep 0.1; done'
session=$(@ Gusgus sessionFor: "$root")
msg=$(@ Inbox send: 'work to interrupt' to: "session:$session" from: termination-owner)
@ AgentWorker tickSession: "$session" >/dev/null
for i in {1..100}; do [[ -s "$STARTED" ]] && break; sleep 0.1; done
check 'real local harness started' true "$([[ -s "$STARTED" ]] && echo true || echo false)"
run=$(@ "$session" activeRun)
delivery=$(@ AgentDelivery offeredFor: "$run")
check 'fixture has active process' true "$(@ "$run" isProcessAlive)"
token=$(cat "$TOKEN_FILE")
check 'live run token resolves before termination' "$run" "$(TRASHTALK_RUN_TOKEN="$token" @ AgentRun current)"

# A stop failure revokes authority but must not falsely finalize a live run.
__ShellDriver__class__interrupt_() { echo false; }
response=$(@ "$session" terminate 2>&1); status=$?
check 'failed stop returns failure' 1 "$status"
contains 'failed stop explains remaining process' 'harness has not stopped' "$response"
check 'failed stop still terminates logical session' terminated "$(field "$session" lifecycleState)"
check 'failed stop leaves active run inspectable' running "$(field "$run" state)"
check 'failed stop leaves offered work unsettled' offered "$(field "$delivery" state)"
check 'terminated session revokes run token' '' "$(TRASHTALK_RUN_TOKEN="$token" @ AgentRun current 2>/dev/null)"
unset -f __ShellDriver__class__interrupt_

# Recovery can have a PID file before the launcher stores processPid.
@ Store patch: "$run" with: '{"processPid":0}'
check 'termination can be retried and uses durable PID file' terminated "$(@ "$session" terminate)"
check 'harness has actually stopped' false "$(@ "$run" isProcessAlive)"
check 'run records interruption' interrupted "$(field "$run" state)"
check 'unsettled delivery requires review' uncertain "$(field "$delivery" state)"
contains 'run records reason' 'terminated by the user' "$(field "$run" error)"
check 'message history retained' 'work to interrupt' "$(@ "$msg" body)"
check 'run logs retained' true "$([[ -f "$(field "$run" outputLog)" ]] && echo true || echo false)"
check 'repeated termination succeeds' terminated "$(@ "$session" terminate)"
@ AgentWorker tickSession: "$session" >/dev/null
check 'terminated session cannot dispatch another run' 1 "$(@ Store countByClass: AgentRun)"

# A reused PID must never receive the old run's signal.
other=$(@ AgentSession new)
mapfile -t started < <(@ AgentRun startFor: "$other" profile: shell)
stale_run=${started[0]}
printf '%s\n' "$$" > "$tmp/stale.pid"
printf '%s\n' 'a different birth' > "$tmp/stale.pid.start"
@ Store patch: "$stale_run" with: "$(jq -cn --arg pf "$tmp/stale.pid" '{pidFile:$pf}')"
@ Tool path >/dev/null
__Tool__class__interruptPid_signal_() { touch "$tmp/unexpected-signal"; echo false; }
check 'stale PID needs no interrupt' true "$(@ AgentDriver interrupt: "$stale_run")"
check 'stale PID is never signalled' false "$([[ -e "$tmp/unexpected-signal" ]] && echo true || echo false)"
check 'starting run can be terminated' terminated "$(@ "$other" terminate)"
check 'unlaunched run records launch cancellation' failed "$(field "$stale_run" state)"
echo "=== $passed termination checks passed ==="
