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
export TRASHTALK_USER=recovery-owner TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
export TRASHTALK_WORKER_INTERVAL=0.1 GATE="$tmp/gate" STARTS="$tmp/starts"
worker=''
cleanup() {
    [[ -z "$worker" ]] || kill "$worker" 2>/dev/null || true
    touch "$GATE"
    # No model/network is involved; stop only test-created harness groups.
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
field() { db_get "$1" | jq -r --arg field "$2" '.[$field] // empty'; }
wait_for() { local i; for i in {1..150}; do if "$@"; then return 0; fi; sleep 0.1; done; echo "FAIL: timed out: $*"; cat "$tmp/worker.log"; exit 1; }
started() { [[ -s "$STARTS" ]]; }
all_processed() { [[ "$(_db_sql "SELECT count(*) FROM instances WHERE class='AgentDelivery' AND json_extract(data,'$.state')!='processed';")" == 0 ]]; }
no_active() { [[ -z "$(@ "$session" activeRun)" ]]; }
export TRASHTALK_SHELL_DRIVER='prompt=$(cat); printf "started\n" >> "$STARTS"; while [[ ! -f "$GATE" ]]; do sleep 0.05; done; ts="$TRASHTALK_DIR/bin/trash-send"; "$ts" AgentRun result: "recovered answer" >/dev/null || exit; for d in $(printf "%s\n" "$prompt" | sed -n "s/^--- delivery //p"); do "$ts" AgentRun settle: "$d" >/dev/null || exit; done'

# A failed outbox insert rolls back the final message snapshot too.
@ AgentQueue ensureSchema
probe=$(@ Message to: session:probe from: recovery-owner subject: atomic body: probe kind: note)
probe_inbox=$(@ Inbox named: session:probe)
before=$(db_get "$probe")
_db_sql "CREATE TRIGGER reject_outbox BEFORE INSERT ON agent_outbox BEGIN SELECT RAISE(ABORT,'fixture disk failure'); END;"
response=$(@ "$probe_inbox" deliver: "$probe" 2>/dev/null); status=$?
check 'failed transaction reports failure' 1 "$status"
check 'failed transaction never reports a message id' '' "$response"
check 'message update rolled back with outbox failure' "$before" "$(db_get "$probe")"
_db_sql 'DROP TRIGGER reject_outbox;'

session=$(@ Gusgus sessionFor: "$root")
# Simulate a crash after final message+outbox commit but before routing.
msg=$(@ Message to: "session:$session" from: recovery-owner subject: recovery body: first kind: note)
@ "$msg" thread: "$msg"
@ "$msg" save
@ AgentQueue persist: "$msg"
check 'message has an unrouted durable outbox row' 1 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE session='';")"
check 'no delivery before routing' 0 "$(@ Store countByClass: AgentDelivery)"
"$root/bin/trash-worker" >"$tmp/worker.log" 2>&1 & worker=$!
wait_for started
run=$(@ "$session" activeRun)
check 'worker creates a run' running "$(field "$run" state)"
pidfile=$(field "$run" pidFile)
read -r harness < "$pidfile"
kill -0 "$harness"
cp "$pidfile.start" "$tmp/birth"
printf '%s\n' 'different process start' > "$pidfile.start"
check 'PID reuse does not count as harness liveness' false "$(@ "$run" isProcessAlive)"
cp "$tmp/birth" "$pidfile.start"
# Simulate crash after launcher persisted pid but before processPid was saved.
@ Store patch: "$run" with: '{"processPid":0}'
kill -KILL "$worker"
wait "$worker" 2>/dev/null || true
worker=''
kill -0 "$harness"
check 'detached harness survives killed worker' started "$(cat "$STARTS")"
second=$(@ Inbox send: second to: "session:$session" from: recovery-owner)
# Simultaneous replay and foreground dispatch race with the replacement worker.
ids=$(jq -cn --arg msg "$second" '[$msg]')
pids=()
for i in {1..6}; do
    (@ AgentDelivery forSession: "$session" messages: "$ids" >/dev/null; @ AgentWorker tickSession: "$session" >/dev/null) & pids+=("$!")
done
"$root/bin/trash-worker" >>"$tmp/worker.log" 2>&1 & worker=$!
for pid in "${pids[@]}"; do wait "$pid"; done
check 'replayed routing creates two logical deliveries total' 2 "$(@ Store countByClass: AgentDelivery)"
check 'concurrent ticks do not relaunch live harness' 1 "$(wc -l < "$STARTS" | tr -d ' ')"
touch "$GATE"
wait_for all_processed
wait_for no_active
check 'queued delivery launches without another tick from the user' 2 "$(wc -l < "$STARTS" | tr -d ' ')"
check 'two replies reached the owner' 2 "$(_db_sql "SELECT count(*) FROM instances WHERE class='Message' AND json_extract(data,'$.to')='recovery-owner';")"
check 'all routing acknowledged' 0 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE session='';")"
kill "$worker"; wait "$worker" 2>/dev/null || true; worker=''

# Failure diagnostics and uncertainty: retain provider stderr, never auto-replay
# a process that emitted output and might already have acted.
export TRASHTALK_SHELL_DRIVER='cat >/dev/null; echo partial-effect; echo provider-auth-rejected >&2; exit 1'
third=$(@ Inbox send: third to: "session:$session" from: recovery-owner)
"$root/bin/trash-worker" >>"$tmp/worker.log" 2>&1 & worker=$!
uncertain() { [[ "$(@ "$session" stalledCount)" == 1 ]]; }
wait_for uncertain
wait_for no_active
latest=$(_db_sql "SELECT id FROM instances WHERE class='AgentRun' ORDER BY rowid DESC LIMIT 1;")
check 'provider error survives reconciliation' provider-auth-rejected "$(field "$latest" error)"
delivery=$(_db_sql "SELECT id FROM instances WHERE class='AgentDelivery' ORDER BY rowid DESC LIMIT 1;")
check 'partial output makes failed work uncertain' uncertain "$(field "$delivery" state)"
check 'uncertain work was attempted once' 1 "$(field "$delivery" attempts)"
kill "$worker"; wait "$worker" 2>/dev/null || true; worker=''

# Codex errors arrive as structured events too (no real provider request).
log=$(field "$latest" outputLog)
printf '%s\n' '{"type":"turn.failed","error":{"message":"unsupported model fixture"}}' > "$log"
check 'structured provider error is normalized' 'unsupported model fixture' "$(@ CodexDriver errorFor: "$latest")"
echo "=== $passed recovery checks passed ==="
