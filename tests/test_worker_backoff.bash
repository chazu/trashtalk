#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# bin/trash-worker service behaviour without a model: idle backoff up to a cap,
# the base interval while a run is active, exit after repeated tick failures,
# and a bounded supervisor log. Foreground delivery is covered by
# tests/test_agent_worker.bash and is untouched by these settings.
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
worker=''
cleanup() {
    [[ -z "$worker" ]] || kill "$worker" 2>/dev/null || true
    rm -rf "$tmp"
}
trap cleanup EXIT
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
stop_worker() { kill "$worker" 2>/dev/null; wait "$worker" 2>/dev/null; worker=''; }

mkdir -p "$tmp/bin"
cat > "$tmp/bin/sleep" <<'SLEEP'
#!/usr/bin/env bash
printf '%s\n' "$1" >> "$SLEEP_LOG"
exec /bin/sleep "$@"
SLEEP
chmod +x "$tmp/bin/sleep"
export SLEEP_LOG="$tmp/sleeps" TRASHTALK_DIR="$root" TRASHTALK_RUN_DIR="$tmp/runs" TRASHTALK_USER=backoff-owner
export SQLITE_JSON_DB="$tmp/store.db"
source "$root/lib/trash.bash"
db_init

echo "1. an idle store backs off from the base interval to the cap"
: > "$SLEEP_LOG"
PATH="$tmp/bin:$PATH" TRASHTALK_WORKER_INTERVAL=0.1 TRASHTALK_WORKER_MAX_INTERVAL=0.4 \
    "$root/bin/trash-worker" 2>"$tmp/idle.err" & worker=$!
sleep 2.5
stop_worker
check 'delays double from the base interval' '0.100 0.200 0.400 0.400' "$(head -4 "$SLEEP_LOG" | tr '\n' ' ' | sed 's/ $//')"
check 'delays never exceed the cap' '' "$(grep -v -e '^0.100$' -e '^0.200$' -e '^0.400$' "$SLEEP_LOG")"
sleeps=$(wc -l < "$SLEEP_LOG" | tr -d ' ')
check 'far fewer ticks than a fixed interval would run' true "$([[ "$sleeps" -le 10 ]] && echo true || echo "false ($sleeps sleeps)")"
check 'idle ticks do not fail' '' "$(grep 'tick failed' "$tmp/idle.err")"

echo "2. an active run keeps the base interval"
db_put agentrun_backoff_fixture '{"class":"Agent::Run","session":"agentsession_missing","state":"running","_vars":["session","state"]}'
check 'probe sees the active run' true "$(@ Agent::Worker hasActiveRuns)"
: > "$SLEEP_LOG"
PATH="$tmp/bin:$PATH" TRASHTALK_WORKER_INTERVAL=0.1 TRASHTALK_WORKER_MAX_INTERVAL=0.4 \
    "$root/bin/trash-worker" 2>"$tmp/active.err" & worker=$!
sleep 2
stop_worker
check 'every delay stays at the base interval' '' "$(grep -v '^0.100$' "$SLEEP_LOG")"
sleeps=$(wc -l < "$SLEEP_LOG" | tr -d ' ')
# A loaded host slows each tick; two base-interval sleeps already prove the
# delay never doubled.
check 'the worker kept polling' true "$([[ "$sleeps" -ge 2 ]] && echo true || echo "false ($sleeps sleeps)")"
db_delete agentrun_backoff_fixture
check 'probe sees no active run' false "$(@ Agent::Worker hasActiveRuns)"

echo "3. repeated tick failures exit for the supervisor's restart throttle"
mkdir -p "$tmp/not-a-database"
: > "$SLEEP_LOG"
PATH="$tmp/bin:$PATH" SQLITE_JSON_DB="$tmp/not-a-database" TRASHTALK_WORKER_INTERVAL=0.05 \
    TRASHTALK_WORKER_MAX_INTERVAL=0.1 TRASHTALK_WORKER_MAX_FAILURES=3 \
    timeout 20 "$root/bin/trash-worker" 2>"$tmp/failing.err"
check 'worker exits non-zero after the failure limit' 1 "$?"
check 'first failure is logged once' 1 "$(grep -c 'tick failed; retrying with backoff' "$tmp/failing.err")"
check 'exit reason names the streak' 1 "$(grep -c 'failed 3 times in a row' "$tmp/failing.err")"
check 'failed ticks back off too' '0.050 0.100' "$(tr '\n' ' ' < "$SLEEP_LOG" | sed 's/ $//')"

echo "4. the supervisor log is bounded when it is the worker's stderr"
mkdir -p "$root/run/worker"
log="$root/run/worker/stderr.log"
head -c 20000 /dev/zero | tr '\0' 'x' > "$log"
printf '\nlast line before restart\n' >> "$log"
TRASHTALK_WORKER_LOG_MAX_BYTES=4096 TRASHTALK_WORKER_INTERVAL=0.1 \
    "$root/bin/trash-worker" 2>>"$log" & worker=$!
sleep 1
stop_worker
size=$(wc -c < "$log" | tr -d ' ')
check 'oversized log was truncated to its tail' true "$([[ "$size" -lt 4096 ]] && echo true || echo "false ($size bytes)")"
check 'retained tail keeps the newest lines' 1 "$(grep -c 'last line before restart' "$log")"
check 'truncation is announced in the log' 1 "$(grep -c 'worker log exceeded 4096 bytes' "$log")"
check 'previous contents were kept once' 20026 "$(wc -c < "$log.1" | tr -d ' ')"
head -c 20000 /dev/zero | tr '\0' 'x' > "$log"
TRASHTALK_WORKER_LOG_MAX_BYTES=4096 TRASHTALK_WORKER_INTERVAL=0.1 \
    "$root/bin/trash-worker" 2>"$tmp/elsewhere.err" & worker=$!
sleep 1
stop_worker
check 'a log that is not our stderr is left alone' 20000 "$(wc -c < "$log" | tr -d ' ')"
echo "=== $passed worker service checks passed ==="
