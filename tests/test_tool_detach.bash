#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Test suite for Tool detachArgvJson: / isAlivePid: / interruptPid:signal:

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"

export SQLITE_JSON_DB="/tmp/test_tool_detach_$$.db"
db_init

PASSED=0
FAILED=0
DETACHED_PIDS=()

pass() {
    echo "  PASS: $1"
    ((PASSED++)) || true
}

fail() {
    echo "  FAIL: $1 (expected: $2, got: $3)"
    ((FAILED++)) || true
}

assert_eq() {
    [[ "$2" == "$3" ]] && pass "$1" || fail "$1" "$2" "$3"
}

assert_contains() {
    [[ "$3" == *"$2"* ]] && pass "$1" || fail "$1" "*$2*" "$3"
}

assert_nonempty() {
    [[ -n "$2" ]] && pass "$1" || fail "$1" "non-empty" ""
}

assert_matches() {
    [[ "$3" =~ $2 ]] && pass "$1" || fail "$1" "=~ $2" "$3"
}

# Wait up to $2 seconds for file $1 to exist.
wait_for_file() {
    local file="$1" limit="${2:-5}" waited=0
    while [[ ! -e "$file" ]]; do
        (( waited >= limit * 10 )) && return 1
        sleep 0.1
        waited=$((waited + 1))
    done
    return 0
}

WORK="$(mktemp -d "${TMPDIR:-/tmp}/tool_detach_test.XXXXXX")"

cleanup() {
    for pid in "${DETACHED_PIDS[@]}"; do
        kill -KILL -- "-$pid" 2>/dev/null
        kill -KILL "$pid" 2>/dev/null
    done
    rm -rf "$WORK" "$SQLITE_JSON_DB" 2>/dev/null
}
trap cleanup EXIT

echo "=== Tool Detach Tests ==="
echo ""

# ==========================================
echo "1. captures stdout, stderr, and exit status"
# ==========================================

run1="$WORK/run1"
argv1='["bash","-c","echo out; echo err >&2; exit 3"]'
pid1=$(@ Tool detachArgvJson: "$argv1" stdinFile: '' dir: "$run1")
DETACHED_PIDS+=("$pid1")
assert_matches "launcher prints a numeric pid" '^[0-9]+$' "$pid1"
assert_eq "pid file holds the printed pid" "$pid1" "$(cat "$run1/pid")"
wait_for_file "$run1/exit" 5 || true
assert_eq "exit file records status 3" "3" "$(cat "$run1/exit" 2>/dev/null)"
assert_eq "stdout captured" "out" "$(cat "$run1/stdout.log")"
assert_eq "stderr captured" "err" "$(cat "$run1/stderr.log")"
assert_eq "wrapper has exited" "false" "$(@ Tool isAlivePid: "$pid1")"

# ==========================================
echo "2. returns before the child finishes"
# ==========================================

run2="$WORK/run2"
start=$(date +%s)
pid2=$(@ Tool detachArgvJson: '["sleep","3"]' stdinFile: '' dir: "$run2")
DETACHED_PIDS+=("$pid2")
elapsed=$(( $(date +%s) - start ))
[[ "$elapsed" -lt 3 ]] && pass "launcher returned in ${elapsed}s" || fail "launcher returned early" "< 3s" "${elapsed}s"
assert_eq "isAlivePid: true while sleeping" "true" "$(@ Tool isAlivePid: "$pid2")"
assert_eq "exit file absent while running" "false" "$([[ -e "$run2/exit" ]] && echo true || echo false)"
wait_for_file "$run2/exit" 6 || true
sleep 0.2
assert_eq "exit file records 0 after sleep" "0" "$(cat "$run2/exit" 2>/dev/null)"
assert_eq "isAlivePid: false after sleep" "false" "$(@ Tool isAlivePid: "$pid2")"

# ==========================================
echo "3. survives the calling shell exiting"
# ==========================================

run3="$WORK/run3"
pid3=$(bash -c "source '$TRASHTALK_DIR/lib/trash.bash'; @ Tool detachArgvJson: '[\"sleep\",\"5\"]' stdinFile: '' dir: '$run3'")
DETACHED_PIDS+=("$pid3")
assert_matches "pid returned from exited subshell" '^[0-9]+$' "$pid3"
sleep 0.5
assert_eq "child alive after caller exited" "true" "$(@ Tool isAlivePid: "$pid3")"
assert_eq "reparented away from the caller" "1" "$(ps -o ppid= -p "$pid3" | tr -d ' ')"

# ==========================================
echo "4. interruptPid:signal: TERM stops the group and records a signal exit"
# ==========================================

run4="$WORK/run4"
pid4=$(@ Tool detachArgvJson: '["sleep","30"]' stdinFile: '' dir: "$run4")
DETACHED_PIDS+=("$pid4")
assert_eq "alive before TERM" "true" "$(@ Tool isAlivePid: "$pid4")"
child4=$(pgrep -P "$pid4" | head -1)
assert_nonempty "sleep child found under wrapper" "$child4"
assert_eq "interruptPid: TERM delivered" "true" "$(@ Tool interruptPid: "$pid4" signal: TERM)"
wait_for_file "$run4/exit" 5 || true
sleep 0.2
assert_eq "exit file records 128+15" "143" "$(cat "$run4/exit" 2>/dev/null)"
assert_eq "dead after TERM" "false" "$(@ Tool isAlivePid: "$pid4")"
assert_eq "sleep child gone" "false" "$(kill -0 "$child4" 2>/dev/null && echo true || echo false)"
assert_eq "interruptPid: on dead pid is false" "false" "$(@ Tool interruptPid: "$pid4" signal: TERM 2>/dev/null)"
assert_eq "interruptPid: rejects unknown signal" "false" "$(@ Tool interruptPid: "$pid4" signal: HUP 2>/dev/null)"

# ==========================================
echo "5. stdin file reaches the child"
# ==========================================

run5="$WORK/run5"
printf 'hello from stdin\n' > "$WORK/input.txt"
pid5=$(@ Tool detachArgvJson: '["cat"]' stdinFile: "$WORK/input.txt" dir: "$run5")
DETACHED_PIDS+=("$pid5")
wait_for_file "$run5/exit" 5 || true
assert_eq "cat exit 0" "0" "$(cat "$run5/exit" 2>/dev/null)"
assert_eq "stdin content echoed to stdout.log" "hello from stdin" "$(cat "$run5/stdout.log")"

# ==========================================
echo "6. runs in its own process group"
# ==========================================

run6="$WORK/run6"
pid6=$(@ Tool detachArgvJson: '["sleep","5"]' stdinFile: '' dir: "$run6")
DETACHED_PIDS+=("$pid6")
assert_eq "pgid equals wrapper pid" "$pid6" "$(ps -o pgid= -p "$pid6" | tr -d ' ')"
child6=$(pgrep -P "$pid6" | head -1)
assert_nonempty "child sleep found under wrapper" "$child6"
assert_eq "child shares the group" "$pid6" "$(ps -o pgid= -p "$child6" | tr -d ' ')"
assert_eq "interruptPid: KILL delivered" "true" "$(@ Tool interruptPid: "$pid6" signal: KILL)"
sleep 0.3
assert_eq "group dead after KILL" "false" "$(@ Tool isAlivePid: "$pid6")"

# ==========================================
echo "7. raw method validates arguments"
# ==========================================

bad=$(@ Tool detachArgvJson: '[]' stdinFile: '' dir: "$WORK/bad" 2>/dev/null)
assert_eq "empty argv yields no pid" "" "$bad"
bad=$(@ Tool detachArgvJson: 'not json' stdinFile: '' dir: "$WORK/bad" 2>/dev/null)
assert_eq "invalid argv yields no pid" "" "$bad"
bad=$(@ Tool detachArgvJson: '["true"]' stdinFile: "$WORK/missing-input" dir: "$WORK/bad" 2>/dev/null)
assert_eq "unreadable stdin file yields no pid" "" "$bad"
assert_eq "isAlivePid: non-numeric is false" "false" "$(@ Tool isAlivePid: "abc")"

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="
[[ $FAILED -eq 0 ]]
