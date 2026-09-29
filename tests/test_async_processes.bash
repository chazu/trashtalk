#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
export TRASH_DEBUG=0
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
scratch=$(mktemp -d)
trap 'touch "$scratch/gate"; rm -rf "$scratch"' EXIT
check() { [[ "$2" == "$3" ]] || { printf 'FAIL %s: expected <%s>, got <%s>\n' "$1" "$2" "$3"; exit 1; }; }
# A watchdog bounds the old blocking-start bug; the gate, not a timing threshold,
# establishes whether start returned before the computation finished.
( sleep 4; touch "$scratch/gate" ) & watchdog=$!
f=$(@ Future for: "while [[ ! -e '$scratch/gate' ]]; do sleep .02; done; printf first")
g=$(@ Future for: "while [[ ! -e '$scratch/gate' ]]; do sleep .02; done; printf second")
pid=$(@ "$f" start)
check 'start returns before release' false "$(test -e "$scratch/gate" && echo true || echo false)"
@ "$g" start >/dev/null
check 'first Future pending' pending "$(@ "$f" status)"
check 'second Future pending' pending "$(@ "$g" status)"
if @ "$f" isDone; then echo 'FAIL pending Future isDone'; exit 1; fi
touch "$scratch/gate"
kill "$watchdog" 2>/dev/null || true
wait "$watchdog" 2>/dev/null || true
check 'first result' first "$(@ "$f" await)"
check 'second result' second "$(@ "$g" await)"
@ "$f" cleanup
@ "$g" cleanup
f=$(@ Future for: 'exit 42')
@ "$f" start >/dev/null
@ "$f" await >/dev/null
check 'Future failed exit' 42 "$(@ "$f" exitCode)"
check 'Future failed state' failed "$(@ "$f" status)"
@ "$f" cleanup
f=$(@ Future for: 'sleep 30')
@ "$f" start >/dev/null
check 'cancel running Future' Cancelled "$(@ "$f" cancel)"
check 'cancel state' cancelled "$(@ "$f" status)"
@ "$f" isDone
@ "$f" cleanup
p=$(@ Process for: 'sleep .2; printf first; printf error-one >&2; exit 42')
q=$(@ Process for: 'sleep .2; printf second; printf error-two >&2')
@ "$p" start >/dev/null
@ "$q" start >/dev/null
check 'Process failure exit' 42 "$(@ "$p" wait)"
check 'Process success exit' 0 "$(@ "$q" wait)"
check 'first stdout' first "$(@ "$p" output)"
check 'second stdout' second "$(@ "$q" output)"
check 'first stderr' error-one "$(@ "$p" errors)"
check 'second stderr' error-two "$(@ "$q" errors)"
check 'failed Process does not succeed' false "$(@ "$p" succeeded)"
f=$(@ Tools::Jq runAsync: "-cn '{answer:42}'")
check 'Tool async uses a started Future' '{"answer":42}' "$(@ "$f" await)"
@ "$f" cleanup
echo 'PASS: nonblocking starts, overlapping results, failures and live cancellation'
# Missing receipts must fail observation, not report a completed/cancelled result.
f=$(@ Future for: true)
@ Runtime assign: '{"status":"pending","pid":"99999999"}' to: "$f" >/dev/null
if @ "$f" status >/dev/null 2>&1; then echo 'FAIL: missing Future receipt accepted'; exit 1; fi
@ Runtime assign: '{"status":"failed"}' to: "$f" >/dev/null
@ "$f" cleanup
p=$(@ Process for: true)
directory=$(@ File tempDirectory)
fields=$(jq -cn --arg dir "$directory" '{status:"running",pid:"99999999",resultDirectory:$dir}')
@ Runtime assign: "$fields" to: "$p" >/dev/null
if @ "$p" isRunning >/dev/null 2>&1; then echo 'FAIL: missing Process receipt accepted'; exit 1; fi
@ Process cleanupDirectory: "$directory"
# The directory convenience wrapper must retain low-level launch failure.
_ensure_class_sourced Tool
__Tool__class__detachArgvJson_stdinFile_stdoutFile_stderrFile_pidFile_exitFile_() { _throw ToolError 'launch fixture'; return 1; }
f=$(@ Future for: true)
if @ "$f" start >/dev/null 2>&1; then echo 'FAIL: launch error became a PID'; exit 1; fi
check 'failed start remains unstarted' created "$(@ "$f" status)"
@ "$f" cleanup
echo 'PASS: missing receipts and failed launches propagate through public wrappers'
