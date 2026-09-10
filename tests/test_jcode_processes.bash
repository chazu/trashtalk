#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
host="$tmp/host with spaces"
mkdir -p "$host/processes"
helper="$root/lib/jcode-processes.bash"
real_bash=$(command -v bash)
own='' foreign=''
cleanup() {
    [[ -z "$own" ]] || kill -KILL -- "-$own" 2>/dev/null || true
    [[ -z "$foreign" ]] || kill -KILL -- "-$foreign" 2>/dev/null || true
    wait 2>/dev/null || true
    rm -rf "$tmp"
}
trap cleanup EXIT
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 expected=$2 got=$3"; exit 1; }; echo "PASS: $1"; }
bash "$helper" shell "$host" "$real_bash" -c 'echo $$ > "$1"; sleep 120' tool "$tmp/own.pid" &
own=$!
perl -MPOSIX -e 'setsid(); exec @ARGV' "$real_bash" -c 'echo $$ > "$1"; sleep 120' other "$tmp/foreign.pid" &
foreign=$!
for i in {1..100}; do [[ -s "$tmp/own.pid" && -s "$tmp/foreign.pid" ]] && break; sleep .05; done
check 'registered Bash has a private process group' "$own" "$(ps -p "$own" -o pgid= | tr -d ' ')"
check 'process birth receipt exists' true "$([[ -s "$host/processes/$own" ]] && echo true || echo false)"
echo 'wrong process birth' > "$host/processes/$foreign"
if bash "$helper" stop "$host" 2>/dev/null; then rc=0; else rc=$?; fi
check 'unknown process ownership prevents a false stop confirmation' 1 "$rc"
check 'unrelated process was not signalled' true "$(kill -0 "$foreign" 2>/dev/null && echo true || echo false)"
wait "$own" 2>/dev/null || true
check 'owned process group stopped' false "$(kill -0 "$own" 2>/dev/null && echo true || echo false)"
own=''
if bash "$helper" shell "$host" "$real_bash" -c 'touch "$1"' tool "$tmp/late"; then rc=0; else rc=$?; fi
check 'stop gate rejects late tool launches' 125 "$rc"
check 'late tool never executed' false "$([[ -e "$tmp/late" ]] && echo true || echo false)"
rm "$host/processes/$foreign"
bash "$helper" stop "$host"
check 'explicitly resolved stop is retryable' true true
echo '=== Jcode process ownership checks passed ==='
