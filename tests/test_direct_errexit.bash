#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# A pragma: direct method runs in the caller's shell. When it is the first
# send of a script running under set -e, the dispatcher's call-depth
# bookkeeping must not end the script (((depth++)) from 0 returns 1).
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
output=$(bash -c 'set -euo pipefail; source "$1/lib/trash.bash"; @ AgentQueue ensureSchema; @ AgentSession ensureSchema; @ AgentQueue ensureSchema; echo survived' _ "$root" 2>&1)
check 'direct sends at depth 0 survive set -e' survived "${output##*$'\n'}"
output=$(bash -c 'set -euo pipefail; source "$1/lib/trash.bash"; @ Counter description; @ AgentSession ensureSchema; echo survived' _ "$root" 2>&1)
check 'captured then direct sends survive set -e' survived "${output##*$'\n'}"
echo "=== $passed direct-send checks passed ==="
