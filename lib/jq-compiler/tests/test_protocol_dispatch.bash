#!/usr/bin/env bash
# Also a deterministic microbenchmark: TRASH_PROTOCOL_ITERATIONS controls loops.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -eo pipefail
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
driver="$ROOT/lib/jq-compiler/driver.bash"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export TRASHTALK_DIR="$work" TRASHDIR="$work/trash" TRASHTALK_COMPILED_DIR="$work/trash/.compiled"
mkdir -p "$TRASHDIR"
printf 'Object subclass: nil\n' > "$TRASHDIR/Object.trash"
cp "$ROOT/trash/Protocol.trash" "$TRASHDIR/Protocol.trash"
printf 'Pingable subclass: Protocol\n requires: ping\n' > "$TRASHDIR/Pingable.trash"
printf 'Probe subclass: Object\n method: ping [ ^ "pong" ]\n method: twice [ ^ @ self ping ]\n' > "$TRASHDIR/Probe.trash"
"$driver" compile-cached "$TRASHDIR/Probe.trash" "$TRASHTALK_COMPILED_DIR/Probe" >/dev/null
cp "$TRASHTALK_COMPILED_DIR/Probe" "$work/plain"
printf ' implements: Pingable\n' >> "$TRASHDIR/Probe.trash"
"$driver" compile-cached "$TRASHDIR/Probe.trash" "$TRASHTALK_COMPILED_DIR/Probe" >/dev/null
cp "$TRASHTALK_COMPILED_DIR/Probe" "$work/nominal"
for mode in plain nominal; do
    awk '/^__[A-Za-z_]+\(\) \{/ {body=1} body {print} /^}/ {body=0}' "$work/$mode" > "$work/$mode.functions"
done
cmp "$work/plain.functions" "$work/nominal.functions"
! rg -q '_conforms_to|isSatisfiedBy|source |declare -[fF]' "$work/nominal.functions"
source "$ROOT/lib/trash.bash"
iterations=${TRASH_PROTOCOL_ITERATIONS:-300}
[[ "$iterations" =~ ^[1-9][0-9]*$ ]]
echo "Bash $BASH_VERSION; $iterations iterations; seconds (cold source separate from hot send)"
TIMEFORMAT='%3R'
for mode in plain nominal; do
    printf '%s cold-source: ' "$mode"
    { time source "$work/$mode"; } 2>&1
    @ Probe ping >/dev/null
    msg_debug() { case "$*" in 'Calling '*|'Method '*|'Found '*|'Sourced '*) printf '%s\n' "$*" >> "$work/$mode.route";; esac; }
    @ Probe twice >/dev/null
    unset -f msg_debug
    msg_debug() { :; }
    # Complete xtrace equivalence covers branches and processes, including the
    # nested send, independently of wall-clock noise.
    (PS4='+ '; set -x; @ Probe twice) > "$work/$mode.value" 2> "$work/$mode.trace"
    printf '%s hot-send: ' "$mode"
    { time for ((i=0;i<iterations;i++)); do @ Probe ping >/dev/null; done; } 2>&1
done
cmp "$work/plain.route" "$work/nominal.route"
cmp "$work/plain.trace" "$work/nominal.trace"
cmp "$work/plain.value" "$work/nominal.value"
_conforms_to Probe Pingable > "$work/result"
test "$(cat "$work/result")" = true
test "$(@ Pingable isSatisfiedBy: Probe)" = true
# Captured checks don't claim to populate the parent process cache.
_protocol_invalidate
captured=$(_conforms_to Probe Pingable)
test "$captured" = true
test ${#_PROTOCOL_CACHE[@]} -eq 0
printf 'dynamic first: '
{ time _conforms_to Probe Pingable >/dev/null; } 2>&1
printf 'dynamic cached: '
{ time for ((i=0;i<iterations;i++)); do _conforms_to Probe Pingable >/dev/null; done; } 2>&1
echo 'PASS: identical generated functions, sends, dispatch traces and process routes'

# Public hot reload must reject a changed contract before replacing artifacts.
cp "$ROOT/trash/.compiled/Trash" "$TRASHTALK_COMPILED_DIR/Trash"
cp "$TRASHTALK_COMPILED_DIR/Pingable" "$work/old-protocol"
printf ' requires: added\n' >> "$TRASHDIR/Pingable.trash"
if @ Trash compileAndReload: Pingable > "$work/reload" 2>&1; then
    echo 'FAIL: public reload accepted a broken promise'; exit 1
fi
rg -q 'lacks: added' "$work/reload"
cmp "$work/old-protocol" "$TRASHTALK_COMPILED_DIR/Pingable"
printf ' method: added [ ^ "added" ]\n' >> "$TRASHDIR/Probe.trash"
@ Trash compileAndReload: Pingable > "$work/reload" 2>&1
@ Trash reloadClass: Probe >/dev/null
test "$(@ Probe added)" = added
test "$(@ Pingable isSatisfiedBy: Probe)" = true
echo 'PASS: public hot reload validates registered dependents before installation'
