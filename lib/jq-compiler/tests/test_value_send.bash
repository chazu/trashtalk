#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
fixture="$ROOT/tests/fixtures/ResultProbe.trash"
driver="$ROOT/lib/jq-compiler/driver.bash"
TRASHTALK_VALUE_SEND=0 "$driver" compile "$fixture" -o "$TMPDIR/off" --check
TRASHTALK_VALUE_SEND=1 "$driver" compile "$fixture" -o "$TMPDIR/on" --check
! rg -q '_trash_value_send|valueMethods\[' "$TMPDIR/off"
rg -qF 'first="$(_trash_value_send ResultProbe constant)"' "$TMPDIR/on"
rg -qF 'second="$(_trash_value_send ResultProbe valueWith: "$first")"' "$TMPDIR/on"
source "$TMPDIR/on"
test "${__ResultProbe__valueMethods[class__constant]}" = argc:0
test "${__ResultProbe__valueMethods[class__valueWith_]}" = 'argc:1 a:1'
test "${__ResultProbe__valueMethods[class__add_to_]}" = 'argc:2 i:1 i:2'
test "${__ResultProbe__valueMethods[class__safeEmpty]}" = argc:0
for method in getNumber arithmetic class__nested class__optionText class__dangerousLiteral class__directEffects class__assignedLegacyShell; do
    test -z "${__ResultProbe__valueMethods[$method]:-}"
done
# A fresh artifact resets capabilities from its predecessor, even without a runtime.
source "$TMPDIR/off"
test ${#__ResultProbe__valueMethods[@]} = 0
# Both directions invalidate the build receipt; an unchanged mode stays warm.
cp "$fixture" "$ROOT/trash/ResultProbe.trash"
artifact="$ROOT/trash/.compiled/ResultProbe"
TRASHTALK_VALUE_SEND=0 "$driver" compile-cached "$ROOT/trash/ResultProbe.trash" "$artifact" > /dev/null
TRASHTALK_VALUE_SEND=1 "$driver" compile-cached "$ROOT/trash/ResultProbe.trash" "$artifact" > /dev/null
rg -q '_trash_value_send' "$artifact"
TRASHTALK_VALUE_SEND=1 "$driver" compile-cached "$ROOT/trash/ResultProbe.trash" "$artifact" > "$TMPDIR/warm"
rg -q 'artifacts unchanged' "$TMPDIR/warm"
TRASHTALK_VALUE_SEND=0 "$driver" compile-cached "$ROOT/trash/ResultProbe.trash" "$artifact" > /dev/null
! rg -q '_trash_value_send' "$artifact"
# Later definitions and aliases must not inherit an earlier body's capability.
cat > "$TMPDIR/duplicates.trash" <<'DSL'
Duplicates subclass: Object
  classMethod: duplicate [ ^ 'first' ]
  rawClassMethod: duplicate [ printf '%s' "$BASH_SUBSHELL" ]
  classMethod: aliased [ ^ 'first' ]
  rawClassMethod: replacement [ printf '%s' "$BASH_SUBSHELL" ]
  alias: aliased for: replacement
DSL
TRASHTALK_VALUE_SEND=1 "$driver" compile "$TMPDIR/duplicates.trash" -o "$TMPDIR/duplicates" --check
source "$TMPDIR/duplicates"
test -z "${__Duplicates__valueMethods[class__duplicate]:-}"
test -z "${__Duplicates__valueMethods[class__aliased]:-}"
echo 'PASS: parsed value lowering, conservative capabilities, mixed artifacts, and mode-aware receipts'
