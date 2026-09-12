#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# A method's stdout is its value. Non-tail statement sends discard their
# output; `pragma: stream` keeps it. Also covers linesDo:, caseOf:, and the
# literal `@ Env get:` peephole.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-statement-values.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/ValueFixture.trash" <<'TRASH'
ValueFixture subclass: Object
  instanceVars: count:0 seen:''
  method: noisy [ ^ 'NOISE' ]
  method: quiet [
    @ self noisy.
    ^ 'done'
  ]
  method: tailSend [
    count := count + 1.
    @ self noisy
  ]
  method: streamed [
    pragma: stream
    @ self noisy.
    @ self noisy.
    ^ 'done'
  ]
  method: conditional: flag [
    (flag = 'yes') ifTrue: [ @ self noisy ].
    ^ 'after'
  ]
  method: conditionalTail: flag [
    (flag = 'yes') ifTrue: [ @ self noisy ] ifFalse: [ ^ 'no' ]
  ]
  method: cascaded [
    @ self noisy; noisy.
    ^ 'done'
  ]
  method: walk: ids [
    count := 0.
    seen := ''.
    ids linesDo: [:id |
      (id = 'stop') ifTrue: [ ^ 'stopped after ' , count ].
      count := count + 1.
      seen := seen , id
    ].
    ^ 'walked ' , count , ' ' , seen
  ]
  method: printAll: ids [
    ids linesDo: [:id | @ self echo: id ]
  ]
  method: echo: s [ ^ s ]
  method: driverFor: profile [
    ^ profile caseOf: {
      'shell' -> [ 'ShellDriver' ].
      #('codex' 'assistant-low-power' '') -> [ 'CodexDriver' ]
    } otherwise: [ @ ProfileError signal: 'Unknown profile ' , profile ]
  ]
  method: label: n [
    | out |
    out := 'none'.
    n caseOf: { 1 -> [ out := 'one' ]. 2 -> [ out := 'two' ] }.
    ^ out
  ]
  method: home [ ^ @ Env get: 'VALUE_FIXTURE_HOME' default: 'fallback' ]
  method: plainEnv [ ^ @ Env get: 'VALUE_FIXTURE_PLAIN' ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/ValueFixture.trash" > "$TRASHDIR/.compiled/ValueFixture"
bash -n "$TRASHDIR/.compiled/ValueFixture"
source "$ROOT/lib/trash.bash"
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
id=$(@ ValueFixture new)
check done @ "$id" quiet
check NOISE @ "$id" tailSend
check $'NOISE\nNOISE\ndone' @ "$id" streamed
check after @ "$id" conditional: yes
check NOISE @ "$id" conditionalTail: yes
check no @ "$id" conditionalTail: nope
check done @ "$id" cascaded
check 'walked 3 abc' @ "$id" walk: $'a\nb\n\nc'
check 'stopped after 1' @ "$id" walk: $'a\nstop\nb'
check 'walked 0 ' @ "$id" walk: ''
check $'x\ny' @ "$id" printAll: $'x\ny'
check ShellDriver @ "$id" driverFor: shell
check CodexDriver @ "$id" driverFor: codex
check CodexDriver @ "$id" driverFor: ''
rc=0; out=$(@ "$id" driverFor: weird) || rc=$?
[[ $rc -ne 0 && -z "$out" ]] || { echo "FAIL: caseOf: otherwise did not fail (rc=$rc out=$out)" >&2; exit 1; }
_clear_error
echo "PASS: caseOf: otherwise raises"
check two @ "$id" label: 2
check none @ "$id" label: 7
check fallback @ "$id" home
VALUE_FIXTURE_HOME=/srv check /srv @ "$id" home
check '' @ "$id" plainEnv
VALUE_FIXTURE_PLAIN=set check set @ "$id" plainEnv

# Shape checks: discard is a redirect, not a capture; the Env read never dispatches;
# linesDo: keeps the body in the method's shell so `^` returns from the method.
rg -q '@ "\$_RECEIVER" noisy >/dev/null' "$TRASHDIR/.compiled/ValueFixture" || { echo "FAIL: non-tail send is not discarded" >&2; exit 1; }
rg -q '\$\{VALUE_FIXTURE_HOME:-fallback\}' "$TRASHDIR/.compiled/ValueFixture" || { echo "FAIL: Env get:default: not inlined" >&2; exit 1; }
if rg -q '@ Env get:' "$TRASHDIR/.compiled/ValueFixture"; then echo "FAIL: literal Env read still dispatches" >&2; exit 1; fi
rg -q 'mapfile -t __lines_' "$TRASHDIR/.compiled/ValueFixture" || { echo "FAIL: linesDo: did not lower to an array loop" >&2; exit 1; }
if rg -q '@ Block params:' "$TRASHDIR/.compiled/ValueFixture"; then echo "FAIL: inline iteration created a Block object" >&2; exit 1; fi
echo "PASS: generated shapes"

# caseOf: keys must be literals.
printf 'BadCase subclass: Object\n  method: run: x [ | k | k := 1. ^ x caseOf: { k -> [ 1 ] } ]\n' > "$scratch/BadCase.trash"
if "$COMPILER_DIR/driver.bash" compile "$scratch/BadCase.trash" >"$scratch/out" 2>"$scratch/error"; then
    echo "FAIL: caseOf: accepted a non-literal key" >&2; exit 1
fi
rg -q 'caseOf: keys must be' "$scratch/error" || { echo "FAIL: missing caseOf: diagnostic" >&2; cat "$scratch/error" >&2; exit 1; }
echo "PASS: caseOf: rejects non-literal keys"
