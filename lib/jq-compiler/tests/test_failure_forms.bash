#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Failure forms: `@ SomeError signal:`, `@ self error:`, `ifFailed:`, re-raise
# with `@ e signal`, and try:catch: clearing the error before the catch body.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-failure-forms.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/FailureFixture.trash" <<'TRASH'
FailureFixture subclass: Object
  instanceVars: state:'open'
  method: requireOpen [
    (state ~= 'open') ifTrue: [ @ StateError signal: 'not open: ' , state ].
    ^ 'open'
  ]
  method: plain [ @ self error: 'plain failure' ]
  method: boom [ @ BoomError signal: 'boom' ]
  method: value [ ^ 'value' ]
  method: guarded [
    (@ self boom) ifFailed: [ ^ 'recovered' ].
    ^ 'not reached'
  ]
  method: guardedValue [
    | r |
    r := @ self boom ifFailed: [:e | ^ 'caught ' , e ].
    ^ r
  ]
  method: guardedOk [
    | r |
    r := @ self value ifFailed: [:e | ^ 'caught ' , e ].
    ^ 'got ' , r
  ]
  method: passthrough [ ^ (@ self value) ifFailed: [ ^ 'fallback' ] ]
  method: rethrown [
    (@ self boom) ifFailed: [:e | @ e signal ].
    ^ 'not reached'
  ]
  method: wrapped [
    (@ self boom) ifFailed: [:e | @ WrapError signal: 'wrapped: ' , e ].
    ^ 'not reached'
  ]
  method: cleared [
    try: [ @ self boom ] catch: [:e | ^ e , '|' , (@ self errorTypeNow) ].
    ^ 'fine'
  ]
  method: errorTypeNow [ ^ @ Runtime lastErrorType ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/FailureFixture.trash" > "$TRASHDIR/.compiled/FailureFixture"
bash -n "$TRASHDIR/.compiled/FailureFixture"
source "$ROOT/lib/trash.bash"
# Test-only introspection of the runtime error state.
__Runtime__class__lastErrorType() { printf '%s' "${_ERROR_TYPE:-}"; }
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
expect_failure() {
    # A failed top-level send is reported by the dispatcher on stderr as
    # "Unhandled error: Type: message" and then cleared.
    local type="$1" message="$2"; shift 2
    local out rc=0
    out=$("$@" 2>"$scratch/stderr") || rc=$?
    [[ $rc -ne 0 ]] || { echo "FAIL: $* succeeded with output '$out'" >&2; exit 1; }
    [[ -z "$out" ]] || { echo "FAIL: $* produced output '$out' while failing" >&2; exit 1; }
    rg -q "Unhandled error: $type: $message" "$scratch/stderr" || { echo "FAIL: $* did not report $type: $message" >&2; cat "$scratch/stderr" >&2; exit 1; }
    echo "PASS: $* fails with $type"
}
id=$(@ FailureFixture new)
check open @ "$id" requireOpen
@ "$id" state: closed
expect_failure StateError 'not open: closed' @ "$id" requireOpen
expect_failure Error 'plain failure' @ "$id" plain
expect_failure BoomError boom @ "$id" boom
check recovered @ "$id" guarded
check 'caught BoomError: boom' @ "$id" guardedValue
check 'got value' @ "$id" guardedOk
check value @ "$id" passthrough
expect_failure BoomError boom @ "$id" rethrown
expect_failure WrapError 'wrapped: BoomError: boom' @ "$id" wrapped
# The catch body observes the bound error text while the runtime state is already clear.
check 'BoomError: boom|' @ "$id" cleared

# Raising from raw code or the REPL goes through the Error class.
expect_failure Error 'from the shell' @ Error signal: 'from the shell'

# ifFailed: needs a message send to guard.
printf 'BadGuard subclass: Object\n  method: run [ | x | x := 1. x ifFailed: [ ^ 2 ]. ^ x ]\n' > "$scratch/BadGuard.trash"
if "$COMPILER_DIR/driver.bash" compile "$scratch/BadGuard.trash" >"$scratch/out" 2>"$scratch/error"; then
    echo "FAIL: ifFailed: accepted a non-send receiver" >&2; exit 1
fi
rg -q 'ifFailed: applies to a message send' "$scratch/error" || { echo "FAIL: missing ifFailed: diagnostic" >&2; cat "$scratch/error" >&2; exit 1; }
echo "PASS: ifFailed: rejects a non-send receiver"
