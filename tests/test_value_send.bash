#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
export LC_ALL=C
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$ROOT/lib/trash.bash" || exit
trap '_env_cleanup_on_exit || true' EXIT
failed=0
check() { if "$@"; then :; else printf 'FAIL: %s\n' "$*" >&2; failed=$((failed+1)); fi; }
compile_probe() {
    TRASHTALK_VALUE_SEND=1 bash "$ROOT/lib/jq-compiler/driver.bash" compile "$1" -o "$TRASHDIR/.compiled/ResultProbe" --check >/dev/null
}
compile_probe "$ROOT/tests/fixtures/ResultProbe.trash" || exit
_ensure_class_sourced ResultProbe || exit
# Create in a subshell: ordinary creation installs legacy getter functions in
# its own shell. The compiled class in this shell keeps its declared methods.
probe=$(@ ResultProbe new)

# Trace transport outside the method body, so the fixture itself stays eligible.
original_send=$(declare -f send)
eval "${original_send/send ()/_value_test_send ()}"
send() {
    [[ ${1:-} != ResultProbe || -z ${VALUE_TRACE:-} ]] || printf '%s:%s\n' "${2:-}" "$BASH_SUBSHELL" >> "$VALUE_TRACE"
    _value_test_send "$@"
}
VALUE_TRACE="$TMPDIR/depth"
export TRASHTALK_VALUE_SEND=0
result=$(_trash_value_send ResultProbe constant)
check test "$result" = 'plain result'
export TRASHTALK_VALUE_SEND=1
result=$(_trash_value_send ResultProbe constant)
check test "$(< "$VALUE_TRACE")" = $'constant:2\nconstant:1'
# Bare public sends keep their capture and update __ in the caller.
: > "$VALUE_TRACE"
__=before
@ ResultProbe constant > "$TMPDIR/public"
check test "$__" = 'plain result'
check test "$(< "$VALUE_TRACE")" = constant:1
VALUE_TRACE=''

# Each case records observable caller state in addition to stdout and status.
# Both modes start with identical process state and use the same compiled code.
record() (
    export TRASHTALK_VALUE_SEND="$1"; shift
    __=previous; EXP_LOCAL=original
    _env_set "$probe" '{"class":"ResultProbe","number":7,"step":3,"_vars":["number","step"]}'
    local captured status
    captured=$(_trash_value_send "$@")
    status=$?
    printf '%s\0' "$captured" "$status" "$__" "$EXP_LOCAL" "$PWD" "$-" \
        "$_CALL_DEPTH" "$_ENSURE_DEPTH" "$_HANDLER_DEPTH" "$(trap -p USR1)" \
        "$_ERROR_TYPE" "${_CALL_STACK[*]}" "$(_env_get "$probe")" "$(db_get "$probe")"
)
compare() {
    record 0 "$@" > "$TMPDIR/off" 2> "$TMPDIR/off.err"
    local off_status=$?
    record 1 "$@" > "$TMPDIR/on" 2> "$TMPDIR/on.err"
    check test "$?" = "$off_status"
    check cmp "$TMPDIR/off" "$TMPDIR/on"
    # Diagnostic source locations differ between the two entrypoint frames.
    sed -E 's/line [0-9]+/line N/g' "$TMPDIR/off.err" > "$TMPDIR/off.normal"
    sed -E 's/line [0-9]+/line N/g' "$TMPDIR/on.err" > "$TMPDIR/on.normal"
    check cmp "$TMPDIR/off.normal" "$TMPDIR/on.normal"
}
for selector in constant safeEmpty nested assignedAdd assignedEmpty assignedRaw empty multiline optionText escapedText failSilent failOutput effects directEffects lastResult nesting exitAfterOutput ensureFailure throwFailure divideByZero callbacks dangerousLiteral; do
    compare ResultProbe "$selector"
done
for value in '' plain 'two words' 'foo:' ':' '-n' '-e' '-ne' $'-n\n' $'first\nsecond\n\n' 'first\nsecond' '"quoted"' '$(printf injected)'; do
    compare ResultProbe valueWith: "$value"
done
compare ResultProbe valueWith:
for value in 0 -4 08 '1 / 0' BASH_SUBSHELL missingVariable '$(printf 8)' 12345678901234567890; do
    compare ResultProbe add: "$value" to: 2
    compare ResultProbe product: "$value" by: 2
 done
compare ResultProbe add: 1 to:
compare "$probe" getNumber
compare "$probe" arithmetic
compare "$probe" writeThenRead
compare "$probe" cascade
captured=$(@ Block params: '[]' code: '_ivar number' captured: "{\"_RECEIVER\":\"$probe\"}")
early=$(@ Block params: '[]' code: 'return 7' captured: '{}')
compare "$captured" value
compare "$early" value
# A deterministic write between receiver decoding and the actual field read.
original_ivar=$(declare -f _ivar)
eval "${original_ivar/_ivar ()/_value_test_ivar ()}"
_ivar() {
    _env_set "$probe" '{"class":"ResultProbe","number":"1 / 0","step":3,"_vars":["number","step"]}'
    _value_test_ivar "$@"
}
compare "$probe" getNumber
eval "$original_ivar"
compare DoesNotExist missing
compare ResultProbe missing

# Hooks and shell modes must keep the original boundary, even for a constant.
assert_fallback() {
    VALUE_TRACE="$TMPDIR/fallback"
    : > "$VALUE_TRACE"
    local answer
    answer=$(_trash_value_send ResultProbe constant)
    check test "$answer" = 'plain result'
    check test "$(< "$VALUE_TRACE")" = "constant:$((BASH_SUBSHELL+2))"
    VALUE_TRACE=''
}
( shopt -s xpg_echo; compare ResultProbe valueWith: 'first\nsecond'; assert_fallback; exit "$failed" ); check test "$?" = 0
( set -E; assert_fallback; exit "$failed" ); check test "$?" = 0
( set -T; assert_fallback; exit "$failed" ); check test "$?" = 0
( set -o posix; assert_fallback; exit "$failed" ); check test "$?" = 0
( shopt -s inherit_errexit; assert_fallback; exit "$failed" ); check test "$?" = 0
( _ENSURE_DEPTH=1; _ENSURE_STACK[0]=':'; assert_fallback; exit "$failed" ); check test "$?" = 0
( _HANDLER_DEPTH=1; _HANDLER_STACK[0]='NoError|:'; assert_fallback; exit "$failed" ); check test "$?" = 0
( _BEFORE_ADVICE=('Other:missing:unused'); assert_fallback; exit "$failed" ); check test "$?" = 0
( _AFTER_ADVICE=('Other:missing:unused'); assert_fallback; exit "$failed" ); check test "$?" = 0
( TRASH_PROFILE=1 TRASH_PROFILE_FILE="$TMPDIR/profile"; assert_fallback; exit "$failed" ); check test "$?" = 0
# Errexit success and missing-argument failure are compared in separate shells.
for mode in 0 1; do
    ( export TRASHTALK_VALUE_SEND="$mode"; set -e; answer=$(_trash_value_send ResultProbe constant); printf '%s' "$answer" ) > "$TMPDIR/strict.$mode"
    check test "$?" = 0
 done
check cmp "$TMPDIR/strict.0" "$TMPDIR/strict.1"

# Class/instance precedence must select the actual method, including values
# ending in ':' which are arguments, not selector fragments.
check test "$(_trash_value_send ResultProbe valueWith: 'foo:')" = 'foo:'
check test "$(_trash_value_send "$probe" constant)" = 'plain result'
# Unmarked inherited/trait implementations and actual dispatch precedence.
for name in ResultChild ResultOverride ResultFlavor ResultTraitUser ResultPrecedence; do
    case $name in
        ResultChild) definition='ResultChild subclass: ResultProbe' ;;
        ResultOverride) definition='ResultOverride subclass: ResultProbe
  rawClassMethod: constant [ printf "%s" "$BASH_SUBSHELL" ]' ;;
        ResultFlavor) definition='ResultFlavor trait
  rawMethod: flavor [ printf "%s" "$BASH_SUBSHELL" ]' ;;
        ResultTraitUser) definition='ResultTraitUser subclass: Object
  include: ResultFlavor' ;;
        ResultPrecedence) definition='ResultPrecedence subclass: Object
  rawMethod: constant [ printf "%s" "$BASH_SUBSHELL" ]
  classMethod: constant [ ^ "class value" ]' ;;
    esac
    file="$TRASHDIR/$name.trash"; output="$TRASHDIR/.compiled/$name"
    if [[ $name == ResultFlavor ]]; then file="$TRASHDIR/traits/$name.trash"; output="$TRASHDIR/.compiled/traits/$name"; fi
    printf '%s\n' "$definition" > "$file"
    TRASHTALK_VALUE_SEND=1 bash "$ROOT/lib/jq-compiler/driver.bash" compile "$file" -o "$output" --check >/dev/null || exit
 done
compare ResultChild constant
compare ResultOverride constant
compare ResultTraitUser flavor
compare ResultPrecedence constant
precedence=$(@ ResultPrecedence new)
compare "$precedence" constant
# Every class callback in the collection primitive loses exactly one capture.
array=$(@ Array new)
@ "$array" setItems: '["one","two","three"]'
VALUE_TRACE="$TMPDIR/map.depth"
for mode in 0 1; do
    export TRASHTALK_VALUE_SEND="$mode"
    : > "$VALUE_TRACE"
    mapped=$(@ "$array" collect: ResultProbe)
    check test "$(@ "$mapped" items | jq -c .)" = '["one","two","three"]'
    cp "$VALUE_TRACE" "$TMPDIR/map.$mode"
 done
check test "$(< "$TMPDIR/map.0")" = $'valueWith::5\nvalueWith::5\nvalueWith::5'
check test "$(< "$TMPDIR/map.1")" = $'valueWith::4\nvalueWith::4\nvalueWith::4'
VALUE_TRACE=''
# Legacy accessor replacement invalidates only the replaced method.
__ResultProbe__valueMethods[getNumber]='argc:0'
_generate_accessor number ResultProbe
check test -z "${__ResultProbe__valueMethods[getNumber]:-}"
check test -n "${__ResultProbe__valueMethods[class__constant]:-}"

# Reloading to an unmarked raw method must not retain a capability. The raw
# method observes nesting, which makes a stale capability a deterministic bug.
printf 'ResultProbe subclass: Object\n  rawClassMethod: constant [ printf "%%s" "$BASH_SUBSHELL" ]\n' > "$TMPDIR/replacement.trash"
compile_probe "$TMPDIR/replacement.trash" || exit
@ Trash reloadClass: ResultProbe > "$TMPDIR/reloaded"
check test -z "${__ResultProbe__valueMethods[class__constant]:-}"
compare ResultProbe constant
check test "$(_trash_value_send ResultProbe constant)" = 2
# Old metadata and raw integrations invalidate the whole class generation.
_trash_invalidate_value_methods ResultProbe
compare ResultProbe constant
_clear_all_class_caches
check test "$failed" = 0
printf 'Value-send runtime failures: %s\n' "$failed"
exit "$((failed > 0))"
