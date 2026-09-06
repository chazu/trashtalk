#!/usr/bin/env bash
set -uo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" || exit 1
source "$EXP_SUPPORT/runtime.bash"
source "$EXP_SUPPORT/generated.bash"
for name in Object Counter Array Block Trash Runtime String ResultProbe ResultChild; do
    _ensure_class_sourced "$name" || exit 1
done
source "$EXP_SUPPORT/capabilities.bash"
# Fixed IDs make independent variants directly comparable.
(_create_instance ResultProbe resultprobe_fixture) || exit 1
(_create_instance Counter counter_fixture) || exit 1
(_create_instance Array array_fixture) || exit 1
for ((i=1;i<10;i++)); do (_create_instance Counter "counter_fixture$i") || exit 1; done
@ array_fixture setItems: '["0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"]' || exit 1
block=$(@ Block params: '["x"]' code: 'printf "%s\n" "$x"' captured: '{}')
[[ -n "$block" ]] || exit 1
captured=$(@ Block params: '[]' code: '_ivar number' captured: '{"_RECEIVER":"resultprobe_fixture"}')
early=$(@ Block params: '[]' code: 'return 7' captured: '{}')

_exp_before() { printf before; }
_exp_after() { printf after; }

semantic_case() (
    local name="$1"; shift
    local observed='' EXP_LOCAL=original __=seed status
    case "$name" in
        xpg_*) shopt -s xpg_echo ;;
        advice) _add_before_advice ResultProbe constant _exp_before; _add_after_advice ResultProbe constant _exp_after ;;
        errtrace) set -E ;;
        functrace) set -T ;;
        profiling) TRASH_PROFILE=1 ;;
        invalid_numeric) _env_set resultprobe_fixture '{"class":"ResultProbe","number":"1 / 0","step":3,"_vars":["number","step"]}' ;;
        invalid_octal) _env_set resultprobe_fixture '{"class":"ResultProbe","number":"08","step":3,"_vars":["number","step"]}' ;;
        missing_numeric) _env_set resultprobe_fixture '{"class":"ResultProbe","number":"missing_fixture_name","step":3,"_vars":["number","step"]}' ;;
        intervening_write)
            _env_set resultprobe_fixture '{"class":"ResultProbe","number":7,"step":3,"_vars":["number","step"]}'
            original_ivar=$(declare -f _ivar)
            # declare -f uses the name-only function spelling on supported Bash.
            eval "${original_ivar/_ivar ()/_exp_original_ivar ()}"
            _ivar() {
                _env_set resultprobe_fixture '{"class":"ResultProbe","number":"1 / 0","step":3,"_vars":["number","step"]}'
                _exp_original_ivar "$@"
            }
            ;;
        reload)
            source "$EXP_SUPPORT/ReloadedProbe"
            ;;
        legacy)
            unset __ResultProbe__sourceHash
            ;;
    esac
    _exp_assign observed "$@" > "$EXP_RESULTS/$name.stdout" 2> "$EXP_RESULTS/$name.stderr"
    status=$?
    printf '%s\0' "$status" "$observed" "$__" "$EXP_LOCAL" "$PWD" "$-" \
        "$(trap -p USR1)" "$_CALL_DEPTH" "$_ENSURE_DEPTH" "$_HANDLER_DEPTH" \
        "$_ERROR_TYPE" "${_CALL_STACK[*]}" > "$EXP_RESULTS/$name.state"
    _env_get resultprobe_fixture | jq -c 'del(.created_at)' > "$EXP_RESULTS/$name.session"
    db_get resultprobe_fixture | jq -c 'del(.created_at)' > "$EXP_RESULTS/$name.store"
)

if [[ "$1" == semantics ]]; then
    for selector in constant nested empty multiline optionText escapedText failSilent failOutput \
        effects directEffects lastResult nesting exitAfterOutput ensureFailure throwFailure divideByZero callbacks handledFailure; do
        semantic_case "$selector" ResultProbe "$selector"
    done
    semantic_case getter resultprobe_fixture getNumber
    semantic_case arithmetic resultprobe_fixture arithmetic
    semantic_case mutation resultprobe_fixture writeThenRead
    semantic_case cascade resultprobe_fixture cascade
    for value in '' '-n' '-e' '-E' '-ne' '-2' 'quote '\''' $'first\nsecond\n\n' 'back\slash' 'ends:'; do
        semantic_case "identity_${i:-0}" ResultProbe valueWith: "$value"
        i=$((i+1))
    done
    semantic_case xpg_raw ResultProbe escapedText
    semantic_case xpg_identity ResultProbe valueWith: 'first\nsecond'
    semantic_case advice ResultProbe constant
    semantic_case inherited ResultChild constant
    semantic_case overridden ResultOverride constant
    semantic_case trait ResultTraitUser traitValue
    semantic_case cold_class ResultCold constant
    semantic_case missing ResultProbe missingMethod
    semantic_case bad_receiver ../escape constant
    semantic_case captured_receiver "$captured" value
    semantic_case block_early_return "$early" value
    semantic_case errtrace ResultProbe constant
    semantic_case functrace ResultProbe constant
    semantic_case profiling ResultProbe constant
    semantic_case invalid_numeric resultprobe_fixture getNumber
    semantic_case invalid_octal resultprobe_fixture getNumber
    semantic_case missing_numeric resultprobe_fixture getNumber
    semantic_case missing_argument ResultProbe valueWith:
    semantic_case intervening_write resultprobe_fixture getNumber
    semantic_case reload ResultProbe constant
    semantic_case legacy ResultProbe constant
    # Isolated instrumented send records actual depth, without timing overhead.
    EXP_TRACE=1
    _exp_assign observed ResultProbe constant
    mv "$EXP_RESULTS/depth" "$EXP_RESULTS/constant.depth"
    _exp_assign observed array_fixture collect: ResultProbe
    mv "$EXP_RESULTS/depth" "$EXP_RESULTS/map.depth"
    mv "$EXP_RESULTS/constant.depth" "$EXP_RESULTS/depth"
    exit 0
fi

run_workload() {
    local name="$1" loops="$2" j result status=0
    for ((j=0;j<loops;j++)); do
        case "$name" in
            constant) _exp_assign result ResultProbe constant ;;
            getter) _exp_assign result resultprobe_fixture getNumber ;;
            arithmetic) _exp_assign result resultprobe_fixture arithmetic ;;
            nested) _exp_assign result ResultProbe nested ;;
            class_map) _exp_assign result array_fixture collect: ResultProbe ;;
            block_map) _exp_assign result array_fixture collect: "$block" ;;
            browser) _exp_assign result Trash instanceRecordsFor: Counter ;;
        esac
        status=$?
        [[ "$status" == 0 ]] || return "$status"
    done
    EXP_RESULT="$result"
}

validate_workload() {
    case "$1" in
        constant|nested) [[ "$EXP_RESULT" == 'plain result' ]] ;;
        getter) [[ "$EXP_RESULT" == 7 ]] ;;
        arithmetic) [[ "$EXP_RESULT" == 10 ]] ;;
        class_map|block_map)
            local data
            data=$(_env_get "$EXP_RESULT")
            jq -e '.items == [range(0;25)|tostring]' <<< "$data" >/dev/null
            ;;
        browser) jq -se 'length==10 and all(.[]; .class_name=="Counter" and .data.value==0)' <<< "$EXP_RESULT" >/dev/null ;;
    esac
}

printf 'READY\n'
while read -r name loops sample; do
    [[ "$name" != stop ]] || break
    start=${EPOCHREALTIME/./}
    run_workload "$name" "$loops" > "$TMPDIR/unexpected.out" 2> "$TMPDIR/unexpected.err"
    status=$?
    elapsed=$((${EPOCHREALTIME/./}-start))
    if [[ "$status" != 0 ]] || [[ -s "$TMPDIR/unexpected.out" || -s "$TMPDIR/unexpected.err" ]] || ! validate_workload "$name"; then
        printf 'INVALID %s %s\n' "$name" "$status"
        cat "$TMPDIR/unexpected.out" "$TMPDIR/unexpected.err" >&2
        exit 1
    fi
    printf '{"mode":"%s","case":"%s","sample":%s,"loops":%s,"microseconds":%s}\n' "$EXP_MODE" "$name" "$sample" "$loops" "$elapsed"
done
