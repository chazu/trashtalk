# Sourced ONLY by the experiment's disposable checkout. Public @ is unchanged.
declare -A _EXP_HASH=() _EXP_OWNER=() _EXP_BFUNC=() _EXP_ARITY=()

_exp_eligible() {
    [[ "$EXP_MODE" != A_all ]] || return 0
    # Global hooks and nondefault shell behavior must use the old boundary.
    [[ ${#_BEFORE_ADVICE[@]} == 0 && ${#_AFTER_ADVICE[@]} == 0 &&
       $_ENSURE_DEPTH == 0 && $_HANDLER_DEPTH == 0 && -z "${TRASH_PROFILE:-}" ]] || return 1
    shopt -q xpg_echo && return 1
    [[ $- != *T* && $- != *E* ]] || return 1
    local __exp_func="${___func_prefix}__${___normalized}" __exp_hash_var
    if [[ -z "$_receiver_instance" ]] && declare -F "${___func_prefix}__class__${___normalized}" >/dev/null; then
        __exp_func="${___func_prefix}__class__${___normalized}"
    fi
    [[ -n "${_EXP_HASH[$__exp_func]:-}" ]] || return 1
    if [[ "$EXP_MODE" == B_* && "${EXP_UNCHECKED_B:-0}" != 1 ]]; then
        (( $# >= ${_EXP_ARITY[$__exp_func]} + 2 )) || return 1
    fi
    __exp_hash_var="${_EXP_OWNER[$__exp_func]}__sourceHash"
    # Recompilation/reload changes the hash: old capabilities become ineligible.
    [[ "${!__exp_hash_var:-}" == "${_EXP_HASH[$__exp_func]}" ]]
}

_exp_capture_into() {
    local __exp_dest="$1" __exp_captured __exp_status; shift
    __exp_captured=$(@ "$@")
    __exp_status=$?
    printf -v "$__exp_dest" '%s' "$__exp_captured"
    return "$__exp_status"
}

_exp_capture_prepared() {
    local __exp_dest="$1" __exp_captured __exp_status; shift
    __exp_captured=$(_exp_public_tail "$@")
    __exp_status=$?
    printf -v "$__exp_dest" '%s' "$__exp_captured"
    return "$__exp_status"
}

_exp_capture_prepared_direct() {
    local __exp_dest="$1" __exp_captured __exp_status; shift
    __exp_captured=$(send "$@")
    __exp_status=$?
    printf -v "$__exp_dest" '%s' "$__exp_captured"
    return "$__exp_status"
}

_exp_return() {
    # The bounded subset uses echo with one argument and no xpg_echo.
    # Match echo's option-only result and capture's trailing-newline removal.
    __exp_value="$1"
    [[ ! "$__exp_value" =~ ^-[neE]+$ ]] || __exp_value=''
    while [[ "$__exp_value" == *$'\n' ]]; do __exp_value=${__exp_value%$'\n'}; done
}

_exp_is_integer() {
    # Conservative range avoids arithmetic syntax, octal and overflow surprises.
    [[ "$1" =~ ^-?(0|[1-9][0-9]*)$ && ${#1} -le 18 ]]
}

_exp_scalar_fallback() {
    local __exp_original="$1" __exp_status; shift
    # Only a selected scalar body can reach here. Keep errors inside the old
    # two capture boundaries; dispatch/cleanup are already owned by this frame.
    __exp_value=$(
        local __exp_output __exp_code
        __exp_output=$("$__exp_original" "$@")
        __exp_code=$?
        [[ -z "$__exp_output" ]] || echo "$__exp_output"
        return "$__exp_code"
    )
    __exp_status=$?
    return "$__exp_status"
}

_exp_invoke() {
    local __exp_fn="$1"; shift
    if [[ -n "${_EXP_BFUNC[$__exp_fn]:-}" ]]; then
        "${_EXP_BFUNC[$__exp_fn]}" "$@"
    else
        "$__exp_fn" "$@"
    fi
}

_exp_assign() {
    case "$EXP_MODE" in
        C|C_native) _exp_capture_into "$@" ;;
        A_guard|A_all)
            local __exp_dest="$1" __exp_captured __exp_status; shift
            __exp_captured=$(_exp_value_send "$@")
            __exp_status=$?
            printf -v "$__exp_dest" '%s' "$__exp_captured"
            return "$__exp_status"
            ;;
        B_naive) _exp_into_naive "$@" ;;
        B_guard) _exp_into "$@" ;;
    esac
}
