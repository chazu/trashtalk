#!/usr/bin/env bash
# Serialization is a Bash/jq boundary shared by DSL read and traversal
# primitives. jq @sh quotes data; only that generated quoting reaches eval.
_TRASH_JSON_QUERY="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/json-values.jq"

_trash_json_get() {
    jq -sr --arg operation "$1" --arg path "$3" --arg fallback "${4:-null}" \
        -f "$_TRASH_JSON_QUERY" <<< "$2"
}

_trash_json_decode() {
    local __tj_target="$1" __tj_quoted
    [[ "$__tj_target" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ ]] || return 2
    __tj_quoted=$(_trash_json_get "$3" "$2" "${4:-}") || return
    # Targets are compiler-owned names; jq supplies shell-quoted values only.
    eval "$__tj_target=($__tj_quoted)"
}

_trash_json_unpack() {
    local __tj_data="$1" __tj_paths="$2" __tj_name __tj_i=0
    local -a __tj_values=()
    shift 2
    for __tj_name in "$@"; do
        [[ "$__tj_name" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ && "$__tj_name" != __tj_* ]] || return 2
    done
    _trash_json_decode __tj_values "$__tj_data" unpack "$__tj_paths" || return
    [[ ${#__tj_values[@]} == $# ]] || { echo 'JSON field/binding count mismatch' >&2; return 2; }
    for __tj_name in "$@"; do
        printf -v "$__tj_name" '%s' "${__tj_values[__tj_i++]}"
    done
}

# Block bodies are Bash code emitted by the compiler. Name binding and eval
# belong here; collection traversal continues to use public message sends.
# String intrinsics that need more than one parameter expansion. The compiler
# emits these inside one capture; they never dispatch.
_trash_str_trim() {
    local __s="$1"
    if [[ "$__s" =~ ^[[:space:]]*(.*[^[:space:]])[[:space:]]*$ ]]; then
        printf '%s' "${BASH_REMATCH[1]}"
    fi
}

# Newline-separated text as a JSON array of non-empty lines.
_trash_str_lines() {
    printf '%s' "$1" | jq -Rsc 'split("\n") | map(select(length > 0))'
}

_trash_block_invoke() {
    local __tb_data __tb_name __tb_i
    local -a __tb_parts=()
    if [[ "$_CLASS" == Block && -n "${__Block__declaredMethods:-}" &&
          "$__Block__declaredMethods" != *' code '* &&
          "$__Block__declaredMethods" != *' params '* &&
          "$__Block__declaredMethods" != *' captured '* ]]; then
        _ensure_loaded "$_RECEIVER" || return
        __tb_data=$(_env_get "$_RECEIVER")
        _trash_json_decode __tb_parts "$__tb_data" block || return
    else
        # Subclasses may override the metadata getters. Preserve that dispatch.
        local __tb_code __tb_params __tb_captured
        __tb_code=$(@ "$_RECEIVER" code) || return
        __tb_params=$(@ "$_RECEIVER" params) || return
        __tb_captured=$(@ "$_RECEIVER" captured) || return
        __tb_data=$(jq -cn --arg code "$__tb_code" --argjson params "${__tb_params:-null}" \
            --argjson captured "${__tb_captured:-null}" '{code:$code,params:$params,captured:$captured}') || return
        _trash_json_decode __tb_parts "$__tb_data" block || return
    fi
    # Dynamic locals restore the receiver when eval returns, including an
    # explicit return or failure inside the block body.
    local _RECEIVER="${__tb_parts[1]:-$_RECEIVER}"
    for ((__tb_i=0; __tb_i<$#; __tb_i++)); do
        __tb_name=${__tb_parts[__tb_i+2]:-}
        [[ -n "$__tb_name" ]] || continue
        [[ "$__tb_name" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ && "$__tb_name" != __tb_* ]] || {
            echo "Invalid block parameter: $__tb_name" >&2; return 2;
        }
        local "$__tb_name"
        printf -v "$__tb_name" '%s' "${@:__tb_i+1:1}"
    done
    eval "${__tb_parts[0]}"
}

# Callback invocation belongs at this primitive boundary. The collection
# classes choose the operation in DSL and install the final value only once.
_trash_json_collect() {
    local __tj_data="$1" __tj_block="$2" __tj_kind="$3" __tj_select="${4:-false}"
    local __tj_i __tj_key __tj_value __tj_result __tj_send=@
    [[ ${TRASHTALK_VALUE_SEND:-0} != 1 ]] || __tj_send=_trash_value_send
    local -a __tj_values=() __tj_results=()
    _trash_json_decode __tj_values "$__tj_data" "$__tj_kind" || return
    for ((__tj_i=0; __tj_i<${#__tj_values[@]}; __tj_i++)); do
        if [[ "$__tj_kind" == object ]]; then
            __tj_key=${__tj_values[__tj_i++]}
        fi
        __tj_value=${__tj_values[__tj_i]}
        if [[ "$__tj_kind" == object && "$__tj_select" == true ]]; then
            __tj_result=$("$__tj_send" "$__tj_block" valueWith: "$__tj_key" and: "$__tj_value")
        else
            __tj_result=$("$__tj_send" "$__tj_block" valueWith: "$__tj_value")
        fi
        # Legacy select: predicates accept nonempty output; preserve that
        # contract, including callbacks whose false branch exits nonzero.
        if [[ "$__tj_select" == true ]]; then
            [[ -n "$__tj_result" ]] || continue
            __tj_result=$__tj_value
        fi
        [[ "$__tj_kind" != object ]] || __tj_results+=("$__tj_key")
        __tj_results+=("$__tj_result")
    done
    # NUL framing preserves empty strings and embedded/trailing newlines and
    # avoids argv size limits. Bash values themselves cannot contain NUL.
    { if ((${#__tj_results[@]})); then printf '%s\0' "${__tj_results[@]}"; fi; } |
        jq -Rsc --arg kind "$__tj_kind" '
          split("\u0000") | if .[-1] == "" then .[:-1] else . end
          | if $kind == "object" then . as $v | reduce range(0;length;2) as $i ({}; .[$v[$i]]=$v[$i+1]) else . end'
}

# Explicit bulk property assignment. Only exact compiler-generated setters may
# share an update; overrides, advice, profiling, and container coercion retain
# ordered public sends. No persistence is deferred or performed here.
_trash_assign_fields() {
    local fields="$1" id="$2" _receiver_class _receiver_instance _receiver_data
    local -A _receiver_values=()
    local -a pairs=()
    local key value fn expected actual updated i fast=true
    _resolve_receiver "$id" || return
    [[ -n "$_receiver_instance" ]] || return 1
    _ensure_class_sourced "$_receiver_class" || return
    _trash_json_decode pairs "$fields" fields || return
    [[ ${#_BEFORE_ADVICE[@]} == 0 && ${#_AFTER_ADVICE[@]} == 0 &&
       $_ENSURE_DEPTH == 0 && $_HANDLER_DEPTH == 0 && -z ${TRASH_PROFILE:-} ]] || fast=false
    for ((i=0;i<${#pairs[@]};i+=2)); do
        key=${pairs[i]} value=${pairs[i+1]}
        # The ordinary setter's JSON-stream coercion remains the authority for
        # container-looking text, including malformed or multiple documents.
        if [[ "$value" =~ ^\[.*\]$ || "$value" =~ ^\{.*\}$ ]]; then fast=false; fi
        fn="__${_receiver_class//::/__}__${key}_"
        printf -v expected '%s () \n{ \n    _ivar_set %s "$1"\n}' "$fn" "$key"
        actual=$(declare -f "$fn") || fast=false
        [[ "$actual" == "$expected" ]] || fast=false
    done
    if [[ "$fast" == true ]]; then
        updated=$(printf '%s' "$_receiver_data" | jq -c --argjson fields "$fields" '
          reduce ($fields|to_entries[]) as $field (.;
            .[$field.key] = ($field.value | if test("^-?[0-9]+$") then tonumber else . end))') || return
        _env_set "$id" "$updated" || return
    else
        for ((i=0;i<${#pairs[@]};i+=2)); do
            @ "$id" "${pairs[i]}:" "${pairs[i+1]}" >/dev/null || return
        done
    fi
    printf '%s\n' "$id"
}
