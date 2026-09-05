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

# Callback invocation belongs at this primitive boundary. The collection
# classes choose the operation in DSL and install the final value only once.
_trash_json_collect() {
    local __tj_data="$1" __tj_block="$2" __tj_kind="$3" __tj_select="${4:-false}"
    local __tj_i __tj_key __tj_value __tj_result
    local -a __tj_values=() __tj_results=()
    _trash_json_decode __tj_values "$__tj_data" "$__tj_kind" || return
    for ((__tj_i=0; __tj_i<${#__tj_values[@]}; __tj_i++)); do
        if [[ "$__tj_kind" == object ]]; then
            __tj_key=${__tj_values[__tj_i++]}
        fi
        __tj_value=${__tj_values[__tj_i]}
        if [[ "$__tj_kind" == object && "$__tj_select" == true ]]; then
            __tj_result=$(@ "$__tj_block" valueWith: "$__tj_key" and: "$__tj_value")
        else
            __tj_result=$(@ "$__tj_block" valueWith: "$__tj_value")
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
