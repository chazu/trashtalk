# Sourced by driver.bash. Receipts describe inputs and the actual output hash;
# they are derived data, never executable shell or authoritative class state.
declare -A _BUILD_ACTIVE=() _BUILD_DONE=()

_build_source_for() {
    local name="${1//::/\/}" candidate
    for candidate in "$TRASHTALK_DIR/trash/$name.trash" \
        "$TRASHTALK_DIR/trash/user/$name.trash" "$TRASHTALK_DIR/trash/traits/$name.trash"; do
        if [[ -f "$candidate" ]]; then printf '%s\n' "$candidate"; return; fi
    done
    return 1
}

_build_hash() { shasum -a 256 "$@" | shasum -a 256 | cut -d' ' -f1; }

cmd_compile_cached() {
    local source_file="$1" output_file="$2"
    source_file=$(cd "$(dirname "$source_file")" && pwd)/${source_file##*/}
    mkdir -p "$(dirname "$output_file")"
    output_file=$(cd "$(dirname "$output_file")" && pwd)/${output_file##*/}
    [[ -z "${_BUILD_DONE[$source_file]:-}" ]] || return 0
    [[ -z "${_BUILD_ACTIVE[$source_file]:-}" ]] || error "Cyclic build dependency: $source_file"
    _BUILD_ACTIVE[$source_file]=1
    _compiler_version >/dev/null
    local receipt="${output_file%/*}/.buildcache/${output_file##*/}.json" source_hash metadata old signature output_hash
    mkdir -p "${receipt%/*}"
    source_hash=$(_build_hash "$source_file")
    old=$(jq -ce --arg source "$source_file" --arg hash "$source_hash" --arg compiler "$_COMPILER_VERSION" \
        'select(.version == 1 and .source == $source and .source_hash == $hash and .compiler == $compiler
         and (.metadata | type == "object") and (.metadata.traits | type == "array"))' "$receipt" 2>/dev/null) || old=''
    if [[ -n "$old" ]]; then metadata=$(jq -c '.metadata' <<< "$old")
    else metadata=$(_parse_single_file "$source_file") || return
        metadata=$(jq -c '{name,package,parent,parentPackage,traits}' <<< "$metadata") || return
    fi

    local parent dependency dependency_source dependency_output dependencies_json
    local -a inputs=("$source_file") requirements=()
    parent=$(_resolved_parent <<< "$metadata")
    if [[ -n "$parent" && "$parent" != nil ]]; then requirements+=("$parent"); fi
    while IFS= read -r dependency; do
        [[ -z "$dependency" ]] || requirements+=("$dependency")
    done < <(jq -r '.traits[]?' <<< "$metadata")
    for dependency in "${requirements[@]}"; do
        if dependency_source=$(_build_source_for "$dependency"); then
            if [[ "$dependency_source" == */traits/* ]]; then
                dependency_output="${TRASHTALK_COMPILED_DIR:-$TRASHTALK_DIR/trash/.compiled}/traits/${dependency//::/__}"
            else
                dependency_output="${TRASHTALK_COMPILED_DIR:-$TRASHTALK_DIR/trash/.compiled}/${dependency//::/__}"
            fi
            cmd_compile_cached "$dependency_source" "$dependency_output" || return
            inputs+=("$dependency_source" "$dependency_output")
        elif [[ "$dependency" != Object ]]; then
            error "Missing build dependency '$dependency' required by $source_file"
        fi
    done
    signature=$(_build_hash "${inputs[@]}")
    if [[ -n "$old" && -f "$output_file" ]]; then
        output_hash=$(_build_hash "$output_file")
        if jq -e --arg signature "$signature" --arg output "$output_hash" \
            --arg strict "${TRASHTALK_STRICT:-}" --arg lenient "${TRASHTALK_LENIENT:-}" \
            '.signature == $signature and .output_hash == $output and .strict == $strict and .lenient == $lenient' \
            <<< "$old" >/dev/null; then
            _BUILD_DONE[$source_file]=1
            unset '_BUILD_ACTIVE[$source_file]'
            printf '  = %s (unchanged)\n' "${output_file##*/}"
            return 0
        fi
    fi
    local candidate receipt_candidate
    candidate=$(mktemp "$output_file.XXXXXX")
    if cmd_compile "$source_file" "$candidate" true; then
        if [[ "$signature" != "$(_build_hash "${inputs[@]}")" ]]; then
            rm -f "$candidate"
            error "Build inputs changed during compilation: $source_file; retry the build"
        fi
        mv -f "$candidate" "$output_file"
        output_hash=$(_build_hash "$output_file")
        receipt_candidate=$(mktemp "$receipt.XXXXXX")
        jq -cn --arg source "$source_file" --arg source_hash "$source_hash" \
            --arg compiler "$_COMPILER_VERSION" --arg signature "$signature" --arg output_hash "$output_hash" \
            --arg strict "${TRASHTALK_STRICT:-}" --arg lenient "${TRASHTALK_LENIENT:-}" \
            --slurpfile metadata <(printf '%s' "$metadata") \
            '{version:1,source:$source,source_hash:$source_hash,compiler:$compiler,signature:$signature,
              output_hash:$output_hash,strict:$strict,lenient:$lenient,metadata:$metadata[0]}' > "$receipt_candidate"
        mv -f "$receipt_candidate" "$receipt"
        _BUILD_DONE[$source_file]=1
        unset '_BUILD_ACTIVE[$source_file]'
        printf '  ✓ %s\n' "${output_file##*/}"
    else
        rm -f "$candidate"
        return 1
    fi
}
