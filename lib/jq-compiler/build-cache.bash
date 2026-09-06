# Build receipts are derived data. One coordinator validates the graph; only
# changed nodes invoke compiler workers, in dependency order.
_build_hash() { shasum -a 256 "$@" | shasum -a 256 | cut -d' ' -f1; }

_build_output_for() {
    local relative=${1#"$TRASHTALK_DIR/trash/"}
    case "$relative" in
        traits/*) printf '%s/traits/%s\n' "$TRASHTALK_COMPILED_DIR" "${relative##*/}" ;;
        user/*) printf '%s/%s\n' "$TRASHTALK_COMPILED_DIR" "${relative##*/}" ;;
        *) printf '%s/%s\n' "$TRASHTALK_COMPILED_DIR" "${relative//\//__}" ;;
    esac
}

# Hash all source/artifact files in one process. Associate by position so spaces
# and backslashes in paths cannot be confused with shasum's display escaping.
_build_hash_inventory() {
    local file line i=0
    local -a files=()
    for file in "${build_sources[@]}" "${build_outputs[@]}"; do
        [[ ! -f "$file" ]] || files+=("$file")
    done
    shasum -a 256 -- "${files[@]}" > "$build_work/hashes.raw" || return
    while IFS= read -r line; do
        line=${line#\\}
        printf '%s\0%s\0' "${files[i++]}" "${line%% *}"
    done < "$build_work/hashes.raw" > "$build_work/hashes.nul"
    jq -Rsc 'split("\u0000") | .[:-1] | . as $v | reduce range(0;length;2) as $i ({}; .[$v[$i]]=$v[$i+1])' \
        "$build_work/hashes.nul" > "$build_work/hashes.json"
}

_build_inventory() {
    local i receipt old metadata relative key priority
    for ((i=0;i<${#build_sources[@]};i++)); do
        receipt="${build_outputs[i]%/*}/.buildcache/${build_outputs[i]##*/}.json"
        old=''; metadata=''
        [[ ! -f "$receipt" ]] || old=$(<"$receipt")
        [[ ! -f "$build_work/meta/$i" ]] || metadata=$(<"$build_work/meta/$i")
        relative=${build_sources[i]#"$TRASHTALK_DIR/trash/"}
        priority=0
        case "$relative" in user/*) relative=${relative#user/}; priority=1;; traits/*) relative=${relative#traits/}; priority=2;; esac
        key=${relative%.trash}; key=${key//\//::}
        printf '%s\0' "${build_sources[i]}" "${build_outputs[i]}" "$receipt" "$old" "$metadata" \
            "$key" "$priority" "${build_requested[${build_sources[i]}]:-false}"
    done > "$build_work/inventory.nul"
    jq -Rsc --slurpfile hashes "$build_work/hashes.json" --arg compiler "$_COMPILER_VERSION" '
      split("\u0000") | .[:-1] | . as $v |
      [range(0;length;8) as $i |
        {index:($i/8),source:$v[$i],output:$v[$i+1],receipt:$v[$i+2],
         old:(try ($v[$i+3]|fromjson) catch null), parsed:(try ($v[$i+4]|fromjson) catch null),
         key:$v[$i+5],priority:($v[$i+6]|tonumber),requested:($v[$i+7]=="true"),
         hash:$hashes[0][$v[$i]],output_hash:$hashes[0][$v[$i+1]]} |
        .metadata=(if .parsed != null then .parsed
          elif .old.source==.source and .old.source_hash==.hash and .old.compiler==$compiler
            and (.old.metadata|type)=="object" and (.old.metadata.traits|type)=="array"
          then .old.metadata else null end) | del(.parsed)]' \
        "$build_work/inventory.nul" > "$build_work/inventory.json"
}

_build_plan() {
    jq --arg mode "$1" --arg compiler "$_COMPILER_VERSION" \
        --arg strict "${TRASHTALK_STRICT:-}" --arg lenient "${TRASHTALK_LENIENT:-}" \
        -f "$SCRIPT_DIR/build-plan.jq" "$build_work/inventory.json"
}

cmd_build_metadata() {
    _parse_single_file "$1" | jq -c '{name,package,parent,parentPackage,traits}' > "$2"
}

cmd_build_worker() {
    local request="$1" source_file output_file source_hash before candidate
    local -a fields=() inputs=()
    mapfile -t fields < <(jq -r '.source,.output,.hash,(.dependencies|to_entries[]|.key,.value.output)' "$request")
    source_file=${fields[0]}; output_file=${fields[1]}; source_hash=${fields[2]}
    inputs=("$source_file" "${fields[@]:3}")
    [[ "$(shasum -a 256 "$source_file" | cut -d' ' -f1)" == "$source_hash" ]] || error "Build source changed: $source_file; retry"
    before=$(_build_hash "${inputs[@]}")
    mkdir -p "${output_file%/*}"
    candidate=$(mktemp "$output_file.XXXXXX")
    if cmd_compile "$source_file" "$candidate" true; then
        if [[ "$before" != "$(_build_hash "${inputs[@]}")" ]]; then
            rm -f "$candidate"
            error "Build inputs changed during compilation: $source_file; retry"
        fi
        mv -f "$candidate" "$output_file"
        printf '  ✓ %s\n' "${output_file##*/}"
    else
        rm -f "$candidate"
        return 1
    fi
}

# Run the coordinator in a subshell so temporary files/traps and planner state
# cannot escape into callers. compile-cached uses the same engine for one root.
cmd_compile_many() (
    local output_dir="$1" jobs="$2"; shift 2
    [[ "$jobs" =~ ^[1-9][0-9]*$ ]] || error 'Build jobs must be positive'
    [[ $# -gt 0 ]] || return 0
    mkdir -p "$output_dir"
    export TRASHTALK_COMPILED_DIR="$(cd "$output_dir" && pwd)"
    TRASHTALK_DIR="$(cd "$TRASHTALK_DIR" && pwd)"
    export TRASHTALK_DIR
    local build_work source output i idx level max_level receipt body tmp initial_compiler
    local -a build_sources=() build_outputs=() pending=() requested=()
    local -A build_requested=() known=()
    build_work=$(mktemp -d "${TMPDIR:-/tmp}/trash-build.XXXXXX")
    trap 'rm -rf "$build_work"' EXIT
    mkdir -p "$build_work/meta" "$build_work/jobs"
    for source in "$@"; do
        source="$(cd "$(dirname "$source")" && pwd)/${source##*/}"
        [[ -f "$source" ]] || error "Source file not found: $source"
        build_requested[$source]=true
        requested+=("$source")
    done
    for source in "${requested[@]}" "$TRASHTALK_DIR/trash/"*.trash "$TRASHTALK_DIR/trash/"*/*.trash; do
        [[ -f "$source" && -z "${known[$source]:-}" ]] || continue
        [[ "$source" != *$'\n'* ]] || error 'Build paths cannot contain newlines'
        known[$source]=1
        output=$(_build_output_for "$source"); output=${output%.trash}
        if [[ -n "${BUILD_SINGLE_OUTPUT:-}" && "$source" == "${requested[0]}" ]]; then output=$BUILD_SINGLE_OUTPUT; fi
        build_sources+=("$source"); build_outputs+=("$output")
    done
    _compiler_version >/dev/null
    export _COMPILER_VERSION
    initial_compiler=$_COMPILER_VERSION
    _build_hash_inventory
    while :; do
        _build_inventory
        _build_plan frontier > "$build_work/frontier.json" || return
        mapfile -t pending < <(jq -r '.[]' "$build_work/frontier.json")
        ((${#pending[@]})) || break
        for idx in "${pending[@]}"; do printf '%s\0%s\0' "${build_sources[idx]}" "$build_work/meta/$idx"; done |
            xargs -0 -P"$jobs" -n2 bash "$SCRIPT_DIR/driver.bash" build-metadata || return
    done
    _build_plan final > "$build_work/plan.json" || return
    # Two selected sources must not silently overwrite the same artifact.
    jq -e 'group_by(.output) | all(.[]; length==1)' "$build_work/plan.json" >/dev/null || error 'Duplicate build output'
    max_level=$(jq '[.[]|select(.dirty)|.level] | max // -1' "$build_work/plan.json")
    if [[ "$max_level" == -1 ]]; then
        printf '  = %s artifacts unchanged\n' "$(jq length "$build_work/plan.json")"
        return 0
    fi
    # Each level contains independent classes; a dependent starts only after
    # every worker in the previous level succeeded.
    for ((level=0;level<=max_level;level++)); do
        jq -rj --argjson level "$level" '.[]|select(.dirty and .level==$level)|(.index|tostring),"\u0000",(tojson),"\u0000"' \
            "$build_work/plan.json" > "$build_work/level.nul"
        pending=()
        while IFS= read -r -d '' idx && IFS= read -r -d '' body; do
            printf '%s\n' "$body" > "$build_work/jobs/$idx"
            pending+=("$build_work/jobs/$idx")
        done < "$build_work/level.nul"
        ((${#pending[@]})) || continue
        printf '%s\0' "${pending[@]}" | xargs -0 -P"$jobs" -n1 bash "$SCRIPT_DIR/driver.bash" build-worker || return
    done
    # Receipts use the final dependency artifacts, including newly built ones.
    _build_hash_inventory
    unset _COMPILER_VERSION
    _compiler_version >/dev/null
    [[ "$_COMPILER_VERSION" == "$initial_compiler" ]] || error 'Compiler changed during build; retry'
    jq -e --slurpfile hashes "$build_work/hashes.json" 'all(.[]; .hash==$hashes[0][.source])' \
        "$build_work/plan.json" >/dev/null || error 'Source changed during build; retry'
    jq -rj --slurpfile hashes "$build_work/hashes.json" --arg compiler "$_COMPILER_VERSION" \
        --arg strict "${TRASHTALK_STRICT:-}" --arg lenient "${TRASHTALK_LENIENT:-}" '
      .[]|select(.dirty)|. as $node|.receipt,"\u0000",
      ({version:2,source:.source,source_hash:.hash,output_hash:$hashes[0][.output],
        compiler:$compiler,strict:$strict,lenient:$lenient,metadata:.metadata,
        dependencies:(.dependencies|with_entries(.value.output_hash=$hashes[0][.value.output]))}|tojson),"\u0000"' \
        "$build_work/plan.json" > "$build_work/receipts.nul"
    while IFS= read -r -d '' receipt && IFS= read -r -d '' body; do
        mkdir -p "${receipt%/*}"
        tmp=$(mktemp "$receipt.XXXXXX")
        printf '%s\n' "$body" > "$tmp"
        mv -f "$tmp" "$receipt"
    done < "$build_work/receipts.nul"
)

cmd_compile_cached() {
    local output="$2"
    mkdir -p "$(dirname "$output")"
    output="$(cd "$(dirname "$output")" && pwd)/${output##*/}"
    BUILD_SINGLE_OUTPUT="$output" cmd_compile_many "${TRASHTALK_COMPILED_DIR:-$TRASHTALK_DIR/trash/.compiled}" 1 "$1"
}
