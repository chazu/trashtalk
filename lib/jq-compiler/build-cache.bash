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
          then .old.metadata else null end) |
        (if (.key|startswith("/")) and .metadata != null then .key=.metadata.identity else . end) | del(.parsed)]' \
        "$build_work/inventory.nul" > "$build_work/inventory.json"
}

_build_plan() {
    jq -L "$SCRIPT_DIR" --arg mode "$1" --arg compiler "$_COMPILER_VERSION" "${build_mode_args[@]}" \
        -f "$SCRIPT_DIR/build-plan.jq" "$build_work/inventory.json"
}

cmd_build_metadata() {
    _parse_single_file "$1" | jq -c -L "$SCRIPT_DIR" 'include "protocols"; protocol_metadata | protocol_semantics' > "$2"
}

# Cache entries are keyed by content hash and compiler fingerprint, so every
# edit and every compiler change adds an entry that is never reused. After a
# successful build keep entries for the current sources in the current
# generation plus the most recently used previous generation (a quick revert),
# and remove staging files an interrupted build left behind. Never fails the
# build; a concurrent build's live staging file is younger than the ten-minute
# threshold and is kept, and a pruned entry only costs one re-parse.
_build_prune_caches() {
    local cache_root="$TRASHTALK_DIR/trash/.compiled" dir name generation content previous='' file i
    local -a stale=() roots=()
    local -A known=()
    if [[ -f "$build_work/hashes.json" ]]; then
        while IFS= read -r content; do known[$content]=1; done < <(jq -r '.[]' "$build_work/hashes.json")
    fi
    for dir in "$cache_root/.astcache" "$cache_root/.symbolcache"; do
        [[ -d "$dir" ]] || continue
        # ls -t lists newest first, so the first foreign generation seen is the
        # most recently used previous one.
        while IFS= read -r name; do
            [[ "$name" =~ ^([0-9a-f]{64})-([0-9a-f]{16})(-[0-9a-f]{64})?\.json$ ]] || continue
            content=${BASH_REMATCH[1]}; generation=${BASH_REMATCH[2]}
            if [[ "$generation" != "$_COMPILER_VERSION" ]]; then
                [[ -n "$previous" ]] || previous=$generation
                [[ "$generation" == "$previous" ]] || { stale+=("$dir/$name"); continue; }
            fi
            [[ ${#known[@]} -eq 0 || -n "${known[$content]:-}" ]] || stale+=("$dir/$name")
        done < <(ls -t "$dir" 2>/dev/null)
    done
    roots=("$TRASHTALK_COMPILED_DIR" "$TRASHTALK_COMPILED_DIR/traits"
        "$TRASHTALK_COMPILED_DIR/.buildcache" "$TRASHTALK_COMPILED_DIR/traits/.buildcache"
        "$cache_root/.astcache" "$cache_root/.symbolcache")
    while IFS= read -r -d '' file; do
        name=${file##*/}
        if [[ "$name" =~ \.[A-Za-z0-9]{6}$ || "$name" == *.tmp ]]; then stale+=("$file"); fi
    done < <(find "${roots[@]}" -maxdepth 1 -type f \( -name '*.??????' -o -name '*.tmp' \) -mmin +10 -print0 2>/dev/null)
    for ((i=0; i<${#stale[@]}; i+=200)); do
        rm -f -- "${stale[@]:i:200}" 2>/dev/null || true
    done
    return 0
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
    if BUILD_VALIDATED_REQUEST="$request" cmd_compile "$source_file" "$candidate" true; then
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

# Reconcile against the filesystem in a private snapshot. Only a successful
# build publishes this retirement; existing source paths still own identities.
_build_manifest_snapshot() {
    local manifest="$TRASHTALK_COMPILED_DIR/.protocol-manifest.json" source
    if [[ ! -f "$manifest" ]]; then
        printf '{"schema":1,"entries":{}}\n' > "$build_work/previous-manifest.json"
        return
    fi
    while IFS= read -r source; do
        [[ ! -f "$source" ]] || printf '%s\n' "$source"
    done < <(jq -r '.entries[].source' "$manifest") > "$build_work/live-sources"
    jq --rawfile live "$build_work/live-sources" '
      ($live|split("\n")) as $paths |
      .entries |= with_entries(select(.value.source as $s | $paths|index($s)))
      ' "$manifest" > "$build_work/previous-manifest.json"
}

# Remove only unchanged artifacts owned by retired entries. A moved source can
# reuse the same output; the new manifest protects it from deletion.
_build_retire_artifacts() {
    local artifact receipt expected actual
    while IFS= read -r -d '' artifact && IFS= read -r -d '' receipt && IFS= read -r -d '' expected; do
        [[ "$artifact" == "$TRASHTALK_COMPILED_DIR/"* && -f "$artifact" ]] || continue
        actual=$(shasum -a 256 "$artifact"); actual=${actual%% *}
        [[ "$actual" == "$expected" ]] || continue
        rm -f -- "$artifact"
        [[ "$receipt" != "$TRASHTALK_COMPILED_DIR/"* ]] || rm -f -- "$receipt"
    done < "$build_work/retired-artifacts.nul"
}

# A full build (TRASH_BUILD_PRUNE_ORPHANS=1, set by `make`) also removes
# generated artifacts that no manifest entry owns and whose source is gone,
# such as output from before manifest tracking. A stale artifact would
# otherwise keep answering sends to a deleted or renamed class.
_build_prune_orphans() {
    [[ "${TRASH_BUILD_PRUNE_ORPHANS:-}" == 1 ]] || return 0
    local manifest="$TRASHTALK_COMPILED_DIR/.protocol-manifest.json" artifact name header source
    local -A owned=()
    [[ -f "$manifest" ]] || return 0
    while IFS= read -r artifact; do owned[$artifact]=1; done < <(jq -r '.entries[].artifact' "$manifest")
    for artifact in "$TRASHTALK_COMPILED_DIR"/* "$TRASHTALK_COMPILED_DIR"/traits/*; do
        [[ -f "$artifact" && -z "${owned[$artifact]:-}" ]] || continue
        name=${artifact##*/}
        [[ "$name" != .* && "$name" != *.* ]] || continue
        header=$(sed -n 2p "$artifact" 2>/dev/null)
        [[ "$header" == "# Generated by Trashtalk Compiler"* ]] || continue
        if [[ "$artifact" == "$TRASHTALK_COMPILED_DIR/traits/$name" ]]; then
            source="$TRASHTALK_DIR/trash/traits/$name.trash"
        else
            source="$TRASHTALK_DIR/trash/${name//__//}.trash"
            [[ -f "$source" ]] || source="$TRASHTALK_DIR/trash/user/$name.trash"
        fi
        [[ -f "$source" ]] && continue
        rm -f -- "$artifact"
        printf '  - removed orphaned %s\n' "${artifact#"$TRASHTALK_COMPILED_DIR/"}"
    done
}

# Fail when two planned nodes share the value at a dotted path, e.g. `output`.
# Args: $1=path, $2=error message
_build_collisions() {
    local collisions
    collisions=$(jq -r --arg path "$1" '($path | split(".")) as $p | group_by(getpath($p)) | map(select(length > 1) |
      "\(.[0] | getpath($p)) (\(map(.source) | sort | join(", ")))") | join("; ")' "$build_work/plan.json") ||
        error "$2"
    [[ -z "$collisions" ]] || error "$2: $collisions"
}

# Publish the manifest, then remove orphaned artifacts and stale cache entries.
_build_finish() {
    _build_publish_manifest || return
    _build_prune_orphans
    _build_prune_caches
}

# Run the coordinator in a subshell so temporary files/traps and planner state
# cannot escape into callers. compile-cached uses the same engine for one root.
cmd_compile_many() (
    local output_dir="$1" jobs="$2"; shift 2
    [[ "$jobs" =~ ^[1-9][0-9]*$ ]] || error 'Build jobs must be positive'
    [[ $# -gt 0 ]] || return 0
    mkdir -p "$output_dir"
    export TRASHTALK_COMPILED_DIR="$(cd "$output_dir" && pwd -P)"
    TRASHTALK_DIR="$(cd "$TRASHTALK_DIR" && pwd -P)"
    export TRASHTALK_DIR
    local build_work source output i idx level max_level receipt body tmp initial_compiler
    local -a build_sources=() build_outputs=() pending=() requested=()
    # Codegen modes recorded in receipts; a change makes every artifact dirty.
    local -a build_mode_args=(--arg value_send "${TRASHTALK_VALUE_SEND:-0}"
        --arg strict "${TRASHTALK_STRICT:-}" --arg lenient "${TRASHTALK_LENIENT:-}")
    local -A build_requested=() known=()
    build_work=$(mktemp -d "${TMPDIR:-/tmp}/trash-build.XXXXXX")
    trap 'rm -rf "$build_work"' EXIT
    mkdir -p "$build_work/meta" "$build_work/jobs"
    _build_manifest_snapshot || return
    for source in "$@"; do
        source="$(cd "$(dirname "$source")" && pwd -P)/${source##*/}"
        [[ -f "$source" ]] || error "Source file not found: $source"
        build_requested[$source]=true
        requested+=("$source")
    done
    if [[ -f "$TRASHTALK_COMPILED_DIR/.protocol-manifest.json" ]]; then
        while IFS= read -r source; do
            [[ -f "$source" ]] || continue
            build_requested[$source]=true
            requested+=("$source")
        done < <(jq -r --slurpfile live "$build_work/previous-manifest.json" --args '
          .entries as $entries |
          # Add every entry that depends on a source already in the set.
          def add_dependents: . as $roots | reduce $entries[] as $e ($roots;
            if any($e.receipt_data.dependencies|keys[]; . as $dep | $roots|index($dep)) then .+[$e.source] else . end) | unique;
          ($ARGS.positional + [$entries | to_entries[] | select($live[0].entries[.key] == null) | .value.source]) | unique |
          until(. == add_dependents; add_dependents) | .[]
          ' "${requested[@]}" < "$TRASHTALK_COMPILED_DIR/.protocol-manifest.json")
    fi
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
    if [[ -f "$TRASHTALK_COMPILED_DIR/.protocol-manifest.json" ]]; then
        jq -e --slurpfile old "$build_work/previous-manifest.json" '
          all(.[]; . as $n | $old[0].entries[$n.metadata.identity] as $e |
            if $e != null and $e.source != $n.source then error("Protocol manifest identity shadowed: " + $n.metadata.identity) else true end)
          ' "$build_work/plan.json" >/dev/null || return
    fi
    _build_collisions metadata.identity 'Ambiguous declared identity'
    mkdir -p "$build_work/api"
    while IFS= read -r -d '' idx && IFS= read -r -d '' body; do
        printf '%s' "$body" > "$build_work/api/$idx"
    done < <(jq -rj '.[] | (.index|tostring),"\u0000",({metadata,surface,hash,dependencies:(.dependencies|map_values(.source_hash))}|tojson),"\u0000"' "$build_work/plan.json")
    shasum -a 256 "$build_work/api/"* > "$build_work/api-hashes"
    jq -Rn '[inputs | capture("^(?<hash>[a-f0-9]+)  .*?/(?<index>[0-9]+)$")] | map({key:.index,value:.hash}) | from_entries' \
        < "$build_work/api-hashes" > "$build_work/api.json"
    jq --slurpfile api "$build_work/api.json" 'map(. + {api_hash:$api[0][(.index|tostring)]})' \
        "$build_work/plan.json" > "$build_work/hashed-plan.json"
    mv "$build_work/hashed-plan.json" "$build_work/plan.json"
    # Two selected sources must not silently overwrite the same artifact.
    _build_collisions output 'Duplicate build output'
    max_level=$(jq '[.[]|select(.dirty)|.level] | max // -1' "$build_work/plan.json")
    if [[ "$max_level" == -1 ]]; then
        printf '  = %s artifacts unchanged\n' "$(jq length "$build_work/plan.json")"
        _build_finish
        return
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
        --slurpfile plan "$build_work/plan.json" "${build_mode_args[@]}" '
      .[]|select(.dirty)|. as $node|.receipt,"\u0000",
      ({version:2,source:.source,source_hash:.hash,output_hash:$hashes[0][.output],
        compiler:$compiler,strict:$strict,lenient:$lenient,value_send:$value_send,metadata:.metadata,
        validation:{schema:1,implementing_surface_hash:.api_hash,
          protocol_hashes:(reduce .metadata.implementedProtocols[] as $p ({};
            .[$p]=([$plan[0][]|select(.key==$p)|.api_hash][0]))),result:"valid"},
        dependencies:(.dependencies|with_entries(.value.output_hash=$hashes[0][.value.output]))}|tojson),"\u0000"' \
        "$build_work/plan.json" > "$build_work/receipts.nul"
    while IFS= read -r -d '' receipt && IFS= read -r -d '' body; do
        mkdir -p "${receipt%/*}"
        tmp=$(mktemp "$receipt.XXXXXX")
        printf '%s\n' "$body" > "$tmp"
        mv -f "$tmp" "$receipt"
    done < "$build_work/receipts.nul"
    _build_finish
)

cmd_compile_cached() {
    local output="$2"
    mkdir -p "$(dirname "$output")"
    output="$(cd "$(dirname "$output")" && pwd -P)/${output##*/}"
    BUILD_SINGLE_OUTPUT="$output" cmd_compile_many "${TRASHTALK_COMPILED_DIR:-$TRASHTALK_DIR/trash/.compiled}" 1 "$1"
}

# One atomic manifest publication contains the same ordinary receipts used by
# the build cache. Sidecars are checked against these copies by boundary users;
# interrupted publication fails closed rather than accepting mixed generations.
_build_publish_manifest() {
    local manifest="$TRASHTALK_COMPILED_DIR/.protocol-manifest.json" tmp receipt
    local -a receipts=()
    mapfile -t receipts < <(jq -r '.[].receipt' "$build_work/plan.json")
    jq -s '.' "${receipts[@]}" > "$build_work/final-receipts.json" || return
    # Merge against the latest publication, including other completed builds.
    _build_manifest_snapshot || return
    tmp=$(mktemp "$manifest.XXXXXX")
    jq --slurpfile previous "$build_work/previous-manifest.json" \
       --slurpfile receipts "$build_work/final-receipts.json" '
      reduce .[] as $n ($previous[0]; .schema=1 |
        if .entries[$n.metadata.identity] != null and .entries[$n.metadata.identity].source != $n.source then
          error("Protocol manifest identity shadowed: " + $n.key)
        else . end |
        .entries[$n.metadata.identity]={identity:$n.metadata.identity,source:$n.source,artifact:$n.output,receipt:$n.receipt,
          kind:$n.metadata.kind,package:$n.metadata.package,imports:$n.metadata.imports,
          api_hash:$n.api_hash,surface:$n.surface,requirements:$n.metadata.requirements,
          receipt_data:([$receipts[0][]|select(.source==$n.source)][0])})
      ' "$build_work/plan.json" > "$tmp" || { rm -f "$tmp"; return 1; }
    : > "$build_work/retired-artifacts.nul"
    if [[ -f "$manifest" ]]; then
        jq -rj --slurpfile next "$tmp" '
          .entries[] | select(.artifact as $a | all($next[0].entries[]; .artifact != $a)) |
          .artifact,"\u0000",.receipt,"\u0000",.receipt_data.output_hash,"\u0000"
          ' "$manifest" > "$build_work/retired-artifacts.nul" || { rm -f "$tmp"; return 1; }
    fi
    if [[ -f "$manifest" ]] && jq -e --slurpfile old "$manifest" 'del(.generation)==($old[0]|del(.generation))' "$tmp" >/dev/null; then
        rm -f "$tmp"
    else
        # Small first line is a generation fence readable with a Bash builtin.
        jq -r --arg generation "$(uuidgen)" '
          "{\"generation\":\"" + $generation + "\",\n" + (del(.generation)|tojson|ltrimstr("{"))' "$tmp" > "$tmp.publish" || return
        mv -f "$tmp.publish" "$manifest"
        rm -f "$tmp"
    fi
    _build_retire_artifacts
}
