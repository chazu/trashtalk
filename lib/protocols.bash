# Opt-in boundaries consume the compiler's shared surface resolver as data.
# Nothing here runs on ordinary sends or sources an artifact.
declare -gA _PROTOCOL_KIND=() _PROTOCOL_API=() _PROTOCOL_SURFACE=() _PROTOCOL_REQUIRES=()
declare -gA _PROTOCOL_VERIFIED=() _PROTOCOL_CACHE=()
_PROTOCOL_MANIFEST_CONTENT=''
_PROTOCOL_GENERATION=''
_protocol_invalidate() {
    _PROTOCOL_MANIFEST_CONTENT=''
    _PROTOCOL_GENERATION=''
    _PROTOCOL_VERIFIED=() _PROTOCOL_CACHE=()
}
_protocol_refresh() {
    local manifest="${TRASHTALK_COMPILED_DIR:-$TRASHDIR/.compiled}/.protocol-manifest.json" content rows identity kind api surface required generation
    [[ -f "$manifest" ]] || { echo 'ProtocolError: no registered build manifest; run make' >&2; return 1; }
    IFS= read -r generation < "$manifest" || return 1
    [[ "$manifest:$generation" != "$_PROTOCOL_GENERATION" ]] || return 0
    content=$(<"$manifest")
    [[ -n "$content" ]] || return 1
    [[ "$content" != "$_PROTOCOL_MANIFEST_CONTENT" ]] || return 0
    rows=$(jq -ce 'select(.schema==1 and (.entries|type)=="object") | .entries' <<<"$content") || return 1
    _PROTOCOL_KIND=() _PROTOCOL_API=() _PROTOCOL_SURFACE=() _PROTOCOL_REQUIRES=()
    _PROTOCOL_VERIFIED=() _PROTOCOL_CACHE=()
    while IFS= read -r -d '' identity && IFS= read -r -d '' kind && IFS= read -r -d '' api &&
          IFS= read -r -d '' surface && IFS= read -r -d '' required; do
        _PROTOCOL_KIND[$identity]=$kind _PROTOCOL_API[$identity]=$api
        _PROTOCOL_SURFACE[$identity]=" $surface " _PROTOCOL_REQUIRES[$identity]=$required
    done < <(jq -rj '.[] | .identity,"\u0000",.kind,"\u0000",.api_hash,"\u0000",
        ((.surface.selectors-.surface.conflicts)|join(" ")),"\u0000",(.requirements|join(" ")),"\u0000"' <<<"$rows")
    _PROTOCOL_MANIFEST_CONTENT=$content
    _PROTOCOL_GENERATION="$manifest:$generation"
}
# Verify ordinary receipts and content hashes before admitting an identity.
# Rebuild publication or explicit reload invalidates admitted generations.
_protocol_verify() {
    local identity="$1" data entry receipt artifact source actual dependency deps
    [[ -n "$identity" && -n "${_PROTOCOL_KIND[$identity]:-}" ]] || {
        printf 'ProtocolError: unregistered identity %s\n' "$identity" >&2; return 1;
    }
    [[ -z "${_PROTOCOL_VERIFIED[$identity]:-}" ]] || return 0
    entry=$(jq -ce --arg id "$identity" '.entries[$id]' <<<"$_PROTOCOL_MANIFEST_CONTENT") || return 1
    local -a fields=() hashes=()
    mapfile -d '' -t fields < <(jq -rj '.receipt,"\u0000",.artifact,"\u0000",.source,"\u0000",.receipt_data.output_hash,"\u0000",.receipt_data.source_hash,"\u0000"' <<<"$entry")
    receipt=${fields[0]} artifact=${fields[1]} source=${fields[2]}
    [[ -f "$receipt" && -f "$artifact" && -f "$source" ]] || return 1
    data=$(<"$receipt")
    jq -e --argjson receipt "$data" '.receipt_data==$receipt and .api_hash==.receipt_data.validation.implementing_surface_hash' <<<"$entry" >/dev/null || {
        printf 'ProtocolError: stale receipt for %s\n' "$identity" >&2; return 1;
    }
    actual=$(shasum -a 256 "$artifact" "$source") || return 1
    mapfile -t hashes <<<"$actual"
    [[ "${hashes[0]%% *}" == "${fields[3]}" && "${hashes[1]%% *}" == "${fields[4]}" ]] || {
        printf 'ProtocolError: stale artifact or source for %s\n' "$identity" >&2; return 1;
    }
    deps=$(jq -ce --argjson entry "$entry" '
      .entries as $all | [$entry.receipt_data.dependencies|to_entries[]|
        . as $dep | [$all[]|select(.source==$dep.key and .artifact==$dep.value.output and
          .receipt_data.output_hash==$dep.value.output_hash and .receipt_data.source_hash==$dep.value.source_hash)] |
        if length==1 then .[0].identity else error("stale dependency receipt") end]' <<<"$_PROTOCOL_MANIFEST_CONTENT") || return 1
    while IFS= read -r dependency; do
        _protocol_verify "$dependency" || return
    done < <(jq -r '.[]' <<<"$deps")
    _PROTOCOL_VERIFIED[$identity]=1
}
_class_has_method() {
    local class_name="$1" selector="$2"
    _protocol_refresh && _protocol_verify "$class_name" || return 1
    [[ "$selector" != _* && "${_PROTOCOL_SURFACE[$class_name]}" == *" $selector "* ]]
}
# Direct use retains positive and negative cache results. Command substitutions
# cannot retain newly populated shell variables in the parent process.
_conforms_to() {
    local class_name="$1" protocol_name="$2" selector key result=true
    if ! _protocol_refresh || ! _protocol_verify "$class_name" || ! _protocol_verify "$protocol_name"; then
        echo false; return 1
    fi
    if [[ "${_PROTOCOL_KIND[$class_name]}" != class || "${_PROTOCOL_KIND[$protocol_name]}" != protocol ]]; then
        echo false; return 1
    fi
    key="1:${_PROTOCOL_API[$class_name]}:${_PROTOCOL_API[$protocol_name]}"
    if [[ -n "${_PROTOCOL_CACHE[$key]:-}" ]]; then printf '%s\n' "${_PROTOCOL_CACHE[$key]}"; return 0; fi
    for selector in ${_PROTOCOL_REQUIRES[$protocol_name]}; do
        if [[ "${_PROTOCOL_SURFACE[$class_name]}" != *" $selector "* ]]; then result=false; break; fi
    done
    _PROTOCOL_CACHE[$key]=$result
    printf '%s\n' "$result"
}
