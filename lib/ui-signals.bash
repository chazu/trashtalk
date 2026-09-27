# Reactive storage belongs to one surface, not the persistent object database.
# Files bridge the runtime's command substitutions; reads/writes are Bash
# builtins. Only binding registration/capture uses directories/temp paths.
ui_signal_path() {
    [[ -d ${TRASHTALK_UI_STATE:-} && $1 =~ ^[a-zA-Z][a-zA-Z0-9_-]{0,63}$ ]] || {
        printf '%s\n' 'UI signal needs a surface context and an identifier' >&2;return 1;
    }
}
ui_signal_set() {
    ui_signal_path "$1" || return
    [[ -z ${TRASHTALK_UI_READONLY:-} ]] || { printf '%s\n' 'Bindings are read-only' >&2;return 1; }
    local old=''
    if [[ -f $TRASHTALK_UI_STATE/signal-$1 ]];then
        IFS= read -r -d '' old < "$TRASHTALK_UI_STATE/signal-$1" || true
        [[ $old != "$2" ]] || return 0
    fi
    printf '%s' "$2" > "$TRASHTALK_UI_STATE/signal-$1"
    printf '%s\n' "$1" >> "$TRASHTALK_UI_STATE/dirty"
}
ui_signal_get() {
    ui_signal_path "$1" || return
    [[ -f $TRASHTALK_UI_STATE/signal-$1 ]] || { printf 'Undefined UI signal: %s\n' "$1" >&2;return 1; }
    [[ -z ${TRASHTALK_UI_CAPTURE:-} ]] || printf '%s\n' "$1" >> "$TRASHTALK_UI_CAPTURE"
    local value='';IFS= read -r -d '' value < "$TRASHTALK_UI_STATE/signal-$1" || true
    printf '%s' "$value"
}
ui_binding_register() {
    ui_signal_path "$1" || return
    [[ -z ${TRASHTALK_UI_READONLY:-} ]] || return 1
    printf '%s' "$2" > "$TRASHTALK_UI_STATE/block-$1"
    ui_binding_evaluate "$1"
}
ui_binding_evaluate() {
    ui_signal_path "$1" || return
    local block='' result='' name=$1 started=0
    if [[ -n ${TRASHTALK_UI_PROFILE:-} ]];then started=${EPOCHREALTIME:-$((SECONDS*1000000))};started=${started/./};fi
    [[ -f $TRASHTALK_UI_STATE/block-$name ]] || return 1
    IFS= read -r -d '' block < "$TRASHTALK_UI_STATE/block-$name" || true
    local TRASHTALK_UI_CAPTURE="$TRASHTALK_UI_STATE/capture-$name" TRASHTALK_UI_READONLY=1
    : > "$TRASHTALK_UI_CAPTURE"
    if ! @ "$block" value > "$TRASHTALK_UI_STATE/evaluated-$name";then return 1;fi
    IFS= read -r -d '' result < "$TRASHTALK_UI_STATE/evaluated-$name" || true
    # Commit dependencies and value only after successful read-only evaluation.
    local deps='';IFS= read -r -d '' deps < "$TRASHTALK_UI_CAPTURE" || true
    printf '%s' "$deps" > "$TRASHTALK_UI_STATE/deps-$name"
    printf '%s' "$result" > "$TRASHTALK_UI_STATE/value-$name"
    ui_profile_elapsed binding "$started"
    printf '%s' "$result"
}
ui_binding_invalidated() {
    [[ -d ${TRASHTALK_UI_STATE:-} ]] || return 1
    local changed='' file dep name match
    [[ -f $TRASHTALK_UI_STATE/dirty ]] || return 0
    IFS= read -r -d '' changed < "$TRASHTALK_UI_STATE/dirty" || true
    for file in "$TRASHTALK_UI_STATE"/deps-*;do
        [[ -f $file ]] || continue
        match=false
        while IFS= read -r dep || [[ -n $dep ]];do
            case $'\n'"$changed"$'\n' in *$'\n'"$dep"$'\n'*) match=true;break;;esac
        done < "$file"
        if [[ $match == true ]];then name=${file##*/deps-};printf '%s\n' "$name";fi
    done
}
ui_binding_clear() {
    [[ -d ${TRASHTALK_UI_STATE:-} && -z ${TRASHTALK_UI_READONLY:-} ]] || return 1
    : > "$TRASHTALK_UI_STATE/dirty"
}
ui_signal_invalidate() {
    ui_signal_path "$1" || return
    [[ -z ${TRASHTALK_UI_READONLY:-} ]] || return 1
    printf '%s\n' "$1" >> "$TRASHTALK_UI_STATE/dirty"
}
# Bounded phase summary shared by command substitutions. No clock reads or
# writes when profiling is disabled; one fixed-size record per enabled phase.
ui_profile_elapsed() {
    [[ -n ${TRASHTALK_UI_PROFILE:-} ]] || return 0
    local phase=$1 start=$2 now elapsed count=0 total=0 maximum=0
    now=${EPOCHREALTIME:-$((SECONDS*1000000))};now=${now/./};elapsed=$((now-start));((elapsed>=0)) || elapsed=0
    [[ ! -f $TRASHTALK_UI_STATE/profile-$phase ]] || read -r count total maximum < "$TRASHTALK_UI_STATE/profile-$phase"
    ((elapsed<=maximum)) || maximum=$elapsed
    printf '%s %s %s\n' "$((count+1))" "$((total+elapsed))" "$maximum" > "$TRASHTALK_UI_STATE/profile-$phase"
}
