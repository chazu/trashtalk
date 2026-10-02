#!/usr/bin/env bash
# Session-scoped UI bridge. Dispatch is synchronous here, while Innards keeps
# keyboard, paint, cached scroll and pipe I/O independent of handler latency.
set -uo pipefail
base=${TRASHTALK_DIR:-${BASH_SOURCE[0]%/lib/*}}
source "$base/lib/trash.bash" >/dev/null
source "$base/lib/ui-signals.bash"
argv_json=$1 handler=$2 context=$3
export LC_ALL=C
state=$(mktemp -d "${TMPDIR:-/tmp}/trash-ui.XXXXXX") || exit 1
export TRASHTALK_UI_STATE=$state
surface_pid='' input='' output=''
cleanup() {
    if [[ -n $surface_pid ]]; then
        exec {input}>&- {output}<&-
        kill -TERM "$surface_pid" 2>/dev/null || true
        wait "$surface_pid" 2>/dev/null || true
    fi
    if [[ -n ${TRASHTALK_UI_PROFILE:-} ]]; then
        local binding_count=0 binding_total=0 binding_max=0 prepare_count=0 prepare_total=0 prepare_max=0 receipts='[]'
        [[ ! -f $state/profile-binding ]] || read -r binding_count binding_total binding_max < "$state/profile-binding"
        [[ ! -f $state/profile-prepare ]] || read -r prepare_count prepare_total prepare_max < "$state/profile-prepare"
        if ((${#handler_receipts[@]}));then printf -v receipts '%s,' "${handler_receipts[@]}";receipts="[${receipts%,}]";fi
        jq -cn --argjson receipts "$receipts" --arg view "${view:-}" --argjson binding "[$binding_count,$binding_total,$binding_max]" --argjson prepare "[$prepare_count,$prepare_total,$prepare_max]" --argjson handlers "$handlers" --argjson polls "$polls" --argjson bytes_in "$bytes_in" --argjson bytes_out "$bytes_out" \
          --argjson handler_us "$handler_us" --argjson max_us "$max_us" --argjson jq_calls "$jq_calls" --argjson buckets "[$b0,$b1,$b2,$b3]" \
          '{schema_version:1,layer:"trashtalk_bridge",view:$view,receipts:$receipts,binding_timing:{count:$binding[0],total_us:$binding[1],max_us:$binding[2]},bridge_prepare_timing:{count:$prepare[0],total_us:$prepare[1],max_us:$prepare[2]},handlers:$handlers,polls:$polls,bytes_in:$bytes_in,bytes_out:$bytes_out,instrumented_bridge_jq_calls:$jq_calls,handler_timing:{clock:"Bash EPOCHREALTIME (SECONDS fallback); wall clock, clamped",count:$handlers,total_us:$handler_us,max_us:$max_us,buckets_le_us:[1000,16667,250000,null],buckets:$buckets}}' > "$TRASHTALK_UI_PROFILE"
    fi
    rm -rf "$state"
}
declare -a handler_receipts=()
request=0
handlers=0 polls=0 bytes_in=0 bytes_out=0 handler_us=0 max_us=0 jq_calls=0 b0=0 b1=0 b2=0 b3=0
trap cleanup EXIT
trap 'exit 0' INT TERM HUP
trap '' PIPE
# Both argv and application frames stay data. No eval or shell command string.
mapfile -d '' -t argv < <(jq -jer 'if type=="array" and length>0 and all(.[];type=="string" and (contains("\u0000")|not)) then .[]|.,"\u0000" else error("invalid argv") end' <<< "$argv_json")
((${#argv[@]})) || exit 1
jq_calls=$((jq_calls+1))
invoke() {
    local started=0 elapsed=0 now=0
    [[ -z ${TRASHTALK_UI_PROFILE:-} ]] || ui_clock_us started
    @ "$handler" "$@"
    local rc=$?
    if [[ -n ${TRASHTALK_UI_PROFILE:-} ]]; then
        ui_clock_us now; elapsed=$((now-started)); ((elapsed>=0)) || elapsed=0
        handler_receipts+=("{\"request_id\":$request,\"handler_us\":$elapsed}")
        ((${#handler_receipts[@]}<=32)) || handler_receipts=("${handler_receipts[@]:1}")
        handlers=$((handlers+1));handler_us=$((handler_us+elapsed));((elapsed<=max_us)) || max_us=$elapsed
        if ((elapsed<=1000));then b0=$((b0+1));elif ((elapsed<=16667));then b1=$((b1+1));elif ((elapsed<=250000));then b2=$((b2+1));else b3=$((b3+1));fi
    fi
    return "$rc"
}
# File capture preserves handler-local profile counters across command sends.
invoke frameFor: "$context" > "$state/result" || exit 1
jq -ce 'select(.schema_version==1 and .type=="init" and (.view|type=="string"))' "$state/result" > "$state/frames" || exit 1
jq_calls=$((jq_calls+1))
IFS= read -r initial < "$state/frames"
((${#initial}<2097152)) || exit 1
view=$(jq -r '.view' "$state/frames");jq_calls=$((jq_calls+1))
coproc SURFACE { exec "${argv[@]}"; }
surface_pid=$SURFACE_PID
exec {input}>&"${SURFACE[1]}" {output}<&"${SURFACE[0]}"
write_frame() {
    ((${#1}<2097152)) || return 1
    printf '%s\n' "$1" 2>/dev/null >&"$input" || return 1
    [[ -z ${TRASHTALK_UI_PROFILE:-} ]] || bytes_out=$((bytes_out+${#1}+1))
}
write_frame "$initial" || exit 0
# Keep a bounded duplicate-action receipt cache. Older IDs are rejected, never
# re-executed. Applications still own durable/idempotent domain action policy.
declare -A replies=()
declare -a receipt_ids=()
highest=0 partial='' previous_context=$context
send_result() {
    local result_context result_frame count=0 result_blob='' prepare_started=0
    [[ -z ${TRASHTALK_UI_PROFILE:-} ]] || ui_clock_us prepare_started
    IFS= read -r -N 2097153 result_blob < "$state/result" || true
    ((${#result_blob}<=2097152)) || return 1
    # Decode the entire response once; frames remain serialized JSONL. Context
    # must be small state/reference data, not the retained collection history.
    jq -jce --argjson previous "$context" --arg view "$view" '
      if type!="object" or ((.frames // [])|type)!="array" or ((.frames // [])|length)>8 then error("invalid handler response") else
      ((.context // $previous)|tojson),"\u0000",
      ((.frames // [])[] | if .schema_version==1 and .view==$view then tojson,"\u0000" else error("invalid response envelope") end) end' "$state/result" > "$state/decoded" || return 1
    jq_calls=$((jq_calls+1))
    exec {decoded}< "$state/decoded"
    IFS= read -r -d '' result_context <&"$decoded" || { exec {decoded}<&-;return 1; }
    ((${#result_context}<=65536)) || { exec {decoded}<&-;return 1; }
    context=$result_context
    : > "$state/reply"
    while IFS= read -r -d '' result_frame <&"$decoded"; do
        write_frame "$result_frame" || { exec {decoded}<&-;return 1; }
        printf '%s\n' "$result_frame" >> "$state/reply"
        count=$((count+1))
    done
    exec {decoded}<&-
    ui_profile_elapsed prepare "$prepare_started"
}
while kill -0 "$surface_pid" 2>/dev/null; do
    fragment=''
    # -n bounds an unterminated/malicious record, including partial timeout reads.
    if IFS= read -r -n $((2097152-${#partial})) -t 1 fragment <&"$output"; then
        frame=$partial$fragment;partial=''
        ((${#frame}<2097152)) || break
        [[ -n $frame ]] || continue
        [[ -z ${TRASHTALK_UI_PROFILE:-} ]] || bytes_in=$((bytes_in+${#frame}+1))
        # Domain validation is the handler's responsibility; dispatch only the
        # finite presentation protocol, with one bounded request decoder.
        request=$(jq -er --arg view "$view" 'select(.schema_version==1 and .view==$view and (.intent=="action" or .intent=="query" or .intent=="window" or .intent=="resync"))|.request_id|select(type=="number" and floor==. and .>0 and .<=9007199254740991)' <<< "$frame") || break
        jq_calls=$((jq_calls+1))
        if ((request<=highest));then
            if [[ ${replies[$request]+yes} ]];then
                while IFS= read -r reply;do write_frame "$reply" || break 2;done <<< "${replies[$request]}"
            else
                # No implicit replay when an old receipt has been evicted.
                reply=$(jq -cn --arg view "$view" --argjson id "$request" '{schema_version:1,view:$view,type:"ack",request_id:$id,ok:false,message:"Receipt expired; outcome unknown. Action was not replayed."}')
                jq_calls=$((jq_calls+1));write_frame "$reply" || break
            fi
            continue
        fi
        highest=$request
        if ! invoke handleFrame: "$frame" context: "$context" > "$state/result";then
            jq -cn --arg view "$view" --argjson id "$request" '{frames:[{schema_version:1,view:$view,type:"ack",request_id:$id,ok:false,message:"Application rejected request; inspect application diagnostics"}]}' > "$state/result"
            jq_calls=$((jq_calls+1))
        fi
        send_result || break
        replies[$request]=$(<"$state/reply");receipt_ids+=("$request")
        if ((${#receipt_ids[@]}>8));then unset 'replies['"${receipt_ids[0]}"']';receipt_ids=("${receipt_ids[@]:1}");fi
    else
        rc=$?;((rc>128)) || break
        partial+=$fragment;((${#partial}<2097152)) || break
        # Never insert a poll while waiting for the remainder of one request.
        [[ -z $partial ]] || continue
        request=0
        polls=$((polls+1))
        invoke pollFor: "$context" > "$state/result" || break
        send_result || break
    fi
done
