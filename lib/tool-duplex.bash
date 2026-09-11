#!/usr/bin/env bash
# A temporary UI child, not a harness host. No signals are forwarded to agents.
set -uo pipefail
argv_json=$1 handler=$2 context=$3
base=${TRASHTALK_DIR:-${BASH_SOURCE[0]%/lib/*}}
source "$base/lib/trash.bash" >/dev/null
jq -e 'type=="array" and length>0 and all(.[]; type=="string" and (contains("\u0000")|not))' <<< "$argv_json" >/dev/null || exit 1
mapfile -d '' -t argv < <(jq -jr '.[] | ., "\u0000"' <<< "$argv_json")
# Validate/read before the UI owns the terminal. No worker is started on attach.
snapshot=$(@ "$handler" frameFor: "$context") || exit 1
[[ -n "$snapshot" ]] || exit 1
coproc SURFACE { exec "${argv[@]}"; }
surface_pid=$SURFACE_PID
exec {input}>&"${SURFACE[1]}" {output}<&"${SURFACE[0]}"
cleanup() {
    exec {input}>&- {output}<&-
    kill -TERM "$surface_pid" 2>/dev/null || true
    wait "$surface_pid" 2>/dev/null || true
}
trap cleanup EXIT
trap 'exit 0' INT TERM HUP
# The surface may close while an acknowledgement is in flight. Treat EPIPE as
# detach and retain the normal outcome; never forward it to the agent process.
trap '' PIPE
write_frame() { printf '%s\n' "$1" 2>/dev/null >&"$input"; }
declare -A replies=()
write_frame "$snapshot" || { printf '%s\n' dismissed; exit 0; }
previous=$snapshot
partial=''
while kill -0 "$surface_pid" 2>/dev/null; do
    fragment=''
    if IFS= read -r -t 1 fragment <&"$output"; then
        frame="$partial$fragment"
        partial=''
        request=$(jq -er '.request_id | select(type=="number" and floor==. and .>0)' <<< "$frame" 2>/dev/null) || break
        if [[ ${replies[$request]+present} ]]; then
            write_frame "${replies[$request]}" || break
            continue
        fi
        result=$(@ "$handler" handleFrame: "$frame" context: "$context") || {
            reply=$(jq -cn --argjson request "$request" '{schema_version:1,type:"ack",request_id:$request,ok:false,message:"Invalid or rejected session action"}')
            write_frame "$reply" || break
            continue
        }
        [[ $(jq -r '.done // false' <<< "$result") != true ]] || break
        context=$(jq -c .context <<< "$result")
        reply=$(jq -c .frame <<< "$result")
        replies[$request]=$reply
        write_frame "$reply" || break
    else
        rc=$?
        (( rc > 128 )) || break
        partial+="$fragment"
    fi
    snapshot=$(@ "$handler" frameFor: "$context") || break
    if [[ "$snapshot" != "$previous" ]]; then
        write_frame "$snapshot" || break
        previous=$snapshot
    fi
done
printf '%s\n' dismissed
