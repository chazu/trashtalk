#!/usr/bin/env bash
# Narrow Jcode Harness API v1 transport. No Trashtalk domain state is mutated
# here. The driver supplies a private config; stdout is incoming NDJSON only.
set -euo pipefail
umask 077
mode=$1 directory=$2
config="$directory/jcode.json"
mapfile -t settings < <(jq -er '.executable,.home,.runtime,.workspace,.model,.ref' "$config")
[[ ${#settings[@]} == 6 ]] || exit 2
executable=${settings[0]} native_ref=${settings[5]}
export JCODE_HOME=${settings[1]} JCODE_RUNTIME_DIR=${settings[2]}
export JCODE_SOCKET="$JCODE_RUNTIME_DIR/jcode.sock" JCODE_API_SOCKET="$JCODE_RUNTIME_DIR/jcode-api.sock"
export JCODE_WAKE_MODE=external
export JCODE_SWARM_ENABLED=0 JCODE_AUTO_POKE=0 JCODE_MEMORY_ENABLED=0 JCODE_MEMORY_SIDECAR_ENABLED=0
export PATH="$JCODE_HOME/bin:$PATH"
unset TRASHTALK_RUN_TOKEN OPENAI_API_KEY CODEX_API_KEY OPENROUTER_API_KEY TRASH_SESSION_ID
cd "${settings[3]}"
coproc BRIDGE { exec "$executable" --no-update --quiet --no-selfdev --provider openai \
    --model "${settings[4]}" --tools bash,read,write,edit,glob,grep,ls,apply_patch \
    api-bridge --stdio; }
bridge_pid=$BRIDGE_PID
exec {input}>&"${BRIDGE[1]}" {output}<&"${BRIDGE[0]}"
cleanup() {
    exec {input}>&- {output}<&-
    kill "$bridge_pid" 2>/dev/null || true
    wait "$bridge_pid" 2>/dev/null || true
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
request_id=0 frame='' event='' reply='' event_session=''
request() {
    request_id=$((request_id + 1))
    jq -cn --argjson id "$request_id" --arg req "$1" --argjson fields "${2:-\{\}}" \
        '$fields + {v:1,id:$id,req:$req}' >&"$input"
}
receive() {
    # Only control exchanges time out. Model/tool execution may take arbitrarily
    # long; the worker can always stop it through a separate API connection.
    if ! IFS= read -r -t "$1" frame <&"$output"; then
        echo 'Jcode API disconnected or control request timed out' >&2
        return 1
    fi
    local metadata
    metadata=$(jq -er 'select(type=="object" and .v==1 and (.ev|type=="string")) |
        [.ev, (.reply_to // "" | tostring), (.session_id // "")] | join("|")' <<<"$frame") || return 1
    printf '%s\n' "$frame"
    IFS='|' read -r event reply event_session <<<"$metadata"
    if [[ "$event" == error && ( "$reply" == "$request_id" || -z "$reply" ) ]]; then
        jq -r '.message // "Jcode API error"' <<<"$frame" >&2
        return 1
    fi
}
expect() {
    local wanted=$1 deadline=$((SECONDS + 40))
    while (( SECONDS < deadline )); do
        receive "$((deadline - SECONDS + 1))" || return 1
        [[ "$reply" != "$request_id" || "$event" != "$wanted" ]] || return 0
    done
    echo "Jcode API did not reply with $wanted" >&2
    return 1
}
attach() {
    request attach_session "$(jq -cn --arg s "$native_ref" '{session_id:$s}')"
    expect attached
    [[ $(jq -r '.session.session_id' <<<"$frame") == "$native_ref" ]]
}
request hello '{"min_version":1,"max_version":1,"client":"trashtalk/1"}'
expect hello_ok
[[ $(jq -r '.version' <<<"$frame") == 1 ]]
if [[ "$mode" == stop ]]; then
    native_ref=$(cat "$directory/conversation")
    [[ -n "$native_ref" ]]
    attach
    request cancel "$(jq -cn --arg s "$native_ref" '{session_id:$s}')"
    expect ok
    # A cancel acknowledgment is insufficient. Ask the daemon for fresh state.
    idle=false
    for attempt in {1..20}; do
        attach
        if [[ $(jq -r '.session.status' <<<"$frame") == idle ]]; then
            idle=true
            break
        fi
        sleep 0.1
    done
    if [[ "$idle" != true ]]; then
        echo 'Jcode cancellation acknowledged but session is not idle' >&2
        exit 1
    fi
    # Bash tool jobs deliberately survive native cancellation/reload. Stop
    # only groups admitted by this private host, then close its daemon.
    real_bash=$(jq -er .bash "$config")
    "$real_bash" "${BASH_SOURCE[0]%/*}/jcode-processes.bash" stop "$JCODE_HOME"
    "$executable" --no-update --quiet --socket "$JCODE_SOCKET" server stop --force >&2
    exit 0
fi
[[ "$mode" == run ]] || exit 2
if [[ -n "$native_ref" ]]; then
    attach
else
    request create_session "$(jq -cn --arg dir "${settings[3]}" '{working_dir:$dir}')"
    expect attached
    native_ref=$(jq -er '.session.session_id | select(length>0)' <<<"$frame")
fi
printf '%s\n' "$native_ref" > "$directory/conversation.tmp"
mv "$directory/conversation.tmp" "$directory/conversation"
if [[ $(jq -r '.session.status' <<<"$frame") != idle ]]; then
    # Even without our own send, a busy attached conversation must be fenced.
    touch "$directory/send-intent"
    echo 'Jcode conversation is already processing; refusing overlapping input' >&2
    exit 1
fi
request set_model "$(jq -cn --arg s "$native_ref" --arg m "${settings[4]}" '{session_id:$s,model:$m}')"
expect ok
request set_reasoning_effort "$(jq -cn --arg s "$native_ref" '{session_id:$s,effort:"medium"}')"
expect ok
touch "$directory/send-intent"
request send_message "$(jq -cn --arg s "$native_ref" --rawfile content "$directory/prompt.txt" '{session_id:$s,content:$content}')"
accepted=false
while receive 86400; do
    [[ "$event_session" == "$native_ref" ]] || continue
    case "$event" in
        message_accepted) accepted=true ;;
        turn_done)
            if [[ "$accepted" == true ]]; then
                printf '%s\n' "$native_ref" > "$directory/completed.tmp"
                mv "$directory/completed.tmp" "$directory/completed"
                exit 0
            fi ;;
    esac
done
exit 1
