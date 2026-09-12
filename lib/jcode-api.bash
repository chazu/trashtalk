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
request_id=0 frame='' event='' reply='' event_session='' partial=''
source "${BASH_SOURCE[0]%/*}/jcode-input.bash"
chat=false
[[ ! -s "$directory/system-reminder.txt" ]] || chat=true
chat_entry() {
    local kind=$1 field=$2 source=$3
    if [[ "$field" == raw ]]; then
        jq -Rsc --arg kind "$kind" --argjson time "${EPOCHREALTIME:-$SECONDS}" \
            '{kind:$kind,text:.,time:($time*1000|floor)}' "$source"
    else
        jq -c --arg kind "$kind" --arg field "$field" --argjson time "${EPOCHREALTIME:-$SECONDS}" \
            '{kind:$kind,text:.[$field],time:($time*1000|floor)}' "$source"
    fi >> "$directory/conversation.jsonl"
}

request() {
    local fields="${2:-}"
    [[ -n "$fields" ]] || fields='{}'
    request_id=$((request_id + 1))
    jq -cn --argjson id "$request_id" --arg req "$1" --argjson fields "$fields" \
        '$fields + {v:1,id:$id,req:$req}' >&"$input"
}
receive() {
    # Only control exchanges time out. Model/tool execution may take arbitrarily
    # long; the worker can always stop it through a separate API connection.
    local fragment='' rc=0
    IFS= read -r -t "$1" fragment <&"$output" || rc=$?
    if (( rc != 0 )); then
        partial+="$fragment"
        (( rc <= 128 )) || return 124
        echo 'Jcode API disconnected' >&2
        return 1
    fi
    frame="$partial$fragment"
    partial=''
    local metadata
    metadata=$(jq -er 'select(type=="object" and .v==1 and (.ev|type=="string")) |
        [.ev, (.reply_to // "" | tostring), (.session_id // "")] | join("|")' <<<"$frame") || return 1
    printf '%s\n' "$frame"
    IFS='|' read -r event reply event_session <<<"$metadata"
    if [[ "$chat" == true && "$event_session" == "$native_ref" && "$event" == text_delta ]]; then
        chat_entry assistant_delta text /dev/stdin <<< "$frame"
    fi
    if [[ "$event" == error ]]; then
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
if [[ "$mode" == compact ]]; then
    source "${BASH_SOURCE[0]%/*}/jcode-context.bash"
    [[ -n "$native_ref" ]] || { echo 'No native conversation to compact' >&2; exit 1; }
    attach
    [[ $(jq -r '.session.status' <<<"$frame") == idle ]] || { echo 'Native conversation is busy; try compaction when idle' >&2; exit 1; }
    printf '%s\n' "$native_ref" > "$directory/conversation"
    before=$(_jcode_compaction_state "$directory/context-before.json")
    after=$(_jcode_compaction_state "$JCODE_HOME/sessions/$native_ref.json")
    if [[ "$after" != null && "$after" != "$before" ]]; then
        printf '%s\n' "$native_ref" > "$directory/completed"
        exit 0
    fi
    touch "$directory/send-intent"
    request compact "$(jq -cn --arg s "$native_ref" '{session_id:$s}')"
    if ! expect compacted; then
        if [[ "$event" == error && "$reply" == "$request_id" ]]; then
            touch "$directory/request-rejected"
        fi
        exit 1
    fi
    # Native compaction is asynchronous. Keep this attachment alive and check
    # durable state; an acknowledgement or unchanged history cannot prove success.
    compact_limit=${TRASHTALK_JCODE_COMPACT_TIMEOUT:-300}
    [[ "$compact_limit" =~ ^[0-9]+$ && "$compact_limit" -gt 0 ]] || compact_limit=300
    compact_deadline=$((SECONDS + compact_limit))
    while :; do
        after=$(_jcode_compaction_state "$JCODE_HOME/sessions/$native_ref.json")
        if [[ "$after" != null && "$after" != "$before" ]]; then
            printf '%s\n' "$native_ref" > "$directory/completed.tmp"
            mv "$directory/completed.tmp" "$directory/completed"
            exit 0
        fi
        if (( SECONDS >= compact_deadline )); then
            echo 'Context compaction did not become durable before the timeout; inspect native logs before retrying' >&2
            exit 1
        fi
        sleep 2
        request ping
        expect pong
    done
fi
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
    source "${BASH_SOURCE[0]%/*}/jcode-context.bash"
    recorded_workspace=$(_jcode_context_value "$JCODE_HOME/sessions/$native_ref.json" "" working_dir | jq -r '. // ""')
    if [[ "$recorded_workspace" != "${settings[3]}" ]]; then
        real_bash=$(jq -er .bash "$config")
        "$real_bash" "${BASH_SOURCE[0]%/*}/jcode-workspace.bash" "$JCODE_SOCKET" "$native_ref" "${settings[3]}" > "$directory/workspace-control.json"
    fi
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
attached_workspace=$(jq -r '.session.working_dir // ""' <<<"$frame")
[[ "$attached_workspace" == "${settings[3]}" ]] || { echo 'Jcode attachment has the wrong execution directory; refusing model input' >&2; exit 1; }
request set_model "$(jq -cn --arg s "$native_ref" --arg m "${settings[4]}" '{session_id:$s,model:$m}')"
expect ok
request set_reasoning_effort "$(jq -cn --arg s "$native_ref" '{session_id:$s,effort:"medium"}')"
expect ok
mkdir -p "$directory/inputs"
touch "$directory/send-intent"
fields=$(jq -cn --arg s "$native_ref" --rawfile content "$directory/prompt.txt" '{session_id:$s,content:$content}')
if [[ "$chat" == true ]]; then
    fields=$(jq -c --rawfile context "$directory/system-reminder.txt" '.system_reminder=$context' <<< "$fields")
fi
request send_message "$fields"
accepted=false pending_input='' turn_finished=false native_idle=false

# Claim requests and close admission under the same lock. This prevents a
# finishing turn from acknowledging input that its adapter will never send.
pump_input() {
    [[ -z "$pending_input" ]] || return 0
    _jcode_input_lock
    local candidate
    for candidate in "$directory"/inputs/*.request; do
        [[ -f "$candidate" ]] || continue
        pending_input=${candidate%.request}
        mv "$candidate" "$pending_input.sent"
        break
    done
    if [[ -z "$pending_input" && "$turn_finished" == true && "$native_idle" == true ]]; then
        rm -f "$directory/input-open"
        _jcode_input_unlock
        return 2
    fi
    _jcode_input_unlock
    [[ -n "$pending_input" ]] || return 0
    chat=true
    turn_finished=false
    native_idle=false
    request soft_interrupt "$(jq -c --arg s "$native_ref" '. + {session_id:$s,urgent:false}' "$pending_input.sent")"
}

while :; do
    rc=0
    receive .1 || rc=$?
    if (( rc == 1 )); then
        if [[ -n "$pending_input" && "$event" == error && "$reply" == "$request_id" ]]; then
            _jcode_input_ack "$pending_input.ack" false "$(jq -r '.message' <<< "$frame")"
            pending_input=''
            continue
        fi
        [[ "$accepted" == true ]] || _jcode_input_ack "$directory/first-input.ack" false 'Session rejected the input; inspect its diagnostic'
        exit 1
    fi
    if (( rc == 0 )); then
        if [[ -n "$pending_input" && "$event" == ok && "$reply" == "$request_id" ]]; then
            chat_entry user content "$pending_input.sent"
            _jcode_input_ack "$pending_input.ack" true 'Input sent directly to the session at its next safe point'
            pending_input=''
        fi
        if [[ "$event_session" == "$native_ref" ]]; then
            case "$event" in
                message_accepted)
                    if [[ "$accepted" != true ]]; then
                        accepted=true
                        [[ "$chat" != true ]] || chat_entry user raw "$directory/prompt.txt"
                        touch "$directory/input-open"
                        _jcode_input_ack "$directory/first-input.ack" true 'Input sent directly to the session'
                    fi ;;
                turn_done) [[ "$accepted" != true ]] || turn_finished=true ;;
            esac
        fi
    fi
    if [[ "$accepted" == true ]]; then
        if [[ "$turn_finished" == true && -z "$pending_input" ]]; then
            # A soft interrupt can land as the preceding turn finishes. Its
            # acknowledgement is not completion of the newly started turn.
            # Confirm fresh native idle before closing this run's authority.
            attach
            native_idle=false
            [[ $(jq -r '.session.status' <<< "$frame") != idle ]] || native_idle=true
        fi
        rc=0
        pump_input || rc=$?
        if (( rc == 2 )); then
            printf '%s\n' "$native_ref" > "$directory/completed.tmp"
            mv "$directory/completed.tmp" "$directory/completed"
            exit 0
        fi
        (( rc == 0 )) || exit "$rc"
    fi
done
