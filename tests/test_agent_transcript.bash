#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
passed=0
check() { [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }; printf 'PASS: %s\n' "$1"; passed=$((passed+1)); }
project() {
    jq -cn --arg profile "$1" --arg log "$tmp/output" --arg error "$tmp/error" \
      '{session:{id:"s",title:"Fixture"},has_earlier:0,rows:[{id:"r",seq:1,data:{class:"AgentRun",state:"running",backendProfile:$profile,outputLog:$log,errorLog:$error}}]}' |
      bash "$root/lib/agent-transcript.bash" 400
}
: > "$tmp/error"
cat > "$tmp/output" <<'JSONL'
{"v":1,"ev":"hello_ok","server":"private"}
{"v":1,"ev":"text_delta","text":"Hello "}
{"v":1,"ev":"text_delta","text":"world"}
{"v":1,"ev":"tool_start","call_id":"c","name":"bash"}
{"v":1,"ev":"tool_input_delta","call_id":"c","delta":"{\"command\":"}
{"v":1,"ev":"tool_input_delta","call_id":"c","delta":"\"pwd\"}"}
{"v":1,"ev":"tool_done","call_id":"c","name":"bash","output":"/repo","error":null}
{"v":1,"ev":"text_delta","text":"Done"}
JSONL
# A growing log's partial trailing JSON must not poison the whole projection.
printf '%s' '{"v":1,"ev":"text_delta","text":"unfinished' >> "$tmp/output"
result=$(project jcode)
check 'Jcode deltas assemble readable prose' 'Hello world' "$(jq -r '.entries[]|select(.kind=="assistant_delta")|.text' <<< "$result" | head -1)"
check 'Jcode tool input assembles' '{"command":"pwd"}' "$(jq -r '.entries[]|select(.kind=="tool_input_delta")|.text' <<< "$result")"
check 'Jcode tool result remains inspectable' true "$(jq -r '.entries|any(.text=="/repo")' <<< "$result")"
check 'handshake and incomplete frame are excluded' false "$(jq -r 'tostring|contains("unfinished") or contains("private")' <<< "$result")"
printf '%s\n' '"}' >> "$tmp/output"
result=$(project jcode)
check 'completed trailing frame appears on refresh' true "$(jq -r '.entries|any(.text|contains("unfinished"))' <<< "$result")"
cat > "$tmp/output" <<'JSONL'
{"type":"item.completed","item":{"type":"agent_message","id":"a","text":"Codex answer"}}
{"type":"item.completed","item":{"type":"command_execution","command":"cargo test","aggregated_output":"all passed"}}
{"type":"turn.failed","error":{"message":"provider unavailable"}}
JSONL
result=$(project codex)
check 'Codex text appears' true "$(jq -r '.entries|any(.text=="Codex answer")' <<< "$result")"
check 'Codex command and result appear' true "$(jq -r '.entries|any(.title=="cargo test" and .text=="all passed")' <<< "$result")"
check 'Codex error appears' true "$(jq -r '.entries|any(.kind=="error" and .text=="provider unavailable")' <<< "$result")"
cat > "$tmp/output" <<'JSONL'
{"type":"assistant","message":{"content":[{"type":"text","text":"Maki answer"},{"type":"tool_use","name":"Read","input":{"file_path":"source.trash"}}]}}
{"type":"result","subtype":"success","result":"Finished"}
JSONL
result=$(project maki)
check 'Maki assistant and tool content have distinct IDs' true "$(jq -r '[.entries[].id] as $ids|($ids|length)==($ids|unique|length)' <<< "$result")"
check 'Maki assistant text appears' true "$(jq -r '.entries|any(.text=="Maki answer")' <<< "$result")"
check 'Maki tool input appears' true "$(jq -r '.entries|any(.title=="Read" and (.text|contains("source.trash")))' <<< "$result")"
printf 'native diagnostic\n' > "$tmp/error"
result=$(project maki)
check 'stderr remains a diagnostic entry' true "$(jq -r '.entries|any(.kind=="error" and .text=="native diagnostic")' <<< "$result")"
rm "$tmp/output"
result=$(project maki)
check 'missing retained logs are an explicit gap' true "$(jq -r '.entries|any(.title=="Log unavailable")' <<< "$result")"
printf '%s transcript checks passed\n' "$passed"
