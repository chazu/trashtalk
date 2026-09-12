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
project() { bash "$root/lib/agent-transcript.bash" 400; }

# A run log contains useful operational evidence, but it is not chat. The
# conversation projection must not expose it in the human/agent transcript.
result=$(jq -cn '{session:{id:"s",title:"Gusgus"},has_earlier:0,rows:[{id:"r",seq:1,data:{class:"AgentRun",state:"running",backendProfile:"jcode",outputLog:"/not/read"}}]}' | project)
check 'run logs are absent from the conversation projection' 0 "$(jq '.entries|length' <<< "$result")"

result=$(jq -cn '{session:{id:"s",title:"Gusgus"},has_earlier:0,rows:[
  {id:"from-human",seq:1,data:{class:"Message",from:"chazu",to:"session:s",created:"now",subject:"",body:"Hello Gusgus"}},
  {id:"from-agent",seq:2,data:{class:"Message",from:"session:s",to:"chazu",created:"now",subject:"",body:"Hello Chazu"}},
  {id:"run",seq:3,data:{class:"AgentRun",state:"succeeded",error:"not chat"}}
]}' | project)
check 'human message remains in conversation' true "$(jq -r '.entries|any(.text|contains("Hello Gusgus"))' <<< "$result")"
check 'agent message remains in conversation' true "$(jq -r '.entries|any(.text|contains("Hello Chazu"))' <<< "$result")"
check 'run metadata is absent from conversation' false "$(jq -r 'tostring|contains("not chat")' <<< "$result")"
check 'only chat messages are projected' 2 "$(jq '.entries|length' <<< "$result")"
printf '%s transcript checks passed\n' "$passed"
