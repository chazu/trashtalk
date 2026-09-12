#!/usr/bin/env bash
# File/serialization boundary only. No worker calls, receipts, or UI authority.
set -euo pipefail
limit=$1
base=${BASH_SOURCE[0]%/*}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-transcript.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
cat > "$scratch/records.json"
jq -c --arg mode rows --arg run '' --argjson seq 0 --arg profile '' --arg stream '' -f "$base/agent-transcript.jq" "$scratch/records.json" > "$scratch/entries.jsonl"
earlier=$(jq -r '.has_earlier == 1' "$scratch/records.json")
# The session adapter records direct user input and visible assistant text.
# Operational stdout/stderr and tool internals remain in the run inspector.
if [[ -n ${2:-} ]]; then
    while IFS= read -r run; do
        [[ "$run" =~ ^[A-Za-z0-9_-]+$ ]] || continue
        path="$2/$run/conversation.jsonl"
        [[ -f "$path" ]] || continue
        jq -Rc --arg run "$run" 'fromjson? | select(.kind=="user" or .kind=="assistant_delta") |
            {id:($run+"/chat/"+(input_line_number|tostring)),kind:.kind,
             title:(if .kind=="user" then "You" else "Assistant" end),text:.text,
             run:$run,order:[.time,input_line_number]}' "$path" >> "$scratch/entries.jsonl"
    done < <(jq -r '.conversation_runs // [] | .[]' "$scratch/records.json")
fi
jq -sc --slurpfile records "$scratch/records.json" --argjson limit "$limit" --argjson earlier "$earlier" '
  sort_by(.order, .id) |
  reduce .[] as $entry ([];
    if ($entry.kind|endswith("_delta")) and length>0 and .[-1].kind == $entry.kind and .[-1].run == $entry.run
    then .[-1].text += $entry.text else . + [$entry] end) |
  {schema_version:1,type:"snapshot",session:$records[0].session,
   has_earlier:($earlier or length>$limit),window:$limit,entries:.[-$limit:]}' "$scratch/entries.jsonl"
