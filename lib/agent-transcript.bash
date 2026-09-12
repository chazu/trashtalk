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
# This is a conversation surface, not a run-log viewer. AgentRun output may
# contain tool calls, reasoning, and implementation noise. Durable Messages
# are the shared chat record between the human and the agent; run details stay
# available through the session browser and run logs.
jq -sc --slurpfile records "$scratch/records.json" --argjson limit "$limit" --argjson earlier "$earlier" '
  sort_by(.order, .id) |
  reduce .[] as $entry ([];
    if ($entry.kind|endswith("_delta")) and length>0 and .[-1].kind == $entry.kind and .[-1].order[0] == $entry.order[0]
    then .[-1].text += $entry.text else . + [$entry] end) |
  {schema_version:1,type:"snapshot",session:$records[0].session,
   has_earlier:($earlier or length>$limit),window:$limit,entries:.[-$limit:]}' "$scratch/entries.jsonl"
