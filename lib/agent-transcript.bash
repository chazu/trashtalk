#!/usr/bin/env bash
# File/serialization boundary only. Cache contents never grant UI authority.
set -euo pipefail
limit=$1 root=${2:-} cache=${3:-}
base=${BASH_SOURCE[0]%/*}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-transcript.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
cat > "$scratch/records.json"
# Uncached callers use exactly the same projection with disposable state.
[[ -n "$cache" ]] || cache="$scratch/cache"
mkdir -p "$cache"
key=$(jq -c --arg root "$root" --argjson limit "$limit" '{session:.session.id,root:$root,limit:$limit}' "$scratch/records.json")
printf '{}\n' > "$scratch/previous.json"
if [[ -f "$cache/state.json" ]] && jq -e --argjson key "$key" '
    .schema_version==1 and .key==$key and (.files|type)=="object"
    and (.entries|type)=="array" and (.rows|type)=="array"' "$cache/state.json" >/dev/null 2>&1; then
    cp "$cache/state.json" "$scratch/previous.json"
fi
jq -c --arg mode rows --arg run '' --argjson seq 0 --arg profile '' --arg stream '' \
    -f "$base/agent-transcript.jq" "$scratch/records.json" > "$scratch/rows.jsonl"
force=$(jq -ns --slurpfile previous "$scratch/previous.json" --slurpfile rows "$scratch/rows.jsonl" \
    'if $previous[0].rows==$rows then 0 else 1 end')
for attempt in 1 2; do
    perl "$base/transcript-files.pl" "$scratch/records.json" "$scratch/previous.json" "$root" "$force" \
        "$scratch/lines" "$scratch/manifest.json"
    jq -Rc 'split("\t") | .[0] as $run | (.[1]|tonumber) as $line | (.[2:]|join("\t")|fromjson?)
      | select(.kind=="user" or .kind=="assistant_delta")
      | {id:($run+"/chat/"+($line|tostring)),kind:.kind,title:(if .kind=="user" then "You" else "Assistant" end),
         text:.text,run:$run,order:[.time,$line]}' "$scratch/lines" > "$scratch/additions.jsonl"
    jq -nc --argjson cache_key "$key" --argjson limit "$limit" \
        --slurpfile previous "$scratch/previous.json" --slurpfile manifest "$scratch/manifest.json" \
        --slurpfile records "$scratch/records.json" --slurpfile rows "$scratch/rows.jsonl" \
        --slurpfile additions "$scratch/additions.jsonl" -f "$base/json/transcript-cache.jq" > "$scratch/state.json"
    if [[ $(<"$scratch/state.json") == '{"retry":true}' ]]; then force=1; else break; fi
done
# One atomic state file couples consumed offsets with the projected entries.
# Keep publication on the cache filesystem even when TMPDIR differs.
staged=$(mktemp "$cache/state.XXXXXX")
if ! cp "$scratch/state.json" "$staged" || ! mv "$staged" "$cache/state.json"; then
    rm -f "$staged"; exit 1
fi
jq -c .snapshot "$cache/state.json"
