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
while IFS= read -r row; do
    run=$(jq -r .id <<< "$row")
    seq=$(jq -r .seq <<< "$row")
    profile=$(jq -r '.data.backendProfile' <<< "$row")
    for stream in stdout stderr; do
        field=outputLog
        [[ "$stream" != stderr ]] || field=errorLog
        path=$(jq -r --arg field "$field" '.data[$field] // ""' <<< "$row")
        [[ -n "$path" ]] || continue
        if [[ ! -r "$path" || ! -f "$path" ]]; then
            jq -cn --arg id "$run/$stream/missing" --arg path "$path" --argjson seq "$seq" \
                '{id:$id,kind:"error",title:"Log unavailable",text:$path,order:[$seq,1]}' >> "$scratch/entries.jsonl"
            continue
        fi
        # Keep physical line IDs even while an append-only file grows. A partial
        # JSON frame is ignored until its writer completes it on the next poll.
        awk -v n="$limit" -v count="$scratch/count" '{lines[NR%n]=$0} END {print NR > count; first=NR-n+1; if(first<1)first=1; for(i=first;i<=NR;i++)printf "%d\t%s\n",i,lines[i%n]}' "$path" > "$scratch/log"
        [[ $(cat "$scratch/count") -le "$limit" ]] || earlier=true
        jq -Rc --arg mode native --arg run "$run" --argjson seq "$seq" --arg profile "$profile" --arg stream "$stream" \
            -f "$base/agent-transcript.jq" "$scratch/log" >> "$scratch/entries.jsonl"
    done
done < <(jq -c '.rows[] | select(.data.class == "AgentRun")' "$scratch/records.json")
jq -sc --slurpfile records "$scratch/records.json" --argjson limit "$limit" --argjson earlier "$earlier" '
  sort_by(.order, .id) |
  reduce .[] as $entry ([];
    if ($entry.kind|endswith("_delta")) and length>0 and .[-1].kind == $entry.kind and .[-1].order[0] == $entry.order[0]
    then .[-1].text += $entry.text else . + [$entry] end) |
  {schema_version:1,type:"snapshot",session:$records[0].session,
   has_earlier:($earlier or length>$limit),window:$limit,entries:.[-$limit:]}' "$scratch/entries.jsonl"
