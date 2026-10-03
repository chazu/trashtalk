#!/usr/bin/env bash
# Read-only, bounded review sample. One decision request per unique message,
# using the selected target (TRASHTALK_DECISION_TARGET=jev or decider).
set -euo pipefail
if [[ $# -lt 1 || $# -gt 2 ]]; then
    printf 'Usage: bash examples/gmail-junk.bash EXPECTED_EMAIL [PREFERENCE_EXAMPLES_JSON]\n' >&2
    exit 2
fi
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
examples='[]'
if [[ $# == 2 ]]; then
    examples=$(jq -ce 'if type == "array" and all(.[]; (.sender|type)=="string" and (.subject|type)=="string" and (.verdict=="keep" or .verdict=="junk")) then . else error("Expected sender/subject/verdict examples") end' "$2")
fi
@ Gmail::Client requireAccount: "$1" >/dev/null
target=$(@ Decision::Target selected)
@ Decision::Target requireReady: "$target" >/dev/null
declare -A seen=() conversations=() threads=()
count=0
# Gmail categories are sampling strata only; never passed to the model.
# Social/all-inbox are fallback strata when the first four yield fewer than 20.
for group in primary promotions updates forums social all; do
    query='in:inbox'
    [[ $group == all ]] || query+=" category:$group"
    listing=$(@ Gmail::Client search: "$query" limit: 10)
    mapfile -t ids < <(jq -r '.messages[]? | [.id, .threadId] | @tsv' <<< "$listing")
    selected=0
    for row in "${ids[@]}"; do
        IFS=$'\t' read -r id thread <<< "$row"
        [[ -z ${seen[$id]:-} ]] || continue
        seen[$id]=1
        [[ -z $thread || -z ${threads[$thread]:-} ]] || continue
        message=$(@ Gmail::Client message: "$id")
        conversation=$(jq -c '[.from,.subject]' <<< "$message")
        [[ -z ${conversations[$conversation]:-} ]] || continue
        conversations[$conversation]=1
        [[ -z $thread ]] || threads[$thread]=1
        proposal=$(@ Gmail::Junk assess: "$message" examples: "$examples" using: "$target")
        count=$((count+1))
        selected=$((selected+1))
        jq -c --arg group "$group" --argjson n "$count" '. + {sampleGroup:$group, number:$n}' <<< "$proposal"
        [[ $count -lt 20 ]] || break 2
        [[ $selected -lt 5 ]] || break
    done
done
printf 'Assessed %s unique inbox emails; suggestions only.\n' "$count" >&2
