#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin"
export DECISION_LOG="$scratch/requests.jsonl"
cat > "$scratch/bin/curl" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
config='' body='' output=''
while (($#)); do
    case "$1" in
        --config) config=$2; shift 2;;
        --data-binary) body=${2#@}; shift 2;;
        --output) output=$2; shift 2;;
        --write-out|--connect-timeout|--max-time) shift 2;;
        *) shift;;
    esac
done
url=$(sed -n 's/^url = "\(.*\)"$/\1/p' "$config")
method=$(sed -n 's/^request = "\(.*\)"$/\1/p' "$config")
jq -cn --arg url "$url" --arg method "$method" '{url:$url,method:$method}' >> "$DECISION_LOG"
if [[ $method == GET ]]; then
    if [[ $url == http://bc250:8700/* ]]; then
        printf '%s' "${DECISION_REMOTE_HEALTH:-}" > "$output"
    else
        printf '%s' "${DECISION_LOCAL_HEALTH:-}" > "$output"
    fi
else
    if [[ ${DECISION_POST_FAIL:-0} == 1 ]]; then echo '{"error":"encoder failed"}' > "$output"; printf 502; exit 0; fi
    if [[ -n ${DECISION_RESPONSE:-} ]]; then printf '%s' "$DECISION_RESPONSE" > "$output"; else
        jq '{model:.model,answers:(.questions|with_entries(.value={type:"noul",noul:0.7})),usage:{input_tokens:10,output_tokens:0}}' "$body" > "$output"
    fi
fi
printf 200
SH
chmod +x "$scratch/bin/curl"
export PATH="$scratch/bin:$PATH"
source "$root/lib/trash.bash" 2>/dev/null
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 ($3 != $2)"; exit 1; }; echo "PASS: $1"; }
export CLM_BC250_URL=http://bc250:8700 CLM_BASE_URL=http://127.0.0.1:8700
export DECISION_LOCAL_HEALTH='{"ok":true,"embedder":true,"models":["clm-latest"]}'
questions='{"ok":{"type":"noul","instructions":"Is this okay?"}}'
result=$(@ CLM::Client decide: 'A synthetic fixture' questions: "$questions")
check 'local typed decision' 0.7 "$(jq -r .answers.ok.noul <<< "$result")"
check 'uses CLM API' http://127.0.0.1:8700/v1/systemone "$(jq -r .url "$DECISION_LOG")"
for health in '{}' '{"ok":true,"embedder":false,"models":["clm-latest"]}' '{"ok":true,"embedder":true,"models":["wrong"]}' '{"ok":true,"embedder":true,"mock":true,"models":["clm-latest"]}'; do
    export DECISION_REMOTE_HEALTH=$health
    target=$(@ Decision::Target named: clm-prefer-bc250)
    check 'unready remote falls back to local' clm-local "$(jq -r .name <<< "$target")"
done
export DECISION_REMOTE_HEALTH=$DECISION_LOCAL_HEALTH
target=$(@ Decision::Target named: clm-prefer-bc250)
check 'ready remote is preferred' clm-bc250 "$(jq -r .name <<< "$target")"
export DECISION_REMOTE_HEALTH='{}'
: > "$DECISION_LOG"
result=$(@ Decision::Target decide: fixture questions: "$questions" using: "$target")
check 'resolved target stays pinned' http://bc250:8700/v1/systemone "$(jq -r .url "$DECISION_LOG")"
export DECISION_POST_FAIL=1
if @ Decision::Target decide: fixture questions: "$questions" using: "$target" >/dev/null 2>&1; then exit 1; fi
check 'failed inference does not switch providers' 2 "$(wc -l < "$DECISION_LOG" | tr -d ' ')"
unset DECISION_POST_FAIL
export DECISION_LOCAL_HEALTH='{}'
if @ Decision::Target named: clm-prefer-bc250 >/dev/null 2>&1; then exit 1; fi
check 'no automatic OpenRouter fallback' 0 "$(jq -s '[.[]|select(.url|contains("openrouter"))]|length' "$DECISION_LOG")"
if @ Decision::Target named: typo >/dev/null 2>&1; then exit 1; fi
for response in '{"answers":{"ok":{"type":"noul","noul":1.1}}}' '{"answers":{"ok":{"type":"noul","noul":-1}}}' '{"answers":{"ok":{"type":"score","score":0}}}'; do
    export DECISION_RESPONSE=$response
    if @ CLM::Client decide: fixture questions: "$questions" >/dev/null 2>&1; then echo 'FAIL: accepted malformed answer'; exit 1; fi
done
unset DECISION_RESPONSE
for url in 'http://host/"bad' 'file:///etc/passwd' 'http://user:secret@host' $'http://host\nheader = "bad"'; do
    export CLM_BASE_URL=$url
    : > "$DECISION_LOG"
    if @ CLM::Client decide: fixture questions: "$questions" >/dev/null 2>&1; then exit 1; fi
    check 'bad endpoint fails before curl' '' "$(cat "$DECISION_LOG")"
done
echo 'PASS: CLM selection, endpoint, schema and failure contracts'
