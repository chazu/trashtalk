#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Decider target selection, readiness, transport, schema and failure contracts.
# A fake curl stands in for the service; no request leaves the machine.
set -euo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin"
export DECISION_LOG="$scratch/requests.jsonl" DECISION_BUSY="$scratch/busy"
cat > "$scratch/bin/curl" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
config='' body='' output='' deadline=''
while (($#)); do
    case "$1" in
        --config) config=$2; shift 2;;
        --data-binary) body=${2#@}; shift 2;;
        --output) output=$2; shift 2;;
        --max-time) deadline=$2; shift 2;;
        --write-out|--connect-timeout) shift 2;;
        *) shift;;
    esac
done
url=$(sed -n 's/^url = "\(.*\)"$/\1/p' "$config")
method=$(sed -n 's/^request = "\(.*\)"$/\1/p' "$config")
auth=$(grep -c Authorization "$config" || true)
jq -cn --arg url "$url" --arg method "$method" --arg deadline "$deadline" --argjson auth "$auth" \
    '{url:$url,method:$method,deadline:$deadline,auth:$auth}' >> "$DECISION_LOG"
if [[ $method == GET ]]; then
    printf '%s' "${DECISION_HEALTH:-}" > "$output"; printf 200; exit 0
fi
# DECISION_BUSY holds how many more requests answer 409.
busy=$(cat "$DECISION_BUSY" 2>/dev/null || echo 0)
if ((busy > 0)); then
    echo $((busy - 1)) > "$DECISION_BUSY"
    echo '{"error":"busy"}' > "$output"; printf 409; exit 0
fi
if [[ -n ${DECISION_STATUS:-} ]]; then printf '%s' "${DECISION_BODY:-{\}}" > "$output"; printf '%s' "$DECISION_STATUS"; exit 0; fi
if [[ -n ${DECISION_RESPONSE:-} ]]; then printf '%s' "$DECISION_RESPONSE" > "$output"; else
    jq '{model:.model,answers:(.questions|with_entries(.value={type:"noul",noul:0.7})),usage:{input_tokens:10,output_tokens:0}}' "$body" > "$output"
fi
printf 200
SH
chmod +x "$scratch/bin/curl"
export PATH="$scratch/bin:$PATH"
source "$root/lib/trash.bash" 2>/dev/null
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 ($3 != $2)"; exit 1; }; echo "PASS: $1"; }
refuses() { local name=$1; shift; if "$@" >/dev/null 2>&1; then echo "FAIL: $name"; exit 1; fi; echo "PASS: $name"; }
posts() { jq -s '[.[]|select(.method=="POST")]|length' "$DECISION_LOG"; }
questions='{"ok":{"type":"noul","instructions":"Is this okay?"}}'
overflow='{"outcome":"http_error","status":400,"body":"{\"error\": \"State exceeds trial context capacity\"}"}'

export TRASHTALK_DECIDER_URL=https://decider.test/
target=$(@ Decision::Target named: decider)
check 'decider target comes from config' \
    '{"name":"decider","provider":"decider","url":"https://decider.test","model":"decider-2b-v11-Q4_K_M","textLimit":4000}' "$target"
check 'decider is the default target' decider "$(@ Decision::Target selected | jq -r .name)"
check 'jev can be selected' jev "$(TRASHTALK_DECISION_TARGET=jev OPENROUTER_API_KEY=x @ Decision::Target selected | jq -r .name)"
refuses 'retired CLM targets are unknown' @ Decision::Target named: clm-local
refuses 'unknown targets fail' @ Decision::Target named: typo

: > "$DECISION_LOG"
result=$(@ Decision::Target decide: 'A synthetic fixture' questions: "$questions" using: "$target")
check 'typed decision' 0.7 "$(jq -r .answers.ok.noul <<< "$result")"
check 'posts to the systemone API with the model' 'https://decider.test/v1/systemone' "$(jq -r .url "$DECISION_LOG")"
check 'sends the decider model' decider-2b-v11-Q4_K_M "$(jq -r .model <<< "$result")"
check 'sends no credential' 0 "$(jq -r .auth "$DECISION_LOG")"
check 'uses a short deadline' 15 "$(jq -r .deadline "$DECISION_LOG")"

for health in '{}' '{"ready":false,"model":"decider-2b-v11-Q4_K_M"}' '{"ready":true,"model":"other"}' 'not json'; do
    export DECISION_HEALTH=$health
    : > "$DECISION_LOG"
    refuses "unready health is refused: $health" @ Decision::Target requireReady: "$target"
    check 'a readiness probe makes no decision request' 0 "$(posts)"
done
export DECISION_HEALTH='{"ready":true,"model":"decider-2b-v11-Q4_K_M","backend":"vulkan","context":2048}'
check 'ready health passes the target through' "$target" "$(@ Decision::Target requireReady: "$target")"
check 'readiness probes the health endpoint' 'https://decider.test/health' "$(jq -r .url "$DECISION_LOG" | tail -1)"
: > "$DECISION_LOG"
jev=$(OPENROUTER_API_KEY=x @ Decision::Target named: jev)
check 'jev needs no probe' "$jev" "$(@ Decision::Target requireReady: "$jev")"
check 'jev readiness makes no request' '' "$(cat "$DECISION_LOG")"

: > "$DECISION_LOG"
echo 2 > "$DECISION_BUSY"
result=$(@ Decision::Target decide: fixture questions: "$questions" using: "$target")
check 'a busy service is retried' 0.7 "$(jq -r .answers.ok.noul <<< "$result")"
check 'two busy answers cost two retries' 3 "$(posts)"
: > "$DECISION_LOG"
echo 9 > "$DECISION_BUSY"
failure=$(@ Decision::Target decide: fixture questions: "$questions" using: "$target" 2>/dev/null) && exit 1
check 'a persistently busy service fails with its status' 409 "$(jq -r .status <<< "$failure")"
check 'busy retries stop after three' 4 "$(posts)"
echo 0 > "$DECISION_BUSY"
: > "$DECISION_LOG"
export DECISION_STATUS=503
refuses 'an unavailable service fails' @ Decision::Target decide: fixture questions: "$questions" using: "$target"
check 'other failures are not retried' 1 "$(posts)"
unset DECISION_STATUS

check 'a context overflow is recognised' true "$(@ Decider::Client overflowed: "$overflow")"
check 'another 400 is not an overflow' false "$(@ Decider::Client overflowed: '{"outcome":"http_error","status":400,"body":"{\"error\":\"Unknown model\"}"}')"
check 'a transport failure is not an overflow' false "$(@ Decider::Client overflowed: '{"outcome":"transport_error","exit_code":7}')"
smaller=$(@ Decision::Target shrink: "$target" after: "DecisionRequestError: $overflow")
check 'an overflow halves the text limit' 2000 "$(jq -r .textLimit <<< "$smaller")"
check 'a shrunk target keeps its endpoint' https://decider.test "$(jq -r .url <<< "$smaller")"
refuses 'other failures do not shrink' @ Decision::Target shrink: "$target" after: 'DecisionRequestError: {"outcome":"http_error","status":503,"body":""}'
refuses 'shrinking stops at 500 characters' @ Decision::Target shrink: "$(jq -c '.textLimit=900' <<< "$target")" after: "DecisionRequestError: $overflow"

for response in '{"answers":{"ok":{"type":"noul","noul":1.1}}}' '{"answers":{"ok":{"type":"noul","noul":-1}}}' '{"answers":{"ok":{"type":"score","score":0}}}'; do
    export DECISION_RESPONSE=$response
    refuses 'malformed answers are rejected' @ Decider::Client decide: fixture questions: "$questions" using: "$target"
done
unset DECISION_RESPONSE
for url in 'http://host/"bad' 'file:///etc/passwd' 'http://user:secret@host' $'http://host\nheader = "bad"'; do
    : > "$DECISION_LOG"
    refuses 'bad endpoint fails' @ Decider::Client decide: fixture questions: "$questions" using: "$(jq -c --arg url "$url" '.url=$url' <<< "$target")"
    check 'bad endpoint fails before curl' '' "$(cat "$DECISION_LOG")"
done
echo 'PASS: Decider selection, readiness, transport, schema and failure contracts'
