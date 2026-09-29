#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
mkdir -p "$test_dir/bin"
export OPENROUTER_CURL_LOG="$test_dir/curl-log.json"
cat > "$test_dir/bin/curl" <<'SH'
#!/usr/bin/env bash
set -uo pipefail
argv=$(jq -cn --args '$ARGS.positional' -- "$@")
config='' body='' output='' writeout=''
while (($#)); do
    case "$1" in
        --config) config=$2; shift 2 ;;
        --data-binary) body=${2#@}; shift 2 ;;
        --output) output=$2; shift 2 ;;
        --write-out) writeout=$2; shift 2 ;;
        --connect-timeout|--max-time) shift 2 ;;
        *) shift ;;
    esac
done
jq -cn --arg config "$config" --argjson argv "$argv" --rawfile body "$body" \
  --arg mode "$(stat -f '%Lp' "$config" 2>/dev/null || stat -c '%a' "$config")" \
  --arg endpoint "$(sed -n 's/^url[[:space:]]*=[[:space:]]*"\(.*\)"$/\1/p' "$config")" \
  --argjson has_authorization "$(grep -q 'Authorization: Bearer fixture-secret' "$config" && echo true || echo false)" \
  '{argv:$argv,config:$config,mode:($mode | tonumber),has_authorization:$has_authorization,endpoint:$endpoint,body:($body | fromjson)}' > "$OPENROUTER_CURL_LOG"
if [[ "${OPENROUTER_CURL_EXIT:-0}" != 0 ]]; then
    printf '%s\n' 'fixture transport failed' >&2
    exit "$OPENROUTER_CURL_EXIT"
fi
if [[ -n "${OPENROUTER_CURL_RESPONSE+x}" ]]; then
    printf '%s' "$OPENROUTER_CURL_RESPONSE" > "$output"
else
    printf '%s' '{"id":"fixture-decision","model":"typesafe/jev-1.13-20260917","answers":{"refund":{"type":"noul","noul":0.99}},"usage":{"input_tokens":42}}' > "$output"
fi
printf '%s' "${OPENROUTER_CURL_STATUS:-200}"
SH
chmod +x "$test_dir/bin/curl"
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
source "$root/lib/trash.bash" 2>/dev/null
trap 'rm -rf "$test_dir"' EXIT
passed=0
check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; passed=$((passed+1)); else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
field() { jq -r "$2" <<< "$1"; }

export OPENROUTER_API_KEY='fixture-secret'
state=$'quote " and newline\nsecond line'
questions='{"refund":{"type":"noul","instructions":"Is the customer requesting a refund?"}}'
umask_before=$(umask)
result=$(@ OpenRouter::Jev decide: "$state" questions: "$questions") || exit 1
check 'preserves served model' 'typesafe/jev-1.13-20260917' "$(field "$result" .model)"
check 'returns typed decision' 0.99 "$(field "$result" .answers.refund.noul)"
check 'preserves usage' 42 "$(field "$result" .usage.input_tokens)"
check 'uses pinned Jev model' 'typesafe/jev-1.13' "$(jq -r .body.model "$OPENROUTER_CURL_LOG")"
check 'uses Decisions endpoint' 'https://openrouter.ai/api/alpha/decisions' "$(jq -r .endpoint "$OPENROUTER_CURL_LOG")"
check 'preserves state as JSON string' "$state" "$(jq -r .body.state "$OPENROUTER_CURL_LOG")"
check 'sends typed questions' "$questions" "$(jq -c .body.questions "$OPENROUTER_CURL_LOG")"
check 'does not send chat messages' false "$(jq -r '.body | has("messages")' "$OPENROUTER_CURL_LOG")"
check 'authorization is absent from curl arguments' false "$(grep -q 'fixture-secret' "$OPENROUTER_CURL_LOG" && echo true || echo false)"
check 'authorization is present only in protected config' true "$(jq -r .has_authorization "$OPENROUTER_CURL_LOG")"
check 'config has restrictive permissions' 600 "$(jq -r .mode "$OPENROUTER_CURL_LOG")"
check 'temporary config is removed' false "$(test -e "$(jq -r .config "$OPENROUTER_CURL_LOG")" && echo true || echo false)"
check 'caller umask is restored' "$umask_before" "$(umask)"

export OPENROUTER_CURL_RESPONSE='{"model":"typesafe/jev-1.13-20260917","answers":{"team":{"type":"choice","choice":"billing","probabilities":{"billing":1},"confidence":1},"urgency":{"type":"score","score":1.99,"legend":{"2":"Deadline"}},"refund":{"type":"noul","noul":0.99}}}'
result=$(@ Examples::JevTicket assess: 'Please refund the duplicate charge before Friday.') || exit 1
check 'DSL example returns choice' billing "$(field "$result" .answers.team.choice)"
check 'DSL example preserves fractional score' 1.99 "$(field "$result" .answers.urgency.score)"
check 'DSL example sends all question types' '["choice","score","noul"]' "$(jq -c '[.body.questions[] | .type]' "$OPENROUTER_CURL_LOG")"
unset OPENROUTER_CURL_RESPONSE

export OPENROUTER_CURL_STATUS=429 OPENROUTER_CURL_RESPONSE='{"error":{"message":"rate limited"}}'
if result=$(@ OpenRouter::Jev decide: retry questions: "$questions" 2>/dev/null); then echo 'FAIL: HTTP error returned success'; exit 1; fi
check 'HTTP error preserves status' 429 "$(field "$result" .status)"
check 'HTTP error has distinct outcome' http_error "$(field "$result" .outcome)"
unset OPENROUTER_CURL_STATUS OPENROUTER_CURL_RESPONSE

export OPENROUTER_CURL_EXIT=28
if result=$(@ OpenRouter::Jev decide: timeout questions: "$questions" 2>/dev/null); then echo 'FAIL: transport error returned success'; exit 1; fi
check 'transport error preserves curl status' 28 "$(field "$result" .exit_code)"
check 'transport error has distinct outcome' transport_error "$(field "$result" .outcome)"
unset OPENROUTER_CURL_EXIT
for response in 'not json' '{"choices":[{"message":{"content":"ready"}}]}' '{"answers":{}}' '{"answers":{"refund":{"type":"choice","choice":"yes"}}}' '{"answers":{"refund":{"type":"noul","noul":"yes"}}}' '{"error":{"message":"failed"}}'; do
    export OPENROUTER_CURL_RESPONSE="$response"
    if result=$(@ OpenRouter::Jev decide: test questions: "$questions" 2>/dev/null); then echo 'FAIL: malformed decision returned success'; exit 1; fi
    check 'rejects malformed decision response' response_shape_error "$(field "$result" .outcome)"
done
unset OPENROUTER_CURL_RESPONSE
for invalid in '' 'broken json' '[]' '{}'; do
    rm -f "$OPENROUTER_CURL_LOG"
    if @ OpenRouter::Jev decide: test questions: "$invalid" >/dev/null 2>&1; then echo 'FAIL: invalid questions returned success'; exit 1; fi
    [[ ! -e "$OPENROUTER_CURL_LOG" ]] || { echo 'FAIL: invalid questions reached network'; exit 1; }
    passed=$((passed+1))
done
if @ OpenRouter::Jev decide: '  ' questions: "$questions" >/dev/null 2>&1; then echo 'FAIL: empty state returned success'; exit 1; fi
passed=$((passed+1))
unset OPENROUTER_API_KEY
if @ OpenRouter::Jev decide: missing-key questions: "$questions" >/dev/null 2>&1; then echo 'FAIL: missing API key returned success'; exit 1; fi
passed=$((passed+1))
printf 'PASS: %s Jev decision checks\n' "$passed"
