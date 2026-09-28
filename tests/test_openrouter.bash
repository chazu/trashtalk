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
jq -cn --arg config "$config" --rawfile body "$body" \
  --arg mode "$(stat -f '%Lp' "$config")" \
  --argjson has_authorization "$(grep -q 'Authorization: Bearer fixture-secret' "$config" && echo true || echo false)" \
  '{config:$config,mode:($mode | tonumber),has_authorization:$has_authorization,body:($body | fromjson)}' > "$OPENROUTER_CURL_LOG"
if [[ "${OPENROUTER_CURL_EXIT:-0}" != 0 ]]; then
    printf '%s\n' 'fixture transport failed' >&2
    exit "$OPENROUTER_CURL_EXIT"
fi
if [[ -n "${OPENROUTER_CURL_RESPONSE+x}" ]]; then
    printf '%s' "$OPENROUTER_CURL_RESPONSE" > "$output"
else
    printf '%s' '{"choices":[{"message":{"content":"ready"}}]}' > "$output"
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
prompt=$'quote " and newline\nsecond line'
umask_before=$(umask)
result=$(@ OpenRouter complete: "$prompt")
check 'uses the fixed Jev model' '~typesafe/jev-latest' "$(field "$result" .model)"
check 'returns a successful result' success "$(field "$result" .outcome)"
check 'returns completion content' ready "$(field "$result" .content)"
check 'uses OpenRouter request schema' '~typesafe/jev-latest' "$(jq -r .body.model "$OPENROUTER_CURL_LOG")"
check 'preserves prompt as JSON data' "$prompt" "$(jq -r '.body.messages[0].content' "$OPENROUTER_CURL_LOG")"
check 'authorization is absent from curl arguments' false "$(grep -q 'fixture-secret' "$OPENROUTER_CURL_LOG" && echo true || echo false)"
check 'authorization is present only in protected config' true "$(jq -r .has_authorization "$OPENROUTER_CURL_LOG")"
check 'config has restrictive permissions' 600 "$(jq -r .mode "$OPENROUTER_CURL_LOG")"
check 'caller umask is restored' "$umask_before" "$(umask)"

export OPENROUTER_CURL_STATUS=429 OPENROUTER_CURL_RESPONSE='{"error":{"message":"rate limited"}}'
if result=$(@ OpenRouter complete: retry 2>/dev/null); then echo 'FAIL: HTTP error returned success'; exit 1; fi
check 'HTTP error preserves status' 429 "$(field "$result" .status)"
check 'HTTP error has distinct outcome' http_error "$(field "$result" .outcome)"
unset OPENROUTER_CURL_STATUS OPENROUTER_CURL_RESPONSE

export OPENROUTER_CURL_EXIT=28
if result=$(@ OpenRouter complete: timeout 2>/dev/null); then echo 'FAIL: transport error returned success'; exit 1; fi
check 'transport error preserves curl status' 28 "$(field "$result" .exit_code)"
check 'transport error has distinct outcome' transport_error "$(field "$result" .outcome)"
unset OPENROUTER_CURL_EXIT OPENROUTER_API_KEY
if @ OpenRouter complete: missing-key >/dev/null 2>&1; then echo 'FAIL: missing API key returned success'; exit 1; fi
passed=$((passed+1))
printf 'PASS: %s OpenRouter adapter checks\n' "$passed"
