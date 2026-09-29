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
export GMAIL_TEST_DIR="$scratch" GMAIL_TEST_ROOT="$root"
cat > "$scratch/bin/gws" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
jq -cn --args '$ARGS.positional' -- "$@" >> "$GMAIL_TEST_DIR/gws.jsonl"
if [[ ${GMAIL_TEST_ERROR:-0} == 1 ]]; then echo '{"error":{"message":"auth required"}}'; exit 2; fi
case "$1 $2 $3 ${4:-}" in
  'gmail users getProfile --params') printf '%s\n' '{"emailAddress":"fixture@example.invalid"}' ;;
  'gmail users messages list')
    if [[ ${GMAIL_TEST_TWO:-0} == 1 ]]; then echo '{"messages":[{"id":"fixture-1"},{"id":"fixture-2"}]}'; elif [[ ${GMAIL_TEST_EMPTY:-0} == 1 ]]; then echo '{"resultSizeEstimate":0}'; else echo '{"messages":[{"id":"fixture-1"}]}'; fi ;;
  'gmail users messages get') jq --argjson params "$6" '.id = $params.id' "$GMAIL_TEST_ROOT/tests/fixtures/gmail/message.json" ;;
  *) echo 'Unexpected Gmail operation' >&2; exit 99 ;;
esac
SH
cat > "$scratch/bin/curl" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
request='' output=''
while (($#)); do
  case "$1" in
    --data-binary) request=${2#@}; shift 2 ;;
    --output) output=$2; shift 2 ;;
    --config|--connect-timeout|--max-time|--write-out) shift 2 ;;
    *) shift ;;
  esac
done
cat "$request" >> "$GMAIL_TEST_DIR/requests.jsonl"
printf '\n' >> "$GMAIL_TEST_DIR/requests.jsonl"
if [[ ${GMAIL_TEST_CURL_FAIL:-0} == 1 ]]; then exit 28; fi
if jq -e '.questions.category' "$request" >/dev/null; then
  jq -cn --argjson confidence "${GMAIL_TEST_CONFIDENCE:-0.92}" \
    '{id:"category-receipt",model:"fixture-jev",answers:{category:{type:"choice",choice:"personal",confidence:$confidence,probabilities:{personal:0.94,other:0.06}}},usage:{input_tokens:100}}' > "$output"
elif jq -e '.questions.junk' "$request" >/dev/null; then
  jq -cn --argjson probability "${GMAIL_TEST_JUNK:-0.95}" '{id:"junk-receipt",model:"fixture-jev",answers:{junk:{type:"noul",noul:$probability}}}' > "$output"
else
  if [[ ${GMAIL_TEST_ATTENTION_FAIL:-0} == 1 ]]; then echo '{"error":{"message":"fixture failure"}}' > "$output"; printf 429; exit; fi
  jq -cn --argjson reply "${GMAIL_TEST_REPLY:-0.91}" --argjson action "${GMAIL_TEST_ACTION:-0.85}" '{id:"attention-receipt",model:"fixture-jev",answers:{reply:{type:"noul",noul:$reply},action:{type:"noul",noul:$action},urgency:{type:"score",score:1.2,confidence:0.6,probabilities:{"0":0,"1":0.8,"2":0.2},legend:{"0":"Routine","1":"Attention","2":"Urgent"}}},usage:{input_tokens:150}}' > "$output"
fi
printf 200
SH
chmod +x "$scratch/bin/gws" "$scratch/bin/curl"
export PATH="$scratch/bin:$PATH" OPENROUTER_API_KEY=fixture-secret
source "$root/lib/trash.bash"
passed=0
check() {
  [[ "$2" == "$3" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; }
  printf 'PASS: %s\n' "$1"; passed=$((passed+1))
}
fail_send() {
  local rc=0
  "$@" > "$scratch/failed.out" 2> "$scratch/failed.err" || rc=$?
  [[ $rc != 0 ]] || { echo 'FAIL: expected failed send'; exit 1; }
  if [[ -s "$scratch/failed.out" ]]; then
    jq -e '(.outcome | IN("http_error", "transport_error", "response_shape_error")) and (has("value") | not)' "$scratch/failed.out" >/dev/null
  fi
}
check 'verifies selected account' fixture@example.invalid "$(@ Gmail::Client requireAccount: fixture@example.invalid)"
fail_send @ Gmail::Client requireAccount: wrong@example.invalid
message=$(@ Gmail::Client message: fixture-1)
check 'decodes base64url MIME body' 'Can you confirm dinner on Friday?' "$(jq -r .body <<< "$message")"
check 'case-insensitive headers' 'Fixture Friend <friend@example.invalid>' "$(jq -r .from <<< "$message")"
check 'plain body provenance' text/plain "$(jq -r .bodySource <<< "$message")"
check 'omits attachments and HTML alternative' false "$(jq '.body | contains("SECRET") or contains("HTML")' <<< "$message")"
html=$(@ Gmail::Client normalize: '{"id":"html","snippet":"HTML summary","payload":{"mimeType":"text/html","body":{"data":"PGI-dGV4dDwvYj4="}}}')
check 'HTML-only mail is parsed' text/html "$(jq -r .bodySource <<< "$html")"
check 'uses HTML text rather than snippet' 'text' "$(jq -r .body <<< "$html")"
long=$(jq -cn '{id:"long",payload:{mimeType:"text/plain",body:{data:(("x"*12001)|@base64)}}}')
long=$(@ Gmail::Client normalize: "$long")
check 'limits model body' 12000 "$(jq '.body|length' <<< "$long")"
check 'marks truncation' true "$(jq -r .truncated <<< "$long")"
query='in:inbox subject:"$(touch never-run)"'
: > "$scratch/gws.jsonl"
result=$(@ Gmail::Review preview: "$query" limit: 1)
check 'one review per listed message' 1 "$(jq -s length <<< "$result")"
check 'correct message id' fixture-1 "$(jq -r .messageId <<< "$result")"
check 'interprets category' personal "$(jq -r .category.value.suggestedCategory <<< "$result")"
check 'retains confidence' 0.92 "$(jq -r .category.value.confidence <<< "$result")"
check 'interprets attention' consider_reply "$(jq -r .attention.value.suggestion <<< "$result")"
check 'preserves fractional urgency' 1.2 "$(jq -r .attention.value.urgency.score <<< "$result")"
check 'preserves first model receipt' category-receipt "$(jq -r .category.response.id <<< "$result")"
check 'preserves second model receipt' attention-receipt "$(jq -r .attention.response.id <<< "$result")"
check 'exactly two model requests' 2 "$(jq -s length "$scratch/requests.jsonl")"
check 'dependent stage receives category' true "$(jq -s '.[1].state | contains("suggestedCategory: personal")' "$scratch/requests.jsonl")"
check 'first stage contains decoded email' true "$(jq -s '.[0].state | contains("body: Can you confirm dinner on Friday?")' "$scratch/requests.jsonl")"
check 'query is passed as data' "$query" "$(jq -sr '.[0][5] | fromjson | .q' "$scratch/gws.jsonl")"
check 'limit is a JSON number' number "$(jq -sr '.[0][5] | fromjson | .maxResults | type' "$scratch/gws.jsonl")"
check 'no mail mutations' true "$(jq -s 'all(.[]; .[3]=="list" or .[3]=="get")' "$scratch/gws.jsonl")"
export GMAIL_TEST_TWO=1
multiple=$(@ Gmail::Review preview: inbox limit: 2)
check 'streams two distinct message proposals' '["fixture-1","fixture-2"]' "$(jq -sc '[.[].messageId]' <<< "$multiple")"
unset GMAIL_TEST_TWO
partial=$(@ Gmail::Review assess: "$(jq '.bodySource = "snippet"' <<< "$html")")
check 'partial context requires review even with high confidence' true "$(jq -r .reviewNeeded <<< "$partial")"
export GMAIL_TEST_CONFIDENCE=0.6
low=$(@ Gmail::Categorizer decide: "$message")
check 'uncertain categorization asks for review' true "$(jq -r .value.reviewNeeded <<< "$low")"
unset GMAIL_TEST_CONFIDENCE
export GMAIL_TEST_REPLY=0.5 GMAIL_TEST_ACTION=0.5
uncertain=$(@ Gmail::Attention decide: "$message")
check 'uncertainty is not interpreted as no action' review_uncertain "$(jq -r .value.suggestion <<< "$uncertain")"
export GMAIL_TEST_REPLY=0.1 GMAIL_TEST_ACTION=0.1
routine=$(@ Gmail::Attention decide: "$message")
check 'low action and reply probabilities permit later reading' read_when_convenient "$(jq -r .value.suggestion <<< "$routine")"
unset GMAIL_TEST_REPLY GMAIL_TEST_ACTION
export GMAIL_TEST_EMPTY=1
: > "$scratch/requests.jsonl"
check 'empty mailbox returns no proposals' '' "$(@ Gmail::Review preview: inbox limit: 1)"
check 'empty mailbox makes no model calls' 0 "$(jq -s length "$scratch/requests.jsonl")"
unset GMAIL_TEST_EMPTY
: > "$scratch/gws.jsonl"
fail_send @ Gmail::Review preview: inbox limit: 11
check 'bad limit fails before CLI' 0 "$(jq -s length "$scratch/gws.jsonl")"
export GMAIL_TEST_ERROR=1
fail_send @ Gmail::Review preview: inbox limit: 1
check 'Gmail failure makes no model calls' 0 "$(jq -s length "$scratch/requests.jsonl")"
unset GMAIL_TEST_ERROR
export GMAIL_TEST_CURL_FAIL=1
fail_send @ Gmail::Review assess: "$message"
check 'stage-one failure prevents stage two' 1 "$(jq -s length "$scratch/requests.jsonl")"
unset GMAIL_TEST_CURL_FAIL
export GMAIL_TEST_ATTENTION_FAIL=1
fail_send @ Gmail::Review assess: "$message"
unset GMAIL_TEST_ATTENTION_FAIL
check 'decimal threshold works' true "$(@ Jev::Answer probability: 0.85 atLeast: 0.8)"
check 'decimal below threshold works' false "$(@ Jev::Answer probability: 0.79 atLeast: 0.8)"
if @ Jev::Answer probability: 2 atLeast: 0.8 >/dev/null 2>&1; then echo 'FAIL: invalid probability'; exit 1; fi
passed=$((passed+1))
printf 'PASS: %s Gmail/Jev experiment checks\n' "$passed"

: > "$scratch/requests.jsonl"
junk=$(@ Gmail::Junk assess: "$message")
check 'one junk request per message' 1 "$(jq -s length "$scratch/requests.jsonl")"
check 'high probability suggests junk' likely_junk "$(jq -r .suggestion <<< "$junk")"
check 'junk receipt retained' junk-receipt "$(jq -r .response.id <<< "$junk")"
for p in 0 0.19 0.2 0.89 0.9 1; do
  export GMAIL_TEST_JUNK=$p
  case $p in 0|0.19) expected=likely_keep;; 0.9|1) expected=likely_junk;; *) expected=uncertain;; esac
  result=$(@ Gmail::Junk assess: "$message")
  check "junk threshold $p" "$expected" "$(jq -r .suggestion <<< "$result")"
done
for partial in "$long" "$(jq '.bodySource = "snippet"' <<< "$message")" "$(jq '.body = ""' <<< "$message")"; do
  result=$(@ Gmail::Junk assess: "$partial")
  check 'incomplete content blocks likely junk' uncertain "$(jq -r .suggestion <<< "$result")"
  check 'incomplete content gate is explicit' incomplete_content "$(jq -r .reason <<< "$result")"
done
export GMAIL_TEST_JUNK=0.1
result=$(@ Gmail::Junk assess: "$long")
check 'partial content may still favor keeping' likely_keep "$(jq -r .suggestion <<< "$result")"
unset GMAIL_TEST_JUNK
export GMAIL_TEST_CURL_FAIL=1
fail_send @ Gmail::Junk assess: "$message"
unset GMAIL_TEST_CURL_FAIL
markup=$(jq -cn '{id:"html",payload:{mimeType:"text/html",body:{data:("<head><style>SECRET</style></head><p>Tom &amp; Zoë</p><script>SECRET</script><p>Receipt &#36;25</p><img src=\"https://example.invalid/tracker\">"|@base64)}}}')
markup=$(@ Gmail::Client normalize: "$markup")
check 'HTML parser decodes entities and ignores script/style' 'Tom & Zoë Receipt $25' "$(jq -r .body <<< "$markup")"
empty=$(@ Gmail::Client normalize: '{"id":"empty","snippet":"summary","payload":{}}')
check 'missing body preserves partial provenance' snippet "$(jq -r .bodySource <<< "$empty")"
: > "$scratch/requests.jsonl"
examples='[{"sender":"friend@example.invalid","subject":"Example","verdict":"keep"}]'
result=$(@ Gmail::Junk assess: "$message" examples: "$examples")
check 'explicit preferences reach the model' true "$(jq -s '.[0].state | contains("preferenceExamples: - sender: friend@example.invalid") and contains("verdict: keep")' "$scratch/requests.jsonl")"
check 'email remains separate from reviewed preferences' true "$(jq -s '.[0].state | contains("email: ") and contains("body: Can you confirm dinner on Friday?")' "$scratch/requests.jsonl")"
result=$(@ Gmail::Junk assess: "$message")
check 'preferences do not leak across calls' false "$(jq -s '.[1].state | contains("verdict: keep")' "$scratch/requests.jsonl")"
fail_send @ Gmail::Junk assess: "$message" examples: 'not-json'
check 'malformed preferences fail before a model request' 2 "$(jq -s length "$scratch/requests.jsonl")"
printf 'PASS: %s total Gmail/Jev checks including junk\n' "$passed"
