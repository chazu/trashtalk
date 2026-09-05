#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
TEST_CLASS="ProposalMilestone${BASHPID}"
SOURCE_FILE="$PROJECT_DIR/trash/user/${TEST_CLASS}.trash"
COMPILED_FILE="$PROJECT_DIR/trash/.compiled/${TEST_CLASS}"
RELATIVE_PATH="trash/user/${TEST_CLASS}.trash"
FAKE_BIN="$TEST_TMP/bin"
FAKE_REVIEW="$TEST_TMP/review.diff"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -f "$SOURCE_FILE" "$COMPILED_FILE"
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN" "$PROJECT_DIR/trash/user"
cat > "$FAKE_BIN/indiff" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
cat > "$FAKE_REVIEW"
hunk_count=$(grep -c '^@@ ' "$FAKE_REVIEW")
all_hunks=$(jq -cn --argjson count "$hunk_count" '[range(0; $count)]')
if [[ -n "${FAKE_INDIFF_MUTATE:-}" ]]; then
    printf '\n# concurrent edit\n' >> "$FAKE_INDIFF_MUTATE"
fi
case "${FAKE_INDIFF_OUTCOME:-accepted}" in
    accepted)
        accepted="${FAKE_INDIFF_ACCEPTED:-$all_hunks}"
        rejected="${FAKE_INDIFF_REJECTED:-[]}"
        jq -cn --argjson accepted "$accepted" --argjson rejected "$rejected" \
            '{schema_version:1,outcome:"accepted",accepted_hunks:$accepted,rejected_hunks:$rejected}'
        exit 0
        ;;
    rejected) jq -cn --argjson rejected "$all_hunks" '{schema_version:1,outcome:"rejected",accepted_hunks:[],rejected_hunks:$rejected}'; exit 3 ;;
    cancelled) jq -cn '{schema_version:1,outcome:"cancelled",accepted_hunks:[],rejected_hunks:[]}'; exit 130 ;;
    malformed) printf 'not-json\n'; exit 0 ;;
esac
FAKE
chmod +x "$FAKE_BIN/indiff"

export FAKE_REVIEW
export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"

write_original() {
    sed "s/__TEST_CLASS__/$TEST_CLASS/g" > "$SOURCE_FILE" <<'EOF'
# marker old
# filler 01
# filler 02
# filler 03
# filler 04
# filler 05
# filler 06
# filler 07
# filler 08
# filler 09
# filler 10

__TEST_CLASS__ subclass: Object

  method: value [
    ^ 'old'
  ]

  rawTestMethod: testValue [
    local instance value failed=0
    instance=$(@ __TEST_CLASS__ new)
    value=$(@ "$instance" value)
    _assert_eq "$value" "old" "proposal candidate has expected value" || failed=1
    @ "$instance" delete >/dev/null
    return "$failed"
  ]
EOF
    "$PROJECT_DIR/lib/jq-compiler/driver.bash" compile "$SOURCE_FILE" > "$COMPILED_FILE"
}

sha256_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d' ' -f1
    else
        shasum -a 256 "$1" | cut -d' ' -f1
    fi
}

proposal_for() {
    local candidate="$1"
    local base_hash="${2:-$(sha256_file "$SOURCE_FILE")}"
    local diff
    diff=$(diff -u --label "a/$RELATIVE_PATH" --label "b/$RELATIVE_PATH" "$SOURCE_FILE" "$candidate")
    jq -cn --arg class "$TEST_CLASS" --arg path "$RELATIVE_PATH" \
        --arg hash "$base_hash" --arg diff "$diff" \
        '{schema_version:1,kind:"trashtalk_source_patch",files:[{class_name:$class,path:$path,base_sha256:$hash,diff:$diff}]}'
}

write_candidate() {
    local value="$1" expected="$2" output="$3"
    sed -e "s/^    \^ 'old'/    ^ '$value'/" \
        -e "s/\"old\" \"proposal/\"$expected\" \"proposal/" \
        "$SOURCE_FILE" > "$output"
}

write_original
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null
trap cleanup EXIT

PASSED=0
FAILED=0
pass() { echo "  PASS: $1"; ((PASSED++)) || true; }
fail() { echo "  FAIL: $1"; ((FAILED++)) || true; }
assert_eq() {
    if [[ "$2" == "$3" ]]; then pass "$1"; else
        echo "    expected: $2"
        echo "    actual:   $3"
        fail "$1"
    fi
}
assert_true() {
    local name="$1"
    shift
    if "$@" >/dev/null; then pass "$name"; else fail "$name"; fi
}

echo "=== Source Proposal Integration Tests ==="

before_source=$(sha256_file "$SOURCE_FILE")
before_artifact=$(sha256_file "$COMPILED_FILE")
result=$(@ SourceProposal reviewAndApply: '{}')
assert_eq "invalid schema is rejected before review" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "invalid schema leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"

candidate="$TEST_TMP/rejected.trash"
write_candidate new new "$candidate"
proposal=$(proposal_for "$candidate")
export FAKE_INDIFF_OUTCOME=rejected
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "explicit rejection is structured" "rejected" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "rejection leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"
assert_eq "rejection leaves artifact unchanged" "$before_artifact" "$(sha256_file "$COMPILED_FILE")"
assert_eq "review receives the proposed diff exactly" "$(printf '%s' "$proposal" | jq -r '.files[0].diff')" "$(cat "$FAKE_REVIEW")"

export FAKE_INDIFF_OUTCOME=cancelled
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "review cancellation is structured" "cancelled" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "review cancellation leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"

export FAKE_INDIFF_OUTCOME=accepted
stale_proposal="$proposal"
printf '\n# changed after proposal\n' >> "$SOURCE_FILE"
stale_source=$(sha256_file "$SOURCE_FILE")
result=$(@ SourceProposal reviewAndApply: "$stale_proposal")
assert_eq "stale proposal is distinguished" "stale" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "stale proposal cannot overwrite newer source" "$stale_source" "$(sha256_file "$SOURCE_FILE")"
write_original
before_source=$(sha256_file "$SOURCE_FILE")
before_artifact=$(sha256_file "$COMPILED_FILE")

malformed="$TEST_TMP/malformed.trash"
printf '%s\n' "$TEST_CLASS subclass: Object" '' '  method: broken [' > "$malformed"
proposal=$(proposal_for "$malformed")
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "compile failure is a gate failure" "gate_failed" "$(printf '%s' "$result" | jq -r .outcome)"
assert_true "compile failure identifies compiler or Bash phase" \
    grep -Eq '^(compile|bash)$' <(printf '%s' "$result" | jq -r .phase)
assert_eq "compile failure leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"
assert_eq "compile failure leaves artifact unchanged" "$before_artifact" "$(sha256_file "$COMPILED_FILE")"

wrong_class="$TEST_TMP/wrong-class.trash"
sed "s/^$TEST_CLASS subclass:/DifferentProposalClass subclass:/" "$SOURCE_FILE" > "$wrong_class"
proposal=$(proposal_for "$wrong_class")
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "class identity change is a gate failure" "gate_failed" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "class identity gate is explicit" "identity" "$(printf '%s' "$result" | jq -r .phase)"
assert_eq "identity failure leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"
assert_eq "identity failure leaves artifact unchanged" "$before_artifact" "$(sha256_file "$COMPILED_FILE")"

failing="$TEST_TMP/failing.trash"
write_candidate new old "$failing"
proposal=$(proposal_for "$failing")
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "test failure is a gate failure" "gate_failed" "$(printf '%s' "$result" | jq -r .outcome)"
assert_eq "test failure identifies phase" "test" "$(printf '%s' "$result" | jq -r .phase)"
assert_eq "test failure leaves source unchanged" "$before_source" "$(sha256_file "$SOURCE_FILE")"
assert_eq "test failure leaves artifact unchanged" "$before_artifact" "$(sha256_file "$COMPILED_FILE")"

successful="$TEST_TMP/successful.trash"
write_candidate new new "$successful"
partial="$TEST_TMP/partial.trash"
sed 's/# marker old/# marker proposed/' "$successful" > "$partial"
proposal=$(proposal_for "$partial")
assert_eq "partial proposal has two reviewable hunks" "2" \
    "$(printf '%s' "$proposal" | jq -r '.files[0].diff' | grep -c '^@@ ')"
export FAKE_INDIFF_ACCEPTED='[1]'
export FAKE_INDIFF_REJECTED='[0]'
result=$(@ SourceProposal reviewAndApply: "$proposal")
unset FAKE_INDIFF_ACCEPTED FAKE_INDIFF_REJECTED
assert_eq "accepted subset of passing hunks is applied" "applied" "$(printf '%s' "$result" | jq -r .outcome)"
assert_true "accepted hunk changed behavior" grep -Fq "    ^ 'new'" "$SOURCE_FILE"
assert_true "rejected hunk made no source change" grep -Fq '# marker old' "$SOURCE_FILE"

write_original
proposal=$(proposal_for "$successful")
result=$(@ Agent reviewAndApplyProposal: "$proposal")
assert_eq "accepted passing proposal is applied" "applied" "$(printf '%s' "$result" | jq -r .outcome)"
assert_true "applied source contains reviewed change" grep -Fq "    ^ 'new'" "$SOURCE_FILE"
assert_true "applied artifact is valid Bash" bash -n "$COMPILED_FILE"
instance=$(@ "$TEST_CLASS" new)
assert_eq "applied class is reloaded" "new" "$(@ "$instance" value)"
@ "$instance" delete >/dev/null

write_original
before_source=$(sha256_file "$SOURCE_FILE")
candidate="$TEST_TMP/race.trash"
write_candidate new new "$candidate"
proposal=$(proposal_for "$candidate")
export FAKE_INDIFF_MUTATE="$SOURCE_FILE"
result=$(@ SourceProposal reviewAndApply: "$proposal")
unset FAKE_INDIFF_MUTATE
assert_eq "post-review concurrent edit is stale" "stale" "$(printf '%s' "$result" | jq -r .outcome)"
assert_true "post-review edit remains present" grep -Fq '# concurrent edit' "$SOURCE_FILE"

write_original
proposal=$(proposal_for "$candidate")
proposal_with_command=$(printf '%s' "$proposal" | jq -c '.command = "touch /tmp/must-not-run"')
result=$(@ SourceProposal reviewAndApply: "$proposal_with_command")
assert_eq "unknown command field is rejected" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"

unsafe_path=$(printf '%s' "$proposal" | jq -c '.files[0].path = "../outside.trash"')
result=$(@ SourceProposal reviewAndApply: "$unsafe_path")
assert_eq "repository traversal path is rejected" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"

wrong_headers=$(printf '%s' "$proposal" | jq -c '.files[0].diff |= sub("a/trash/"; "a/other/")')
result=$(@ SourceProposal reviewAndApply: "$wrong_headers")
assert_eq "diff headers must match declared source" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"

export FAKE_INDIFF_OUTCOME=malformed
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "malformed reviewer result fails closed" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"
export FAKE_INDIFF_OUTCOME=accepted

export FAKE_INDIFF_ACCEPTED='[99]'
export FAKE_INDIFF_REJECTED='[]'
result=$(@ SourceProposal reviewAndApply: "$proposal")
unset FAKE_INDIFF_ACCEPTED FAKE_INDIFF_REJECTED
assert_eq "out-of-range hunk decision fails closed" "invalid" "$(printf '%s' "$result" | jq -r .outcome)"

context=$(@ AxeAgent proposalContextFor: 'change value' class: "$TEST_CLASS" workingDirectory: "$PROJECT_DIR")
assert_eq "proposal context supplies exact source hash" "$(sha256_file "$SOURCE_FILE")" "$(printf '%s' "$context" | jq -r .base_sha256)"
assert_eq "proposal context supplies repository path" "$RELATIVE_PATH" "$(printf '%s' "$context" | jq -r .path)"
assert_true "proposer uses read-only tools" grep -Fq 'tools = ["list_directory", "read_file"]' "$PROJECT_DIR/axe/agents/trashtalk-proposer.toml"
if grep -Eq 'write_file|edit_file|run_command|sub_agents[[:space:]]*=' "$PROJECT_DIR/axe/agents/trashtalk-proposer.toml"; then
    fail "proposer enables no mutation, command, or delegation tools"
else
    pass "proposer enables no mutation, command, or delegation tools"
fi

PATH="/opt/homebrew/bin:/usr/bin:/bin"
hash -r
result=$(@ SourceProposal reviewAndApply: "$proposal")
assert_eq "missing reviewer fails closed" "unavailable" "$(printf '%s' "$result" | jq -r .outcome)"
PATH="$ORIGINAL_PATH"
export PATH
hash -r

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
