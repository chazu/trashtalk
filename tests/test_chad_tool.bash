#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
mkdir -p "$test_dir/bin" "$test_dir/work tree" "$test_dir/deps"
export CHAD_TOOL_LOG="$test_dir/invocation.json" CHAD_TOOL_STDIN="$test_dir/stdin"
cat > "$test_dir/bin/chad" <<'SH'
#!/usr/bin/env bash
set -uo pipefail
args=$(printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]')
jq -cn --argjson argv "$args" --arg cwd "$PWD" '{argv:$argv,cwd:$cwd}' > "$CHAD_TOOL_LOG"
case "${1:-}" in
    --version) echo 'chad 2.0.3'; exit ;;
    levers) echo '{"levers":{"example":{"description":"fixture"}}}'; exit ;;
esac
cat > "$CHAD_TOOL_STDIN"
printf 'harness diagnostic\n' >&2
printf 'native text output\n'
exit "${CHAD_TOOL_EXIT:-0}"
SH
chmod +x "$test_dir/bin/chad"
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
source "$root/lib/trash.bash" 2>/dev/null
trap 'rm -rf "$test_dir"' EXIT
passed=0
check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; passed=$((passed+1)); else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
field() { jq -r "$2" <<< "$1"; }
logged() { jq -r "$1" "$CHAD_TOOL_LOG"; }
check 'Chad version' 'chad 2.0.3' "$(@ Tools::Chad version)"
result=$(@ Tools::Chad levers)
check 'Chad metadata has structured data' fixture "$(field "$result" .data.levers.example.description)"
rm "$CHAD_TOOL_LOG"
prompt=$'--plan $(touch unexpected-file); "quoted"\nsecond line'
result=$(@ Tools::Chad dryRun: "$prompt" workingDirectory: "$test_dir/work tree")
check 'dry run creates no child process' false "$([[ -e "$CHAD_TOOL_LOG" ]] && echo true || echo false)"
check 'dry run reports exact prompt' "$prompt" "$(field "$result" '.result.argv[-1]')"
check 'dry run supplies noninteractive stdin' '' "$(field "$result" .result.stdin)"
check 'dry run identifies working directory' "$test_dir/work tree" "$(field "$result" .result.working_directory)"
before=$PWD
result=$(@ Tools::Chad run: "$prompt" model: '/models/my model' workingDirectory: "$test_dir/work tree")
check 'headless task is a literal positional argument' "$prompt" "$(logged '.argv[-1]')"
check 'option-looking task follows separator' -- "$(logged '.argv[-2]')"
check 'model selection is explicit' '/models/my model' "$(logged '.argv[1]')"
check 'headless run receives EOF on stdin' 0 "$(wc -c < "$CHAD_TOOL_STDIN" | tr -d ' ')"
check 'headless run selects the requested directory' "$test_dir/work tree" "$(logged .cwd)"
check 'headless run keeps caller directory' "$before" "$PWD"
check 'headless run preserves text output' 'native text output' "$(field "$result" '.result.content | rtrimstr("\n")')"
check 'headless run identifies its backend' chad "$(field "$result" .backend)"
check 'headless run identifies text protocol' text "$(field "$result" .result.format)"
check 'headless run keeps diagnostics' 'harness diagnostic' "$(field "$result" '.stderr | rtrimstr("\n")')"
check 'headless run does not implicitly resume' false "$(logged '.argv | any(. == "--continue" or . == "--resume")')"
check 'prompt is never evaluated as shell' false "$([[ -e "$test_dir/work tree/unexpected-file" ]] && echo true || echo false)"
result=$(@ Tools::Chad plan: inspect workingDirectory: "$test_dir/work tree")
check 'plan explicitly selects the native read-only mode' --plan "$(logged '.argv[0]')"
check 'plan result exposes execution mode' plan "$(field "$result" .result.mode)"
result=$(@ Tools::Chad continueLatest: followup workingDirectory: "$test_dir/work tree")
check 'continueLatest uses directory-based continuation' --continue "$(logged '.argv[0]')"
check 'continueLatest does not open interactive resume picker' false "$(logged '.argv | any(. == "--resume")')"
check 'result discloses latest-session semantics' true "$(field "$result" .result.continue_latest)"
for prompt in serve levers prove; do
    result=$(@ Tools::Chad run: "$prompt" workingDirectory: "$test_dir/work tree")
    check "task named $prompt is not a subcommand" -- "$(logged '.argv[0]')"
done
for code in 1 2 72; do
    export CHAD_TOOL_EXIT=$code
    result=$(@ Tools::Chad run: inspect workingDirectory: "$test_dir/work tree")
    expected=runtime_error
    [[ "$code" != 2 ]] || expected=configuration_error
    check "exit $code is classified" "$expected" "$(field "$result" .outcome)"
    check "exit $code is preserved" "$code" "$(field "$result" .exit_code)"
    check "exit $code has no fabricated final answer" null "$(field "$result" .result)"
done
unset CHAD_TOOL_EXIT
for prompt in '' '   '; do
    rm "$CHAD_TOOL_LOG"
    if @ Tools::Chad run: "$prompt" workingDirectory: "$test_dir/work tree" >/dev/null 2>&1; then echo 'FAIL: blank prompt accepted'; exit 1; fi
    check 'blank prompt cannot launch a TUI' false "$([[ -e "$CHAD_TOOL_LOG" ]] && echo true || echo false)"
    : > "$CHAD_TOOL_LOG"
done
if @ Tools::Chad run: inspect workingDirectory: '' >/dev/null 2>&1; then echo 'FAIL: empty cwd accepted'; exit 1; fi
passed=$((passed+1))
ln -s "$(command -v jq)" "$test_dir/deps/jq"
PATH="$test_dir/deps:/usr/bin:/bin"
hash -r
result=$(@ Tools::Chad run: inspect workingDirectory: "$test_dir/work tree")
check 'missing harness is not installed by a run' missing_tool "$(field "$result" .outcome)"
check 'missing harness retains common backend field' chad "$(field "$result" .backend)"
result=$(@ Tools::Chad dryRun: inspect workingDirectory: "$test_dir/work tree")
check 'dry run can describe an uninstalled harness' chad "$(field "$result" '.result.argv[0]')"
printf 'PASS: %s Chad adapter checks\n' "$passed"
