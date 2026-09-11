#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
original_path=$PATH
mkdir -p "$test_dir/bin" "$test_dir/work repo" "$test_dir/deps"
export CODE_TOOL_LOG="$test_dir/invocation.json" CODE_TOOL_STDIN="$test_dir/stdin"

# Record actual process inputs, then exercise valid output, no matches, and
# independent protocol/process failures. No external installations are needed.
cat > "$test_dir/bin/fixture" <<'SH'
#!/usr/bin/env bash
set -uo pipefail
tool=${0##*/}
args=$(printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]')
jq -cn --argjson argv "$args" --arg cwd "$PWD" --arg db "${ROAM_DB_DIR:-}" \
    --arg no_auto "${ROAM_NO_AUTO_INDEX:-}" '{argv:$argv,cwd:$cwd,db:$db,no_auto:$no_auto}' > "$CODE_TOOL_LOG"
: > "$CODE_TOOL_STDIN"
for arg in "$@"; do
    [[ "$arg" != --stdin ]] || cat > "$CODE_TOOL_STDIN"
done
if [[ "${1:-}" == --version ]]; then printf '%s test-version\n' "$tool"; exit; fi
case "${CODE_TOOL_SCENARIO:-ok}" in
    invalid) printf 'progress chatter\n{}\n'; exit 0 ;;
    scalar) printf 'false\n'; exit 0 ;;
    multiple) printf '{}\n{}\n'; exit 0 ;;
    failure) printf 'tool failed\n' >&2; exit 8 ;;
    missing_index) printf '{"error_code":"INDEX_MISSING","summary":{"partial_success":true}}\n'; exit 3 ;;
    stderr_json) printf '{"error":{"kind":"missing-index","message":"Index first"}}\n' >&2; exit 3 ;;
    partial) printf '{"summary":{"partial_success":true},"results":[{"name":"kept"}]}\n'; exit 0 ;;
    no_matches) printf '[]\n'; exit 1 ;;
esac
printf 'diagnostic\n' >&2
if [[ "$tool" == ast-grep ]]; then
    printf '[{"file":"code.py","text":"greet(name)","range":{"start":{"line":4,"column":2}}}]\n'
else
    printf '{"results":[{"name":"greet","source_path":"session.jsonl","line_number":7}],"_meta":{"fresh":false}}\n'
fi
SH
for tool in roam ast-grep cass; do cp "$test_dir/bin/fixture" "$test_dir/bin/$tool"; chmod +x "$test_dir/bin/$tool"; done
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
source "$root/lib/trash.bash" 2>/dev/null
trap 'rm -rf "$test_dir"' EXIT
passed=0
check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; passed=$((passed+1)); else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
field() { jq -r "$2" <<< "$1"; }
logged() { jq -r "$1" "$CODE_TOOL_LOG"; }

check 'Roam version uses exact argv' 'roam test-version' "$(@ Tools::Roam version)"
check 'AstGrep version uses exact argv' 'ast-grep test-version' "$(@ Tools::AstGrep version)"
check 'Cass version uses exact argv' 'cass test-version' "$(@ Tools::Cass version)"

before=$PWD
export ROAM_DB_DIR='/incorrect/shared store' ROAM_NO_AUTO_INDEX=0
hostile=$'--kind $(touch unexpected-file); `echo oops` "quoted"\nnext line'
result=$(@ Tools::Roam search: "$hostile" inDirectory: "$test_dir/work repo")
check 'Roam result has native structured data' greet "$(field "$result" '.data.results[0].name')"
check 'Roam result keeps process stderr' diagnostic "$(field "$result" '.stderr | rtrimstr("\n")')"
check 'Roam picks the requested directory' "$test_dir/work repo" "$(logged .cwd)"
check 'caller directory is unchanged' "$before" "$PWD"
check 'Roam overrides a global DB location' .roam "$(logged .db)"
check 'Roam queries disable automatic indexing' 1 "$(logged .no_auto)"
check 'caller Roam environment is unchanged' '/incorrect/shared store' "$ROAM_DB_DIR"
check 'Roam query is one literal argument' "$hostile" "$(logged '.argv[-1]')"
check 'Roam protects option-looking symbols' -- "$(logged '.argv[-2]')"
check 'Roam global JSON flag precedes command' '["--json","--budget","8000","search"]' "$(jq -c '.argv[0:4]' "$CODE_TOOL_LOG")"
check 'query content was not evaluated' false "$([[ -e "$test_dir/work repo/unexpected-file" ]] && echo true || echo false)"

for selector in context impact; do
    result=$(@ Tools::Roam "$selector:" greet inDirectory: "$test_dir/work repo")
    check "Roam $selector selects its command" "$selector" "$(logged '.argv[3]')"
done
result=$(@ Tools::Roam indexInDirectory: "$test_dir/work repo")
check 'explicit indexing uses index, without init/hooks' index "$(logged '.argv[-1]')"
result=$(@ Tools::Roam statusInDirectory: "$test_dir/work repo")
check 'Roam status queries index statistics' index-stats "$(logged '.argv[-1]')"

export CODE_TOOL_SCENARIO=missing_index
result=$(@ Tools::Roam search: greet inDirectory: "$test_dir/work repo")
check 'missing Roam index is distinct from empty search' index_missing "$(field "$result" .outcome)"
check 'missing index preserves child status' 3 "$(field "$result" .exit_code)"
check 'missing index preserves recovery payload' INDEX_MISSING "$(field "$result" .data.error_code)"
export CODE_TOOL_SCENARIO=partial
result=$(@ Tools::Roam impact: greet inDirectory: "$test_dir/work repo")
check 'partial Roam output is not complete success' partial "$(field "$result" .outcome)"
check 'partial Roam output retains findings' kept "$(field "$result" '.data.results[0].name')"
unset CODE_TOOL_SCENARIO

pattern='greet($A)'
result=$(@ Tools::AstGrep search: "$pattern" language: python files: '["-option.py","a file.py"]' inDirectory: "$test_dir/work repo")
check 'ast-grep returns matches' 'greet(name)' "$(field "$result" '.data[0].text')"
check 'ast-grep preserves native zero-based coordinates' 4 "$(field "$result" '.data[0].range.start.line')"
check 'ast-grep keeps pattern literal' "$pattern" "$(logged '.argv[2]')"
check 'ast-grep protects exact filenames' '["--","-option.py","a file.py"]' "$(jq -c '.argv[-3:]' "$CODE_TOOL_LOG")"
check 'ast-grep uses the requested cwd' "$test_dir/work repo" "$(logged .cwd)"
check 'ast-grep has no edit or interactive flags' false "$(logged '.argv | any(. == "--rewrite" or . == "--update-all" or . == "--interactive")')"
code=$'def run():\n    return greet("two words")\n'
result=$(@ Tools::AstGrep search: "$pattern" language: python in: "$code")
check 'stdin mode is explicit' --stdin "$(logged '.argv[-1]')"
check 'source text is delivered through stdin' "${code%$'\n'}" "$(cat "$CODE_TOOL_STDIN")"
export CODE_TOOL_SCENARIO=no_matches
result=$(@ Tools::AstGrep search: "$pattern" language: python in: "$code")
check 'ast-grep exit one with empty array means no matches' no_matches "$(field "$result" .outcome)"
check 'no matches preserves original exit one' 1 "$(field "$result" .exit_code)"
check 'no matches still provides an array' '[]' "$(field "$result" .data)"
unset CODE_TOOL_SCENARIO

result=$(@ Tools::Cass search: "$hostile" inWorkspace: '/a worktree' agent: codex limit: 7 dataDirectory: '/search index')
check 'cass retains source references' session.jsonl "$(field "$result" '.data.results[0].source_path')"
check 'cass retains upstream freshness metadata' false "$(field "$result" .data._meta.fresh)"
check 'cass query is literal and option protected' "$hostile" "$(logged '.argv[-1]')"
check 'cass query terminates options' -- "$(logged '.argv[-2]')"
check 'cass workspace filter is explicit' '/a worktree' "$(logged '.argv as $a | $a[($a|index("--workspace"))+1]')"
check 'cass agent filter is explicit' codex "$(logged '.argv as $a | $a[($a|index("--agent"))+1]')"
check 'cass data directory is distinct from workspace' '/search index' "$(logged '.argv as $a | $a[($a|index("--data-dir"))+1]')"
check 'cass query is bounded' 7 "$(logged '.argv as $a | $a[($a|index("--limit"))+1]')"
check 'cass selects lexical search' lexical "$(logged '.argv as $a | $a[($a|index("--mode"))+1]')"
check 'cass search does not refresh or spawn a daemon' false "$(logged '.argv | any(. == "--refresh" or . == "--daemon" or . == "index")')"
result=$(@ Tools::Cass search: history)
check 'ordinary cass search has a finite default' 20 "$(logged '.argv as $a | $a[($a|index("--limit"))+1]')"
for invalid_limit in 0 -1 1001 '1+2' '1;touch nope'; do
    if @ Tools::Cass search: history inWorkspace: '' agent: '' limit: "$invalid_limit" dataDirectory: '' > /dev/null 2>&1; then
        printf 'FAIL: accepted invalid limit %s\n' "$invalid_limit"; exit 1
    fi
    passed=$((passed+1))
done
result=$(@ Tools::Cass statusInDataDirectory: '/search index')
check 'cass status requests freshness metadata' --robot-meta "$(logged '.argv[-1]')"
result=$(@ Tools::Cass indexInDataDirectory: '/search index')
check 'cass indexing is explicitly incremental' false "$(logged '.argv | any(. == "--full" or . == "--watch" or . == "--semantic")')"
result=$(@ Tools::Cass view: '-session file.jsonl' line: 12)
check 'cass view protects the source path' '["--","-session file.jsonl"]' "$(jq -c '.argv[-2:]' "$CODE_TOOL_LOG")"

for scenario in invalid scalar multiple; do
    export CODE_TOOL_SCENARIO=$scenario
    result=$(@ Tools::Cass search: history)
    check "$scenario JSON cannot pass as success" invalid_output "$(field "$result" .outcome)"
    check "$scenario JSON has no fabricated data" null "$(field "$result" .data)"
done
export CODE_TOOL_SCENARIO=failure
result=$(@ Tools::AstGrep search: bad language: python in: 'x')
check 'ast-grep errors are not empty matches' command_error "$(field "$result" .outcome)"
check 'process failure retains exit status' 8 "$(field "$result" .exit_code)"
check 'process failure retains diagnostics' 'tool failed' "$(field "$result" '.stderr | rtrimstr("\n")')"
unset CODE_TOOL_SCENARIO
export CODE_TOOL_SCENARIO=stderr_json
result=$(@ Tools::Cass search: history)
check 'cass decodes structured stderr failures' missing-index "$(field "$result" .data.error.kind)"
check 'cass distinguishes a missing index' index_missing "$(field "$result" .outcome)"
check 'cass preserves the original empty stdout' '' "$(field "$result" .stdout)"
unset CODE_TOOL_SCENARIO
result=$(@ Tools::Roam search: greet inDirectory: "$test_dir/absent")
check 'invalid cwd fails without running a tool' command_error "$(field "$result" .outcome)"
check 'invalid cwd uses dedicated child status' 72 "$(field "$result" .exit_code)"

# The shared process primitive supports cwd with stdin and rejects NUL paths.
argv=$(jq -cn --arg bash "$BASH" '[$bash,"-c","cat; printf cwd=%s \"$PWD\"; exit 23"]')
result=$(@ Tool captureArgvJson: "$argv" input: 'input text' inDirectory: "$test_dir/work repo")
check 'cwd capture preserves stdin and child status' 23 "$(field "$result" .exit_code)"
check 'cwd capture passes stdin exactly' "input textcwd=$test_dir/work repo" "$(field "$result" .stdout)"
odd_directory="$test_dir/"$'literal $(touch nope); `echo nope` "quoted"\n'
mkdir -p "$odd_directory"
argv=$(jq -cn --arg bash "$BASH" '[$bash,"-c","printf \"%s|end\" \"$PWD\""]')
result=$(@ Tool captureArgvJson: "$argv" inDirectory: "$odd_directory")
check 'cwd preserves metacharacters and a trailing newline' "$odd_directory|end" "$(field "$result" .stdout)"
bad_request='{"argv":["true"],"working_directory":"bad\u0000path","capture":true}'
if @ Tool runProcessRequestJson: "$bad_request" >/dev/null 2>&1; then echo 'FAIL: NUL cwd accepted'; exit 1; fi
passed=$((passed+1))

ln -s "$(command -v jq)" "$test_dir/deps/jq"
PATH="$test_dir/deps:/usr/bin:/bin"
hash -r
for class in Tools::Roam Tools::AstGrep Tools::Cass; do
    check "$class missing version" 'not installed' "$(@ "$class" version)"
done
result=$(@ Tools::Roam indexInDirectory: "$test_dir/work repo")
check 'missing Roam does not install automatically' missing_tool "$(field "$result" .outcome)"
check 'missing tool result has null data' null "$(field "$result" .data)"
result=$(@ Tools::AstGrep search: 'x' language: python in: 'x')
check 'missing ast-grep uses command-not-found status' 127 "$(field "$result" .exit_code)"
result=$(@ Tools::Cass search: history)
check 'missing cass carries installation guidance' true "$(field "$result" '.stderr | contains("cargo install")')"
PATH=$original_path
printf 'PASS: %s code and session tool checks\n' "$passed"
