#!/usr/bin/env bash
# Opt-in compatibility check: installs nothing and indexes only a disposable
# code repository. Cass never ingests the operator's session history here.
if [[ "${TRASHTALK_CODE_TOOLS_LIVE:-}" != 1 ]]; then
    printf 'SKIP: set TRASHTALK_CODE_TOOLS_LIVE=1 with roam, ast-grep, and cass on PATH\n'
    exit 0
fi
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
export LC_ALL=C
for dependency in roam ast-grep cass; do
    command -v "$dependency" >/dev/null || { printf 'Missing dependency: %s\n' "$dependency" >&2; exit 1; }
done
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
test_dir=$(cd "$test_dir" && pwd -P)
trap 'rm -rf "$test_dir"' EXIT
repo="$test_dir/main repo"
worktree="$test_dir/linked copy"
mkdir -p "$repo" "$test_dir/cass store"
export SQLITE_JSON_DB="$test_dir/instances.db"
source "$root/lib/trash.bash" 2>/dev/null
passed=0
check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; passed=$((passed+1)); else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
field() { jq -r "$2" <<< "$1"; }

git -C "$repo" init -q || exit 1
cat > "$repo/sample.py" <<'PY'
def greet(name):
    return "hello " + name

def run():
    return greet("world")
PY
git -C "$repo" add sample.py || exit 1
git -C "$repo" -c user.name='Trashtalk fixture' -c user.email='fixture@example.invalid' \
    -c core.hooksPath=/dev/null -c commit.gpgsign=false commit -qm fixture || exit 1
git -C "$repo" -c core.hooksPath=/dev/null worktree add -q --detach "$worktree" || exit 1
cat > "$worktree/sample.py" <<'PY'
def farewell(name):
    return "goodbye " + name

def run():
    return farewell("world")
PY

before=$PWD
export ROAM_DB_DIR="$test_dir/wrong shared index" ROAM_NO_AUTO_INDEX=0
result=$(@ Tools::Roam search: greet inDirectory: "$repo")
check 'native Roam refuses an absent index' index_missing "$(field "$result" .outcome)"
check 'native Roam preserves missing-index payload' INDEX_MISSING "$(field "$result" .data.error_code)"
check 'query did not create the index' false "$([[ -f "$repo/.roam/index.db" ]] && echo true || echo false)"
result=$(@ Tools::Roam indexInDirectory: "$repo")
check 'native Roam indexes a checkout explicitly' success "$(field "$result" .outcome)"
result=$(@ Tools::Roam search: greet inDirectory: "$repo")
check 'native Roam returns the definition' greet "$(field "$result" '.data.results[0].name')"
check 'native Roam retains source locations' sample.py:1 "$(field "$result" '.data.results[0].location')"
result=$(@ Tools::Roam context: greet inDirectory: "$repo")
check 'native Roam context resolves a symbol' greet "$(field "$result" .data.symbol)"
check 'native Roam context includes a caller' true "$(field "$result" '.data.callers | length > 0')"
result=$(@ Tools::Roam impact: greet inDirectory: "$repo")
check 'native Roam impact succeeds' success "$(field "$result" .outcome)"
result=$(@ Tools::Roam statusInDirectory: "$repo")
db_path=$(field "$result" .data.db_path)
[[ "$db_path" == /* ]] || db_path="$repo/$db_path"
check 'native Roam statistics refer to the checkout-local index' "$repo/.roam/index.db" "$db_path"
result=$(@ Tools::Roam indexInDirectory: "$worktree")
check 'native Roam indexes a linked worktree' success "$(field "$result" .outcome)"
result=$(@ Tools::Roam search: farewell inDirectory: "$worktree")
check 'linked worktree sees its dirty content' farewell "$(field "$result" '.data.results[0].name')"
result=$(@ Tools::Roam search: farewell inDirectory: "$repo")
check 'main checkout excludes the other worktree content' 0 "$(field "$result" '.data.results | length')"
result=$(@ Tools::Roam search: greet inDirectory: "$worktree")
check 'linked worktree excludes the main checkout content' 0 "$(field "$result" '.data.results | length')"
result=$(@ Tools::Roam statusInDirectory: "$worktree")
db_path=$(field "$result" .data.db_path)
[[ "$db_path" == /* ]] || db_path="$worktree/$db_path"
check 'linked worktree has its own index file' "$worktree/.roam/index.db" "$db_path"
check 'both index files exist independently' true "$([[ -f "$repo/.roam/index.db" && -f "$worktree/.roam/index.db" && ! "$repo/.roam/index.db" -ef "$worktree/.roam/index.db" ]] && echo true || echo false)"
check 'native queries preserve caller cwd' "$before" "$PWD"
check 'native queries preserve caller configuration' "$test_dir/wrong shared index" "$ROAM_DB_DIR"
check 'native queries did not create the inherited shared store' false "$([[ -e "$ROAM_DB_DIR" ]] && echo true || echo false)"

result=$(@ Tools::AstGrep search: 'greet($A)' language: python inDirectory: "$repo")
check 'native ast-grep matches a call' 'greet("world")' "$(field "$result" '.data[0].text')"
check 'native ast-grep keeps zero-based source lines' 4 "$(field "$result" '.data[0].range.start.line')"
result=$(@ Tools::AstGrep search: 'greet($A)' language: python in: 'greet("stdin")')
check 'native ast-grep accepts source on stdin' 'greet("stdin")' "$(field "$result" '.data[0].text')"
result=$(@ Tools::AstGrep search: 'nonexistent($A)' language: python inDirectory: "$repo")
check 'native empty search is distinct from failure' no_matches "$(field "$result" .outcome)"
check 'native empty search preserves exit one' 1 "$(field "$result" .exit_code)"
result=$(@ Tools::AstGrep search: 'greet($A)' language: not-a-real-language in: 'greet("stdin")')
check 'native invalid language is a command error' command_error "$(field "$result" .outcome)"

# An alternate data directory does not restrict cass source discovery. Check
# status, missing-index search, and source view without requesting indexing.
result=$(@ Tools::Cass statusInDataDirectory: "$test_dir/cass store")
check 'native cass status succeeds for an empty store' success "$(field "$result" .outcome)"
check 'native cass reports the uninitialized store' false "$(field "$result" .data.healthy)"
result=$(@ Tools::Cass search: fixture inWorkspace: "$repo" agent: '' limit: 3 dataDirectory: "$test_dir/cass store")
check 'native cass refuses search before indexing' index_missing "$(field "$result" .outcome)"
check 'native cass stderr JSON becomes structured error data' missing-index "$(field "$result" .data.error.kind)"
check 'native cass still preserves stderr' true "$(field "$result" '.stderr | contains("missing-index")')"
cat > "$test_dir/session.jsonl" <<'JSONL'
{"role":"user","content":"How does greet work?"}
{"role":"assistant","content":"It returns a greeting."}
JSONL
result=$(@ Tools::Cass view: "$test_dir/session.jsonl" line: 1)
check 'native cass can follow a source reference' success "$(field "$result" .outcome)"
check 'native cass retains the requested source line' 1 "$(field "$result" .data.target_line)"
check 'native cass returns the source content' true "$(field "$result" '.data.lines[0].content | contains("How does greet work?")')"

if command -v chad >/dev/null; then
    result=$(@ Tools::Chad levers)
    check 'native Chad metadata succeeds without loading a model' success "$(field "$result" .outcome)"
    check 'native Chad metadata is structured' object "$(field "$result" '.data | type')"
fi
printf '\n%d live code/session Tool checks passed\n' "$passed"
