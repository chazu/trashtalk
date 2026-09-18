#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_dir=$(mktemp -d)
original_path=$PATH
mkdir -p "$test_dir/bin" "$test_dir/repository"
trap 'rm -rf "$test_dir"' EXIT

cat > "$test_dir/bin/git" <<'SH'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$PWD" > "$GIT_CWD"
printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]' > "$GIT_ARGV"
[[ "${1:-}" == '--version' ]] && { printf 'git version test\n'; exit 0; }
printf 'fixture output\n'
SH
chmod +x "$test_dir/bin/git"
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
export GIT_CWD="$test_dir/cwd" GIT_ARGV="$test_dir/argv.json"
source "$root/lib/trash.bash" 2>/dev/null

check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }
check 'Git command name' git "$(@ Tools::Git name)"
check 'Git version uses the executable' 'git version test' "$(@ Tools::Git version)"

result=$(@ Tools::Git worktreeListInDirectory: "$test_dir/repository")
check 'Git worktree inspection preserves output' 'fixture output' "$(printf '%s' "$result" | jq -r '.stdout | rtrimstr("\n")')"
check 'Git worktree inspection uses explicit checkout' "$test_dir/repository" "$(cat "$GIT_CWD")"
check 'Git worktree inspection has fixed porcelain argv' '["worktree","list","--porcelain"]' "$(jq -c . "$GIT_ARGV")"

result=$(@ Tools::Git statusInDirectory: "$test_dir/repository")
check 'Git status uses fixed observational argv' '["status","--porcelain=v1","--branch","--untracked-files=normal"]' "$(jq -c . "$GIT_ARGV")"
result=$(@ Tools::Git headInDirectory: "$test_dir/repository")
check 'Git revision inspection is explicit' '["rev-parse","HEAD"]' "$(jq -c . "$GIT_ARGV")"

PATH="$original_path"
export PATH
