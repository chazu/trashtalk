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

cat > "$test_dir/bin/wt" <<'SH'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$PWD" > "$WORKTRUNK_CWD"
printf '%s\0' "$@" | jq -Rs 'split("\u0000")[:-1]' > "$WORKTRUNK_ARGV"
[[ "${1:-}" == '--version' ]] && { printf 'wt 0.test\n'; exit 0; }
printf 'feature/agent ready\n'
SH
chmod +x "$test_dir/bin/wt"
export PATH="$test_dir/bin:$PATH" SQLITE_JSON_DB="$test_dir/instances.db"
export WORKTRUNK_CWD="$test_dir/cwd" WORKTRUNK_ARGV="$test_dir/argv.json"
source "$root/lib/trash.bash" 2>/dev/null

check() { if [[ "$2" == "$3" ]]; then printf 'PASS: %s\n' "$1"; else printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; fi; }

check 'Worktrunk command name' wt "$(@ Tools::Worktrunk name)"
check 'Worktrunk installation guidance' 'brew install worktrunk' "$(@ Tools::Worktrunk installCommand)"
check 'Worktrunk version uses the executable' 'wt 0.test' "$(@ Tools::Worktrunk version)"
result=$(@ Tools::Worktrunk listInDirectory: "$test_dir/repository")
check 'Worktrunk list preserves child output' 'feature/agent ready' "$(printf '%s' "$result" | jq -r '.stdout | rtrimstr("\n")')"
check 'Worktrunk list uses explicit checkout directory' "$test_dir/repository" "$(cat "$WORKTRUNK_CWD")"
check 'Worktrunk list uses a fixed read-only argv' '["list"]' "$(jq -c . "$WORKTRUNK_ARGV")"

PATH="$original_path"
export PATH
