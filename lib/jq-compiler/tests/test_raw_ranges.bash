#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cat > "$tmp/RawRanges.trash" <<'EOF'
RawRanges subclass: Object
  rawClassMethod: retries [
    local attempt count=0
    for attempt in {1..50}; do
      count=$((count + 1))
    done
    printf '%s %s\n' "$count" "$attempt"
  ]
  rawClassMethod: ranges [
    printf '%s\n' {3..1} {-2..2..2} {01..03} {a..e..2}
  ]
  rawClassMethod: literals [
    printf '%s\n' '{1..50}' "{1..50}" '{ 1 ..50 }'
    { printf '%s\n' block; }
  ]
EOF
"$root/lib/jq-compiler/driver.bash" compile "$tmp/RawRanges.trash" --check > "$tmp/compiled"
source "$tmp/compiled"
check() {
    [[ "$2" == "$3" ]] || { printf 'FAIL: %s expected=%s actual=%s\n' "$1" "$2" "$3"; exit 1; }
    printf 'PASS: %s\n' "$1"
}
check 'retry loop executes all fifty attempts' '50 50' "$(__RawRanges__class__retries)"
check 'raw ranges retain Bash sequence semantics' $'3\n2\n1\n-2\n0\n2\n01\n02\n03\na\nc\ne' "$(__RawRanges__class__ranges)"
check 'quoted ranges and shell blocks remain literal' $'{1..50}\n{1..50}\n{ 1 ..50 }\nblock' "$(__RawRanges__class__literals)"
