#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
export LC_ALL=C
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
TEST_TMP=$(mktemp -d)
trap 'rm -rf "$TEST_TMP"' EXIT
export TRASHTALK_DIR="$TEST_TMP"
cp -R "$ROOT/lib/jq-compiler" "$TEST_TMP/compiler"
driver="$TEST_TMP/compiler/driver.bash"
first="$TEST_TMP/first source.trash"
second="$TEST_TMP/second"$'\n'"source.trash"
printf 'First subclass: Object\n  method: alpha [ ^ 1 ]\n' >"$first"
printf 'Second subclass: Object\n  method: beta [ ^ 2 ]\n' >"$second"
"$driver" symbols-many "$first" "$second" >"$TEST_TMP/cold"
"$driver" symbols-many "$first" "$second" >"$TEST_TMP/warm"
cmp "$TEST_TMP/cold" "$TEST_TMP/warm"
jq -se --arg first "$first" --arg second "$second" \
  'any(.[]; .class_name == "First" and .path == $first) and any(.[]; .class_name == "Second" and .path == $second)' "$TEST_TMP/warm"
cp -p "$first" "$TEST_TMP/mtime"
printf 'First subclass: Object\n  method: gamma [ ^ 1 ]\n' >"$first"
touch -r "$TEST_TMP/mtime" "$first"
"$driver" symbols-many "$first" "$second" >"$TEST_TMP/changed"
jq -se 'any(.[]; .selector == "gamma") and all(.[]; .selector != "alpha")' "$TEST_TMP/changed"
# Corrupt one cached JSON array; a warm query must repair it before publishing.
for cache in "$TEST_TMP/trash/.compiled/.symbolcache/"*.json; do
    printf '{' >"$cache"
done
"$driver" symbols-many "$first" "$second" >"$TEST_TMP/repaired"
cmp "$TEST_TMP/changed" "$TEST_TMP/repaired"
mv "$first" "$TEST_TMP/renamed.trash"
"$driver" symbols-many "$TEST_TMP/renamed.trash" >"$TEST_TMP/renamed"
jq -se --arg path "$TEST_TMP/renamed.trash" 'all(.[]; .class_name == "First" and .path == $path)' "$TEST_TMP/renamed"
printf '\n| .label += " query changed"\n' >>"$TEST_TMP/compiler/symbols.jq"
"$driver" symbols-many "$TEST_TMP/renamed.trash" >"$TEST_TMP/query"
jq -se 'all(.[]; .label | endswith(" query changed"))' "$TEST_TMP/query"
# Invalid new source must not silently return the last cached symbol list.
printf 'Not a valid class at all\n' >"$TEST_TMP/renamed.trash"
if "$driver" symbols-many "$TEST_TMP/renamed.trash" >"$TEST_TMP/invalid.out" 2>"$TEST_TMP/invalid.err"; then
    echo 'FAIL: invalid source used stale symbols'; exit 1
fi
test ! -s "$TEST_TMP/invalid.out"
echo 'PASS: symbol cache content, path, corruption, query, and error contracts'
