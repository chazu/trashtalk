#!/usr/bin/env bash
# Run a test in a disposable checkout. Tests may compile classes and mutate
# sources as well as SQLite state, so changing only the DB path is insufficient.
set -euo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_file=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
shift
case "$test_file" in
    "$root"/*) relative=${test_file#"$root"/} ;;
    *) echo "Test must be inside $root: $test_file" >&2; exit 2 ;;
esac
work=$(mktemp -d /tmp/trash-test.XXXXXX)
trap 'if [[ "${TRASH_TEST_KEEP:-0}" == 1 ]]; then echo "Test checkout: $work" >&2; else rm -rf "$work"; fi' EXIT
mkdir -p "$work/repo" "$work/tmp"
tar -C "$root" --exclude='.astcache' --exclude='.symbolcache' \
    --exclude='.buildcache' --exclude='*.db*' --exclude='trash/user' \
    --exclude='*.dylib' --exclude='*.so' -cf - lib trash tests bin axe Makefile |
    tar -C "$work/repo" -xf -
# Copy only this compiler's warm caches, never share writable cache entries.
# A cold browser index in every parallel test otherwise repeats the full parse.
cache_version=$(TRASHTALK_DIR="$root" bash "$root/lib/jq-compiler/driver.bash" fingerprint)
for cache_kind in .astcache .symbolcache; do
    mkdir -p "$work/repo/trash/.compiled/$cache_kind"
    for cache in "$root/trash/.compiled/$cache_kind/"*"-$cache_version"*.json; do
        [[ -f "$cache" ]] || continue
        cp "$cache" "$work/repo/trash/.compiled/$cache_kind/"
    done
done
export TRASHTALK_TEST_ISOLATED=1 TRASHTALK_SKIP_USER_CONFIG=1 LC_ALL=C
export TRASHTALK_DIR="$work/repo" TRASHDIR="$work/repo/trash"
export SQLITE_JSON_DB="$work/instances.db" TMPDIR="$work/tmp"
unset TRASH_SESSION_ID TRASH_PROFILE TRASH_PROFILE_FILE
cd "$work/repo"
test_bash_args=()
[[ "${TRASH_TEST_TRACE:-0}" != 1 ]] || test_bash_args+=(-x)
bash "${test_bash_args[@]}" "$relative" "$@"
