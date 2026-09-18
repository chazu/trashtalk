#!/usr/bin/env bash
# Run a test in a disposable checkout. Tests may compile classes and mutate
# sources as well as SQLite state, so changing only the DB path is insufficient.
set -euo pipefail
export LC_ALL=C
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# Populate $1 with the checkout a test runs against: the tracked build inputs
# plus this compiler's warm caches. Cache entries are copied, never shared, so
# a test may rewrite them without touching the source checkout. A cold browser
# index in every parallel test would otherwise repeat the full parse.
prepare_checkout() {
    local target="$1" cache_version cache_kind cache
    local -a caches
    mkdir -p "$target"
    tar -C "$root" --exclude='.astcache' --exclude='.symbolcache' \
        --exclude='.buildcache' --exclude='*.db*' --exclude='trash/user' \
        --exclude='*.dylib' --exclude='*.so' -cf - lib trash tests bin schemas Makefile |
        tar -C "$target" -xf -
    cache_version=${TRASH_TEST_FINGERPRINT:-$(TRASHTALK_DIR="$root" bash "$root/lib/jq-compiler/driver.bash" fingerprint)}
    for cache_kind in .astcache .symbolcache; do
        mkdir -p "$target/trash/.compiled/$cache_kind"
        caches=()
        for cache in "$root/trash/.compiled/$cache_kind/"*"-$cache_version"*.json; do
            [[ -f "$cache" ]] && caches+=("$cache")
        done
        ((${#caches[@]} == 0)) || cp -- "${caches[@]}" "$target/trash/.compiled/$cache_kind/"
    done
}

# Duplicate a prepared checkout. APFS and reflink-capable filesystems clone the
# data instead of copying it; the result is still private to one test.
clone_checkout() {
    local base="$1" target="$2"
    if [[ "$OSTYPE" == darwin* ]]; then
        cp -cRp "$base" "$target"
    else
        cp -a --reflink=auto "$base" "$target" 2>/dev/null || cp -a "$base" "$target"
    fi
}

# The suite runner prepares one base checkout per run and exports it as
# TRASH_TEST_BASE; every test then clones it instead of repeating the tar.
if [[ "${1:-}" == --prepare-base ]]; then
    [[ $# -eq 2 ]] || { echo 'Usage: test-isolated.bash --prepare-base <dir>' >&2; exit 2; }
    prepare_checkout "$2"
    exit 0
fi

test_file=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
shift
case "$test_file" in
    "$root"/*) relative=${test_file#"$root"/} ;;
    *) echo "Test must be inside $root: $test_file" >&2; exit 2 ;;
esac
work=$(mktemp -d /tmp/trash-test.XXXXXX)
trap 'if [[ "${TRASH_TEST_KEEP:-0}" == 1 ]]; then echo "Test checkout: $work" >&2; else rm -rf "$work"; fi' EXIT
mkdir -p "$work/tmp"
if [[ -n "${TRASH_TEST_BASE:-}" && -d "$TRASH_TEST_BASE" ]]; then
    clone_checkout "$TRASH_TEST_BASE" "$work/repo"
else
    prepare_checkout "$work/repo"
fi
export TRASHTALK_TEST_ISOLATED=1 TRASHTALK_SKIP_USER_CONFIG=1 LC_ALL=C
export TRASHTALK_DIR="$work/repo" TRASHDIR="$work/repo/trash"
export SQLITE_JSON_DB="$work/instances.db" TMPDIR="$work/tmp"
unset TRASH_SESSION_ID TRASH_PROFILE TRASH_PROFILE_FILE
cd "$work/repo"
test_bash_args=()
[[ "${TRASH_TEST_TRACE:-0}" != 1 ]] || test_bash_args+=(-x)
bash "${test_bash_args[@]}" "$relative" "$@"
