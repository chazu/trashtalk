#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# A successful build keeps the current compiler-cache generation plus the most
# recently used previous one, and removes staging leftovers older than ten
# minutes. Live entries, fresh staging files, and unrelated files survive.
set -euo pipefail
export LC_ALL=C
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
TEST_TMP=$(mktemp -d "$TMPDIR/prune-test.XXXXXX")
trap 'rm -rf "$TEST_TMP"' EXIT
export TRASHTALK_DIR="$TEST_TMP/root"
export TRASHTALK_COMPILED_DIR="$TRASHTALK_DIR/trash/.compiled"
mkdir -p "$TRASHTALK_DIR/trash" "$TRASHTALK_COMPILED_DIR/.buildcache"
cp -R "$ROOT/lib/jq-compiler" "$TEST_TMP/compiler"
driver="$TEST_TMP/compiler/driver.bash"
ast="$TRASHTALK_COMPILED_DIR/.astcache"
sym="$TRASHTALK_COMPILED_DIR/.symbolcache"
mkdir -p "$sym"
source_file="$TRASHTALK_DIR/trash/Alpha.trash"
printf 'Alpha subclass: Object\n  method: read [ ^ 1 ]\n' > "$source_file"
build() { "$driver" compile-many "$TRASHTALK_COMPILED_DIR" 1 "$source_file" > "$TEST_TMP/build.out"; }
fingerprint() { "$driver" fingerprint; }
generations() { ls "$ast" | sed -E 's/^[0-9a-f]{64}-([0-9a-f]{16})\.json$/\1/' | sort -u | tr '\n' ' '; }
hex64=$(printf 'a%.0s' {1..64})
digest=$(shasum -a 256 "$source_file" | cut -d' ' -f1)
old=202001010000

build
gen_a=$(fingerprint)
test "$(ls "$ast" | wc -l | tr -d ' ')" = 1
# Symbol cache entries share the source digest; fake the query hash only.
printf '[]' > "$sym/$digest-$gen_a-$hex64.json"
touch -t "$old" "$ast"/*"-$gen_a.json" "$sym/$digest-$gen_a-$hex64.json"

printf '\n# generation b\n' >> "$TEST_TMP/compiler/codegen.jq"
build
gen_b=$(fingerprint)
test "$gen_b" != "$gen_a"
case "$(generations)" in *"$gen_a"*"$gen_b"*|*"$gen_b"*"$gen_a"*) ;; *) echo "FAIL: previous generation was not retained: $(generations)"; exit 1 ;; esac
printf '[]' > "$sym/$digest-$gen_b-$hex64.json"

# Staging leftovers: an interrupted build leaves mktemp candidates and temp
# cache writes behind. Only stale ones (older than ten minutes) are removed.
stale_artifact="$TRASHTALK_COMPILED_DIR/Alpha.abcdef"
fresh_artifact="$TRASHTALK_COMPILED_DIR/Alpha.fresh1"
stale_receipt="$TRASHTALK_COMPILED_DIR/.buildcache/Alpha.json.Zz9Yy8"
stale_ast_tmp="$ast/$hex64-$gen_b.json.12345.tmp"
stale_symbol_tmp="$sym/$digest-$gen_b-$hex64.json.Q1w2E3"
unrelated="$TRASHTALK_COMPILED_DIR/notes.txt"
: > "$stale_artifact"; : > "$fresh_artifact"; : > "$stale_receipt"; : > "$stale_ast_tmp"; : > "$stale_symbol_tmp"
printf 'keep me\n' > "$unrelated"
touch -t "$old" "$stale_artifact" "$stale_receipt" "$stale_ast_tmp" "$stale_symbol_tmp" "$unrelated"

printf '\n# generation c\n' >> "$TEST_TMP/compiler/codegen.jq"
build
gen_c=$(fingerprint)
test "$gen_c" != "$gen_b"
gens=$(generations)
case "$gens" in *"$gen_a"*) echo "FAIL: oldest generation survived: $gens"; exit 1 ;; esac
case "$gens" in *"$gen_b"*) ;; *) echo "FAIL: previous generation was pruned: $gens"; exit 1 ;; esac
case "$gens" in *"$gen_c"*) ;; *) echo "FAIL: current generation is missing: $gens"; exit 1 ;; esac
test ! -e "$sym/$digest-$gen_a-$hex64.json"
test -e "$sym/$digest-$gen_b-$hex64.json"
test ! -e "$stale_artifact"
test ! -e "$stale_receipt"
test ! -e "$stale_ast_tmp"
test ! -e "$stale_symbol_tmp"
test -e "$fresh_artifact"
test -e "$unrelated"
test -s "$TRASHTALK_COMPILED_DIR/Alpha"
test -s "$TRASHTALK_COMPILED_DIR/.buildcache/Alpha.json"
echo 'PASS: build keeps current and previous cache generations and removes stale staging files'

# A warm build that compiles nothing still sweeps stale leftovers.
: > "$stale_artifact"
touch -t "$old" "$stale_artifact"
build
grep -q 'artifacts unchanged' "$TEST_TMP/build.out"
test ! -e "$stale_artifact"
test "$(generations)" = "$gens"
echo 'PASS: warm build sweeps leftovers without touching live generations'

# Editing a source adds an entry per content hash; only entries for the
# current sources survive a build, in either retained generation.
entries_for() { ls "$ast" | grep -c -- "-$1.json"; }
test "$(entries_for "$gen_c")" = 1
printf 'Alpha subclass: Object\n  method: read [ ^ 2 ]\n' > "$source_file"
build
test "$(entries_for "$gen_c")" = 1
test "$(entries_for "$gen_b")" = 0
printf 'Beta subclass: Object\n  method: read [ ^ 3 ]\n' > "$TRASHTALK_DIR/trash/Beta.trash"
"$driver" compile-many "$TRASHTALK_COMPILED_DIR" 1 "$source_file" "$TRASHTALK_DIR/trash/Beta.trash" > "$TEST_TMP/build.out"
test "$(entries_for "$gen_c")" = 2
ls "$ast" | grep -q "$(shasum -a 256 "$source_file" | cut -d' ' -f1)-$gen_c.json"
ls "$ast" | grep -q "$(shasum -a 256 "$TRASHTALK_DIR/trash/Beta.trash" | cut -d' ' -f1)-$gen_c.json"
echo 'PASS: entries for superseded source content are pruned'
