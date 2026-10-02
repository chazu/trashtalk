#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$TRASHTALK_DIR
driver="$ROOT/lib/jq-compiler/driver.bash"
export TRASHTALK_DIR=$(mktemp -d "$TMPDIR/build-moves.XXXXXX")
TRASHTALK_DIR=$(cd "$TRASHTALK_DIR" && pwd -P)
export TRASHTALK_COMPILED_DIR="$TRASHTALK_DIR/trash/.compiled"
mkdir -p "$TRASHTALK_DIR/trash/user"
printf 'Object subclass: nil\n' > "$TRASHTALK_DIR/trash/Object.trash"
printf 'Alpha subclass: Object\n  method: value [ ^ 1 ]\n' > "$TRASHTALK_DIR/trash/Alpha.trash"
build() { bash "$driver" compile-many "$TRASHTALK_COMPILED_DIR" 2 "$@"; }
build "$TRASHTALK_DIR/trash/Alpha.trash" >/dev/null
mv "$TRASHTALK_DIR/trash/Alpha.trash" "$TRASHTALK_DIR/trash/user/Alpha.trash"
build "$TRASHTALK_DIR/trash/user/Alpha.trash" >/dev/null
manifest="$TRASHTALK_COMPILED_DIR/.protocol-manifest.json"
jq -e --arg source "$TRASHTALK_DIR/trash/user/Alpha.trash" '.entries.Alpha.source==$source' "$manifest" >/dev/null
cp "$TRASHTALK_DIR/trash/user/Alpha.trash" "$TRASHTALK_DIR/trash/Alpha.trash"
cp "$manifest" "$TMPDIR/manifest-before"
if build "$TRASHTALK_DIR/trash/Alpha.trash" > "$TMPDIR/shadow.log" 2>&1; then echo 'FAIL: live identity shadow accepted'; exit 1; fi
cmp "$manifest" "$TMPDIR/manifest-before"
rm "$TRASHTALK_DIR/trash/Alpha.trash" "$TRASHTALK_DIR/trash/user/Alpha.trash"
build "$TRASHTALK_DIR/trash/Object.trash" >/dev/null
jq -e '.entries | has("Alpha") | not' "$manifest" >/dev/null
[[ ! -e "$TRASHTALK_COMPILED_DIR/Alpha" ]]
# Replacing a deleted source remains legal, even after its entry was retired.
printf 'Alpha subclass: Object\n  method: value [ ^ 2 ]\n' > "$TRASHTALK_DIR/trash/Alpha.trash"
build "$TRASHTALK_DIR/trash/Alpha.trash" >/dev/null
source "$TRASHTALK_COMPILED_DIR/Alpha"
[[ "$(__Alpha__value)" == 2 ]]
# Two sources declaring one class in a single build name the class and both files.
printf 'Gamma subclass: Object\n' | tee "$TRASHTALK_DIR/trash/Gamma.trash" > "$TRASHTALK_DIR/trash/user/Gamma.trash"
if build "$TRASHTALK_DIR/trash/Gamma.trash" "$TRASHTALK_DIR/trash/user/Gamma.trash" > "$TMPDIR/ambiguous.log" 2>&1; then
    echo 'FAIL: duplicate declared identity accepted'; exit 1
fi
grep -q "Gamma (.*trash/Gamma.trash.*trash/user/Gamma.trash)" "$TMPDIR/ambiguous.log" ||
    { echo 'FAIL: duplicate identity error does not name the class and sources'; cat "$TMPDIR/ambiguous.log"; exit 1; }
rm "$TRASHTALK_DIR/trash/Gamma.trash" "$TRASHTALK_DIR/trash/user/Gamma.trash"
echo 'PASS: moved/deleted sources reconcile; live clashes fail without publication'

# A full build removes generated artifacts no manifest entry owns once their
# source is gone (e.g. output from before manifest tracking). Other files and
# artifacts whose source still exists survive, and partial builds prune nothing.
mkdir -p "$TRASHTALK_COMPILED_DIR/traits" "$TRASHTALK_DIR/trash/Pkg"
printf 'Beta subclass: Object\n  method: value [ ^ 3 ]\n' > "$TRASHTALK_DIR/trash/Pkg/Beta.trash"
for orphan in Ghost Pkg__Ghost traits/Ghost Pkg__Beta; do
    cp "$TRASHTALK_COMPILED_DIR/Alpha" "$TRASHTALK_COMPILED_DIR/$orphan"
done
printf 'hand written\n' > "$TRASHTALK_COMPILED_DIR/notes"
build "$TRASHTALK_DIR/trash/Alpha.trash" >/dev/null
[[ -e "$TRASHTALK_COMPILED_DIR/Ghost" ]] || { echo 'FAIL: partial build pruned an orphan'; exit 1; }
TRASH_BUILD_PRUNE_ORPHANS=1 build "$TRASHTALK_DIR/trash/Alpha.trash" > "$TMPDIR/prune.log"
for orphan in Ghost Pkg__Ghost traits/Ghost; do
    [[ ! -e "$TRASHTALK_COMPILED_DIR/$orphan" ]] || { echo "FAIL: orphan $orphan survived"; exit 1; }
    grep -qx "  - removed orphaned $orphan" "$TMPDIR/prune.log"
done
[[ -e "$TRASHTALK_COMPILED_DIR/Pkg__Beta" && -e "$TRASHTALK_COMPILED_DIR/notes" && -e "$TRASHTALK_COMPILED_DIR/Alpha" ]]
echo 'PASS: full builds prune unowned artifacts of deleted sources'
