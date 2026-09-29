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
echo 'PASS: moved/deleted sources reconcile; live clashes fail without publication'
