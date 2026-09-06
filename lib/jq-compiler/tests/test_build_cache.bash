#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
CACHE_ROOT=$(mktemp -d "$TMPDIR/cache-test.XXXXXX")
export TRASHTALK_DIR="$CACHE_ROOT/space root"
export TRASHTALK_COMPILED_DIR="$TRASHTALK_DIR/trash/.compiled"
mkdir -p "$TRASHTALK_DIR/trash/traits" "$TRASHTALK_COMPILED_DIR" "$CACHE_ROOT/bin"
cp -R "$ROOT/lib/jq-compiler" "$CACHE_ROOT/compiler"
driver="$CACHE_ROOT/compiler/driver.bash"
export CACHE_JQ=$(command -v jq) CACHE_CALLS="$CACHE_ROOT/calls"
cat > "$CACHE_ROOT/bin/jq" <<'SH'
#!/usr/bin/env bash
for arg in "$@"; do
    [[ "$arg" != */codegen.jq ]] || printf 'codegen\n' >> "$CACHE_CALLS"
done
exec "$CACHE_JQ" "$@"
SH
chmod +x "$CACHE_ROOT/bin/jq"
export PATH="$CACHE_ROOT/bin:$PATH"
base="$TRASHTALK_DIR/trash/Base.trash"
child="$TRASHTALK_DIR/trash/Child.trash"
trait="$TRASHTALK_DIR/trash/traits/Flavor.trash"
artifact="$TRASHTALK_COMPILED_DIR/Child"
printf 'Base subclass: Object\n  instanceVars: old:1\n' > "$base"
printf 'Flavor trait\n  method: flavor [ ^ "first" ]\n' > "$trait"
printf 'Child subclass: Base\n  include: Flavor\n  method: read [ ^ added ]\n' > "$child"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/cold"
test -s "$TRASHTALK_COMPILED_DIR/Base"
test -s "$TRASHTALK_COMPILED_DIR/traits/Flavor"
before=$(wc -l < "$CACHE_CALLS")
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/warm"
test "$(wc -l < "$CACHE_CALLS")" = "$before"
source "$TRASHTALK_COMPILED_DIR/traits/Flavor"
test "$(__Flavor__flavor)" = first
cp -p "$base" "$CACHE_ROOT/base-time"
printf 'Base subclass: Object\n  instanceVars: old:1 added:42\n' > "$base"
touch -r "$CACHE_ROOT/base-time" "$base"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/parent"
source "$artifact"
_ivar() { test "$1" = added; printf '42\n'; }
test "$(__Child__read)" = 42
printf 'Flavor trait\n  method: flavor [ ^ "second" ]\n' > "$trait"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/trait"
source "$TRASHTALK_COMPILED_DIR/traits/Flavor"
test "$(__Flavor__flavor)" = second
printf '{' > "$TRASHTALK_COMPILED_DIR/.buildcache/Child.json"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/receipt"
printf 'corrupt output\n' > "$artifact"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/repaired"
bash -n "$artifact"
before=$(wc -l < "$CACHE_CALLS")
printf '\n# compiler fingerprint test\n' >> "$CACHE_ROOT/compiler/codegen.jq"
"$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/compiler-change"
test "$(wc -l < "$CACHE_CALLS")" -gt "$before"
# Same-package parents resolve exactly as generated runtime metadata does.
mkdir -p "$TRASHTALK_DIR/trash/Example"
printf 'package: Example\nParent subclass: Object\n  instanceVars: added:42\n' > "$TRASHTALK_DIR/trash/Example/Parent.trash"
printf 'package: Example\nChild subclass: Parent\n  method: read [ ^ added ]\n' > "$TRASHTALK_DIR/trash/Example/Child.trash"
"$driver" compile-cached "$TRASHTALK_DIR/trash/Example/Child.trash" "$TRASHTALK_COMPILED_DIR/Example__Child" > "$CACHE_ROOT/namespace"
source "$TRASHTALK_COMPILED_DIR/Example__Child"
test "$__Example__Child__superclass" = 'Example::Parent'
test "$(__Example__Child__read)" = 42
cp "$artifact" "$CACHE_ROOT/good"
printf 'Not a class\n' > "$child"
if "$driver" compile-cached "$child" "$artifact" > "$CACHE_ROOT/invalid.out" 2> "$CACHE_ROOT/invalid.err"; then exit 1; fi
cmp "$artifact" "$CACHE_ROOT/good"
printf 'Child subclass: MissingParent\n' > "$child"
if "$driver" compile-cached "$child" "$artifact" > /dev/null 2> "$CACHE_ROOT/missing.err"; then exit 1; fi
cmp "$artifact" "$CACHE_ROOT/good"
printf 'Child subclass: Base\n' > "$child"
printf 'Base subclass: Child\n' > "$base"
if "$driver" compile-cached "$child" "$artifact" > /dev/null 2> "$CACHE_ROOT/cycle.err"; then exit 1; fi
rg -q 'Cyclic build dependency' "$CACHE_ROOT/cycle.err"
echo 'PASS: warm build skips codegen; parent, trait, compiler, corruption, failure, and cycle invalidation'

# One graph for multiple roots: build a shared parent once, skip every worker
# when warm, and keep hashing process count independent of node count.
export TRASHTALK_DIR="$CACHE_ROOT/batch"
export TRASHTALK_COMPILED_DIR="$TRASHTALK_DIR/trash/.compiled"
mkdir -p "$TRASHTALK_DIR/trash"
printf 'Shared subclass: Object\n  instanceVars: added:42\n' > "$TRASHTALK_DIR/trash/Shared.trash"
roots=()
for n in 1 2 3 4; do
    printf 'Leaf%s subclass: Shared\n  method: read [ ^ added ]\n' "$n" > "$TRASHTALK_DIR/trash/Leaf$n.trash"
    roots+=("$TRASHTALK_DIR/trash/Leaf$n.trash")
done
before=$(wc -l < "$CACHE_CALLS")
"$driver" compile-many "$TRASHTALK_COMPILED_DIR" 3 "${roots[@]}" > "$CACHE_ROOT/batch-cold"
test "$(($(wc -l < "$CACHE_CALLS") - before))" = 5
source "$TRASHTALK_COMPILED_DIR/Leaf4"
test "$(__Leaf4__read)" = 42
export CACHE_SHA=$(command -v shasum) CACHE_HASH_CALLS="$CACHE_ROOT/hash-calls"
cat > "$CACHE_ROOT/bin/shasum" <<'SHA'
#!/usr/bin/env bash
printf 'hash\n' >> "$CACHE_HASH_CALLS"
exec "$CACHE_SHA" "$@"
SHA
chmod +x "$CACHE_ROOT/bin/shasum"
before=$(wc -l < "$CACHE_CALLS")
"$driver" compile-many "$TRASHTALK_COMPILED_DIR" 3 "${roots[@]}" > "$CACHE_ROOT/batch-warm"
test "$(wc -l < "$CACHE_CALLS")" = "$before"
test "$(wc -l < "$CACHE_HASH_CALLS")" -le 3
rg -q '5 artifacts unchanged' "$CACHE_ROOT/batch-warm"
# Preserve mtimes to ensure a content edit invalidates every dependent.
cp -p "$TRASHTALK_DIR/trash/Shared.trash" "$CACHE_ROOT/shared-time"
printf 'Shared subclass: Object\n  instanceVars: added:99\n' > "$TRASHTALK_DIR/trash/Shared.trash"
touch -r "$CACHE_ROOT/shared-time" "$TRASHTALK_DIR/trash/Shared.trash"
"$driver" compile-many "$TRASHTALK_COMPILED_DIR" 3 "${roots[@]}" > "$CACHE_ROOT/batch-changed"
test "$(($(wc -l < "$CACHE_CALLS") - before))" = 5
# Corrupt metadata outside the requested closure does not block a single build.
printf 'not a class\n' > "$TRASHTALK_DIR/trash/Unrelated.trash"
"$driver" compile-cached "${roots[0]}" "$TRASHTALK_COMPILED_DIR/Leaf1" > "$CACHE_ROOT/batch-scoped"
echo 'PASS: batch roots share dependency work; warm hash count stays constant'
