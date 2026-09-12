#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Declared primitives: `primitive: selector calls: bash_function` generates a
# stub that forwards its arguments positionally to one Bash function.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-primitives.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/Bridge.trash" <<'TRASH'
Bridge subclass: Object
  # Class-side boundary to a shell function.
  classPrimitive: greet: name calls: bridge_greet
  classPrimitive: pair: a with: b calls: bridge_pair
  classPrimitive: ping calls: bridge_ping
  # Instance-side primitives see the runtime context like any raw method.
  primitive: whoami calls: bridge_receiver
  method: viaDsl: name [ ^ @ Bridge greet: name ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/Bridge.trash" > "$TRASHDIR/.compiled/Bridge"
bash -n "$TRASHDIR/.compiled/Bridge"
source "$ROOT/lib/trash.bash"
bridge_greet() { printf 'hello %s\n' "$1"; }
bridge_pair() { printf '%s+%s\n' "$1" "$2"; }
bridge_ping() { printf 'pong\n'; }
bridge_receiver() { printf '%s\n' "$_RECEIVER"; }
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
check 'hello ada' @ Bridge greet: ada
check 'hello a b' @ Bridge greet: 'a b'
check 'x+y' @ Bridge pair: x with: y
check pong @ Bridge ping
check 'hello dsl' @ Bridge viaDsl: dsl
id=$(@ Bridge new)
check "$id" @ "$id" whoami

# The stub is exactly one forwarding call.
rg -q '^  bridge_pair "\$1" "\$2"$' "$TRASHDIR/.compiled/Bridge" || { echo "FAIL: primitive stub shape" >&2; exit 1; }
rg -q '^  bridge_ping$' "$TRASHDIR/.compiled/Bridge" || { echo "FAIL: unary primitive stub shape" >&2; exit 1; }
echo "PASS: primitive stubs forward positionally"

# Declared primitives are listed like any method.
rg -q "__Bridge__declaredMethods=' greet_ pair_with_ ping whoami viaDsl_ '" "$TRASHDIR/.compiled/Bridge" || { echo "FAIL: primitives missing from declaredMethods" >&2; rg 'declaredMethods' "$TRASHDIR/.compiled/Bridge" >&2; exit 1; }
echo "PASS: primitives appear in declaredMethods"

# A primitive without calls: is a parse error, reported with the class.
printf 'BadBridge subclass: Object\n  classPrimitive: greet: name\n  method: ok [ ^ 1 ]\n' > "$scratch/BadBridge.trash"
if "$COMPILER_DIR/driver.bash" compile "$scratch/BadBridge.trash" >"$scratch/out" 2>"$scratch/error"; then
    if ! rg -q 'primitive' "$scratch/error"; then
        echo "FAIL: missing calls: was accepted silently" >&2; exit 1
    fi
fi
rg -q 'calls:' "$scratch/error" || { echo "FAIL: missing primitive diagnostic" >&2; cat "$scratch/error" >&2; exit 1; }
echo "PASS: primitive without calls: is diagnosed"
