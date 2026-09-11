#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Compile actual source with the production compiler, then test public sends.
# A compiler failure or any failed assertion must fail the test process.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-expr-codegen.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
cat > "$scratch/ExpressionFixture.trash" <<'TRASH'
ExpressionFixture subclass: Object
  instanceVars: value:10 step:2
  method: localAssignment [ | x | x := 5. ^ x ]
  method: readIvar [ | result | result := value. ^ result ]
  method: arithmetic [ ^ value + step ]
  method: assignedArithmetic [ | result | result := value + step. ^ result ]
  method: echo: result [ ^ result ]
  method: readValue [ ^ value ]
  method: increment [ value := value + 5. ^ value ]
  method: shadow [ | value | value := 99. ^ value ]
  method: symbol [ ^ #ready ]
  method: array [ ^ #(1 'two' #three) asJson ]
  method: dictionary [ ^ #{name: 'Ada' age: 37} asJson ]
  method: sendNegative [ ^ @ self echo: -1 ]
  method: choose [ ^ 'unary' ]
  method: choose: arg [ ^ arg ]
  method: invoke: block [ ^ @ block value ]
  method: customReturn [
    | block result |
    block := [^ 'block'].
    result := @ self invoke: block.
    ^ result , ':caller'
  ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/ExpressionFixture.trash" > "$TRASHDIR/.compiled/ExpressionFixture"
bash -n "$TRASHDIR/.compiled/ExpressionFixture"
source "$ROOT/lib/trash.bash"
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
id=$(@ ExpressionFixture new)
check 5 @ "$id" localAssignment
check 10 @ "$id" readIvar
check 12 @ "$id" arithmetic
check 12 @ "$id" assignedArithmetic
check 'a b' @ "$id" echo: 'a b'
check 10 @ "$id" readValue
check 15 @ "$id" increment
check 99 @ "$id" shadow
check 15 @ "$id" readValue
check ready @ "$id" symbol
check '[1,"two","three"]' @ "$id" array
check '{"name":"Ada","age":37}' @ "$id" dictionary
check -1 @ "$id" sendNegative
check unary @ "$id" choose
check keyword @ "$id" choose: keyword
check block:caller @ "$id" customReturn

# Backend pragmas must not silently change formerly native-only semantics.
for pragma in procyonOnly procyonNative bashOnly; do
    printf 'RetiredPragma subclass: Object\n  method: run [\n    pragma: %s\n    ^ 1\n  ]\n' "$pragma" > "$scratch/RetiredPragma.trash"
    if "$COMPILER_DIR/driver.bash" compile "$scratch/RetiredPragma.trash" >"$scratch/out" 2>"$scratch/error"; then
        echo "FAIL: accepted retired pragma $pragma" >&2; exit 1
    fi
    if ! rg -q 'Retired backend pragma' "$scratch/error"; then
        echo "FAIL: missing migration diagnostic for $pragma" >&2
        cat "$scratch/error" >&2
        exit 1
    fi
    echo "PASS: rejects $pragma with a migration diagnostic"
done
