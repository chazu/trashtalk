#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# String intrinsics: messages on an implicit receiver that lower to parameter
# expansion or [[ ]] tests. Compile real source, then exercise public sends.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-string-intrinsics.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/StringFixture.trash" <<'TRASH'
StringFixture subclass: Object
  instanceVars: label:'alpha:beta'
  method: prefix: s [ ^ s startsWith: 'ab' ]
  method: suffix: s [ ^ s endsWith: 'yz' ]
  method: has: s [ ^ s includes: '*' ]
  method: dropPrefix: s [ ^ s withoutPrefix: 'a*' ]
  method: dropSuffix: s [ ^ s withoutSuffix: '.txt' ]
  method: head: s [ ^ s upTo: ':' ]
  method: tail: s [ ^ s after: ':' ]
  method: lastSegment: s [ ^ s afterLast: '/' ]
  method: dirname: s [ ^ s upToLast: '/' ]
  method: dashed: s [ ^ s replaceAll: '.' with: '-' ]
  method: dashedOnce: s [ ^ s replaceFirst: '.' with: '-' ]
  method: length: s [ ^ s size ]
  method: shout: s [ ^ s asUppercase ]
  method: hush: s [ ^ s asLowercase ]
  method: middle: s [ ^ s copyFrom: 2 to: 4 ]
  method: firstTwo: s [ ^ s first: 2 ]
  method: lastTwo: s [ ^ s last: 2 ]
  method: clean: s [ ^ s trimmed ]
  method: lines: s [ ^ s lines ]
  method: firstLine: s [ ^ s firstLine ]
  method: labelHead [ ^ label upTo: ':' ]
  method: long: s [ (s size > 3) ifTrue: [^ 'long']. ^ 'short' ]
  method: guard: s [ (s startsWith: 'x') ifTrue: [^ 'x-prefixed']. ^ 'plain' ]
  method: assigned: s [ | t | t := s withoutSuffix: '!'. ^ t , '?' ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/StringFixture.trash" > "$TRASHDIR/.compiled/StringFixture"
bash -n "$TRASHDIR/.compiled/StringFixture"
source "$ROOT/lib/trash.bash"
check() {
    local expected="$1" actual; shift
    actual=$("$@") || { echo "FAIL: $* exited nonzero" >&2; exit 1; }
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
id=$(@ StringFixture new)
check true @ "$id" prefix: abc
check false @ "$id" prefix: cab
check true @ "$id" suffix: xyz
check true @ "$id" has: 'a*b'
check false @ "$id" has: 'aXb'
# Patterns are literal: a glob character in the argument never matches as a glob.
check 'a*bcd' @ "$id" dropPrefix: 'a*a*bcd'
check 'abcd' @ "$id" dropPrefix: 'abcd'
check 'notes' @ "$id" dropSuffix: 'notes.txt'
check 'session' @ "$id" head: 'session:abc:def'
check 'abc:def' @ "$id" tail: 'session:abc:def'
check 'file.txt' @ "$id" lastSegment: '/a/b/file.txt'
check '/a/b' @ "$id" dirname: '/a/b/file.txt'
check 'a-b-c' @ "$id" dashed: 'a.b.c'
check 'a-b.c' @ "$id" dashedOnce: 'a.b.c'
check 5 @ "$id" length: 'hello'
check HELLO @ "$id" shout: hello
check hello @ "$id" hush: HeLLo
check bcd @ "$id" middle: abcdef
check ab @ "$id" firstTwo: abcdef
check ef @ "$id" lastTwo: abcdef
check 'hi there' @ "$id" clean: '   hi there  '
check '["a","b","c"]' @ "$id" lines: $'a\nb\n\nc'
check 'first' @ "$id" firstLine: $'first\nsecond'
check alpha @ "$id" labelHead
check long @ "$id" long: abcd
check short @ "$id" long: abc
check x-prefixed @ "$id" guard: xray
check plain @ "$id" guard: ray
check 'hey?' @ "$id" assigned: 'hey!'

# Local receivers expand in place with no capture or dispatch.
if rg -q 'withoutSuffix|@ String' "$TRASHDIR/.compiled/StringFixture"; then
    echo "FAIL: intrinsic still dispatches" >&2; exit 1
fi
rg -q '\$\{s%'"'"'\.txt'"'"'\}' "$TRASHDIR/.compiled/StringFixture" || { echo "FAIL: withoutSuffix: did not lower to parameter expansion" >&2; exit 1; }
echo "PASS: intrinsics lower to parameter expansion"

# String class methods keep their public contract on top of the intrinsics.
check 'bc' @ String trimPrefix: 'a*' from: 'a*bc'
check 'true' @ String startsWith: 'hello' prefix: 'he'
check 'cd' @ String substring: 'abcdef' from: 2 length: 2
check 6 @ String length: 'abcdef'
check 'ef' @ String suffix: 'abcdef' length: 2
if @ String suffix: 'abc' length: x 2>/dev/null; then
    echo "FAIL: suffix:length: accepted a non-numeric length" >&2; exit 1
fi
echo "PASS: suffix:length: rejects a non-numeric length"

# An unknown message on an implicit receiver is a compile error, not shell text.
printf 'BadMessage subclass: Object\n  method: run: path [\n    | base |\n    base := path frobnicate: 1.\n    ^ base\n  ]\n' > "$scratch/BadMessage.trash"
if "$COMPILER_DIR/driver.bash" compile "$scratch/BadMessage.trash" >"$scratch/out" 2>"$scratch/error"; then
    echo "FAIL: accepted an unknown implicit-receiver message" >&2; exit 1
fi
rg -q "Unknown message 'frobnicate:'" "$scratch/error" || { echo "FAIL: missing unknown-message diagnostic" >&2; cat "$scratch/error" >&2; exit 1; }
echo "PASS: unknown implicit-receiver message is rejected at compile time"
