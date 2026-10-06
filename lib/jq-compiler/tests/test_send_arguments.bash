#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
# Each send reaches `@` with the words the source wrote: a parenthesized send
# passes its output as one argument, keywords keep their spelling, cascades
# repeat the receiver, and returned text is never split or globbed.
cat > "$tmp/SendArgs.trash" <<'EOF'
SendArgs subclass: Object
  classMethod: nested: x [
    ^ @ Target put: (@ Target key) value: x
  ]
  classMethod: assigned: x [
    | r |
    r := @ Target put: (@ Target key) value: x.
    ^ r
  ]
  classMethod: underscored: x [
    ^ @ Target _load: x from: 'y'
  ]
  classMethod: joined [
    ^ @ Target foo_bar: 1
  ]
  classMethod: cascadeOn: x [
    ^ @ x at: 1 put: 'a b'; at: 2 put: (@ Target key); size
  ]
  classMethod: effects: x [
    @ x first: 1; second.
    ^ 'done'
  ]
  classMethod: streamed: x [
    pragma: stream
    @ x first: 1; second
  ]
  classMethod: words [
    ^ "a   b *"
  ]
  classMethod: me [
    ^ self
  ]
EOF
"$root/lib/jq-compiler/driver.bash" compile "$tmp/SendArgs.trash" --check > "$tmp/compiled"
source "$tmp/compiled"
# Record each send's argv in a log and as output; `@ Target key` answers
# text with a double space.
@() {
    if [[ "$1" == Target && "$2" == key ]]; then printf 'k  1\n'; return; fi
    printf '<%s>' "$@" | tee -a "$tmp/sends"; printf '\n' | tee -a "$tmp/sends"
}
check() {
    [[ "$2" == "$3" ]] || { printf 'FAIL: %s\n  expected=%s\n  actual=  %s\n' "$1" "$2" "$3"; exit 1; }
    printf 'PASS: %s\n' "$1"
}
cd "$tmp"
check 'a parenthesized send is one evaluated argument' \
    '<Target><put:><k  1><value:><v w>' "$(__SendArgs__class__nested_ 'v w')"
check 'the nested send is evaluated in an assignment too' \
    '<Target><put:><k  1><value:><v>' "$(__SendArgs__class__assigned_ v)"
check 'a keyword beginning with _ keeps its spelling' \
    '<Target><_load:><v><from:><y>' "$(__SendArgs__class__underscored_ v)"
check 'a keyword containing _ is one keyword' \
    '<Target><foo_bar:><1>' "$(__SendArgs__class__joined)"
check 'a keyword cascade answers its last message' \
    '<obj id><size>' "$(__SendArgs__class__cascadeOn_ 'obj id')"
rm -f "$tmp/sends"; __SendArgs__class__cascadeOn_ obj >/dev/null
check 'every cascade message reaches the receiver' \
    $'<obj><at:><1><put:><a b>\n<obj><at:><2><put:><k  1>\n<obj><size>' "$(cat "$tmp/sends")"
check 'a cascade made for its effect prints nothing' 'done' "$(__SendArgs__class__effects_ obj)"
check 'pragma: stream keeps every cascade message' $'<obj><first:><1>\n<obj><second>' \
    "$(__SendArgs__class__streamed_ obj)"
touch glob-bait
check 'returned text keeps its spaces and is not globbed' 'a   b *' "$(__SendArgs__class__words)"
check 'self is returned as one word' 'id with space' "$(_RECEIVER='id with space' __SendArgs__class__me)"
