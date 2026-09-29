#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
cat > "$TMPDIR/ExpressionProbe.trash" <<'TRASH'
ExpressionProbe subclass: Object
  classMethod: both: a with: b [ (a > 0) and: [b > 0] ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: either: a with: b [ (a > 0) or: [b > 0] ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: notBoth: a with: b [ (a > 0) and: [b > 0] ifFalse: [^ 'yes']. ^ 'no' ]
  classMethod: notEither: a with: b [ (a > 0) or: [b > 0] ifFalse: [^ 'yes']. ^ 'no' ]
  classMethod: grouped: a with: b [ ((a > 0) or: [b > 0]) and: [b > 0] ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: negateGroup: a with: b [ ((a > 0) and: [b > 0]) not ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: inverse: n [ (n > 0) not ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: double: n [ (n > 0) not not ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: equal: a to: b [ (a = b) ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: different: a from: b [ (a ~= b) ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: email: s [ (s matches: '^[^@]+@[^@]+$') ifTrue: [^ 'yes']. ^ 'no' ]
  classMethod: file: path [ ^ path fileExists ]
  classMethod: empty: s [ ^ s isEmpty ]
  classMethod: mixed: path number: n [ (path fileExists) and: [n > 0] ifTrue: [^ 'yes']. ^ 'no' ]
TRASH
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TMPDIR/ExpressionProbe.trash" > "$TRASHDIR/.compiled/ExpressionProbe"
check() { local want=$1; shift; local got; got=$(@ ExpressionProbe "$@"); [[ "$got" == "$want" ]] || { printf 'FAIL %s: expected %s, got %s\n' "$*" "$want" "$got"; exit 1; }; }
for a in 0 1; do for b in 0 1; do
    both=no; either=no; notBoth=yes; notEither=yes
    if ((a && b)); then both=yes; notBoth=no; fi
    if ((a || b)); then either=yes; notEither=no; fi
    check "$both" both: "$a" with: "$b"
    check "$either" either: "$a" with: "$b"
    check "$notBoth" notBoth: "$a" with: "$b"
    check "$notEither" notEither: "$a" with: "$b"
    check "$notBoth" negateGroup: "$a" with: "$b"
    grouped=no; if ((b)); then grouped=yes; fi
    check "$grouped" grouped: "$a" with: "$b"
done; done
check yes inverse: 0; check no inverse: 1
check no double: 0; check yes double: 1
check yes equal: 'x y' to: 'x y'; check no equal: x to: y
check no different: x from: x; check yes different: x from: y
check yes email: a@b; check no email: invalid
check true file: "$TMPDIR/ExpressionProbe.trash"; check false file: "$TMPDIR/missing"
check true empty: ''; check false empty: text
check yes mixed: "$TMPDIR/ExpressionProbe.trash" number: 1
check no mixed: "$TMPDIR/ExpressionProbe.trash" number: 0
check no mixed: "$TMPDIR/missing" number: 1
[[ "$(@ String concat: 'Hello ' with: World)" == 'Hello World' ]]
echo 'PASS: compiled boolean truth tables, negation, strings, regex, and file predicates'
