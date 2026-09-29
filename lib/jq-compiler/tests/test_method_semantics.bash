#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
cat > "$scratch/ReturnProbe.trash" <<'TRASH'
ReturnProbe subclass: Object
  instanceVars: value:'initial'
  method: blockAssign [ (value = 'initial') ifTrue: [value := 'failed']. ^ value ]
  method: guard [ value := (@ self fail) ifFailed: [^ 'caught']. ^ 'missed' ]
  method: fail [ @ StateError signal: 'fixture' ]
  classMethod: early [
    ^ 'first'
    ^ 'second'
  ]
  classMethod: numeric [
    ^ 1
    ^ 2
  ]
  classMethod: json [ ^ '["starting:running","running:failed"]' ]
  classMethod: literal [ ^ '-n' ]
  classMethod: quoted [ | text | text := '"$HOME" \ literal'. ^ text , ' end' ]
  classMethod: unpack [
    '{"value":"ok"}' jsonUnpack: #('value') into: [:value | @ Console print: 'effect'].
    ^ 'result'
  ]
  classMethod: text [ ^ 'my self portrait' ]
TRASH
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$scratch/ReturnProbe.trash" > "$scratch/ReturnProbe"
cp "$scratch/ReturnProbe" "$TRASHDIR/.compiled/ReturnProbe"
source "$scratch/ReturnProbe"
check() { [[ "$2" == "$3" ]] || { printf 'FAIL %s: expected <%s>, got <%s>\n' "$1" "$2" "$3"; exit 1; }; }
check 'literal return terminates' first "$(@ ReturnProbe early)"
check 'numeric return terminates' 1 "$(@ ReturnProbe numeric)"
check 'self inside literal is text' 'my self portrait' "$(@ ReturnProbe text)"
check 'public two-part concatenation' helloworld "$(@ String concat: hello with: world)"
check 'public three-part concatenation' abc "$(@ String concat: a with: b with: c)"
check 'JSON return preserves quotes' '["starting:running","running:failed"]' "$(@ ReturnProbe json)"
check 'literal echo flag remains data' '-n' "$(@ ReturnProbe literal)"
check 'literal assignment/concat does not interpolate' '"$HOME" \ literal end' "$(@ ReturnProbe quoted)"
check 'non-tail JSON binding discards effect output' result "$(@ ReturnProbe unpack)"
probe=$(@ ReturnProbe new)
check 'block ivar assignment preserves text' failed "$(@ "$probe" blockAssign)"
check 'ivar assignment retains failure' caught "$(@ "$probe" guard)"
check 'failed assignment leaves value intact' failed "$(@ "$probe" value)"
echo 'PASS: ordinary method semantics through public messages'
