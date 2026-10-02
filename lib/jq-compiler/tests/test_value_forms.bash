#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../../test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Value forms that once compiled silently wrong: comparisons used as values,
# keyword arguments followed by an intrinsic-named keyword, triple-quoted text
# outside a local assignment, qualified error classes, code after a class, and
# nested blocks closed with an adjacent `]]`.
set -eo pipefail
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$TEST_DIR")"
ROOT="$(cd "$COMPILER_DIR/../.." && pwd)"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/trash-value-forms.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/ValueForms.trash" <<'TRASH'
package: Forms

ValueForms subclass: Object
  instanceVars: text:''
  method: one [ ^ 'x' ]
  method: two [ ^ 'y' ]
  method: same: a as: b [ | r | r := a = b. ^ r ]
  method: differs: a from: b [ | r | r := (a ~= b). ^ r ]
  method: less: a than: b [ | r | r := a < b. ^ r ]
  method: sameInBlock: a as: b [ | r | (a notEmpty) ifTrue: [ r := a = b ]. ^ r ]
  method: sendsEqual [ ^ (@ self one) = (@ self two) ]
  method: sendsEqualCond [ ((@ self one) = (@ self two)) ifTrue: [ ^ 'same' ]. ^ 'diff' ]
  method: sendsEqualSelf [ ((@ self one) = (@ self one)) ifTrue: [ ^ 'same' ]. ^ 'diff' ]
  method: join: a after: b [ ^ a , '+' , b ]
  method: callAfter [ ^ @ self join: 'a' after: 'b' ]
  method: check: s matches: re [ ^ s , '~' , re ]
  method: callMatches [ ^ @ self check: 'hello' matches: '^h' ]
  method: callGrouped: s [ ^ @ self check: (s upTo: ':') matches: 'r' ]
  method: pairs: n [ ^ #{name: n after: 2} asJson ]
  method: fails [ @ Forms::FormError signal: 'nope' ]
  method: failWith: y [ @ Forms::FormError signal: y ]
  method: guarded: y [ | r | r := @ self failWith: y ifFailed: [ ^ 'failed' ]. ^ r ]
  method: guardedOk: y [ | r | r := @ self check: y matches: 'z' ifFailed: [ ^ 'failed' ]. ^ r ]
  method: literal [ ^ '''a $(echo INJECTED) `b` "q"''' ]
  method: literalArg [ ^ @ self check: '''$(echo INJECTED)''' matches: 'x' ]
  method: literalConcat: y [ ^ '''$(echo INJECTED)''' , y ]
  method: literalIvar [ text := '''$HOME "q"'''. ^ text ]
  method: literalInBlock: y [ (y notEmpty) ifTrue: [ ^ '''$(echo INJECTED)''' ]. ^ 'no' ]
  method: nestedClose: a [ | r | r := 'no'. (a notEmpty) ifTrue: [(a notEmpty) ifTrue: [r := 'yes']]. ^ r ]
  method: afterNested [ ^ 'reached' ]
  rawMethod: rawTest: a [ [[ "$a" == x ]] && echo hit || echo miss ]
  method: qualifiedFailure [
    (@ self fails) ifFailed: [:e | ^ e ].
    ^ 'not reached'
  ]
TRASH
"$COMPILER_DIR/driver.bash" compile "$scratch/ValueForms.trash" > "$TRASHDIR/.compiled/Forms__ValueForms"
bash -n "$TRASHDIR/.compiled/Forms__ValueForms"
source "$ROOT/lib/trash.bash"
check() {
    local expected="$1" actual; shift
    actual=$("$@" 2>&1) || true
    [[ "$actual" == "$expected" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$*" "$expected" "$actual" >&2; exit 1; }
    echo "PASS: $*"
}
id=$(@ Forms::ValueForms new)

# Comparisons used as values yield true/false, never Bash arithmetic.
check true @ "$id" same: abc as: abc
check false @ "$id" same: abc as: abd
check true @ "$id" differs: abc from: abd
check true @ "$id" less: 2 than: 10
check false @ "$id" less: 10 than: 2
check true @ "$id" sameInBlock: q as: q
check false @ "$id" sendsEqual
check diff @ "$id" sendsEqualCond
check same @ "$id" sendsEqualSelf
# Grouped sends are not a Bash arithmetic command; real arithmetic still is.
for form in '((@ x a) = (@ x b))|LPAREN' '(( n + 1 ))|ARITH_CMD' '(( (a + b) * 2 ))|ARITH_CMD'; do
    kind=$(printf '%s' "${form%|*}" | "$COMPILER_DIR/tokenizer.bash" | jq -r '.[0].type')
    [[ "$kind" == "${form#*|}" ]] || { echo "FAIL: ${form%|*} tokenized as $kind" >&2; exit 1; }
done
echo "PASS: double parentheses tokenize by structure"

# A keyword after a keyword argument continues the selector.
check 'a+b' @ "$id" callAfter
check 'hello~^h' @ "$id" callMatches
check 'r~r' @ "$id" callGrouped: 'r:s'
check '{"name":"n","after":2}' @ "$id" pairs: n
check 'failed' @ "$id" guarded: y
check 'y~z' @ "$id" guardedOk: y

# Triple-quoted text is literal in every position.
check 'a $(echo INJECTED) `b` "q"' @ "$id" literal
check '$(echo INJECTED)~x' @ "$id" literalArg
check '$(echo INJECTED)!' @ "$id" literalConcat: '!'
check '$HOME "q"' @ "$id" literalIvar
check '$(echo INJECTED)' @ "$id" literalInBlock: y

# `]]` closing two blocks is two closes, not a Bash test; the next method survives.
check yes @ "$id" nestedClose: q
check no @ "$id" nestedClose: ''
check reached @ "$id" afterNested
check hit @ "$id" rawTest: x
check miss @ "$id" rawTest: y

# A qualified error class raises like an unqualified one.
check 'Forms::FormError: nope' @ "$id" qualifiedFailure
if @ "$id" fails 2>/dev/null; then echo "FAIL: qualified signal did not fail" >&2; exit 1; fi
echo "PASS: qualified signal fails the method"

# Code after the class body would never run, so it is a compile error.
printf 'Late subclass: Object\n  method: a [ ^ 1 ]\n\n@ Late a\n' > "$scratch/Late.trash"
for attempt in 1 2; do
    if "$COMPILER_DIR/driver.bash" compile "$scratch/Late.trash" >/dev/null 2>"$scratch/late.err"; then
        echo "FAIL: code after the class body compiled (attempt $attempt)" >&2; exit 1
    fi
    grep -q 'Code outside a method never runs' "$scratch/late.err" || { cat "$scratch/late.err" >&2; exit 1; }
done
echo "PASS: code after the class body is rejected, including from the AST cache"

# `compile --check` names the syntax error instead of exiting silently.
printf 'Bad subclass: Object\n  rawMethod: bad [\n    if then fi\n  ]\n' > "$scratch/Bad.trash"
if "$COMPILER_DIR/driver.bash" compile "$scratch/Bad.trash" --check >/dev/null 2>"$scratch/bad.err"; then
    echo "FAIL: --check accepted invalid Bash" >&2; exit 1
fi
grep -q 'Syntax errors in compiled output' "$scratch/bad.err" && grep -q "unexpected token" "$scratch/bad.err" \
    || { echo "FAIL: --check did not report the syntax error:" >&2; cat "$scratch/bad.err" >&2; exit 1; }
echo "PASS: compile --check reports syntax errors"
