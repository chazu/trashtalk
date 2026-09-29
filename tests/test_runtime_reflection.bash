#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
check() { [[ "$2" == "$3" ]] || { printf 'FAIL %s: expected <%s>, got <%s>\n' "$1" "$2" "$3"; exit 1; }; }
check 'cold class exists' true "$(@ Runtime classExists: Counter)"
@ Counter new >/dev/null
check 'loaded class exists' true "$(@ Runtime classExists: Counter)"
check 'missing class' false "$(@ Runtime classExists: MissingReflectionClass)"
check 'qualified superclass' Agent::Driver "$(@ Runtime superclassOf: Agent::CodexDriver)"
id=$(@ Agent::CodexDriver new)
check 'qualified inheritance' true "$(@ "$id" isKindOf: Agent::Driver)"
check 'keyword selector' true "$(@ Runtime class: Counter hasMethod: incrementBy:)"
check 'inherited selector' true "$(@ Runtime class: Counter hasMethod: isKindOf:)"
check 'trait selector' true "$(@ Runtime class: Process hasMethod: debug:)"
check 'missing selector' false "$(@ Runtime class: Counter hasMethod: missingReflectionMethod)"
check 'qualified method enumeration' true "$(@ Runtime methodsFor: Agent::CodexDriver | jq 'index("class__capabilities") != null')"
check 'pattern selection' '["increment","incrementBy_"]' "$(@ Runtime methodsFor: Counter matching: 'increment*')"
echo 'PASS: cold/loaded, namespaced, inherited, trait and keyword reflection'
# Superclass traits are not inherited by ordinary sends; reflection must agree.
cat > "$TMPDIR/ReflectionTrait.trash" <<'TRASH'
ReflectionTrait trait
  method: directOnly [ ^ 'direct' ]
TRASH
cat > "$TMPDIR/ReflectionParent.trash" <<'TRASH'
ReflectionParent subclass: Object
  include: ReflectionTrait
TRASH
cat > "$TMPDIR/ReflectionChild.trash" <<'TRASH'
ReflectionChild subclass: ReflectionParent
TRASH
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TMPDIR/ReflectionTrait.trash" > "$TRASHDIR/.compiled/traits/ReflectionTrait"
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TMPDIR/ReflectionParent.trash" > "$TRASHDIR/.compiled/ReflectionParent"
bash "$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TMPDIR/ReflectionChild.trash" > "$TRASHDIR/.compiled/ReflectionChild"
check 'direct trait remains visible' true "$(@ Runtime class: ReflectionParent hasMethod: directOnly)"
check 'parent trait is not falsely inherited' false "$(@ Runtime class: ReflectionChild hasMethod: directOnly)"
child=$(@ ReflectionChild new)
if @ "$child" directOnly 2>/dev/null; then echo 'FAIL: fixture disagrees with dispatch'; exit 1; fi
