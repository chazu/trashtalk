#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }

# Exercise actual package sources through compilation, public sends and reload.
# These files and all created instances live in the test's isolated checkout.
mkdir -p "$TRASHDIR/TraitFixture"
cat > "$TRASHDIR/TraitFixture/Reporting.trash" <<'TRASH'
package: TraitFixture
Reporting trait
  method: report [ ^ @ self label ]
  classMethod: subject [ ^ self ]
  method: revision [ ^ 'first' ]
  method: removed [ ^ 'old' ]
TRASH
cat > "$TRASHDIR/TraitFixture/Task.trash" <<'TRASH'
package: TraitFixture
Task subclass: Object
  include: Persistable
  include: TraitFixture::Reporting
  instanceVars: label:'assigned work'
  classMethod: kind [ ^ 'package task' ]
TRASH
must make -s -C "$root" single CLASS=TraitFixture/Task > "$TMPDIR/trait-build.log" 2>&1
task=$(must @ TraitFixture::Task new)
check 'trait instance method keeps the consuming receiver' 'assigned work' "$(@ "$task" report)"
check 'trait class method keeps the consuming class' 'TraitFixture::Task' "$(@ TraitFixture::Task subject)"
must @ "$task" save
check 'global Persistable trait still mixes with qualified traits' 'TraitFixture::Task' "$(db_get "$task" | jq -r .class)"
check 'namespace trait is listed once' 1 "$(@ Trash listTraits | grep -cx 'TraitFixture::Reporting')"
check 'namespace trait is excluded from class listing' 0 "$(@ Trash listObjects | grep -cx 'TraitFixture__Reporting')"
check 'namespace trait source is discoverable' "$TRASHDIR/TraitFixture/Reporting.trash" "$(@ Trash sourceFileFor: TraitFixture::Reporting)"
methods=$(@ Trash methodsFor: TraitFixture::Task)
check 'consumer inspection names the trait' true "$([[ "$methods" == *'From trait TraitFixture::Reporting:'* ]] && echo true || echo false)"
methods=$(@ Trash methodsFor: TraitFixture::Reporting)
check 'trait inspection includes its methods' 2 "$(grep -Ec '^[[:space:]]+(report|subject)$' <<< "$methods")"
# Load the old definitions into this shell before replacing the trait. A reload
# must replace existing functions and remove methods no longer in the source.
must _ensure_class_sourced TraitFixture::Task
must _ensure_trait_sourced TraitFixture::Reporting
check 'initial trait implementation' first "$(@ "$task" revision)"
cat > "$TRASHDIR/TraitFixture/Reporting.trash" <<'TRASH'
package: TraitFixture
Reporting trait
  method: report [ ^ @ self label ]
  classMethod: subject [ ^ self ]
  method: revision [ ^ 'second' ]
TRASH
if ! @ Trash compileAndReload: TraitFixture::Reporting > "$TMPDIR/trait-reload.log" 2>&1; then
    cat "$TMPDIR/trait-reload.log"
    exit 1
fi
check 'recompile reloads trait in the calling shell' second "$(@ "$task" revision)"
check 'recompile preserves the original receiver' 'assigned work' "$(@ "$task" report)"
check 'reload removes deleted trait methods' false "$(declare -F __TraitFixture__Reporting__removed > /dev/null && echo true || echo false)"
check 'reloaded trait class method still dispatches' 'TraitFixture::Task' "$(@ TraitFixture::Task subject)"

# Like Assignment, a public root class may share its name with the package.
# Reloading it must remove its old methods without touching loaded helpers.
cat > "$TRASHDIR/TraitFixture.trash" <<'TRASH'
TraitFixture subclass: Object
  include: TraitFixture::Reporting
  instanceVars: label:'root work'
  method: obsolete [ ^ 'old root method' ]
TRASH
must @ Trash compileAndReload: TraitFixture > "$TMPDIR/root-reload.log" 2>&1
root_task=$(must @ TraitFixture new)
must _ensure_class_sourced TraitFixture
check 'root class mixes in its package trait' 'root work' "$(@ "$root_task" report)"
check 'old root method is installed' 'old root method' "$(@ "$root_task" obsolete)"
cat > "$TRASHDIR/TraitFixture.trash" <<'TRASH'
TraitFixture subclass: Object
  include: TraitFixture::Reporting
  instanceVars: label:'root work'
TRASH
must @ Trash compileAndReload: TraitFixture > "$TMPDIR/root-reload.log" 2>&1
check 'root reload removes its deleted methods' false "$(declare -F __TraitFixture__obsolete > /dev/null && echo true || echo false)"
check 'root reload preserves loaded trait methods' 'root work' "$(@ "$root_task" report)"
check 'root reload preserves loaded package class methods' 'package task' "$(@ TraitFixture::Task kind)"
check 'root inspection excludes package helper functions' 0 "$(@ Trash methodsFor: TraitFixture | grep -c 'Reporting__')"
printf 'PASS: %s namespaced trait checks\n' "$passed"
