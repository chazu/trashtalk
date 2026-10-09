#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export TRASHTALK_DIR
source "$TRASHTALK_DIR/lib/trash.bash"

test_tmp=$(mktemp -d "${TMPDIR:-/tmp}/trash-bash-shortcuts.XXXXXX")
trap 'rm -rf "$test_tmp"' EXIT
export TRASHTALK_BASHRC="$test_tmp/.bashrc"

pass=0 fail=0
ok() { printf '  PASS: %s\n' "$1"; ((pass++)) || true; }
not_ok() { printf '  FAIL: %s\n' "$1"; ((fail++)) || true; }

printf '# existing setting\n' > "$TRASHTALK_BASHRC"
@ Trash installBashShortcuts > "$test_tmp/first.out" || not_ok 'installer succeeds'
grep -Fqx '# existing setting' "$TRASHTALK_BASHRC" && ok 'keeps existing startup settings' || not_ok 'keeps existing startup settings'
grep -Fqx '# >>> trashtalk bash shortcuts >>>' "$TRASHTALK_BASHRC" && ok 'writes opening marker' || not_ok 'writes opening marker'
grep -Fq 'bind -x '\''"\ey": trashtalk_browse_code'\''' "$TRASHTALK_BASHRC" && ok 'installs Alt-Y browser binding' || not_ok 'installs Alt-Y browser binding'
grep -Fq 'bind -x '\''"\eu": trashtalk_focus_gusgus'\''' "$TRASHTALK_BASHRC" && ok 'installs Alt-U focus binding' || not_ok 'installs Alt-U focus binding'
grep -Fq 'bind -x '\''"\ei": trashtalk_browse_inbox'\''' "$TRASHTALK_BASHRC" && ok 'installs Alt-I inbox binding' || not_ok 'installs Alt-I inbox binding'
bash -n "$TRASHTALK_BASHRC" && ok 'writes valid Bash startup syntax' || not_ok 'writes valid Bash startup syntax'
backup=$(awk -F': ' '/^Backup: / { print $2 }' "$test_tmp/first.out")
[[ -f "$backup" ]] && ok 'creates a backup before changing startup file' || not_ok 'creates a backup before changing startup file'

@ Trash installBashShortcuts > "$test_tmp/second.out" || not_ok 'second installer invocation succeeds'
[[ $(grep -Fc '# >>> trashtalk bash shortcuts >>>' "$TRASHTALK_BASHRC") == 1 ]] && ok 'replaces rather than duplicates managed block' || not_ok 'replaces rather than duplicates managed block'
grep -Fq 'already installed' "$test_tmp/second.out" && ok 'reports unchanged installation' || not_ok 'reports unchanged installation'

printf 'Passed: %d, Failed: %d\n' "$pass" "$fail"
(( fail == 0 ))
