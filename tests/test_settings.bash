#!/usr/bin/env bash
# Settings groups and preferences classes: docs/settings-design.md.
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
cd "$(dirname "${BASH_SOURCE[0]}")/.."
source lib/trash.bash

echo "=== Settings Tests ==="

PASS=0
FAIL=0

test_pass() { echo "  ✓ $1"; PASS=$((PASS + 1)); }
test_fail() { echo "  ✗ $1"; FAIL=$((FAIL + 1)); }
check() {
    if [[ "$2" == "$3" ]]; then test_pass "$1"; else test_fail "$1: expected [$3], got [$2]"; fi
}
fails() {
    local name=$1 pattern=$2 output
    shift 2
    if output=$("$@" 2>&1); then
        test_fail "$name: succeeded"
    elif [[ $output == *$pattern* ]]; then
        test_pass "$name"
    else
        test_fail "$name: expected [$pattern] in [$output]"
    fi
}

unset TRASHTALK_JCODE_MODEL TRASHTALK_GUSGUS_PROFILE TRASHTALK_CONTROL_WAIT TRASHTALK_JCODE_PROVIDER
export TRASHTALK_CONFIG="$TMPDIR/no-config"
user="$TRASHDIR/user"

# Groups: generated getters, describe, and the group listing.
check "getter reads the default" "$(@ Agent::JcodeSettings model)" 'gpt-5.6-terra'
check "getter honors env" "$(TRASHTALK_JCODE_MODEL=env @ Agent::JcodeSettings model)" 'env'
check "Config at: agrees with the getter" "$(@ Config at: 'agent.controlWait')" '30'
check "derived env name and explicit env:" \
    "$(@ Config list | awk '$1 == "jcode.contextWindow" {print $NF}'; TRASHTALK_CONTROL_WAIT=5 @ Config list | awk '$1 == "agent.controlWait" {print $NF}')" \
    $'default\nTRASHTALK_CONTROL_WAIT'
description=$(@ Agent::JcodeSettings describe)
[[ $description == *'contextWindow = 65536  (default)'*'TRASHTALK_JCODE_CONTEXT_WINDOW'* ]] &&
    test_pass "describe shows value, source, and env" || test_fail "describe shows value, source, and env: $description"
[[ $(@ Settings groups) == *'Agent::JcodeSettings'*'jcode'*'(4 settings)'* ]] &&
    test_pass "groups lists each group" || test_fail "groups lists each group"

# Preferences apply only when user configuration is on.
export TRASHTALK_SKIP_USER_CONFIG=
baseline=$(@ Config list)
path=$(@ Trash createPreferencesClass: 'Chaz' subclassing: 'Preferences')
check "preferences class is written to trash/user" "$path" "$user/Chaz.trash"
check "a new preferences class changes nothing" "$(@ Config list)" "$baseline"
check "the only root class is active" "$(@ Preferences active)" 'Chaz'
grep -q "^  # Agent::JcodeSettings model: 'gpt-5.6-terra'$" "$path" &&
    test_pass "template lists settings commented out" || test_fail "template lists settings commented out"
fails "taken names are refused" 'already exists' @ Trash createPreferencesClass: 'Chaz' subclassing: 'Preferences'
fails "system class names are refused" 'already exists' @ Trash createUserClass: 'Config'
fails "superclass must be a preferences class" 'neither Preferences' @ Trash createPreferencesClass: 'Bad' subclassing: 'Object'

# Writes edit the active class and recompile it.
@ Config at: 'jcode.model' put: 'chaz-model'
@ Agent::JcodeSettings provider: 'local'
check "put is read back" "$(@ Agent::JcodeSettings model)" 'chaz-model'
check "group setter is read back" "$(@ Config at: 'jcode.provider')" 'local'
check "source reports the class" "$(@ Config list | awk '$1 == "jcode.model" {print $(NF-1), $NF}')" 'preferences Chaz'
check "put appends a preference line" "$(grep -c "^  Agent::JcodeSettings model: 'chaz-model'$" "$path")" '1'
sed -i.bak "s/^  Agent::JcodeSettings model: 'chaz-model'$/  Agent::JcodeSettings model: 'chaz-model'   # pinned/" "$path"
rm -f "$path.bak"
@ Config at: 'jcode.model' put: "it's"
check "put keeps a trailing comment and quotes apostrophes" \
    "$(grep 'JcodeSettings model:' "$path" | grep -v '^  #')" "  Agent::JcodeSettings model: \"it's\"  # pinned"
fails "put validates against the declaration" 'must be one of' @ Config at: 'gusgus.profile' put: 'bogus'
fails "put rejects both quote kinds" 'both single and double' @ Config at: 'jcode.model' put: "a'b\"c"
check "failed puts change nothing" "$(@ Agent::JcodeSettings model)" "it's"
warning=$(TRASHTALK_JCODE_MODEL=env @ Config at: 'jcode.model' put: 'shadowed' 2>&1)
[[ $warning == *TRASHTALK_JCODE_MODEL*overrides* ]] && test_pass "put warns about env shadowing" ||
    test_fail "put warns about env shadowing: $warning"

# A machine subclass overrides some values and inherits the rest.
@ Trash createPreferencesClass: 'Sol' subclassing: 'Chaz' >/dev/null
sed -i.bak 's/^  # host: name.*$/  host: sol/' "$user/Sol.trash"
rm -f "$user/Sol.trash.bak"
"$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile-cached "$user/Sol.trash" "$TRASHDIR/.compiled/Sol" >/dev/null
check "host: picks the machine's class" "$(HOSTNAME=sol.example.net @ Preferences active)" 'Sol'
check "other machines use the root class" "$(HOSTNAME=elsewhere @ Preferences active)" 'Chaz'
HOSTNAME=sol @ Config at: 'jcode.provider' put: 'openai'
check "a write on the machine lands in its class" "$(grep -c 'provider:' "$user/Sol.trash" | tr -d ' ')" '2'
check "the machine sees its override" "$(HOSTNAME=sol @ Agent::JcodeSettings provider)" 'openai'
check "other machines keep the shared value" "$(HOSTNAME=elsewhere @ Agent::JcodeSettings provider)" 'local'
check "the machine inherits the rest" "$(HOSTNAME=sol @ Agent::JcodeSettings model)" 'shadowed'
HOSTNAME=sol @ Config at: 'codex.model' put: 'shared-codex' in: 'Chaz'
check "put:in: writes the shared class" "$(HOSTNAME=elsewhere @ Config at: 'codex.model')" 'shared-codex'
check "TRASHTALK_PREFERENCES names the class" "$(HOSTNAME=sol TRASHTALK_PREFERENCES=Chaz @ Preferences active)" 'Chaz'
fails "unknown TRASHTALK_PREFERENCES fails" 'not a compiled Preferences class' env TRASHTALK_PREFERENCES=Nobody bash -c 'source lib/trash.bash; @ Config at: jcode.model'
check "skip flag ignores preferences" "$(TRASHTALK_SKIP_USER_CONFIG=1 @ Agent::JcodeSettings provider)" 'openai'
HOSTNAME=elsewhere @ Agent::JcodeSettings reset: 'provider'
check "reset removes the line" "$(HOSTNAME=elsewhere @ Agent::JcodeSettings provider)" 'openai'

# Writes go through a dotfiles symlink.
mkdir -p "$TMPDIR/dotfiles"
mv "$user/Chaz.trash" "$TMPDIR/dotfiles/Chaz.trash"
ln -s "$TMPDIR/dotfiles/Chaz.trash" "$user/Chaz.trash"
HOSTNAME=elsewhere @ Config at: 'maki.model' put: 'openai/linked'
[[ -L "$user/Chaz.trash" ]] && test_pass "symlink survives a write" || test_fail "symlink survives a write"
grep -q "Agent::MakiSettings model: 'openai/linked'" "$TMPDIR/dotfiles/Chaz.trash" &&
    test_pass "symlink target receives the write" || test_fail "symlink target receives the write"

# Doctor: a hand edit is stale until the class is compiled again.
printf '  Agent::CodexSettings model: %s\n' "'hand'" >> "$TMPDIR/dotfiles/Chaz.trash"
report=$(HOSTNAME=elsewhere @ Config check)
[[ $report == *$'warn\tPreferences class Chaz changed since it was compiled'* ]] &&
    test_pass "check reports stale preferences" || test_fail "check reports stale preferences: $report"

# Import moves a legacy config file into a new class.
printf 'jcode.model = "from-file"\nagent.controlWait = 45\n' > "$TMPDIR/legacy"
TRASHTALK_CONFIG="$TMPDIR/legacy" @ Config import: 'Imported' 2>/dev/null >/dev/null
check "import writes the file's values" "$(TRASHTALK_PREFERENCES=Imported @ Config at: 'agent.controlWait')" '45'
grep -q "^  Agent::JcodeSettings model: 'from-file'$" "$user/Imported.trash" &&
    test_pass "import writes preference lines" || test_fail "import writes preference lines"

# Imported is a second root class, so nothing picks one on another machine.
fails "two roots with no host are ambiguous" 'could each apply' bash -c \
    "source lib/trash.bash; HOSTNAME=elsewhere @ Config at: 'jcode.model' put: x"

# A plain user class is header-only.
path=$(@ Trash createUserClass: 'Scratch')
check "user class is header-only" "$(cat "$path")" 'Scratch subclass: Object'

echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]] || exit 1
