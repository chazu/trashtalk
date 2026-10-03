#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
cd "$(dirname "${BASH_SOURCE[0]}")/.."
source lib/trash.bash

echo "=== Config Tests ==="

PASS=0
FAIL=0

test_pass() { echo "  ✓ $1"; PASS=$((PASS + 1)); }
test_fail() { echo "  ✗ $1"; FAIL=$((FAIL + 1)); }
check() {
    if [[ "$2" == "$3" ]]; then test_pass "$1"; else test_fail "$1: expected [$3], got [$2]"; fi
}

unset TRASHTALK_JCODE_MODEL TRASHTALK_GUSGUS_PROFILE TRASHTALK_CONTROL_WAIT TRASHTALK_DECISION_TARGET
config="$TMPDIR/config-home/trashtalk/config"
export TRASHTALK_CONFIG="$config"

# Resolution: default, then file, then env.
check "default without a file" "$(@ Config at: 'jcode.model')" 'gpt-5.6-terra'
check "path names TRASHTALK_CONFIG" "$(@ Config path)" "$config"
mkdir -p "${config%/*}"
cat > "$config" <<'EOF'
# personal settings
jcode.model = "file-model"   # pinned
agent.controlWait = 45

gusgus.profile = "maki"
EOF
check "file overrides default" "$(@ Config at: 'jcode.model')" 'file-model'
check "bare integer from file" "$(@ Config at: 'agent.controlWait')" '45'
check "env overrides file" "$(TRASHTALK_JCODE_MODEL=env-model @ Config at: 'jcode.model')" 'env-model'
check "empty env is unset" "$(TRASHTALK_JCODE_MODEL= @ Config at: 'jcode.model')" 'file-model'
check "callers read through Config" "$(@ Agent::JcodeDriver model)" 'file-model'
check "Gusgus profile from file" "$(@ Gusgus profile)" 'maki'
check "Worker control wait from file" "$(@ Agent::Worker controlWait)" '45'
check "Worker keeps its fallback for invalid env" "$(TRASHTALK_CONTROL_WAIT=abc @ Agent::Worker controlWait)" '30'
check "list reports sources" \
    "$(TRASHTALK_CODEX_MODEL=x @ Config list | awk '$1 == "jcode.model" || $1 == "codex.model" || $1 == "maki.model" {print $1, $NF}' | tr '\n' ';')" \
    'jcode.model file;codex.model TRASHTALK_CODEX_MODEL;maki.model default;'

# Failures name the problem.
if @ Config at: 'jcode.modle' >/dev/null 2>&1; then test_fail "undeclared key fails"; else test_pass "undeclared key fails"; fi
if TRASHTALK_GUSGUS_PROFILE=bogus @ Config at: 'gusgus.profile' >/dev/null 2>&1; then
    test_fail "invalid env enum fails"
else
    test_pass "invalid env enum fails"
fi
error=$(TRASHTALK_DECISION_TARGET=bogus @ Decision::Target selected 2>&1 >/dev/null)
[[ $error == *ConfigurationError*TRASHTALK_DECISION_TARGET* ]] && test_pass "DSL caller re-raises the config error" ||
    test_fail "DSL caller re-raises the config error: $error"
printf '[jcode]\nmodel = "x"\n' > "$TMPDIR/bad-config"
error=$(TRASHTALK_CONFIG="$TMPDIR/bad-config" @ Config at: 'jcode.model' 2>&1 >/dev/null)
[[ $error == *bad-config:1:*table\ headers* ]] && test_pass "table header rejected with line number" ||
    test_fail "table header rejected with line number: $error"
printf 'jcode.model = unquoted\n' > "$TMPDIR/bad-config"
if TRASHTALK_CONFIG="$TMPDIR/bad-config" @ Config at: 'jcode.model' >/dev/null 2>&1; then
    test_fail "bare string rejected"
else
    test_pass "bare string rejected"
fi
printf 'agent.controlWait = "soon"\n' > "$TMPDIR/bad-config"
if TRASHTALK_CONFIG="$TMPDIR/bad-config" @ Config at: 'agent.controlWait' >/dev/null 2>&1; then
    test_fail "file value checked against its type"
else
    test_pass "file value checked against its type"
fi
check "invalid value of another key is ignored by at:" \
    "$(TRASHTALK_CONFIG="$TMPDIR/bad-config" @ Config at: 'jcode.model')" 'gpt-5.6-terra'

# Writes keep comments, order, and trailing comments; they replace duplicates.
@ Config at: 'jcode.model' put: 'new-model'
@ Config at: 'decider.model' put: 'decider-next'
@ Config at: 'agent.controlWait' put: '60'
expected='# personal settings
jcode.model = "new-model"  # pinned
agent.controlWait = 60

gusgus.profile = "maki"
decider.model = "decider-next"'
check "put rewrites in place and appends new keys" "$(cat "$config")" "$expected"
printf 'jcode.model = "second"\n' >> "$config"
@ Config at: 'jcode.model' put: 'only'
check "put collapses duplicates" "$(grep -c '^jcode.model' "$config")" '1'
check "put value is read back" "$(@ Config at: 'jcode.model')" 'only'
if @ Config at: 'gusgus.profile' put: 'bogus' 2>/dev/null; then test_fail "put validates"; else test_pass "put validates"; fi
if @ Config at: 'jcode.model' put: 'a"b' 2>/dev/null; then test_fail "put rejects quotes"; else test_pass "put rejects quotes"; fi
check "failed puts leave the file unchanged" "$(@ Config at: 'gusgus.profile')" 'maki'
warning=$(TRASHTALK_JCODE_MODEL=env @ Config at: 'jcode.model' put: 'shadowed' 2>&1)
[[ $warning == *TRASHTALK_JCODE_MODEL*overrides* ]] && test_pass "put warns about env shadowing" ||
    test_fail "put warns about env shadowing: $warning"
@ Config reset: 'jcode.model'
check "reset restores the default" "$(@ Config at: 'jcode.model')" 'gpt-5.6-terra'
check "reset keeps other lines" "$(grep -c '' "$config")" '5'

# Writes go through a dotfiles symlink instead of replacing it.
mkdir -p "$TMPDIR/dotfiles"
printf '# tracked\n' > "$TMPDIR/dotfiles/config"
ln -s "$TMPDIR/dotfiles/config" "$TMPDIR/linked-config"
TRASHTALK_CONFIG="$TMPDIR/linked-config" @ Config at: 'maki.model' put: 'openai/linked'
[[ -L "$TMPDIR/linked-config" ]] && test_pass "symlink survives a write" || test_fail "symlink survives a write"
check "symlink target receives the write" "$(tail -n 1 "$TMPDIR/dotfiles/config")" 'maki.model = "openai/linked"'

# The default user file is skipped under TRASHTALK_SKIP_USER_CONFIG.
unset TRASHTALK_CONFIG
export XDG_CONFIG_HOME="$TMPDIR/config-home"
check "skip flag ignores the default file" "$(@ Config at: 'gusgus.profile')" 'jcode'
if @ Config at: 'jcode.model' put: 'x' 2>/dev/null; then test_fail "put refuses when skipped"; else test_pass "put refuses when skipped"; fi
check "default file is read when not skipped" "$(TRASHTALK_SKIP_USER_CONFIG= @ Config at: 'gusgus.profile')" 'maki'

# A template is a valid config that changes nothing.
@ Config template > "$TMPDIR/template"
check "template parses to defaults" "$(TRASHTALK_CONFIG="$TMPDIR/template" @ Config list)" "$(@ Config list)"
sed 's/^# \([a-z][A-Za-z.]* = \)/\1/' "$TMPDIR/template" > "$TMPDIR/uncommented"
check "uncommented template is valid" "$(TRASHTALK_CONFIG="$TMPDIR/uncommented" @ Config at: 'agent.controlWait')" '30'

# Doctor findings.
printf 'jcode.mdoel = "typo"\nagent.controlWait = "soon"\ndecider.model = "a"\ndecider.model = "b"\n' > "$TMPDIR/doctor-config"
report=$(TRASHTALK_CONFIG="$TMPDIR/doctor-config" TRASHTALK_CODEX_MODEL=x TRASHTALK_MAKI_MODEL=$'a\nb' @ Config check)
[[ $report == *$'warn\tUnknown config key jcode.mdoel'* ]] && test_pass "check flags unknown keys" || test_fail "check flags unknown keys"
[[ $report == *$'bad\tConfig agent.controlWait'* ]] && test_pass "check flags invalid values" || test_fail "check flags invalid values"
[[ $report == *$'warn\tConfig key decider.model appears more than once'* ]] && test_pass "check flags duplicates" || test_fail "check flags duplicates"
[[ $report == *$'bad\tTRASHTALK_MAKI_MODEL is invalid'* ]] && test_pass "check flags invalid env" || test_fail "check flags invalid env"
[[ $report != *TRASHTALK_CODEX_MODEL* ]] && test_pass "unshadowing env is not reported" || test_fail "unshadowing env is not reported"
doctor=$(TRASHTALK_CONFIG="$TMPDIR/doctor-config" TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_AGENT_BACKEND=none \
    @ Trash doctor 2>&1)
[[ $doctor == *WARN*'Unknown config key jcode.mdoel'* && $doctor == *FAIL*'Config agent.controlWait'* ]] &&
    test_pass "doctor shows config findings" || test_fail "doctor shows config findings"

echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]] || exit 1
