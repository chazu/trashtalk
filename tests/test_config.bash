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

unset TRASHTALK_CONFIG XDG_CONFIG_HOME TRASHTALK_SKIP_USER_CONFIG
unset TRASHTALK_JCODE_MODEL TRASHTALK_GUSGUS_PROFILE TRASHTALK_CONTROL_WAIT TRASHTALK_DECISION_TARGET

# Resolution: env, then the active preferences class, then the default.
# Preferences-class reads and writes are covered by tests/test_settings.bash.
check "default" "$(@ Config at: 'jcode.model')" 'gpt-5.6-terra'
check "integer default" "$(@ Config at: 'agent.controlWait')" '30'
check "env overrides default" "$(TRASHTALK_JCODE_MODEL=env-model @ Config at: 'jcode.model')" 'env-model'
check "empty env is unset" "$(TRASHTALK_JCODE_MODEL= @ Config at: 'jcode.model')" 'gpt-5.6-terra'
check "callers read through Config" "$(TRASHTALK_JCODE_MODEL=env-model @ Agent::JcodeDriver model)" 'env-model'
check "Worker keeps its fallback for invalid env" "$(TRASHTALK_CONTROL_WAIT=abc @ Agent::Worker controlWait)" '30'
check "list reports sources" \
    "$(TRASHTALK_CODEX_MODEL=x @ Config list | awk '$1 == "jcode.model" || $1 == "codex.model" || $1 == "maki.model" {print $1, $NF}' | tr '\n' ';')" \
    'codex.model TRASHTALK_CODEX_MODEL;jcode.model default;maki.model default;'

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

# Writes go to a Preferences class (tests/test_settings.bash); without one,
# put points at the way to create one.
if @ Config at: 'jcode.model' put: 'new-model' 2>/dev/null; then test_fail "put needs a preferences class"; else test_pass "put needs a preferences class"; fi
error=$(@ Config at: 'jcode.model' put: 'x' 2>&1 >/dev/null)
[[ $error == *newPreferencesClass* ]] && test_pass "put suggests creating a preferences class" ||
    test_fail "put suggests creating a preferences class: $error"

# Doctor findings.
report=$(TRASHTALK_MAKI_MODEL=$'a\nb' @ Config check)
[[ $report == *$'bad\tTRASHTALK_MAKI_MODEL is invalid'* ]] && test_pass "check flags invalid env" || test_fail "check flags invalid env"
[[ $report != *jcode.model* ]] && test_pass "valid keys are not flagged" || test_fail "valid keys are not flagged: $report"
doctor=$(TRASHTALK_MAKI_MODEL=$'a\nb' TRASHTALK_AGENT_BACKEND=none @ Trash doctor 2>&1)
[[ $doctor == *FAIL*TRASHTALK_MAKI_MODEL* ]] && test_pass "doctor shows config findings" || test_fail "doctor shows config findings"

echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]] || exit 1
