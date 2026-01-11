#!/usr/bin/env bash
cd ~/.trashtalk
export TRASHTALK_DISABLE_NATIVE=1
source lib/trash.bash

echo "=== Env Primitive Methods Tests ==="

PASS=0
FAIL=0

test_pass() { echo "  ✓ $1"; ((PASS++)); }
test_fail() { echo "  ✗ $1"; ((FAIL++)); }

# Test get: with existing var
home=$(@ Env get: 'HOME')
if [[ -n "$home" && "$home" == "$HOME" ]]; then
    test_pass "get: HOME = $home"
else
    test_fail "get: HOME expected $HOME, got $home"
fi

# Test get: with non-existing var
nonexist=$(@ Env get: 'TRASHTALK_NONEXISTENT_VAR_12345')
if [[ -z "$nonexist" ]]; then
    test_pass "get: non-existent returns empty"
else
    test_fail "get: non-existent expected empty, got $nonexist"
fi

# Test get:default: with existing var
home=$(@ Env get: 'HOME' default: '/fallback')
if [[ "$home" == "$HOME" ]]; then
    test_pass "get:default: existing var returns value"
else
    test_fail "get:default: expected $HOME, got $home"
fi

# Test get:default: with non-existing var
fallback=$(@ Env get: 'TRASHTALK_NONEXISTENT_VAR_12345' default: 'mydefault')
if [[ "$fallback" == "mydefault" ]]; then
    test_pass "get:default: non-existent returns default"
else
    test_fail "get:default: expected mydefault, got $fallback"
fi

# Test set:to: and get:
@ Env set: 'TRASHTALK_TEST_VAR' to: 'test_value_123'
result=$(@ Env get: 'TRASHTALK_TEST_VAR')
if [[ "$result" == "test_value_123" ]]; then
    test_pass "set:to: and get: work together"
else
    test_fail "set:to: expected test_value_123, got $result"
fi

# Test has: with existing var
hasHome=$(@ Env has: 'HOME')
if [[ "$hasHome" == "true" ]]; then
    test_pass "has: HOME = true"
else
    test_fail "has: HOME expected true, got $hasHome"
fi

# Test has: with non-existing var
hasNone=$(@ Env has: 'TRASHTALK_NONEXISTENT_VAR_12345')
if [[ "$hasNone" == "false" ]]; then
    test_pass "has: non-existent = false"
else
    test_fail "has: non-existent expected false, got $hasNone"
fi

# Test hasNonEmpty: with non-empty var
hasNonEmptyHome=$(@ Env hasNonEmpty: 'HOME')
if [[ "$hasNonEmptyHome" == "true" ]]; then
    test_pass "hasNonEmpty: HOME = true"
else
    test_fail "hasNonEmpty: HOME expected true, got $hasNonEmptyHome"
fi

# Test unset:
@ Env unset: 'TRASHTALK_TEST_VAR'
result=$(@ Env get: 'TRASHTALK_TEST_VAR')
if [[ -z "$result" ]]; then
    test_pass "unset: removes variable"
else
    test_fail "unset: expected empty, got $result"
fi

# Test which:
bashPath=$(@ Env which: 'bash')
if [[ -n "$bashPath" && -x "$bashPath" ]]; then
    test_pass "which: bash = $bashPath"
else
    test_fail "which: bash expected executable path"
fi

# Test hasExecutable:
hasBash=$(@ Env hasExecutable: 'bash')
if [[ "$hasBash" == "true" ]]; then
    test_pass "hasExecutable: bash = true"
else
    test_fail "hasExecutable: bash expected true, got $hasBash"
fi

# Test hasExecutable: with non-existent
hasNonexist=$(@ Env hasExecutable: 'nonexistent_command_12345')
if [[ "$hasNonexist" == "false" ]]; then
    test_pass "hasExecutable: non-existent = false"
else
    test_fail "hasExecutable: non-existent expected false, got $hasNonexist"
fi

# Test home
envHome=$(@ Env home)
if [[ "$envHome" == "$HOME" ]]; then
    test_pass "home = $envHome"
else
    test_fail "home expected $HOME, got $envHome"
fi

# Test user
envUser=$(@ Env user)
if [[ -n "$envUser" ]]; then
    test_pass "user = $envUser"
else
    test_fail "user expected non-empty"
fi

# Test shell
envShell=$(@ Env shell)
if [[ -n "$envShell" ]]; then
    test_pass "shell = $envShell"
else
    test_fail "shell expected non-empty"
fi

# Test cwd
envCwd=$(@ Env cwd)
if [[ "$envCwd" == "$(pwd)" ]]; then
    test_pass "cwd = $envCwd"
else
    test_fail "cwd expected $(pwd), got $envCwd"
fi

# Test temp
envTemp=$(@ Env temp)
if [[ -n "$envTemp" ]]; then
    test_pass "temp = $envTemp"
else
    test_fail "temp expected non-empty"
fi

# Test hostname
envHostname=$(@ Env hostname)
if [[ -n "$envHostname" ]]; then
    test_pass "hostname = $envHostname"
else
    test_fail "hostname expected non-empty"
fi

# Test os
envOs=$(@ Env os)
if [[ -n "$envOs" ]]; then
    test_pass "os = $envOs"
else
    test_fail "os expected non-empty"
fi

# Test isMac (on macOS should be true)
isMac=$(@ Env isMac)
if [[ "$OSTYPE" == "darwin"* ]]; then
    if [[ "$isMac" == "true" ]]; then
        test_pass "isMac = true (on macOS)"
    else
        test_fail "isMac expected true on macOS"
    fi
else
    if [[ "$isMac" == "false" ]]; then
        test_pass "isMac = false (not on macOS)"
    else
        test_fail "isMac expected false on non-macOS"
    fi
fi

# Test isLinux
isLinux=$(@ Env isLinux)
if [[ "$OSTYPE" == "linux"* ]]; then
    if [[ "$isLinux" == "true" ]]; then
        test_pass "isLinux = true (on Linux)"
    else
        test_fail "isLinux expected true on Linux"
    fi
else
    if [[ "$isLinux" == "false" ]]; then
        test_pass "isLinux = false (not on Linux)"
    else
        test_fail "isLinux expected false on non-Linux"
    fi
fi

echo ""
echo "Results: $PASS passed, $FAIL failed"
[[ $FAIL -eq 0 ]] || exit 1
