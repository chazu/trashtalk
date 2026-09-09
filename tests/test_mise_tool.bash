#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi

set -uo pipefail
export LC_ALL=C

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
FAKE_BIN="$TEST_TMP/bin"
FAKE_MISE_ARGV="$TEST_TMP/mise-argv.txt"
ORIGINAL_PATH="$PATH"

cleanup() {
    rm -rf "$TEST_TMP"
}

mkdir -p "$FAKE_BIN"

# A fake mise that records exact argv and answers like the real CLI.
cat > "$FAKE_BIN/mise" <<'FAKE'
#!/usr/bin/env bash
set -uo pipefail
printf '%s\n' "$@" > "$FAKE_MISE_ARGV"

if [[ "${1:-}" == "--version" ]]; then
    echo '2025.9.1 linux-x64 (abc1234 2025-09-01)'
    exit 0
fi

# Skip the optional -C <dir> prefix.
if [[ "${1:-}" == "-C" ]]; then shift 2; fi

case "${1:-}" in
    ls)       echo '{"node":[{"version":"22.4.1","install_path":"/tools/node/22.4.1","active":true}]}' ;;
    current)  echo '22.4.1' ;;
    latest)   echo '22.9.0' ;;
    which)    echo '/tools/node/22.4.1/bin/node' ;;
    tasks)    echo '[{"name":"build","description":"Build it","source":"mise.toml"}]' ;;
    env)
        if [[ "${2:-}" == "--json" ]]; then
            echo '{"DATABASE_URL":"postgres://localhost/app","PATH":"/tools/node/22.4.1/bin:/usr/bin"}'
        else
            echo 'export DATABASE_URL=postgres://localhost/app'
        fi
        ;;
    config)   echo '[{"path":"mise.toml","tools":["node"]}]' ;;
    settings) echo '{"experimental":false}' ;;
    exec)
        while [[ $# -gt 0 && "$1" != "--" ]]; do shift; done
        shift
        case "${FAKE_MISE_SCENARIO:-success}" in
            success) printf 'ran: %s\n' "$*" ;;
            failure) echo 'command failed' >&2; exit 3 ;;
        esac
        ;;
    run)
        case "${FAKE_MISE_SCENARIO:-success}" in
            success) printf 'task %s ok\n' "${2:-}" ;;
            failure) echo 'task failed' >&2; exit 2 ;;
        esac
        ;;
    install|use|uninstall|trust|doctor) ;;
esac
FAKE
chmod +x "$FAKE_BIN/mise"

export FAKE_MISE_ARGV
export PATH="$FAKE_BIN:$PATH"
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
source "$PROJECT_DIR/lib/trash.bash" 2>/dev/null
# trash.bash installs its own EXIT handler.
trap cleanup EXIT

PASSED=0
FAILED=0

pass() { echo "  PASS: $1"; ((PASSED++)) || true; }
fail() { echo "  FAIL: $1"; ((FAILED++)) || true; }

assert_eq() {
    if [[ "$2" == "$3" ]]; then
        pass "$1"
    else
        echo "    expected: $2"
        echo "    actual:   $3"
        fail "$1"
    fi
}

assert_argv() {
    local name="$1"
    shift
    local expected
    expected=$(printf '%s\n' "$@")
    assert_eq "$name" "$expected" "$(cat "$FAKE_MISE_ARGV")"
}

field() { printf '%s' "$1" | jq -r "$2"; }

echo "=== Tools::Mise Tests ==="

export FAKE_MISE_SCENARIO=success

assert_eq "version keeps only the version token" '2025.9.1' "$(@ Tools::Mise version)"

listed=$(@ Tools::Mise list)
assert_argv "list asks for JSON" ls --json
assert_eq "list returns the tool map" '22.4.1' "$(field "$listed" '.node[0].version')"

listed=$(@ Tools::Mise list: node)
assert_argv "list: filters to one tool" ls --json node

listed=$(@ Tools::Mise listInDirectory: "$TEST_TMP/proj dir")
assert_argv "listInDirectory: prefixes -C" -C "$TEST_TMP/proj dir" ls --json

assert_eq "current: trims the version" '22.4.1' "$(@ Tools::Mise current: node)"
assert_argv "current: passes the tool" current node
assert_eq "latest: trims the version" '22.9.0' "$(@ Tools::Mise latest: node)"
assert_eq "which: returns the resolved path" '/tools/node/22.4.1/bin/node' "$(@ Tools::Mise which: node)"
assert_argv "which: passes the binary" which node

result=$(@ Tools::Mise install: 'node@22')
assert_argv "install: is non-interactive" install --yes node@22
assert_eq "install: succeeds" 'success' "$(field "$result" .outcome)"

result=$(@ Tools::Mise install: '["node@22","python@3.12"]')
assert_argv "install: expands a JSON spec array" install --yes node@22 python@3.12

result=$(@ Tools::Mise installAllInDirectory: "$TEST_TMP/proj")
assert_argv "installAll installs the project config" -C "$TEST_TMP/proj" install --yes

result=$(@ Tools::Mise use: 'node@22')
assert_argv "use: pins in the project" use --yes node@22
result=$(@ Tools::Mise useGlobal: 'node@22')
assert_argv "useGlobal: pins globally" use --yes --global node@22
result=$(@ Tools::Mise uninstall: 'node@20')
assert_argv "uninstall: passes the spec" uninstall node@20

result=$(@ Tools::Mise exec: 'node@22' argv: '["node","-e","console.log(1 + 1)"]')
assert_argv "exec: separates tools from the command" exec node@22 -- node -e 'console.log(1 + 1)'
assert_eq "exec: succeeds" 'success' "$(field "$result" .outcome)"
assert_eq "exec: keeps stdout" 'ran: node -e console.log(1 + 1)' "$(field "$result" '.stdout | rtrimstr("\n")')"

result=$(@ Tools::Mise execArgv: '["npm","test"]' inDirectory: "$TEST_TMP/proj")
assert_argv "execArgv: uses the project tools" -C "$TEST_TMP/proj" exec -- npm test

result=$(@ Tools::Mise exec: 'node@22' shell: 'node -v && npm -v')
assert_argv "exec:shell: wraps the snippet in bash -c" exec node@22 -- bash -c 'node -v && npm -v'

tasks=$(@ Tools::Mise tasks)
assert_argv "tasks lists as JSON" tasks ls --json
assert_eq "tasks returns task records" 'build' "$(field "$tasks" '.[0].name')"

result=$(@ Tools::Mise runTask: build)
assert_argv "runTask: runs the task" run build
assert_eq "runTask: succeeds" 'success' "$(field "$result" .outcome)"
assert_eq "runTask: names the command" 'run' "$(field "$result" .command)"

result=$(@ Tools::Mise runTask: test args: '["--filter","unit"]' inDirectory: "$TEST_TMP/proj")
assert_argv "runTask:args:inDirectory: passes everything exactly" -C "$TEST_TMP/proj" run test --filter unit

env_json=$(@ Tools::Mise env)
assert_argv "env asks for JSON" env --json
assert_eq "env returns the variable map" 'postgres://localhost/app' "$(field "$env_json" .DATABASE_URL)"
assert_eq "envValue: reads one variable" 'postgres://localhost/app' "$(@ Tools::Mise envValue: DATABASE_URL)"
assert_eq "envValue: is empty when absent" '' "$(@ Tools::Mise envValue: MISSING_VAR)"
assert_eq "envFor: returns shell activation" 'export DATABASE_URL=postgres://localhost/app' "$(@ Tools::Mise envFor: bash)"
assert_argv "envFor: passes the shell" env --shell bash

config=$(@ Tools::Mise config)
assert_argv "config lists config files as JSON" config ls --json
assert_eq "config returns records" 'mise.toml' "$(field "$config" '.[0].path')"
settings=$(@ Tools::Mise settings)
assert_argv "settings asks for JSON" settings --json

result=$(@ Tools::Mise trust: "$TEST_TMP/proj/mise.toml")
assert_argv "trust: passes the path" trust "$TEST_TMP/proj/mise.toml"
result=$(@ Tools::Mise doctor)
assert_argv "doctor runs" doctor
assert_eq "doctor succeeds" 'success' "$(field "$result" .outcome)"

export FAKE_MISE_SCENARIO=failure
result=$(@ Tools::Mise exec: 'node@22' argv: '["node","bad.js"]')
assert_eq "failed exec is a command_error" 'command_error' "$(field "$result" .outcome)"
assert_eq "failed exec preserves the child status" '3' "$(field "$result" .exit_code)"
assert_eq "failed exec keeps stderr" 'command failed' "$(field "$result" '.stderr | rtrimstr("\n")')"
result=$(@ Tools::Mise runTask: build)
assert_eq "failed task is a task_error" 'task_error' "$(field "$result" .outcome)"
assert_eq "failed task preserves the task status" '2' "$(field "$result" .exit_code)"
assert_eq "succeeded: is false for failures" 'false' "$(@ Tools::Mise succeeded: "$result")"

# Hide the tested executable, but keep jq for envelope construction/assertions.
# On macOS jq is installed by Homebrew rather than in /usr/bin.
mkdir -p "$TEST_TMP/dependencies"
ln -s "$(command -v jq)" "$TEST_TMP/dependencies/jq"
PATH="$TEST_TMP/dependencies:/usr/bin:/bin"
hash -r
missing=$(@ Tools::Mise runTask: build)
assert_eq "missing mise has a distinct outcome" 'missing_tool' "$(field "$missing" .outcome)"
assert_eq "missing mise uses command-not-found status" '127' "$(field "$missing" .exit_code)"
if grep -Fq 'curl https://mise.run | sh' <(field "$missing" .stderr); then
    pass "missing mise result carries install guidance"
else
    fail "missing mise result carries install guidance"
fi
assert_eq "missing mise current: is empty" '' "$(@ Tools::Mise current: node)"
assert_eq "missing mise version" 'not installed' "$(@ Tools::Mise version)"
PATH="$ORIGINAL_PATH"
export PATH
hash -r

echo ""
echo "Passed: $PASSED, Failed: $FAILED"
[[ $FAILED -eq 0 ]]
