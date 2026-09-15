#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Instance creation must not spawn a tr process for the identifier prefix and
# must not regenerate accessors a compiled artifact already defines.
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }

mkdir -p "$TMPDIR/bin"
cat > "$TMPDIR/bin/tr" <<'TR'
#!/usr/bin/env bash
printf 'tr\n' >> "$TR_LOG"
exec /usr/bin/tr "$@"
TR
chmod +x "$TMPDIR/bin/tr"
export TR_LOG="$TMPDIR/tr.calls"
export PATH="$TMPDIR/bin:$PATH"

: > "$TR_LOG"
counter=$(@ Counter new)
check 'creating an instance spawns no tr process' 0 "$(wc -l < "$TR_LOG" | tr -d ' ')"
check 'identifier prefix is the lowercase class name' counter "${counter%%_*}"
qualified=$(@ Runtime generateId: 'Kube::Cluster')
check 'namespaced prefix lowercases and joins the package' kube_cluster "${qualified%_*}"
check 'prefix helper agrees' myapp_counter "$(_to_instance_prefix 'MyApp::Counter')"

# Generated accessors persist in the creating shell (raw methods rely on the
# unary setters), so the first creation generates them once and later
# creations in the same shell must not re-evaluate them.
accessor_log="$TMPDIR/accessors"
(
    _generate_accessor() { printf '%s\n' "$1" >> "$accessor_log"; eval "__Counter__set${1^}() { :; }"; }
    _create_instance Counter counter_fixture_one >/dev/null
    _create_instance Counter counter_fixture_two >/dev/null
    _create_instance Counter counter_fixture_three >/dev/null
)
check 'accessors are generated once per shell, not per creation' $'value\nstep' "$(cat "$accessor_log")"
: > "$accessor_log"
(
    __Kid__superclass=Counter
    __Kid__instanceDefaults='{"vars":["extra"],"values":{"extra":"x"}}'
    __Kid__instanceNames='extra'
    _generate_accessor() { printf '%s\n' "$1" >> "$accessor_log"; eval "__Kid__set${1^}() { :; }"; }
    _create_instance Kid kid_fixture >/dev/null
    _create_instance Kid kid_fixture_two >/dev/null
)
check 'inherited accessors are generated once as well' $'extra\nvalue\nstep' "$(cat "$accessor_log")"
check 'inherited defaults still merge' '["extra","value","step"]' "$(db_get kid_fixture | command jq -c '._vars')"
check 'child defaults still win' x "$(db_get kid_fixture | command jq -r '.extra')"
future=$(@ Future for: 'printf ok')
check 'raw creation still reaches the unary setters' 'printf ok' "$(_env_get "$future" | command jq -r '.command')"
echo "=== $passed instance creation checks passed ==="
