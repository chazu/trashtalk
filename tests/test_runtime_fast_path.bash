#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Public dispatch and diagnostic contracts, with isolated persistent state.
set -uo pipefail
export LC_ALL=C
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_TMP=$(mktemp -d)
export SQLITE_JSON_DB="$TEST_TMP/instances.db"
unset TRASH_SESSION_ID
source "$PROJECT_DIR/lib/trash.bash" >"$TEST_TMP/start.out" 2>"$TEST_TMP/start.err"
trap '_env_cleanup; rm -rf "$TEST_TMP"' EXIT
failed=0
check() {
    if "$@"; then printf 'PASS: %s\n' "$*"; else printf 'FAIL: %s\n' "$*"; failed=$((failed + 1)); fi
}
check test ! -s "$TEST_TMP/start.out"
check test ! -s "$TEST_TMP/start.err"

@ Json new >"$TEST_TMP/instance" 2>"$TEST_TMP/normal.err"
check test ! -s "$TEST_TMP/normal.err"
TRASHTALK_LOG_LEVEL=debug @ Json new >"$TEST_TMP/debug.out" 2>"$TEST_TMP/debug.err"
check test -s "$TEST_TMP/debug.err"
TRASHTALK_LOG_LEVEL=error msg_info hidden >"$TEST_TMP/log.out" 2>"$TEST_TMP/log.err"
check test "$?" = 0
check test ! -s "$TEST_TMP/log.out"
check test ! -s "$TEST_TMP/log.err"
@ Console warn: 'visible warning' >"$TEST_TMP/warn.out" 2>"$TEST_TMP/warn.err"
check test ! -s "$TEST_TMP/warn.out"
check test -s "$TEST_TMP/warn.err"

# A constant class method must not touch persistence. Unlike a timing assertion,
# this remains deterministic on a loaded CI host.
original_db_get=$(declare -f db_get)
db_get() { printf '%s\n' "$1" >>"$TEST_TMP/db.calls"; return 1; }
@ Counter description >"$TEST_TMP/description"
check test "$(<"$TEST_TMP/description")" = 'A simple counter'
check test ! -e "$TEST_TMP/db.calls"
eval "$original_db_get"

counter=$(@ Counter new)
original_class_lookup=$(declare -f _get_instance_class)
eval "${original_class_lookup/_get_instance_class ()/_test_class_lookup ()}"
_get_instance_class() {
    printf '%s\n' "$1" >>"$TEST_TMP/class.calls"
    _test_class_lookup "$@"
}
@ "$counter" increment >"$TEST_TMP/increment"
check test "$(<"$TEST_TMP/increment")" = 1
check test "$(wc -l <"$TEST_TMP/class.calls" | tr -d ' ')" = 1
check test "$__" = 1

# Unloaded custom identifiers still resolve from Store, including uppercase IDs.
db_put CustomIdentifier '{"class":"Counter","value":9,"step":1,"_vars":["value","step"]}'
check test "$(@ CustomIdentifier getValue)" = 9
@ DoesNotExist missing >"$TEST_TMP/missing.out" 2>"$TEST_TMP/missing.err"
check test "$?" != 0
check test ! -s "$TEST_TMP/missing.out"
check test -s "$TEST_TMP/missing.err"

# The facade must construct a value in one serializer without creating objects.
before=$(_db_sql 'SELECT count(*) FROM instances;')
jq() { printf 'jq\n' >>"$TEST_TMP/json.calls"; command jq "$@"; }
@ Agent contextFor: 'quote " and newline' workingDirectory: "$TEST_TMP" status: 7 lastResult: '{}' >"$TEST_TMP/context"
check test "$?" = 0
check test "$(wc -l <"$TEST_TMP/json.calls" | tr -d ' ')" = 1
check command jq -e '.schema_version == 1 and .last_status == 7 and .last_result == "{}"' "$TEST_TMP/context"
check test "$(_db_sql 'SELECT count(*) FROM instances;')" = "$before"
exit "$((failed > 0))"
