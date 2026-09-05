#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
fixture_dir="$TRASHTALK_DIR/tests/isolation-fixtures"
mkdir -p "$fixture_dir"
printf 'parent database sentinel\n' > "$SQLITE_JSON_DB"
cp "$TRASHDIR/Counter.trash" "$TRASHTALK_DIR/counter-before"
cat > "$fixture_dir/test_one.bash" <<'SH'
set -euo pipefail
test "$TRASHTALK_TEST_ISOLATED" = 1
test ! -e "$SQLITE_JSON_DB"
test ! -e "$TRASHDIR/isolation-sentinel"
printf 'owned state\n' > "$SQLITE_JSON_DB"
printf 'owned source\n' > "$TRASHDIR/isolation-sentinel"
printf 'changed\n' > "$TRASHDIR/Counter.trash"
sleep .1
test "$(cat "$SQLITE_JSON_DB")" = 'owned state'
SH
cp "$fixture_dir/test_one.bash" "$fixture_dir/test_two.bash"
TRASH_TEST_JOBS=2 bash "$TRASHTALK_DIR/lib/run-tests.sh" "$fixture_dir" > "$TRASHTALK_DIR/isolation.log"
rg -q 'Passed: 2, Failed: 0, Timed out: 0' "$TRASHTALK_DIR/isolation.log"
test "$(cat "$SQLITE_JSON_DB")" = 'parent database sentinel'
test ! -e "$TRASHDIR/isolation-sentinel"
cmp "$TRASHDIR/Counter.trash" "$TRASHTALK_DIR/counter-before"
printf 'exit 7\n' > "$fixture_dir/test_fail.bash"
if bash "$TRASHTALK_DIR/lib/run-tests.sh" "$fixture_dir" > "$TRASHTALK_DIR/failure.log"; then exit 1; fi
rg -q 'Failed: 1' "$TRASHTALK_DIR/failure.log"
echo 'PASS: concurrent test checkouts isolate databases/sources and propagate failures'
