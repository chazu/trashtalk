#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
export TRASHTALK_USER=delegation-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
coordinator=$(@ Gusgus sessionFor: "$TRASHTALK_DIR")
mapfile -t pair < <(@ Agent::Run startFor: "$coordinator" profile: shell)
parent=${pair[0]}
@ "$parent" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN=${pair[1]}
a=$(@ Agent::Run delegate: first criteria: evidence key: first)
# Simulate another conversation owning the existing specialist's Assignment.
# This bypasses only the requesting conversation's one-child check in the fixture.
_db_sql "UPDATE instances SET data=json_set(data,'$.requesterSession','fixture_other_conversation') WHERE id='$a';"
if @ Agent::Run delegate: second criteria: evidence key: second >"$TMPDIR/result.log" 2>"$TMPDIR/rejection.log"; then
    echo 'FAIL: accepted work for a busy specialist' >&2
    exit 1
fi
grep -Fq "Specialist session already has an open Assignment: $a" "$TMPDIR/rejection.log"
if grep -q 'arithmetic syntax error' "$TMPDIR/rejection.log"; then
    cat "$TMPDIR/rejection.log" >&2
    exit 1
fi
[[ $(_db_sql "SELECT count(*) FROM instances WHERE class='Assignment';") == 1 ]]
[[ $(_db_sql "SELECT json_extract(data,'$.state') FROM instances WHERE id='$a';") == open ]]
[[ ! -s "$TMPDIR/result.log" ]]
echo 'PASS: busy specialist rejection names the Assignment without cascading arithmetic errors'
echo 'PASS: rejection preserves existing work and creates no new Assignment'
