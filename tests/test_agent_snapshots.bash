#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
@ AgentQueue ensureSchema
session=$(@ AgentSession new)
other=$(@ AgentSession new)
for state in pending pending blocked failed uncertain processed; do
    id="agentdelivery_fixture_${state}_$RANDOM"
    db_put "$id" "$(jq -cn --arg s "$session" --arg state "$state" '{class:"AgentDelivery",session:$s,state:$state}')"
done
db_put agentdelivery_other "$(jq -cn --arg s "$other" '{class:"AgentDelivery",session:$s,state:"pending"}')"
db_put agentrun_old "$(jq -cn --arg s "$session" '{class:"AgentRun",session:$s,state:"failed",error:"earlier failure"}')"
db_put agentrun_latest "$(jq -cn --arg s "$session" '{class:"AgentRun",session:$s,state:"running",error:""}')"
_db_sql "WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i+1 FROM n WHERE i<1000)
  INSERT INTO instances(id,data) SELECT 'message_unrelated_'||i,json_object('class','Message','body','unrelated') FROM n;"

# Exercise the public query and inspect its real plan on the same database.
definition=$(declare -f _db_sql)
eval "${definition/_db_sql ()/_snapshot_original_sql ()}"
plan="$TMPDIR/snapshot-plan"
_db_sql() {
    if [[ "${1:-}" == 'SELECT json_object('* ]]; then
        _snapshot_original_sql "EXPLAIN QUERY PLAN $1" > "$plan"
    fi
    _snapshot_original_sql "$@"
}
snapshot=$(@ "$session" snapshot)
jq -e --arg id "$session" '.id==$id and .pending==2 and .questions==1 and .stalled==2
  and .run=="agentrun_latest" and .activity=="running" and .error=="earlier failure"' <<< "$snapshot"
[[ $(rg -c 'SEARCH [dr] USING INDEX' "$plan") == 6 ]]
! rg -q 'SCAN [dr]( |$)' "$plan"
[[ $(@ "$session" deliveryCountIn: '["failed","uncertain"]') == 2 ]]
printf 'PASS: snapshot values, ordering, isolation and six indexed subqueries\n'
batch=$(@ AgentSession snapshotsFor: "$other"$'\n'"$session")
jq -se --arg first "$other" --arg second "$session" 'map(.id)==[$first,$second] and .[1].pending==2 and .[0].pending==1' <<< "$batch" >/dev/null
dir=$(mktemp -d)
records=$(@ AgentBrowser sessionRecordsFor: "$other"$'\n'"$session" in: "$dir")
jq -se --arg first "$other" --arg second "$session" 'map(.id)==[$first,$second] and all(.[]; .schema_version==1 and .kind=="agent")' <<< "$records" >/dev/null
[[ $(cat "$dir/$session.txt") == "$(@ AgentBrowser describeSession: "$snapshot")" ]]
[[ -z $(@ AgentBrowser sessionRecordsFor: '' in: "$dir") ]]
rm -rf "$dir"
printf 'PASS: batch snapshot order, preview parity and empty browser\n'
