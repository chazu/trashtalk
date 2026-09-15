#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user TRASHTALK_WORKSTATION_FIXTURES=1
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
digest=$(@ Workstation::Schema digest)
@ Workstation::EventSubscription createFrom: "$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/EventSubscription.valid.json)" >/dev/null
a=$(@ Workstation::Attention createFrom: "$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/Attention.valid.json)")
stream=$(@ eventsubscription_fixture streamName)
coordinate() { jq -cn --arg s "$stream" --argjson o "$1" '{subscription:"eventsubscription_fixture",streamName:$s,partition:"default",offset:$o}'; }
# Test-only callbacks use the public Store messages.
@ Store info >/dev/null 2>&1 || true
__Store__class__fixtureRecord() { @ Store recordCoordinate: "$1" forAttention: "$a"; }
__Store__class__fixtureRollback() { @ Store recordCoordinate: "$1" forAttention: "$a" >/dev/null || return; return 1; }
__Store__class__fixtureTwice() { @ Store recordCoordinate: "$1" forAttention: "$a" >/dev/null || return; @ Store recordCoordinate: "$1" forAttention: "$a"; }
__Store__class__fixtureRange() { @ Store coordinateRange: "$1" through: 100 limit: 2; }
if @ Store recordCoordinate: "$(coordinate 10)" forAttention: "$a" >/dev/null 2>&1; then exit 1; fi
[[ $(@ Store getInstance: "$a" | jq .eventCount) == 0 ]]
for selector in claimedCoordinate:; do
 if @ Store "$selector" "$(coordinate 10)" >/dev/null 2>&1; then exit 1; fi
done
# Both callbacks stage against the same empty key before either commits.
barrier=$(mktemp -d)
_store_tx_before_commit() {
 [[ ${_STORE_READONLY:-0} == 0 ]] || return 0
 touch "$barrier/$BASHPID"
 for ((i=0;i<200;i++)); do
  [[ $(find "$barrier" -type f | wc -l) -ge 2 ]] && return 0
  sleep .05
 done
 return 1
}
@ Store transaction: Store sending: fixtureRecord with: "$(coordinate 10)" replaying: true >"$barrier-result1" & p1=$!
@ Store transaction: Store sending: fixtureRecord with: "$(coordinate 10)" replaying: true >"$barrier-result2" & p2=$!
wait "$p1"; wait "$p2"
unset -f _store_tx_before_commit
[[ $(cat "$barrier-result1") == "$a" && $(cat "$barrier-result2") == "$a" ]]
[[ $(@ Store getInstance: "$a" | jq .eventCount) == 1 ]]
if @ Store transaction: Store sending: fixtureRollback with: "$(coordinate 11)" >/dev/null 2>&1; then exit 1; fi
[[ -z $(@ Store transaction: Store sending: claimedCoordinate: with: "$(coordinate 11)") ]]
[[ $(@ Store getInstance: "$a" | jq .eventCount) == 1 ]]
for offset in 11 8 9; do @ Store transaction: Store sending: fixtureTwice with: "$(coordinate "$offset")" >/dev/null; done
@ Store getInstance: "$a" | jq -e '.eventCount==4 and .firstCoordinate.offset==8 and .lastCoordinate.offset==11 and .state=="open"' >/dev/null
[[ $(@ Store transaction: Store sending: fixtureRange with: "$(coordinate 0)" | jq length) == 2 ]]
for change in '.subscription="eventsubscription_other"' '.streamName="other"'; do
 [[ -z $(@ Store transaction: Store sending: claimedCoordinate: with: "$(coordinate 10 | jq -c "$change")") ]]
done
[[ $(_db_sql 'select count(*) from workstation_coordinates') == 4 ]]
if _db_sql "INSERT INTO workstation_coordinates SELECT * FROM workstation_coordinates LIMIT 1" 2>/dev/null; then exit 1; fi
echo 'PASS: concurrent replay, staged duplicates, rollback, bounded range and coordinate isolation'
