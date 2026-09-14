#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user TRASHTALK_WORKSTATION_FIXTURES=1
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
digest=$(@ WorkstationSchema digest)
doc=$(jq -c --arg digest "$digest" '.schemaDigest=$digest' schemas/workstation/v1/fixtures/EventSubscription.valid.json)
sub=$(@ EventSubscription createFrom: "$doc")
[[ $sub == eventsubscription_fixture ]]
@ "$sub" reload
[[ $(@ "$sub" owner) == local-user ]]
[[ $(@ EventSubscription listByOwner: local-user | jq -r '.[0]') == "$sub" ]]
@ "$sub" resumeDispatch: 'manual resume'
@ "$sub" disable: 'manual disable'
@ "$sub" reload
[[ $(@ "$sub" revision) == 3 && $(@ "$sub" enabled) == false && $(@ "$sub" dispatchState) == enabled ]]
@ "$sub" enable: 'manual enable'
@ "$sub" pauseDispatch: 'manual pause'
@ "$sub" reload
[[ $(@ "$sub" revision) == 5 ]]
for op in duplicate owner adapter grouping consumer unknown; do
 bad=$doc
 case $op in
 owner) bad=$(jq -c '.owner="another"' <<<"$doc");;
 adapter) bad=$(jq -c '.adapterKind="$(touch forbidden)"' <<<"$doc");;
 grouping) bad=$(jq -c '.grouping="unknown"' <<<"$doc");;
 consumer) bad=$(jq -c '.consumerName="wrong"' <<<"$doc");;
 unknown) bad=$(jq -c '.secret="not accepted"' <<<"$doc");;
 esac
 if @ EventSubscription createFrom: "$bad" >/dev/null 2>&1; then echo "FAIL: $op"; exit 1; fi
done
if @ "$sub" pauseDispatch: redundant >/dev/null 2>&1; then exit 1; fi
if @ Store transaction: EventSubscription sending: createWithin: with: "$doc" >/dev/null 2>&1; then exit 1; fi
[[ $(@ Store countByClass: EventSubscription) == 1 ]]
@ WorkstationSchema ensureSchema
@ WorkstationSchema ensureSchema
counter=$(@ Counter new)
[[ $(@ Store getClass: "$counter") == Counter ]]
echo 'PASS: subscription create reload list transitions native validation and migration'
exit 0
