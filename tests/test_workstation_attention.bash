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
subdoc=$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/EventSubscription.valid.json)
@ EventSubscription createFrom: "$subdoc" >/dev/null
doc=$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/Attention.valid.json)
a=$(@ Attention createFrom: "$doc")
@ "$a" acknowledge >/dev/null
@ "$a" reload
[[ $(@ "$a" state) == acknowledged ]]
@ "$a" snoozeUntil: '2099-01-01T00:00:00Z' >/dev/null
@ "$a" reload
[[ $(@ "$a" state) == snoozed ]]
@ "$a" resolveWithNote: investigated >/dev/null
@ "$a" reload
[[ $(@ "$a" state) == resolved ]]
@ "$a" reopen >/dev/null
@ "$a" suppress: duplicate >/dev/null
@ "$a" reload
[[ $(@ "$a" state) == suppressed ]]
@ "$a" reopen >/dev/null
for state in open acknowledged snoozed resolved suppressed; do
 for target in open acknowledged snoozed resolved suppressed; do
  # Fixture setup only, enumerate every source state without conflating setup and transitions.
  @ Store patch: "$a" with: "{\"state\":\"$state\",\"snoozeUntil\":\"\"}" >/dev/null
  until=''; [[ $target != snoozed ]] || until='2099-01-01T00:00:00Z'
  allowed=$(@ Attention allowedTransitions | jq -r --arg p "$state:$target" 'index($p)!=null')
  if @ "$a" transitionTo: "$target" until: "$until" note: test >/dev/null 2>&1; then result=true; else result=false; fi
  [[ $result == "$allowed" ]] || { echo "FAIL transition $state:$target"; exit 1; }
  @ "$a" reload
  expected=$state; [[ $allowed != true ]] || expected=$target
  [[ $(@ "$a" state) == "$expected" ]]
 done
done
@ Store patch: "$a" with: '{"state":"open","snoozeUntil":""}' >/dev/null
for timestamp in '2020-01-01T00:00:00Z' '2099-02-30T00:00:00Z' '2099-01-01'; do
 if @ "$a" snoozeUntil: "$timestamp" >/dev/null 2>&1; then exit 1; fi
done
if @ "$a" resolveWithNote: ' ' >/dev/null 2>&1; then exit 1; fi
if @ "$a" suppress: '' >/dev/null 2>&1; then exit 1; fi
for class in Message AgentDelivery AgentRun AgentSession Stream; do
 [[ $(@ Store countByClass: "$class") == 0 ]]
done
echo 'PASS: Attention complete transition table reload validation and no effects'
exit 0
