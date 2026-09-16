#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_WORKSTATION_FIXTURES=1
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
sub=$(cat schemas/workstation/v1/fixtures/EventSubscription.valid.json)
event=$(jq -cn '{schema_version:1,coordinate:{subscription:"eventsubscription_fixture",streamName:"workstation.fixture.v1",partition:"default",offset:3},groupKey:"opaque",display:{title:"safe",summary:"bounded"}}')
input=$(jq -cn --argjson s "$sub" --argjson e "$event" '{subscription:$s,records:[$e,$e]}')
[[ $(@ Workstation::EventSourceAdapter forKind: fixture) == Workstation::FixtureEventSourceAdapter ]]
[[ $(@ Workstation::FixtureEventSourceAdapter consumerFor: "$sub") == workstation/eventsubscription_fixture ]]
[[ $(@ Workstation::FixtureEventSourceAdapter read: "$input" limit: 1 | jq length) == 1 ]]
[[ $(@ Workstation::FixtureEventSourceAdapter groupKeyFor: "$event") == opaque ]]
@ Workstation::FixtureEventSourceAdapter displayFor: "$event" | jq -e 'keys==["summary","title"]' >/dev/null
for selector in kind validateSubscription: consumerFor: read: normalize: groupKeyFor: displayFor:; do
 if @ Workstation::EventSourceAdapter "$selector" '{}' '{}' >/dev/null 2>&1; then exit 1; fi
done
for limit in 0 -1 1001 '1; touch forbidden'; do
 if @ Workstation::FixtureEventSourceAdapter read: "$input" limit: "$limit" >/dev/null 2>&1; then exit 1; fi
done
for edit in '.enabled=false' '.schema_version=2' '.adapterKind="Stream"'; do
 bad=$(jq -c "$edit" <<<"$sub")
 if @ Workstation::FixtureEventSourceAdapter normalize: "$event" for: "$bad" >/dev/null 2>&1; then exit 1; fi
done
for edit in '.coordinate.offset=-1' '.coordinate.subscription="eventsubscription_other"' '.payload="secret"'; do
 if @ Workstation::FixtureEventSourceAdapter normalize: "$(jq -c "$edit" <<<"$event")" for: "$sub" >/dev/null 2>&1; then exit 1; fi
done
badinput=$(jq -c '.records[0].coordinate.offset=-1' <<<"$input")
if @ Workstation::FixtureEventSourceAdapter read: "$badinput" limit: 1 >/dev/null 2>&1; then exit 1; fi
export TRASHTALK_WORKSTATION_FIXTURES=0
if @ Workstation::FixtureEventSourceAdapter read: "$input" limit: 1 >/dev/null 2>&1; then exit 1; fi
if @ Workstation::EventSourceAdapter forKind: '$(touch forbidden)' >/dev/null 2>&1; then exit 1; fi
for class in Workstation::EventSubscription Workstation::Attention Message Agent::Delivery Agent::Run Stream; do [[ $(@ Store countByClass: "$class") == 0 ]]; done
echo 'PASS: closed fixture adapter bounded normalized envelopes and no persistence'
