#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
honker_available || { echo 'SKIP: Honker not installed'; exit 0; }
honker_bootstrap
sub=$(jq -c --arg d "$(@ Workstation::Schema digest)" '.adapterKind="command-receipt"|.streamName="workstation.command-receipts.v1"|.schemaDigest=$d|.filter={exitNot:0}' schemas/workstation/v1/fixtures/EventSubscription.valid.json)
[[ $(@ Workstation::EventSourceAdapter forKind: command-receipt) == Workstation::CommandReceiptSourceAdapter ]]
@ Workstation::EventSubscription createFrom: "$sub" >/dev/null
consumer=$(@ Workstation::CommandReceiptSourceAdapter consumerFor: "$sub")
fixture=$(cat schemas/workstation/v1/fixtures/CommandReceipt.valid.json)
record=$(jq -cn --argjson p "$fixture" '{offset:42,topic:"workstation.command-receipts.v1",key:"partition-a",payload:($p|tojson)}')
envelope=$(@ Workstation::CommandReceiptSourceAdapter normalize: "$record" for: "$sub")
jq -e '.coordinate.offset==42 and .coordinate.partition=="partition-a" and .coordinate.subscription=="eventsubscription_fixture" and (.groupKey|length)==64' <<<"$envelope" >/dev/null
[[ $(@ Workstation::CommandReceiptSourceAdapter normalize: "$(jq -c '.payload|= (fromjson|.exitCode=0|tojson)' <<<"$record")" for: "$sub") == '' ]]
other=$(@ Workstation::CommandReceiptSourceAdapter normalize: "$(jq -c '.payload|=(fromjson|.workspace="/other"|tojson)' <<<"$record")" for: "$sub")
[[ $(jq -r .groupKey <<<"$envelope") != $(jq -r .groupKey <<<"$other") ]]
[[ $(@ Workstation::CommandReceiptSourceAdapter normalize: "$record" for: "$sub") == "$envelope" ]]
for change in '.payload="not-json"' '.offset=-1' '.topic="other"' '.key="bad partition"' '.payload|=(fromjson|.schema_version=2|tojson)' '.payload|=(fromjson|.secret="dont-echo"|tojson)'; do
 if @ Workstation::CommandReceiptSourceAdapter normalize: "$(jq -c "$change" <<<"$record")" for: "$sub" >"$TMPDIR/invalid" 2>&1; then echo "FAIL: accepted invalid record"; exit 1; fi
 ! grep -q dont-echo "$TMPDIR/invalid"
done
for change in '.enabled=false' '.streamName="wrong"' '.consumerName="unsafe"' '.filter={unknown:1}' '.filter.exitNot="0"' '.targetIdentity="not-an-identity"'; do
 if @ Workstation::CommandReceiptSourceAdapter consumerFor: "$(jq -c "$change" <<<"$sub")" >/dev/null 2>&1; then exit 1; fi
done
@ Workstation::CommandReceipt publish: "$fixture"
@ Workstation::CommandReceipt publish: "$fixture"
[[ $(@ Workstation::CommandReceiptSourceAdapter read: "$consumer" limit: 1 | jq length) == 1 ]]
for limit in 0 9 -1 '1;false'; do
 if @ Workstation::CommandReceiptSourceAdapter read: "$consumer" limit: "$limit" >/dev/null 2>&1; then exit 1; fi
done
[[ $(@ "$consumer" offset) == 0 ]]
[[ $(@ Store countByClass: Workstation::Attention) == 0 && $(@ Store countByClass: Message) == 0 && $(@ Store countByClass: Agent::Delivery) == 0 ]]
# Atomic, insert-only initial position, including offset zero for an empty log.
@ "$consumer" initializeFrom: from-now
[[ $(@ "$consumer" offset) == 2 ]]
@ Workstation::CommandReceipt publish: "$fixture"
@ "$consumer" initializeFrom: from-now
[[ $(@ "$consumer" offset) == 2 ]]
@ "$consumer" acknowledgeThrough: 3
@ "$consumer" acknowledgeThrough: 1
[[ $(@ "$consumer" offset) == 3 ]]
empty=$(@ Stream named: empty consumer: empty)
@ "$empty" initializeFrom: from-now
@ "$empty" publish: '{}'
@ "$empty" initializeFrom: from-now
[[ $(@ "$empty" offset) == 0 ]]
echo 'PASS: closed receipt adapter, filters, coordinates, bounded reads, stable groups, no domain side effects and native consumer registration'
