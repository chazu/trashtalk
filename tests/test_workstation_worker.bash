#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user
export TRASHTALK_RUN_DIR="$TRASHTALK_DIR/run"
indicator() { cat "$TRASHTALK_RUN_DIR/attention"; }
command -v cue >/dev/null || { echo 'SKIP: CUE not installed'; exit 0; }
honker_available || { echo 'SKIP: Honker not installed'; exit 0; }
honker_bootstrap
sub=$(jq -c --arg d "$(@ Workstation::Schema digest)" '.adapterKind="command-receipt"|.streamName="workstation.command-receipts.v1"|.schemaDigest=$d|.filter={exitNot:0}|.debounceSeconds=3600' schemas/workstation/v1/fixtures/EventSubscription.valid.json)
@ Workstation::EventSubscription createFrom: "$sub" >/dev/null
consumer=$(@ Workstation::CommandReceiptSourceAdapter consumerFor: "$sub")
stream=$(@ Stream named: workstation.command-receipts.v1)
fixture=$(cat schemas/workstation/v1/fixtures/CommandReceipt.valid.json)
publish() { @ Workstation::CommandReceipt publish: "$fixture" >/dev/null; }
count() { @ Store countByClass: "$1"; }
data() { @ Store getInstance: "$1"; }
assert_count() { [[ $(data "$a" | jq -r .eventCount) == "$1" && $(data "$m" | jq -r .attentionEventCount) == "$1" ]]; }
# Real public producer -> named Stream -> supervised worker tick -> local Inbox.
bin/trash-receipt --publish "$fixture"
@ Agent::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 1 && $(count Workstation::Attention) == 1 && $(count Message) == 1 ]]
a=$(@ Workstation::Attention findAll); m=$(data "$a" | jq -r .message)
inbox=$(@ Inbox named: local-user)
[[ $(@ "$inbox" attentionCount) == 1 && $(indicator) == '!1' ]]
assert_count 1
data "$m" | jq -e --arg a "$a" '.to=="local-user" and .attention==$a and .kind=="alert" and .dispatchMode=="manual" and .attentionFirstCoordinate.offset==1 and .attentionLastCoordinate.offset==1' >/dev/null
# Marking read and archiving are not acknowledgements or resolution.
@ "$m" markViewed >/dev/null
@ "$m" archive >/dev/null
[[ $(@ "$inbox" attentionCount) == 1 ]]
# Replay after commit-before-ack leaves both documents byte-for-byte unchanged.
before=$(data "$a"); message_before=$(data "$m")
@ "$consumer" ack: 0 >/dev/null
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(data "$a") == "$before" && $(data "$m") == "$message_before" && $(@ "$consumer" offset) == 1 ]]
# Debounced grouping updates one message, retaining status and first coordinate.
hint=$(data "$m" | jq -r .attentionHintAt)
publish
@ Workstation::Worker tick >/dev/null || exit 1
assert_count 2
[[ $(count Message) == 1 && $(data "$m" | jq -r .attentionHintAt) == "$hint" ]]
# Local controls, paused dispatch independent from consumption, no agent effects.
@ "$m" acknowledgeAttention >/dev/null
[[ $(@ "$inbox" attentionCount) == 0 && -z $(indicator) ]]
publish; @ Workstation::Worker tick >/dev/null || exit 1; assert_count 3
[[ $(data "$a" | jq -r .state) == acknowledged ]]
@ "$m" snoozeAttentionUntil: '2099-01-01T00:00:00Z' >/dev/null
publish; @ Workstation::Worker tick >/dev/null || exit 1; assert_count 4
[[ $(@ "$inbox" attentionCount) == 0 && $(data "$m" | jq -r .attentionHintAt) == "$hint" ]]
# Clock fixture: expired snooze is visible, but inspection does not mutate it.
@ Store patch: "$a" with: '{"snoozeUntil":"2020-01-01T00:00:00Z"}' >/dev/null
[[ $(@ "$inbox" attentionCount) == 1 ]]
@ "$m" inspectAttention | jq -e '.state=="snoozed" and .eventCount==4' >/dev/null
@ "$m" suppressAttention: noise >/dev/null
publish; @ Workstation::Worker tick >/dev/null || exit 1; assert_count 5
[[ $(@ "$inbox" attentionCount) == 0 && $(count Workstation::Attention) == 1 ]]
@ "$m" reopenAttention >/dev/null
[[ $(indicator) == '!1' ]]
@ "$m" resolveAttentionWithNote: fixed >/dev/null
[[ -z $(indicator) ]]
publish; @ Workstation::Worker tick >/dev/null || exit 1
[[ $(count Workstation::Attention) == 2 && $(count Message) == 2 && $(@ "$inbox" attentionCount) == 1 ]]
assert_count 5
# Valid filtered records advance without any domain write.
@ Workstation::CommandReceipt publish: "$(jq -c '.exitCode=0' <<<"$fixture")" >/dev/null
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 7 && $(count Workstation::Attention) == 2 ]]
# Ack failure after commit is replay safe.
@ Stream help >/dev/null
saved_ack=$(declare -f __Stream__acknowledgeThrough_)
__Stream__acknowledgeThrough_() { return 1; }
publish
if @ Workstation::Worker tick >/dev/null 2>&1; then echo 'FAIL: ack failure hidden'; exit 1; fi
[[ $(@ "$consumer" offset) == 7 ]]
snapshot=$(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Workstation::Attention','Message');")
eval "$saved_ack"
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 8 && $(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Workstation::Attention','Message');") == "$snapshot" ]]
# Failed domain creation must roll back claim, attention and message, and not ack.
saved_factory=$(declare -f __Message__class__forAttention_display_to_)
__Message__class__forAttention_display_to_() { _throw StoreError 'injected projection failure'; return 1; }
publish
if @ Workstation::Worker tick >/dev/null 2>&1; then echo 'FAIL: projection failure hidden'; exit 1; fi
[[ $(@ "$consumer" offset) == 8 && $(_db_sql "SELECT json_group_array(data) FROM instances WHERE class IN ('Workstation::Attention','Message');") == "$snapshot" ]]
eval "$saved_factory"
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 9 ]]
# Malformed record blocks later records, never mistaken for a filter match.
@ "$stream" publish: '{"schema_version":99,"secret":"not-for-diagnostics"}' >/dev/null
publish
if @ Workstation::Worker tick >"$TMPDIR/bad-record" 2>&1; then exit 1; fi
[[ $(@ "$consumer" offset) == 9 ]]
! grep -q not-for-diagnostics "$TMPDIR/bad-record"
# Disabled policy is not read or advanced. Explicit operator replay skip only.
@ eventsubscription_fixture disable: maintenance >/dev/null
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 9 ]]
@ "$consumer" ack: 10 >/dev/null
@ eventsubscription_fixture enable: repaired >/dev/null
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 11 ]]
# Bound each batch to four records.
for i in {1..5}; do publish; done
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 15 ]]
@ Workstation::Worker tick >/dev/null || exit 1
[[ $(@ "$consumer" offset) == 16 ]]
# No deliveries/outbox, execution, identities, sessions, or routing side effects.
for class in Agent::Delivery Agent::Run Agent::Session Agent::Identity AgentAssignment; do [[ $(count "$class") == 0 ]]; done
[[ $(_db_sql 'SELECT count(*) FROM agent_outbox;') == 0 ]]
[[ $(count Inbox) == 1 && $(count Stream) == 2 ]]
echo 'PASS: local receipt journey, replay, ack/transaction failures, lifecycle, filtering, batches and no agent effects'
