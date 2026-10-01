#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
export TRASHTALK_USER=closed-requester-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
field() { db_get "$1" | jq -r "$2"; }
coordinator=$(@ Gusgus sessionFor: "$TRASHTALK_DIR")
mapfile -t pair < <(@ Agent::Run startFor: "$coordinator" profile: shell)
parent=${pair[0]}
@ "$parent" transitionTo: running >/dev/null
export TRASHTALK_RUN_TOKEN=${pair[1]}
a=$(@ Agent::Run delegate: 'Outlive the requesting conversation' criteria: evidence key: closed)
unset TRASHTALK_RUN_TOKEN
@ "$parent" finishWith: succeeded outcome: '{}' error: '' >/dev/null
@ "$coordinator" close >/dev/null
[[ $(field "$coordinator" .lifecycleState) == closed ]]
# The outcome cannot go to a closed conversation; the owner must still be able to cancel.
@ "$a" cancel: 'Cancelled after requester closed' >/dev/null
[[ $(field "$a" .state) == cancelled ]]
echo 'PASS: owner cancels an Assignment whose requesting conversation is closed'
[[ $(field "message_${a}_outcome" .to) == closed-requester-owner ]]
echo 'PASS: outcome falls back to the owner inbox'
