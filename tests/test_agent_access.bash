#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=access-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN
db_init
mkdir "$tmp/workspace"
session=$(@ Gusgus sessionFor: "$tmp/workspace")
identity=$(@ "$session" identity)
context=$(jq -cn --arg session "$session" '{session:$session,window:400}')
passed=0
check() {
    [[ "$2" == "$3" ]] || { printf 'FAIL: %s\nexpected: %s\nactual: %s\n' "$1" "$2" "$3"; exit 1; }
    printf 'PASS: %s\n' "$1"
    passed=$((passed+1))
}
rejects() {
    local label="$1"; shift
    if "$@" >"$tmp/result" 2>"$tmp/error"; then
        printf 'FAIL: %s accepted\n' "$label"; exit 1
    fi
    check "$label emits no successful result" '' "$(cat "$tmp/result")"
}
check 'owner can inspect its session' "$session" "$(@ AgentAccess session: "$session")"
check 'owner can focus an open session' "$session" "$(@ AgentAccess liveSession: "$session")"
@ "$session" pause >/dev/null
check 'paused session remains focusable' "$session" "$(@ AgentAccess liveSession: "$session")"
check 'focus validation does not resume' paused "$(@ "$session" lifecycleState)"
before=$(db_get "$session")
@ AgentFocus frameFor: "$context" >/dev/null
check 'frame leaves durable session unchanged' "$before" "$(db_get "$session")"
@ "$session" close >/dev/null
rejects 'closed session cannot open a live view' @ AgentAccess liveSession: "$session"
check 'closed history remains inspectable by owner' "$session" "$(@ AgentAccess session: "$session")"
@ "$session" reopen >/dev/null

# Prime this shell with a snapshot, then change only the durable owner.
@ "$identity" reload >/dev/null
@ Store patch: "$identity" with: '{"owner":"other-owner"}' >/dev/null
rejects 'stale identity cache grants no authority' @ AgentAccess session: "$session"
rejects 'foreign session cannot open the applet' @ AgentFocus open: "$session"
rejects 'foreign session cannot refresh its transcript' @ AgentFocus frameFor: "$context"
rejects 'foreign session cannot pause through focus' @ AgentFocus perform: pause_session session: "$session" body: '' run: ''
check 'unauthorized pause leaves lifecycle unchanged' open "$(db_get "$session" | jq -r .lifecycleState)"
frame='{"schema_version":1,"request_id":1,"intent":"load_older"}'
rejects 'retained view context cannot load foreign history' @ AgentFocus handleFrame: "$frame" context: "$context"
@ Store patch: "$identity" with: '{"owner":"access-owner","enabled":"false"}' >/dev/null
rejects 'disabled identity cannot open live focus' @ AgentAccess liveSession: "$session"
@ Store patch: "$identity" with: '{"enabled":"true","owner":""}' >/dev/null
rejects 'ownerless identity does not borrow current user authority' @ AgentAccess session: "$session"
@ Store patch: "$identity" with: '{"owner":"access-owner"}' >/dev/null
_store_tx_before_commit() {
    @ Store patch: "$identity" with: '{"owner":"other-owner"}' >/dev/null
}
rejects 'ownership change during validation conflicts' @ AgentAccess session: "$session"
unset -f _store_tx_before_commit
@ Store patch: "$identity" with: '{"owner":"access-owner"}' >/dev/null
export TRASHTALK_RUN_TOKEN=not-human
rejects 'run token cannot act as a human viewer' @ AgentAccess session: "$session"
unset TRASHTALK_RUN_TOKEN
rejects 'missing session cannot authorize' @ AgentAccess session: agentsession_missing
rejects 'identity ID is not a session' @ AgentAccess session: "$identity"
# A view admission observes membership too, including deletion during validation.
members=$(_db_sql "SELECT json_group_array(json_object('identity',identity_id,'scope',scope_key,'session',session_id,'revision',policy_revision)) FROM agent_session_memberships WHERE identity_id='$identity';")
_store_tx_before_commit() { _db_sql "DELETE FROM agent_session_memberships WHERE identity_id='$identity';"; }
rejects 'membership change during validation conflicts' @ AgentAccess liveSession: "$session"
unset -f _store_tx_before_commit
_db_sql "INSERT INTO agent_session_memberships SELECT json_extract(value,'$.identity'),json_extract(value,'$.scope'),json_extract(value,'$.session'),json_extract(value,'$.revision') FROM json_each('$(_db_escape "$members")');"
check 'restored membership admits current session' "$session" "$(@ AgentAccess liveSession: "$session")"
# The lighter path and ordinary guarded DSL validator agree on the live fixture.
check 'snapshot admission matches transaction validator' "$(@ Store transaction: AgentAccess sending: validateLiveSession: with: "$session")" "$(@ AgentAccess liveSession: "$session")"
printf '%d agent access checks passed\n' "$passed"
