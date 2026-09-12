#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
tmp=$(cd "$tmp" && pwd -P)
mkdir "$tmp/one" "$tmp/two"
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_RUN_DIR="$tmp/runs" TRASHTALK_USER=workspace-owner
export TRASHTALK_GUSGUS_PROFILE=shell TRASHTALK_NO_AUTOTICK=1
db_init
check() { [[ "$2" == "$3" ]] || { echo "FAIL: $1 expected=$2 got=$3"; exit 1; }; echo "PASS: $1"; }
reject() { if "$@" >/dev/null 2>&1; then echo 'FAIL: expected rejection'; exit 1; fi; }
session=$(@ Gusgus sessionFor: "$tmp/one")
first=$(@ Inbox send: 'First directory' to: "session:$session" from: workspace-owner in: "$tmp/one")
second=$(@ Inbox send: 'Second directory' to: "session:$session" from: workspace-owner in: "$tmp/two")
check 'directory changes retain the current session' "$session" "$(@ Gusgus sessionFor: "$tmp/two")"
check 'message retains its execution context' "$tmp/two" "$(@ "$second" executionWorkspace)"
export TRASHTALK_SHELL_DRIVER='prompt=$(cat); ts="$TRASHTALK_DIR/bin/trash-send"; for delivery in $(printf "%s\n" "$prompt" | sed -n "s/^--- delivery //p"); do "$ts" AgentRun result: "$(pwd -P)" forDelivery: "$delivery" >/dev/null; "$ts" AgentRun settle: "$delivery" >/dev/null; done'
for attempt in {1..100}; do
    @ AgentWorker tickSession: "$session" >/dev/null
    [[ "$(@ "$session" pendingCount)" != 0 || -n "$(@ "$session" activeRun)" ]] || break
    sleep .1
done
check 'both directories finish without stalled work' 0 "$(@ "$session" pendingCount)"
run_ids=$(@ Store idsOf: AgentRun matching: "{\"session\":\"$session\"}")
check 'incompatible directories use separate runs' 2 "$(jq length <<<"$run_ids")"
for message in "$first" "$second"; do
    reply=$(@ Store idsOf: Message matching: "{\"replyTo\":\"$message\"}" | jq -r '.[0]')
    check 'driver executes in the requested directory' "$(@ "$message" executionWorkspace)" "$(@ "$reply" body)"
done
check 'creation workspace remains provenance' "$tmp/one" "$(@ "$session" workspace)"

# A changed policy must reject the old delivery claim in storage, even if the
# caller retained its ID and a previously started compatible run.
third=$(@ Inbox send: 'Check authorization' to: "session:$session" from: workspace-owner in: "$tmp/two")
delivery=$(@ "$session" pendingDeliveries)
mapfile -t started < <(@ AgentRun startFor: "$session" profile: shell workspace: "$tmp/two")
run=${started[0]}
@ "$run" transitionTo: running >/dev/null
role=$(@ "$session" role)
@ "$role" workspacePolicy: "[\"$tmp/one\"]"
@ "$role" save
check 'revoked workspace cannot be claimed' false "$(@ AgentDelivery claim: "$delivery" run: "$run" 2>/dev/null)"
check 'rejected claim preserves the queued delivery' pending "$(@ "$delivery" state)"
reject @ Store patch: "$run" with: "{\"executionWorkspace\":\"$tmp/one\"}"
@ "$run" finishWith: failed outcome: '{}' error: 'fixture completed' >/dev/null
reject @ AgentRun startFor: "$session" profile: shell workspace: "$tmp/two"
check 'workspace policy matches directory boundaries' false "$(@ "$role" allowsWorkspace: "$tmp/one-other")"
@ AgentWorker tickSession: "$session" >/dev/null 2>&1
check 'denied dispatch becomes a visible failed delivery' failed "$(@ "$delivery" state)"
check 'denied dispatch never leaves a starting run' '' "$(@ "$session" activeRun)"
echo 'Workspace delivery checks passed'
