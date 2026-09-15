#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
  exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=workboard-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }

identity=$(must @ AgentIdentity named: workboard-specialist)
@ "$identity" owner: workboard-owner; @ "$identity" save
arch=$(must @ AgentArchetype define: workboard-specialist revision: 1 instructions: 'Read inbox.' profile: shell)
role=$(must @ AgentRole define: workboard-specialist revision: 1 capabilities: '["inbox.read","message.send","assignment.work"]' workspacePolicy: '[]' runBudget: '{}')
session=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)

ready=$(must @ Assignment draft: 'Review queue latency' in: "$root")
@ "$ready" criteria: 'Report evidence and a benchmark command.' >/dev/null
@ "$ready" assignTo: "$identity" >/dev/null
@ "$ready" workIn: "$session" >/dev/null

waiting=$(must @ Assignment draft: 'Explain a stalled delivery' in: "$root")
@ "$waiting" assignTo: "$identity" >/dev/null
@ "$waiting" workIn: "$session" >/dev/null
question=$(must @ "$waiting" ask: 'Is the worker intentionally paused?')

report=$(must @ Trash workStatus)
check 'status reports schema version' 1 "$(jq -r .schema_version <<<"$report")"
check 'status is scoped to local owner' workboard-owner "$(jq -r .owner <<<"$report")"
check 'status lists both open assignments' 2 "$(jq -r .assignments.count <<<"$report")"
check 'status derives ready manual work' 1 "$(jq -r .assignments.readyForManualWork <<<"$report")"
check 'status derives waiting question work' 1 "$(jq -r .assignments.waitingOnQuestion <<<"$report")"
check 'status includes a durable question count' 1 "$(jq -r --arg id "$waiting" '.assignments.items[] | select(.id==$id) | .unansweredQuestions' <<<"$report")"
check 'status is read-only' 0 "$(jq -r '[.assignments.items[] | select(.activity == "running" or .activity == "needs review")] | length' <<<"$report")"
shown=$(must @ Trash showWorkboard)
check 'text view identifies the workboard owner' true "$([[ "$shown" == *'Workboard for workboard-owner'* ]] && echo true || echo false)"
check 'text view lists waiting assignment' true "$([[ "$shown" == *"Assignment $waiting [waiting on question]"* ]] && echo true || echo false)"

echo "All $passed agent workboard tests passed."
