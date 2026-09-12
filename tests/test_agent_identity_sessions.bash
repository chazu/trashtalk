#!/usr/bin/env bash
# Public session API regression journey, including real Store conflict replay.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=session-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected.out" 2>"$tmp/rejected.err"; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
ensure() { @ AgentSession ensureFor: "$1" archetype: "$arch" role: "$role" workspace: "$2" profile: shell; }
mkdir "$tmp/one" "$tmp/two"
one=$(cd "$tmp/one" && pwd -P)
two=$(cd "$tmp/two" && pwd -P)
arch=$(must @ Gusgus archetype)
role=$(must @ Gusgus role)
id=$(must @ AgentIdentity named: workspace-agent)
check 'default scope is workspace' workspace "$(@ "$id" sessionScope)"
s1=$(must ensure "$id" "$one")
s2=$(must ensure "$id" "$two")
check 'workspace sessions differ' false "$([[ "$s1" == "$s2" ]] && echo true || echo false)"
check 'workspace ensure reuses' "$s1" "$(must ensure "$id" "$one")"
ln -s "$one" "$tmp/alias"
check 'workspace aliases resolve to one session' "$s1" "$(must ensure "$id" "$tmp/alias")"
reject 'direct workspace duplicate rejected' @ AgentSession openFor: "$id" archetype: "$arch" role: "$role" workspace: "$tmp/alias" profile: shell
check 'workspace lookup remains workspace-specific' "$s2" "$(@ AgentSession currentFor: "$id" workspace: "$two")"
@ "$s1" pause >/dev/null
check 'paused current is reused' "$s1" "$(must ensure "$id" "$one")"
@ "$s1" close >/dev/null
check 'closed session excluded' '' "$(@ AgentSession currentFor: "$id" workspace: "$one")"
s3=$(must ensure "$id" "$one")
@ "$s3" terminate >/dev/null
check 'terminated session excluded' '' "$(@ AgentSession currentFor: "$id" workspace: "$one")"

# New Gusgus identities start with identity scope. Existing conversations require selection.
gid=$(must @ Gusgus identity)
g1=$(must @ Gusgus sessionFor: "$one")
check 'new Gusgus identity scope' identity "$(@ "$gid" sessionScope)"
check 'two directories reuse one conversation' "$g1" "$(must @ Gusgus sessionFor: "$two")"
check 'reuse keeps execution workspace' "$one" "$(@ "$g1" workspace)"
check 'identity stores current selection' "$g1" "$(@ "$gid" currentSession)"
check 'fresh runtime resolves persisted session' "$g1" "$(bash -c 'source "$1/lib/trash.bash"; @ Gusgus sessionFor: "$2"' bash "$root" "$two")"
msg=$(must @ Gusgus chat: hello workingDirectory: "$two" status: 7 lastResult: result)
check 'chat routes to shared inbox' "session:$g1" "$(@ "$msg" to)"
check 'delivery retains calling-directory context' true "$(@ "$msg" body | grep -Fq "working_directory=$two last_status=7" && echo true || echo false)"
@ "$g1" pause >/dev/null
g2=$(must @ Gusgus fresh: "$two")
check 'fresh closes paused current across directories' closed "$(@ "$g1" lifecycleState)"
check 'fresh opens in requested workspace' "$two" "$(@ "$g2" workspace)"
check 'both dirs resolve fresh session' "$g2" "$(must @ Gusgus sessionFor: "$one")"
# Every creation entry point must preserve the single active conversation.
reject 'direct open rejects duplicate identity session' @ AgentSession openFor: "$gid" archetype: "$arch" role: "$role" workspace: "$one" profile: shell
g3=$(must @ Gusgus fresh: "$one")
check 'fresh closes selected current only' closed "$(@ "$g2" lifecycleState)"
reject 'historical reopen cannot displace current' @ "$g1" reopen
check 'replacement stays current despite duplicates' "$g3" "$(must @ Gusgus sessionFor: "$two")"
mapfile -t run_info < <(@ AgentRun startFor: "$g3" profile: shell)
reject 'fresh rejects active run' @ Gusgus fresh: "$two"
check 'active current remains open' open "$(@ "$g3" lifecycleState)"
reject 'second concurrent run rejected' @ AgentRun startFor: "$g3" profile: shell
check 'failed fresh preserves pointer' "$g3" "$(@ "$gid" currentSession)"
reject 'missing directory cannot close current' @ Gusgus fresh: "$tmp/missing"

# Owner-selected migration keeps the chosen conversation and old work intact.
legacy=$(must @ AgentIdentity new)
@ "$legacy" handle: legacy-agent
@ "$legacy" owner: session-owner
@ "$legacy" save
old=$(must @ AgentSession openFor: "$legacy" archetype: "$arch" role: "$role" workspace: "$one" profile: shell)
chosen=$(must @ AgentSession openFor: "$legacy" archetype: "$arch" role: "$role" workspace: "$two" profile: shell)
@ "$chosen" lastConversationRef: retained-provider-history
@ "$chosen" save
stale=$(db_get "$old")
legacy_message=$(must @ Inbox send: 'Historical work' to: "session:$old" from: session-owner)
delivery=$(must @ AgentDelivery forSession: "$old" messages: "[\"$legacy_message\"]")
check 'explicit selection returns chosen session' "$chosen" "$(must @ "$legacy" selectCurrentSession: "$chosen")"
check 'selected provider history survives migration' retained-provider-history "$(@ "$chosen" lastConversationRef)"
check 'historical session is closed' closed "$(db_get "$old" | jq -r .lifecycleState)"
check 'historical work remains pending in its own session' pending "$(@ "$delivery" state)"
check 'historical work retains attribution' "$old" "$(@ "$delivery" session)"
check 'membership follows identity across directories' "$chosen" "$(must @ "$legacy" currentSessionFor: "$one")"
check 'durable membership is unique' 1 "$(_db_sql "SELECT count(*) FROM agent_session_memberships WHERE identity_id='$legacy';")"
reject 'stale save cannot resurrect old conversation' @ Store put: "$old" data: "$stale"
reject 'old conversation cannot start a run' @ AgentRun startFor: "$old" profile: shell
reject 'historical session cannot receive live focus' @ AgentAccess liveSession: "$old"
check 'historical session remains inspectable' "$old" "$(must @ AgentAccess session: "$old")"
reject 'run-token caller cannot migrate' env TRASHTALK_RUN_TOKEN=foreign bash -c 'source "$1/lib/trash.bash"; @ AgentSession select: "$2" forIdentity: "$3" owner: session-owner' fixture "$root" "$chosen" "$legacy"
# A selection after a launch snapshot fences the staged launch at commit.
policy_before=$(@ "$legacy" sessionPolicyRevision)
check 'repeated selection is idempotent' "$chosen" "$(must @ "$legacy" selectCurrentSession: "$chosen")"
check 'repeated selection retains policy revision' "$policy_before" "$(@ "$legacy" sessionPolicyRevision)"
cutover=$(must @ AgentIdentity new)
@ "$cutover" owner: session-owner
@ "$cutover" save
cutover_session=$(must @ AgentSession openFor: "$cutover" archetype: "$arch" role: "$role" workspace: "$one" profile: shell)
_store_tx_before_commit() { @ "$cutover" selectCurrentSession: "$cutover_session" >/dev/null; }
reject 'migration fences a competing staged run start' @ AgentRun startFor: "$cutover_session" profile: shell
unset -f _store_tx_before_commit
check 'fenced start publishes no active run' '' "$(@ "$cutover_session" activeRun)"
reject 'raw policy downgrade rejected' @ Store patch: "$legacy" with: '{"sessionPolicyRevision":0,"sessionScope":"workspace"}'
reject 'creation workspace is immutable' @ Store patch: "$chosen" with: "$(jq -cn --arg ws "$one" '{workspace:$ws}')"
reject 'historical inbox rejects new work' @ Inbox send: 'stale target' to: "session:$old" from: session-owner
check 'migration marker does not survive commit' 0 "$(_db_sql 'SELECT count(*) FROM agent_session_policy_changes;')"

# Failed creation commits neither a session nor an inbox.
bad=$(must @ AgentIdentity named: restricted)
strict=$(must @ AgentRole define: strict-session revision: 1 capabilities: '[]' workspacePolicy: '["/allowed"]' runBudget: '{}')
before=$(@ Store countByClass: AgentSession)
boxes=$(@ Store countByClass: Inbox)
reject 'denied ensure rolls back' @ AgentSession ensureFor: "$bad" archetype: "$arch" role: "$strict" workspace: "$one" profile: shell
check 'no denied session leaked' "$before" "$(@ Store countByClass: AgentSession)"
check 'no denied inbox leaked' "$boxes" "$(@ Store countByClass: Inbox)"
@ "$bad" sessionScope: invalid
@ "$bad" save
reject 'unknown scope rejected' ensure "$bad" "$one"

# Force two independent processes to stage an empty lookup before either commits.
race=$(must @ AgentIdentity named: racing)
@ "$race" sessionScope: identity
@ "$race" save
_store_tx_before_commit() {
    touch "$tmp/ready-$BASHPID"
    local attempt
    for ((attempt=0; attempt<1500; attempt++)); do
        [[ ! -f "$tmp/release" ]] || return 0
        sleep 0.01
    done
    return 1
}
(ensure "$race" "$one" >"$tmp/race1.out" 2>"$tmp/race1.err"; echo "$?" >"$tmp/race1.status") & p1=$!
(ensure "$race" "$two" >"$tmp/race2.out" 2>"$tmp/race2.err"; echo "$?" >"$tmp/race2.status") & p2=$!
for ((attempt=0; attempt<1500; attempt++)); do
    ready=("$tmp"/ready-*)
    ((${#ready[@]} == 2)) && break
    sleep 0.01
done
touch "$tmp/release"
must wait "$p1"; must wait "$p2"
unset -f _store_tx_before_commit
check 'both creators staged concurrently' 2 "${#ready[@]}"
cat "$tmp/race1.err" "$tmp/race2.err"
check 'first ensure succeeds' 0 "$(cat "$tmp/race1.status")"
check 'second ensure succeeds through read-only replay' 0 "$(cat "$tmp/race2.status")"
check 'concurrent callers receive same session' "$(cat "$tmp/race1.out")" "$(cat "$tmp/race2.out")"
ids=$(@ Store idsOf: AgentSession matching: "{\"identity\":\"$race\"}")
check 'exactly one durable session created' 1 "$(printf '%s' "$ids" | jq length)"
winner=$(cat "$tmp/race1.out")
check 'winner owns one durable inbox' 1 "$(@ Store idsOf: Inbox matching: "{\"name\":\"session:$winner\"}" | jq length)"
check 'winner selected on identity' "$winner" "$(@ "$race" currentSession)"
echo "Persistent identity sessions: $passed checks passed"
