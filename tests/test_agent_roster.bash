#!/usr/bin/env bash
# Configured permanent agents compile from user source and reconcile safely.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
mkdir -p "$root/trash/user"
cat > "$root/trash/user/Analyst.trash" <<'EOF'
Analyst subclass: Agent::AgentProfile
  handle: 'analyst'
  displayName: 'Analyst'
  profile: 'shell'
  archetype: 'assistant' revision: 1
  role: 'assistant' revision: 1
  sessionScope: 'identity'
  enabled: true
EOF
cat > "$root/trash/user/Researcher.trash" <<'EOF'
Researcher subclass: Agent::AgentProfile
  handle: 'researcher'
  displayName: 'Researcher'
  profile: 'shell'
  archetype: 'assistant' revision: 1
  role: 'assistant' revision: 1
  sessionScope: 'workspace'
  enabled: true
EOF
make -C "$root" bash >/dev/null
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=roster-owner TRASHTALK_NO_AUTOTICK=1
unset TRASHTALK_RUN_TOKEN
db_init
source tests/helpers/check.bash
passed=0
reject() { local name="$1"; shift; if "$@" >"$tmp/rejected.out" 2>"$tmp/rejected.err"; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }

check 'compiled profile manifest handle' researcher "$(jq -r '.profiles[1].handle' "$root/trash/.compiled/.agent-profile-manifest.json")"
check 'compiled profile manifest includes every declared agent' 2 "$(jq '.profiles | length' "$root/trash/.compiled/.agent-profile-manifest.json")"

# A human interactive shell reconciles the already compiled manifest. Agent runs
# and test shells skip this startup write, so this uses a separate human-owned DB.
startup_db="$tmp/startup.db"
env -u TRASHTALK_RUN_TOKEN TRASHTALK_DIR="$root" TRASHDIR="$root/trash" SQLITE_JSON_DB="$startup_db" TRASHTALK_USER=roster-startup TRASHTALK_SKIP_USER_CONFIG=1 bash -c '
  source "$TRASHTALK_DIR/lib/trash.bash"
  @ Agent::Archetype define: assistant revision: 1 instructions: "Research carefully." profile: shell >/dev/null
  @ Agent::Role define: assistant revision: 1 capabilities: "[\"assignment.work\"]" workspacePolicy: "[]" runBudget: "{\"retryLimit\":0}" >/dev/null
'
startup=$(env -u TRASHTALK_RUN_TOKEN -u TRASHTALK_SKIP_USER_CONFIG HOME="$tmp/home" TRASHTALK_DIR="$root" TRASHDIR="$root/trash" SQLITE_JSON_DB="$startup_db" TRASHTALK_USER=roster-startup bash --noprofile --norc -ic '
  source "$TRASHTALK_DIR/lib/trash.bash"
  @ Agent::Roster inspect: researcher
' 2>/dev/null)
check 'interactive startup reconciles roster' managed "$(printf '%s' "$startup" | jq -r .status)"

arch=$(must @ Agent::Archetype define: assistant revision: 1 instructions: 'Research carefully.' profile: shell)
role=$(must @ Agent::Role define: assistant revision: 1 capabilities: '["assignment.work"]' workspacePolicy: '[]' runBudget: '{"retryLimit":0}')
id=$(must @ Agent::Roster reconcile)
check 'reconcile creates configured identity' researcher "$(@ "$id" handle)"
analyst=$(must @ Agent::Identity findByHandle: analyst)
check 'reconcile creates every configured identity' analyst "$(@ "$analyst" handle)"
check 'reconcile applies each profile scope' identity "$(@ "$analyst" sessionScope)"
check 'reconcile records display name' Researcher "$(@ "$id" displayName)"
check 'reconcile records profile default' shell "$(@ "$id" defaultProfile)"
check 'reconcile records roster provenance' Researcher "$(@ "$id" configProfile)"
inspection=$(must @ Agent::Roster inspect: researcher)
check 'inspect reports managed roster state' managed "$(printf '%s' "$inspection" | jq -r .status)"
check 'inspect reports configured handle' researcher "$(printf '%s' "$inspection" | jq -r .handle)"
@ Agent::Roster disable: researcher >/dev/null
check 'disable prevents new configured sessions' false "$(@ "$id" enabled)"
@ Agent::Roster reconcile >/dev/null
check 'reconcile is idempotent' "$id" "$(must @ Agent::Roster reconcile)"
mkdir "$tmp/work"
session=$(must @ "$id" ensureCurrentSessionFor: "$tmp/work")
check 'configured identity session uses profile' shell "$(@ "$session" backendProfile)"

@ "$id" enabled: false
@ "$id" save
reject 'disabled configured identity cannot create a session' @ "$id" ensureCurrentSessionFor: "$tmp/other"
@ Agent::Roster reconcile >/dev/null
check 'reconcile restores declared enabled state' true "$(@ "$id" enabled)"

# A normal profile cannot take over a manually managed matching identity.
manual=$(must @ Agent::Identity named: reviewer)
@ "$manual" owner: roster-owner
@ "$manual" save
reject 'profile cannot silently adopt unmanaged identity' @ Agent::Roster reconcileProfile: '{"identity":"Researcher","handle":"reviewer"}'

@ Agent::Roster retire: analyst reason: 'No longer needed' >/dev/null
check 'retire disables durable identity' false "$(@ "$analyst" enabled)"
check 'retire removes roster provenance' '' "$(@ "$analyst" configProfile)"

echo "PASS: $passed roster checks"
