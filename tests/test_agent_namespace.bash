#!/usr/bin/env bash
# Exercise the actual pre-package policy DDL, including its rename guards.
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
  exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
_db_sql -bail < tests/fixtures/agent-namespace/legacy-schema.sql
_db_sql -bail <<'SQL'
INSERT INTO instances VALUES ('agentidentity_legacy',json_object('class','AgentIdentity','id','agentidentity_legacy','handle','namespace-fixture','enabled','true','sessionScope','identity','sessionPolicyRevision',1,'currentSession','agentsession_legacy','created_at','original','extra',json('{"nested":[1,null,"unchanged"]}')));
INSERT INTO instances VALUES ('agentrole_legacy',json_object('class','AgentRole','id','agentrole_legacy','revision',1,'workspacePolicy','any'));
INSERT INTO instances VALUES ('agentarchetype_legacy',json_object('class','AgentArchetype','id','agentarchetype_legacy','instructions','unchanged'));
INSERT INTO instances VALUES ('agentsession_legacy',json_object('class','AgentSession','id','agentsession_legacy','identity','agentidentity_legacy','role','agentrole_legacy','archetype','agentarchetype_legacy','workspace','/legacy','sessionPolicyRevision',1,'lifecycleState','open'));
INSERT INTO instances VALUES ('agentrun_legacy',json_object('class','AgentRun','id','agentrun_legacy','session','agentsession_legacy','state','running','capabilityTokenHash','fixture-hash-replaced-below'));
INSERT INTO instances VALUES ('agentdelivery_legacy',json_object('class','AgentDelivery','id','agentdelivery_legacy','session','agentsession_legacy','run','agentrun_legacy','state','pending','message','message_legacy'));
INSERT INTO instances VALUES ('message_legacy',json_object('class','Message','id','message_legacy','to','session:agentsession_legacy','body','unchanged'));
INSERT INTO instances VALUES ('agentidentity_unenrolled',json_object('class','AgentIdentity','id','agentidentity_unenrolled','sessionPolicyRevision',0));
INSERT INTO instances VALUES ('agentsession_unenrolled',json_object('class','AgentSession','id','agentsession_unenrolled','identity','agentidentity_unenrolled','sessionPolicyRevision',0,'lifecycleState','open'));
UPDATE instances SET data=json_set(data,'$._vars',json((SELECT json_group_array(key) FROM json_each(instances.data) WHERE key NOT IN ('class','id'))));
CREATE TABLE namespace_unrelated (value TEXT);
INSERT INTO namespace_unrelated VALUES ('untouched');
SQL
secret=namespace-fixture-secret
hash=$(printf '%s' "$secret" | shasum -a 256 | cut -d' ' -f1)
_db_sql "UPDATE instances SET data=json_set(data,'$.capabilityTokenHash','$hash') WHERE id='agentrun_legacy'"
records=$(_db_sql "SELECT id,json_remove(data,'$.class') FROM instances ORDER BY id")
memberships=$(_db_sql 'SELECT * FROM agent_session_memberships ORDER BY session_id')
[[ -n $memberships ]]
# Simulate an unexpected failure after some discriminators have changed. Both
# records and dropped policy DDL must roll back, leaving no half-migration.
_db_sql "CREATE TRIGGER namespace_test_abort BEFORE UPDATE ON instances WHEN OLD.class='AgentRun' BEGIN SELECT RAISE(ABORT,'injected migration failure'); END;"
schema=$(_db_sql 'SELECT sql FROM sqlite_master ORDER BY name')
old=$(_db_sql 'SELECT id,data FROM instances ORDER BY id')
if _db_sql -bail < lib/agent-session-schema.sql >/dev/null 2>&1; then
  echo 'FAIL: injected migration unexpectedly succeeded'; exit 1
fi
[[ $(_db_sql 'SELECT sql FROM sqlite_master ORDER BY name') == "$schema" ]]
[[ $(_db_sql 'SELECT id,data FROM instances ORDER BY id') == "$old" ]]
[[ $(_db_sql 'SELECT * FROM agent_session_memberships ORDER BY session_id') == "$memberships" ]]
if bash -c 'source lib/trash.bash' >/dev/null 2>&1; then
  echo 'FAIL: runtime accepted failed migration'; exit 1
fi
[[ $(_db_sql 'SELECT id,data FROM instances ORDER BY id') == "$old" ]]
_db_sql 'DROP TRIGGER namespace_test_abort;'
# A fresh runtime upgrades before a public read can resolve a legacy class.
bash -euo pipefail -c 'source lib/trash.bash; [[ $(@ Store getClass: agentsession_legacy) == Agent::Session ]]; [[ $(@ agentsession_legacy sessionPolicyRevision) == 1 ]]'
[[ $(_db_sql "SELECT count(*) FROM instances WHERE class IN ('AgentIdentity','AgentSession','AgentRun','AgentDelivery','AgentArchetype','AgentRole')") == 0 ]]
[[ $(_db_sql "SELECT id,json_remove(data,'$.class') FROM instances ORDER BY id") == "$records" ]]
[[ $(_db_sql 'SELECT * FROM agent_session_memberships ORDER BY session_id') == "$memberships" ]]
[[ $(_db_sql 'SELECT value FROM namespace_unrelated') == untouched ]]
[[ $(_db_sql "SELECT count(*) FROM agent_session_memberships WHERE session_id='agentsession_unenrolled'") == 0 ]]
[[ $(@ Store getClass: agentsession_unenrolled) == Agent::Session ]]
[[ $(@ agentsession_unenrolled sessionPolicyRevision) == 0 ]]
[[ $(TRASHTALK_RUN_TOKEN="agentrun_legacy:$secret" @ Agent::Run current) == agentrun_legacy ]]
if TRASHTALK_RUN_TOKEN=agentrun_legacy:wrong @ Agent::Run current >/dev/null 2>&1; then
  echo 'FAIL: migrated run accepted wrong capability'; exit 1
fi
for name in Identity Session Run Delivery Archetype Role; do
  id="agent${name,,}_legacy"
  [[ $(@ Store getClass: "$id") == "Agent::$name" ]]
  @ Store read: "$id" class: "Agent::$name" >/dev/null
  # New authority records retain precisely the old prefix, with a new class.
  new=$(@ "Agent::$name" new)
  [[ $new == "agent${name,,}_"* ]]
  [[ $(@ Store getClass: "$new") == "Agent::$name" ]]
done
schema=$(_db_sql 'SELECT sql FROM sqlite_master ORDER BY name')
records=$(_db_sql 'SELECT id,data FROM instances ORDER BY id')
for attempt in 1 2; do
  unset _AGENT_SESSION_SCHEMA_DB
  @ Agent::Session ensureSchema
done
[[ $(_db_sql 'SELECT sql FROM sqlite_master ORDER BY name') == "$schema" ]]
[[ $(_db_sql 'SELECT id,data FROM instances ORDER BY id') == "$records" ]]
[[ $(_db_sql 'SELECT * FROM agent_session_memberships ORDER BY session_id') == "$memberships" ]]
# Installed guards retain their policy, provenance, and active-run protections.
reject_sql() {
  if _db_sql -bail "$1" >/dev/null 2>&1; then
    echo "FAIL: accepted forbidden write: $1"; exit 1
  fi
}
reject_sql "UPDATE instances SET data=json_set(data,'$.sessionScope','workspace') WHERE id='agentidentity_legacy'"
reject_sql "UPDATE instances SET data=json_set(data,'$.workspace','/other') WHERE id='agentsession_legacy'"
reject_sql "UPDATE instances SET data=json_set(data,'$.lifecycleState','closed') WHERE id='agentsession_legacy'"
reject_sql "INSERT INTO instances VALUES ('agentrun_duplicate',json_object('class','Agent::Run','session','agentsession_legacy','state','starting'))"
reject_sql "UPDATE instances SET data=json_set(data,'$.class','AgentSession') WHERE id='agentsession_legacy'"
reject_sql "INSERT INTO instances VALUES ('agentidentity_stale',json_object('class','AgentIdentity'))"
# All moved declarations are genuinely packaged, not compatibility aliases.
for name in Identity Session Run Delivery Archetype Role Queue Worker Focus Conversation Browser Transcript Context WorkContext Access Driver; do
  [[ ! -f trash/Agent${name}.trash ]]
  grep -q '^package: Agent$' "trash/Agent/$name.trash"
done
for name in Codex Jcode Maki Shell; do
  [[ ! -f trash/${name}Driver.trash ]]
  grep -q '^package: Agent$' "trash/Agent/${name}Driver.trash"
done
for name in Agent Gusgus Jcode Maki CodexAgent; do
  [[ -f trash/$name.trash ]]
  ! grep -q '^package: Agent$' "trash/$name.trash"
done
echo 'PASS: Agent namespace migration, rollback, idempotence, IDs, policy guards, and facades'
