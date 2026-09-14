#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
export TRASHTALK_USER=local-user TRASHTALK_WORKSTATION_FIXTURES=1
# Install without CUE or Honker. Fresh and populated stores use the same
# idempotent feature-DDL boundary as AgentSession, not an event migration log.
@ WorkstationSchema ensureSchema
counter=$(@ Counter new)
@ "$counter" incrementBy: 7 >/dev/null
@ "$counter" save
before=$(@ Store getInstance: "$counter")
schema=$(_db_sql 'select sql from sqlite_master order by name')
@ WorkstationSchema ensureSchema
@ WorkstationSchema ensureSchema
[[ $(_db_sql 'select sql from sqlite_master order by name') == "$schema" ]]
[[ $(@ Store getInstance: "$counter") == "$before" ]]
[[ $(_db_sql "select count(*) from sqlite_master where name='workstation_coordinates'") == 1 ]]
# Capability probes are independent, read-only, and never create Honker tables.
@ WorkstationSchema capabilities | jq -e '(.cue|type)=="boolean" and (.stream|type)=="boolean"' >/dev/null
[[ $(_db_sql 'select sql from sqlite_master order by name') == "$schema" ]]
# Fake discovery only, keep the real doctor and all basic runtime dependencies.
@ Tools::Cue isInstalled >/dev/null
@ WorkstationSchema streamAvailable >/dev/null
__Cue__class__isInstalled() { echo false; }
# Namespaced compiler symbol.
__Tools__Cue__class__isInstalled() { echo false; }
__WorkstationSchema__class__streamAvailable() { echo false; }
@ Jcode path >/dev/null
fake=$(mktemp -d)
printf '#!/usr/bin/env bash\necho "jcode fixture"\n' > "$fake/jcode"
chmod +x "$fake/jcode"
export PATH="$fake:$PATH" TRASHTALK_SESSION_PROFILE=jcode
out=$(@ Trash doctor 2>&1)
[[ $out == *'Workstation CUE unavailable'* && $out == *'Workstation Honker Stream unavailable'* ]]
[[ $(@ Store getClass: "$counter") == Counter ]]
# Missing CUE must reject workstation creation, not just report a warning.
if @ WorkstationSchema validate: '{}' as: Attention >/dev/null 2>&1; then exit 1; fi
unset -f __Tools__Cue__class__isInstalled __Cue__class__isInstalled __WorkstationSchema__class__streamAvailable
command -v cue >/dev/null || { echo 'PASS: migration and optional capabilities (CUE projection tests skipped)'; exit 0; }
digest=$(@ WorkstationSchema digest)
sub=$(@ EventSubscription createFrom: "$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/EventSubscription.valid.json)")
a=$(@ Attention createFrom: "$(jq -c --arg d "$digest" '.schemaDigest=$d' schemas/workstation/v1/fixtures/Attention.valid.json)")
for cls in EventSubscription Attention; do
 records=$(@ Trash instanceRecordsFor: "$cls")
 jq -e '(.display.columns|length)>5 and (.data|has("schemaDigest"))' <<<"$records" >/dev/null
 id=$(jq -r .id <<<"$records")
 inspector=$(@ Trash inspectionRecordFor: "$id")
 jq -e '.data.id==.object_id and (.data|has("schemaDigest"))' <<<"$inspector" >/dev/null
 ! grep -Eq 'payload|stdout|stderr|artifactContents|secret' <<<"$records$inspector"
done
@ Trash instanceRecordsFor: EventSubscription | jq -e '.display.columns|any(.name=="dispatchState" and .value=="paused")' >/dev/null
@ "$a" acknowledge >/dev/null
@ Trash instanceRecordsFor: Attention | jq -e '.display.columns|any(.name=="state" and .value=="acknowledged")' >/dev/null
@ Trash inspectionRecordFor: "$a" | jq -e '.data.state=="acknowledged" and .data.eventCount==0' >/dev/null
@ "$sub" display | jq -e '.enabled==true and .dispatchState=="paused"' >/dev/null
@ "$a" display | jq -e '.state=="acknowledged"' >/dev/null
echo 'PASS: atomic idempotent installation, optional doctor capabilities, browser columns and inspector values'
