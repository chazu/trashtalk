#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -eo pipefail
TRASHTALK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$TRASHTALK_DIR/lib/trash.bash"
honker_available || { echo 'SKIP: Honker not installed'; exit 0; }
@ GitHub::BuildEvents ensureSchema
endpoint=https://example.invalid/events
batch='{"schema_version":1,"retention_floor":0,"next_cursor":2,"events":[{"sequence":2,"received_at":"2026-09-18T12:00:00Z","event":{"schema_version":1,"delivery_id":"12345678-abcd-abcd-abcd-123456789abc","repository":"chazu/test","owner":"chazu","kind":"workflow_run","name":"it is a quoted '\''name"}}]}'
@ GitHub::BuildEvents acceptBatch: "$batch" from: "$endpoint" cursor: 0 >/dev/null
[[ $(@ GitHub::BuildEvents cursorFor: "$endpoint") == 2 ]]
consumer=$(@ GitHub::BuildEvents consumerNamed: github-test)
rows=$(@ "$consumer" read: 100)
[[ $(jq 'length' <<<"$rows") == 1 ]]
[[ $(jq -r '.[0].payload | if type=="string" then fromjson else . end | .repository' <<<"$rows") == chazu/test ]]
# A stale concurrent poll must fail atomically without a duplicate append.
if @ GitHub::BuildEvents acceptBatch: "$batch" from: "$endpoint" cursor: 0 >/dev/null 2>&1; then exit 1; fi
[[ $(jq 'length' <<<"$(@ "$consumer" read: 100)") == 1 ]]
# A duplicate delivery with a later cloud sequence moves the remote checkpoint,
# but does not republish locally, even when it contains SQL quoting characters.
later=$(jq '.next_cursor=3 | .events[0].sequence=3' <<<"$batch")
@ GitHub::BuildEvents acceptBatch: "$later" from: "$endpoint" cursor: 2 >/dev/null
[[ $(@ GitHub::BuildEvents cursorFor: "$endpoint") == 3 ]]
[[ $(jq 'length' <<<"$(@ "$consumer" read: 100)") == 1 ]]
# Fail after Honker publication but before receipt/checkpoint commit. The stream
# append must roll back too, proving that the database boundary is atomic.
failure=$(jq '.next_cursor=4 | .events[0].sequence=4 | .events[0].event.delivery_id="87654321-abcd-abcd-abcd-123456789abc"' <<<"$batch")
_db_sql "CREATE TRIGGER fail_receipt BEFORE INSERT ON _github_build_receipts BEGIN SELECT RAISE(ABORT,'injected failure'); END;"
if @ GitHub::BuildEvents acceptBatch: "$failure" from: "$endpoint" cursor: 3 >/dev/null 2>&1; then exit 1; fi
_db_sql 'DROP TRIGGER fail_receipt;'
[[ $(@ GitHub::BuildEvents cursorFor: "$endpoint") == 3 ]]
[[ $(jq 'length' <<<"$(@ "$consumer" read: 100)") == 1 ]]
gap=$(jq '.retention_floor=99' <<<"$later")
if @ GitHub::BuildEvents acceptBatch: "$gap" from: "$endpoint" cursor: 3 >/dev/null 2>&1; then exit 1; fi
[[ $(@ GitHub::BuildEvents cursorFor: "$endpoint") == 3 ]]
# Independent consumers and acknowledgements retain normal Stream semantics.
offset=$(jq -r '.[0].offset' <<<"$rows")
@ "$consumer" acknowledgeThrough: "$offset" >/dev/null
[[ $(@ "$consumer" read) == '[]' ]]
other=$(@ GitHub::BuildEvents consumerNamed: github-other)
[[ $(jq 'length' <<<"$(@ "$other" read)") == 1 ]]
echo 'PASS: GitHub atomic import, deduplication, cursor fencing, retention gaps and independent consumers'
