#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
trap '_env_cleanup || true' EXIT
cat > "$TRASHDIR/SnapshotFixture.trash" <<'TRASH'
SnapshotFixture subclass: Object
  classMethod: accept: data [
    @ Store requireSnapshot.
    ^ data jsonTextAt: 'value'
  ]
  rawClassMethod: swallow: data [
    @ Require reject: 'fixture failure' >/dev/null
    echo 'must not escape'
  ]
  rawClassMethod: read: data [
    @ Store getInstance: counter_snapshot >/dev/null
    echo 'must not escape'
  ]
  rawClassMethod: write: data [
    @ Store patch: counter_snapshot with: '{"value":99}' >/dev/null
    echo 'must not escape'
  ]
  rawClassMethod: sql: data [
    @ Store executeBatch: "UPDATE instances SET data=json_set(data,'$.value',99) WHERE id='counter_snapshot';" >/dev/null
    echo 'must not escape'
  ]
TRASH
"$TRASHTALK_DIR/lib/jq-compiler/driver.bash" compile "$TRASHDIR/SnapshotFixture.trash" --check > "$TRASHDIR/.compiled/SnapshotFixture"
db_put counter_snapshot '{"class":"Counter","value":1}'
query="SELECT data FROM instances WHERE id='counter_snapshot'"
[[ $(@ Store validateSnapshot: "$query" using: SnapshotFixture sending: accept:) == 1 ]]
for selector in swallow: read: write: sql:; do
    if @ Store validateSnapshot: "$query" using: SnapshotFixture sending: "$selector" > "$TMPDIR/snapshot-result" 2>/dev/null; then exit 1; fi
    [[ ! -s "$TMPDIR/snapshot-result" ]]
    [[ $(db_get counter_snapshot | jq .value) == 1 ]]
done
_store_tx_before_commit() { db_put counter_snapshot '{"class":"Counter","value":2}'; }
if @ Store validateSnapshot: "$query" using: SnapshotFixture sending: accept: > "$TMPDIR/snapshot-result" 2>/dev/null; then exit 1; fi
[[ ! -s "$TMPDIR/snapshot-result" ]]
unset -f _store_tx_before_commit
[[ $(@ Store validateSnapshot: "$query" using: SnapshotFixture sending: accept:) == 2 ]]
printf 'PASS: guarded snapshot, changed reads, failure suppression, and forbidden Store access\n'
