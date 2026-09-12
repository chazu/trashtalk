#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
export TRASHTALK_USER=reader TRASHTALK_NO_AUTOTICK=1 TZ=America/New_York
for TZ in America/New_York UTC; do
    export TZ
    calendar=$(@ Time calendarFor: '[0,1704202500,1710052200,1710055800,1730611800,1730615400]')
    for stamp in 0 1704202500 1710052200 1710055800 1730611800 1730615400; do
        for field in day short full clock; do
            case "$field" in
                day) format='%Y-%m-%d';; short) format='%Y-%m-%d %H:%M';;
                full) format='%Y-%m-%d %H:%M %Z';; clock) format='%H:%M';;
            esac
            [[ $(jq -r --arg s "$stamp" --arg f "$field" '.[$s][$f]' <<< "$calendar") == "$(@ Time format: "$stamp" as: "$format")" ]]
        done
    done
done
export TZ=America/New_York
[[ $(@ Time calendarFor: '[]') == '{}' ]]
if @ Time calendarFor: '[1,"bad"]' > "$TMPDIR/calendar-error" 2>/dev/null; then exit 1; fi
[[ ! -s "$TMPDIR/calendar-error" ]]
message=$(@ Message new)
base=$(@ "$message" asJson)
escaped=$(_db_escape "$base")
_db_sql "WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i+1 FROM n WHERE i<200)
  INSERT INTO instances(id,data) SELECT 'message_batch_'||i,
    json_set('$escaped','$.from','あいうえおかきくけこさしすせそたちつてとなに','$.to','reader',
      '$.kind','question','$.status','unread','$.subject','question',
      '$.body','First line'||char(10)||char(10)||'Second paragraph'||char(10),
      '$.sentAt',1704202500000) FROM n;"
ids=$(_db_sql "SELECT id FROM instances WHERE id LIKE 'message_batch_%' ORDER BY rowid DESC;")
# Batch reload has the same deliberate discard semantics as the old picker.
@ message_batch_200 body: 'unsaved change' >/dev/null
snapshots=$(@ Runtime reloadDataFor: "$ids")
[[ $(@ message_batch_200 body) == $'First line\n\nSecond paragraph' ]]
calls="$TMPDIR/projection-calls"
jq() { printf 'jq\n' >> "$calls"; command jq "$@"; }
definition=$(declare -f _db_sql)
eval "${definition/_db_sql ()/_original_projection_sql ()}"
_db_sql() { printf 'sql\n' >> "$calls"; _original_projection_sql "$@"; }
: > "$calls"
documents=$(@ MessagePresentation documentsFor: "$snapshots")
[[ $(rg -c '^sql$' "$calls") == 1 ]]
[[ $(rg -c '^jq$' "$calls") -lt 225 ]]
command jq -se 'length==200 and .[0].record.id=="message_batch_200" and .[-1].record.id=="message_batch_1"
  and all(.[]; .record.label=="First line" and .record.display.prefix=="●? あいうえおかきくけこさしすせそたちつて…  2024-01-02 08:35"
    and .record.kind=="question" and .record.path==.path
    and .text=="あいうえおかきくけこさしすせそたちつてとなに → You  ·  2024-01-02 08:35 EST\n\nFirst line\n\nSecond paragraph")' <<< "$documents" >/dev/null
[[ $(@ message_batch_200 status) == unread ]]
[[ $(@ MessagePresentation documentsFor: '') == '' ]]
printf 'PASS: 200 ordered rows, one participant query, bounded serializers, fresh cache, Unicode, preview text and DST parity\n'
