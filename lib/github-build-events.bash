# GitHub HTTP / JSON / atomic SQLite boundaries for GitHub::BuildEvents.
_github_build_ensure_schema() {
    honker_available || { _throw Error 'GitHub build events require Honker'; return 1; }
    honker_bootstrap || return 1
    _db_sql -bail "CREATE TABLE IF NOT EXISTS _github_build_sources (
      source TEXT PRIMARY KEY, cursor INTEGER NOT NULL CHECK(cursor >= 0));
      CREATE TABLE IF NOT EXISTS _github_build_receipts (delivery_id TEXT PRIMARY KEY);" >/dev/null

}

_github_build_cursor() {
    local source
    source=$(_db_escape "$1")
    _db_sql "SELECT COALESCE((SELECT cursor FROM _github_build_sources WHERE source='$source'),0);"

}

_github_build_fetch() {
    [[ "$1" == https://* && "$1" != *'?'* && "$1" != *'#'* && "$2" =~ ^[0-9]{1,16}$ ]] || {
      _throw Error 'Expected an HTTPS events endpoint and numeric cursor'; return 1;
    }
    curl --fail --silent --show-error --connect-timeout 5 --max-time 12 \
      --max-filesize 1048576 --proto '=https' -- "$1?after=$2&limit=100"

}

_github_build_validate() {
    [[ ${#1} -le 1048576 && "$2" =~ ^[0-9]{1,16}$ ]] || {
      _throw Error 'Invalid or oversized GitHub event batch'; return 1;
    }
    printf '%s' "$1" | jq -ce --argjson cursor "$2" '
      def uint: type=="number" and floor==. and .>=0 and .<=9007199254740991;
      select(.schema_version==1 and (.retention_floor|uint) and
        .retention_floor<=$cursor and (.next_cursor|uint) and .next_cursor>=$cursor) |
      select((.events|type)=="array" and (.events|length)<=100) |
      select(all(.events[]; (.sequence|uint) and .sequence>$cursor and
        (.received_at|type)=="string" and .event.schema_version==1 and
        (.event.delivery_id|type)=="string" and
        (.event.delivery_id|test("^[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}$")) and
        (.event.owner=="chazu" or .event.owner=="loosh-industries") and
        (.event.repository|type)=="string" and (.event.kind|type)=="string")) |
      select(([.events[].sequence]|sort|unique)==[.events[].sequence]) |
      select(([.events[].event.delivery_id]|unique|length)==(.events|length)) |
      select(.next_cursor==(.events[-1].sequence // $cursor))' 2>/dev/null || {
      _throw Error 'Invalid GitHub batch or retention gap; cursor was not advanced'; return 1;
    }

}

_github_build_commit() {
    local batch source
    batch=$(_db_escape "$1")
    source=$(_db_escape "$2")
    # BEGIN IMMEDIATE fences competing pollers. Checkpoint and every Honker
    # publish commit together; a crash/retry cannot lose or republish a delivery.
    # Feed statements on stdin: sqlite CLI command-line SQL can continue
    # through an error despite -bail, which could otherwise commit a partial batch.
    _db_sql -bail >/dev/null <<SQL || return 1
      BEGIN IMMEDIATE;
      INSERT INTO _github_build_sources VALUES ('$source',0) ON CONFLICT DO NOTHING;
      CREATE TEMP TABLE checkpoint_guard (valid INTEGER CHECK(valid=1));
      INSERT INTO checkpoint_guard SELECT cursor=$3 FROM _github_build_sources WHERE source='$source';
      SELECT honker_stream_publish('github.builds.v1', json_extract(value,'$.event.repository'),
        json_set(json_extract(value,'$.event'), '$.sequence',json_extract(value,'$.sequence'),
          '$.received_at',json_extract(value,'$.received_at')))
        FROM json_each('$batch','$.events')
        WHERE json_extract(value,'$.event.delivery_id') NOT IN (SELECT delivery_id FROM _github_build_receipts)
        ORDER BY json_extract(value,'$.sequence');
      INSERT INTO _github_build_receipts
        SELECT json_extract(value,'$.event.delivery_id') FROM json_each('$batch','$.events') WHERE true
        ON CONFLICT DO NOTHING;
      UPDATE _github_build_sources SET cursor=json_extract('$batch','$.next_cursor') WHERE source='$source';
      COMMIT;
SQL
    printf '%s' "$1" | jq '.events|length'

}
