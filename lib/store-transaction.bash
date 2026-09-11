# Optimistic, selective Store transactions. DSL sends and Persistable.save use
# a private SQLite store/cache across command substitutions. Only tracked object
# reads, guarded queries and staged row inserts participate; external effects
# and arbitrary SQL are deliberately outside this API.
_store_tx_fail() {
    [[ -z ${_STORE_TX:-} ]] || printf '%s\n' "$1" > "$_STORE_TX/failed"
    printf 'Store transaction: %s\n' "$1" >&2
    return 1
}

_store_tx_sql() {
    local rc=0
    _db_sql_direct "$@" || rc=$?
    ((rc == 0)) || { _store_tx_fail 'SQLite operation failed'; return "$rc"; }
}

_store_tx_import() {
    local id="$1"
    _db_validate_id "$id" || { _store_tx_fail 'Invalid object id'; return 1; }
    [[ $(_store_tx_sql "SELECT EXISTS(SELECT 1 FROM instances WHERE id='$id') OR EXISTS(SELECT 1 FROM store_reads WHERE id='$id');") == 0 ]] || return 0
    _store_tx_sql -bail <<SQL
ATTACH '$(_db_escape "$_STORE_LIVE")' AS origin;
BEGIN;
INSERT INTO store_reads SELECT '$id',(SELECT data FROM origin.instances WHERE id='$id');
INSERT INTO instances(id,data) SELECT id,data FROM store_reads WHERE id='$id' AND data IS NOT NULL;
COMMIT;
SQL
}

_store_tx_new() {
    _store_tx_import "$1" || return
    [[ $(_store_tx_sql "SELECT count(*) FROM instances WHERE id='$1';") == 0 ]] || {
        _store_tx_fail 'Object constructor requires an unused id'; return 1;
    }
}

_store_tx_get() {
    _store_tx_import "$1" || return
    _store_tx_sql "SELECT data FROM instances WHERE id='$1';"
}

_store_tx_put() {
    [[ ${_STORE_READONLY:-0} == 0 ]] || { _store_tx_fail 'Conflict replay would write; retry explicitly'; return 1; }
    # Record absence too: a constructor may not overwrite an existing live id.
    _store_tx_import "$1" || return
    _store_tx_sql "INSERT OR REPLACE INTO instances(id,data) VALUES('$1',json('$(_db_escape "$2")'));"
}

# Internal storage adapters supply scalar SELECTs (without trailing semicolons).
# Membership is checked again under the commit lock, including empty results.
_store_guarded_query() {
    [[ -n ${_STORE_TX:-} ]] || { _db_sql "$1"; return; }
    local query="$1" escaped result
    escaped=$(_db_escape "$query")
    result=$(_store_tx_sql "SELECT result FROM store_queries WHERE sql='$escaped';") || return
    if [[ -z "$result" ]]; then
        result=$(SQLITE_JSON_DB="$_STORE_LIVE" _store_tx_sql "$query") || return
        _store_tx_sql "INSERT INTO store_queries VALUES('$escaped','$(_db_escape "$result")');" || return
    fi
    printf '%s\n' "$result"
}

# Structured indexed equality/membership query. No domain rules in this layer.
_store_matching() {
    local cls="$1" filter="$2" predicates
    predicates=$(printf '%s' "$filter" | jq -er '
      def quote: "\u0027" + (tostring | gsub("\u0027";"\u0027\u0027")) + "\u0027";
      def literal:
        if type=="string" then quote
        elif type=="number" then tojson
        elif type=="boolean" then if . then "1" else "0" end
        else error("Expected scalar query value") end;
      if type != "object" then error("Expected field map") else to_entries end | map(
        if (.key | test("^[A-Za-z_][A-Za-z0-9_]*$")) then
          ("json_extract(data," + ("$." + .key | quote) + ")") as $path |
          (if (.value|type)=="array" then .value else [.value] end) as $values |
          ($path + " IN (" + ($values | map(select(.!=null)|literal) | join(",")) + ")") as $members |
          if ($values | index(null)) != null then "(" + $members + " OR " + $path + " IS NULL)" else $members end
        else error("Invalid field") end) | if length==0 then "1" else join(" AND ") end') || return 1
    _store_guarded_query "SELECT coalesce(json_group_array(id),'[]') FROM
      (SELECT id FROM instances WHERE class='$(_db_escape "$cls")' AND $predicates ORDER BY id)"
}

# Auxiliary rows are inserts, never replacement/upsert: the live schema owns
# uniqueness and constraints. Their SQL is generated from validated identifiers.
_store_tx_insert() {
    [[ -n ${_STORE_TX:-} && ${_STORE_READONLY:-0} == 0 ]] || { _store_tx_fail 'Row insert requires a writable transaction'; return 1; }
    [[ "$1" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ && "$1" != instances && "$1" != store_* ]] || { _store_tx_fail 'Invalid auxiliary table'; return 1; }
    local statement
    statement=$(printf '%s' "$2" | jq -er --arg table "$1" '
      def quote: "\u0027" + (tostring | gsub("\u0027";"\u0027\u0027")) + "\u0027";
      to_entries | if length>0 and all(.key | test("^[A-Za-z_][A-Za-z0-9_]*$")) then
        "INSERT INTO " + $table + "(" + (map(.key)|join(",")) + ") VALUES(" +
        (map(if .value==null then "NULL" elif (.value|type)=="object" or (.value|type)=="array" then (.value|tojson|quote) else (.value|quote) end)|join(",")) + ");"
      else error("Invalid row") end') || { _store_tx_fail 'Invalid row'; return 1; }
    _store_tx_sql "INSERT INTO store_inserts(sql) VALUES('$(_db_escape "$statement")');"
}

_store_tx_commit() {
    local guards inserts
    guards=$(SQLITE_JSON_DB="$_STORE_TX/work.db" _db_sql_direct "SELECT
      'INSERT INTO store_guard SELECT (' || sql || ') IS ' || quote(result) || ';' FROM store_queries;") || return
    inserts=$(SQLITE_JSON_DB="$_STORE_TX/work.db" _db_sql_direct 'SELECT sql FROM store_inserts ORDER BY rowid;') || return
    _db_sql_direct -bail <<SQL
ATTACH '$(_db_escape "$_STORE_TX/work.db")' AS staged;
BEGIN IMMEDIATE;
CREATE TEMP TABLE store_guard(ok INTEGER NOT NULL CONSTRAINT store_transaction_conflict CHECK(ok=1));
INSERT INTO store_guard SELECT NOT EXISTS(SELECT 1 FROM staged.store_reads r
 LEFT JOIN main.instances current ON current.id=r.id WHERE current.data IS NOT r.data);
$guards
UPDATE main.instances SET data=(SELECT data FROM staged.instances s WHERE s.id=main.instances.id)
 WHERE id IN (SELECT s.id FROM staged.instances s JOIN staged.store_reads b USING(id) WHERE b.data IS NOT NULL AND s.data!=b.data);
INSERT INTO main.instances(id,data)
 SELECT s.id,s.data FROM staged.instances s JOIN staged.store_reads b USING(id) WHERE b.data IS NULL;
$inserts
COMMIT;
SQL
}

_store_transaction_once() (
    [[ -z ${_STORE_TX:-} ]] || { _store_tx_fail 'Nested transaction'; return 1; }
    local live_env="$_ENV_DIR" status=0 id
    export _STORE_LIVE="$SQLITE_JSON_DB" _STORE_TX
    _STORE_TX=$(mktemp -d "${TMPDIR:-/tmp}/trash-store-tx.XXXXXX") || return
    trap 'rm -rf "$_STORE_TX"' EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    export TRASHTALK_NO_NATIVE=1
    local live_honker_cmd="${_HONKER_LOAD_CMD:-}" live_honker_available="${_HONKER_AVAILABLE:-0}"
    local _HONKER_LOAD_CMD='' _HONKER_AVAILABLE=0
    mkdir "$_STORE_TX/env" || return
    SQLITE_JSON_DB="$_STORE_TX/work.db" _db_sql_direct <<'SQL' || return
CREATE TABLE instances(id TEXT PRIMARY KEY,data JSON NOT NULL,class TEXT GENERATED ALWAYS AS (json_extract(data,'$.class')) VIRTUAL);
      CREATE TABLE store_reads(id TEXT PRIMARY KEY,data TEXT);
      CREATE TABLE store_queries(sql TEXT PRIMARY KEY,result TEXT NOT NULL);
      CREATE TABLE store_inserts(sql TEXT NOT NULL);
      CREATE TABLE store_callbacks(receiver TEXT,selector TEXT,argument TEXT);
SQL
    (
        export SQLITE_JSON_DB="$_STORE_TX/work.db" _ENV_DIR="$_STORE_TX/env"
        @ "$@"
    ) > "$_STORE_TX/result" || status=$?
    [[ $status == 0 && ! -e "$_STORE_TX/failed" ]] || return 1
    # Test seam: defined only by fault/concurrency tests, against the live store.
    if [[ ${_STORE_IN_BARRIER:-0} == 0 ]] && declare -F _store_tx_before_commit >/dev/null; then
        (export _STORE_BARRIER_TX="$_STORE_TX"; local _STORE_IN_BARRIER=1; unset _STORE_TX; _store_tx_before_commit) || return
    fi
    if ! _store_tx_commit 2>"$_STORE_TX/commit-error"; then
        if [[ $(<"$_STORE_TX/commit-error") == *store_transaction_conflict* ]]; then return 75; fi
        cat "$_STORE_TX/commit-error" >&2
        return 1
    fi
    while IFS= read -r id; do
        [[ -z "$id" ]] || rm -f "$live_env/$id"
    done < <(SQLITE_JSON_DB="$_STORE_TX/work.db" _db_sql_direct 'SELECT id FROM store_reads;')
    local callback receiver selector argument
    while IFS= read -r callback; do
        receiver=$(printf '%s' "$callback" | jq -r .receiver)
        selector=$(printf '%s' "$callback" | jq -r .selector)
        argument=$(printf '%s' "$callback" | jq -r .argument)
        (_STORE_TX='' _HONKER_LOAD_CMD="$live_honker_cmd" _HONKER_AVAILABLE="$live_honker_available" @ "$receiver" "$selector" "$argument") >/dev/null 2>&1 || true
    done < <(SQLITE_JSON_DB="$_STORE_TX/work.db" _db_sql_direct "SELECT json_object('receiver',receiver,'selector',selector,'argument',argument) FROM store_callbacks;")
    cat "$_STORE_TX/result"
)

_store_transaction() {
    local replay="$1" rc=0
    shift
    _store_transaction_once "$@" || rc=$?
    if [[ $rc == 75 && $replay == true ]]; then
        # Only recognize a result already committed by a competitor. Never
        # automatically rerun mutations or rebase an obsolete decision.
        _STORE_READONLY=1 _store_transaction_once "$@"
        return
    fi
    [[ $rc != 75 ]] || printf 'Store transaction conflicted; reload and retry.\n' >&2
    return "$rc"
}
