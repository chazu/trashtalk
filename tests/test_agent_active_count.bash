#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
api="$TRASHTALK_DIR/bin/trash-active-sessions"
[[ $(@ Agent::Session activeCount) == 0 ]]
[[ $("$api") == 0 ]]
# Fixtures use raw persisted records: no workers, inboxes or lifecycle effects.
_db_sql "INSERT INTO instances(id,data) VALUES
 ('s1','{\"class\":\"Agent::Session\",\"lifecycleState\":\"closed\"}'),
 ('s2','{\"class\":\"Agent::Session\"}'), ('s3','{\"class\":\"Agent::Session\"}'),
 ('idle','{\"class\":\"Agent::Session\"}'), ('wrong','{\"class\":\"Object\"}');"
fixture=0
for pair in 's1 starting' 's1 running' 's2 recovering' 's3 running' \
            'idle completed' 'idle failed' 'idle cancelled' 'idle stopped' \
            'missing running' 'wrong running'; do
    read -r session state <<< "$pair"
    fixture=$((fixture + 1))
    _db_sql "INSERT INTO instances(id,data) VALUES('r_$fixture',json_object('class','Agent::Run','session','$session','state','$state'));"
done
_db_sql "INSERT INTO instances(id,data) VALUES
 ('empty_run','{\"class\":\"Agent::Run\",\"session\":\"\",\"state\":\"running\"}'),
 ('null_run','{\"class\":\"Agent::Run\",\"state\":\"running\"}'),
 ('wrong_run','{\"class\":\"Object\",\"session\":\"idle\",\"state\":\"running\"}');
 WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i+1 FROM n WHERE i<3000)
 INSERT INTO instances(id,data) SELECT 'history_'||i,json_object('class','Agent::Run','session','idle','state','completed') FROM n;"
[[ $(@ Agent::Session activeCount) == 3 ]]
[[ $("$api") == 3 ]]
# Inspect the actual API SQL, not an independently maintained query.
cat > "$TMPDIR/sqlite-probe" <<'PROBE'
#!/usr/bin/env bash
sql=${!#}
printf '%s' "$sql" > "$COUNT_SQL"
exec "$REAL_SQLITE" "$@"
PROBE
chmod +x "$TMPDIR/sqlite-probe"
export COUNT_SQL="$TMPDIR/count.sql" REAL_SQLITE="$_SQLITE3"
[[ $(TRASH_SQLITE3="$TMPDIR/sqlite-probe" "$api") == 3 ]]
sql=$(cat "$COUNT_SQL")
plan=$(_db_sql "EXPLAIN QUERY PLAN SELECT ${sql#*SELECT }")
[[ $plan == *'SCAN r USING INDEX agent_active_runs_session'* ]]
[[ $plan == *'SEARCH s'*'USING INDEX sqlite_autoindex_instances_1 (id=?)'* ]]
[[ $plan != *'SCAN s'* ]]
# No jq, runtime, jo, uuidgen or other external process is needed by the CLI.
mkdir "$TMPDIR/path"
ln -s "$BASH" "$TMPDIR/path/bash"
ln -s "$(command -v sqlite3)" "$TMPDIR/path/sqlite3"
[[ $(PATH="$TMPDIR/path" TRASH_SQLITE3= SQLITE3= _SQLITE3= "$api") == 3 ]]
[[ $(PATH="$TMPDIR/path" TRASH_SQLITE3= SQLITE3= _SQLITE3= /bin/bash "$api") == 3 ]]
[[ $(jq() { return 99; }; @ Agent::Session activeCount) == 3 ]]
# Changes are visible on the next read, including removal of a tracked session.
_db_sql "UPDATE instances SET data=json_set(data,'$.state','completed') WHERE class='Agent::Run';"
[[ $("$api") == 0 ]]
_db_sql "UPDATE instances SET data=json_set(data,'$.state','recovering') WHERE id='history_1';"
[[ $("$api") == 1 ]]
_db_sql "DELETE FROM instances WHERE id='idle';"
[[ $("$api") == 0 ]]
# Exact counts are not truncated by a listing limit.
_db_sql "WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i+1 FROM n WHERE i<1100)
 INSERT INTO instances(id,data) SELECT 'bulk_s_'||i,json_object('class','Agent::Session') FROM n;
 INSERT INTO instances(id,data) SELECT 'bulk_r_'||id,json_object('class','Agent::Run','session',id,'state','running') FROM instances WHERE id LIKE 'bulk_s_%';"
[[ $("$api") == 1100 ]]
[[ $(@ Agent::Session activeCount) == 1100 ]]
# No absent-store creation. Errors are not false zeroes.
[[ $(SQLITE_JSON_DB="$TMPDIR/absent.db" "$api") == 0 ]]
[[ ! -e "$TMPDIR/absent.db" ]]
printf invalid > "$TMPDIR/broken.db"
if SQLITE_JSON_DB="$TMPDIR/broken.db" "$api" > "$TMPDIR/out" 2>/dev/null; then exit 1; fi
[[ ! -s "$TMPDIR/out" ]]
echo 'PASS: active session semantics, indexed plan, jq-free CLI and failures'
