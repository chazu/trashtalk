# Shared SQL boundary for Agent::Session activeCount and ordinary shell callers.
# No runtime load, JSON decoder, object hydration, or worker side effects.
trashtalk_active_session_count() {
    if [[ -n ${_STORE_TX:-} ]]; then
        printf 'Active session count is a persisted snapshot, not a Store transaction query.\n' >&2
        return 1
    fi
    local db="${SQLITE_JSON_DB:-${TRASHTALK_DIR:-$HOME/.trashtalk}/instances.db}"
    local sqlite="${TRASH_SQLITE3:-${SQLITE3:-${_SQLITE3:-sqlite3}}}"
    [[ -f "$db" ]] || { printf '0\n'; return; }
    # Idempotent lazy index installation also supports existing stores. Only
    # active runs occupy this index, so terminal history never enters the scan.
    # A short busy timeout bounds lock contention for interactive consumers.
    "$sqlite" -batch -bail -cmd '.timeout 50' "$db" "
      CREATE INDEX IF NOT EXISTS agent_active_runs_session
        ON instances(json_extract(data,'\$.session'))
        WHERE class='Agent::Run' AND json_extract(data,'\$.state') IN ('starting','running','recovering');
      SELECT count(DISTINCT json_extract(r.data,'\$.session'))
        FROM instances r INDEXED BY agent_active_runs_session
        WHERE r.class='Agent::Run' AND json_extract(r.data,'\$.state') IN ('starting','running','recovering')
          AND EXISTS (SELECT 1 FROM instances s
            WHERE s.id=json_extract(r.data,'\$.session') AND s.class='Agent::Session');"
}
