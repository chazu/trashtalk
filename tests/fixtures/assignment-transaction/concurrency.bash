# Sourced by the production transaction regression after its original regression journey. Every
# interference test pauses after the actual DSL reads, before live validation.

other_identity=$(must @ AgentIdentity named: independent-specialist)
other_session=$(must @ AgentSession openFor: "$other_identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
traffic_session=$(must @ AgentSession openFor: "$identity" archetype: "$arch" role: "$role" workspace: "$root" profile: shell)
traffic_work=$(must new_work 'Background progress')
traffic_msg=$(must @ Inbox send: 'Unrelated message' to: "session:$traffic_session" from: assignment-owner)

independent_pair() {
    local label="$1" left right left_delivery right_delivery pair_dir r1 r2 t1 t2
    local p1 p2 ready attempt
    pair_dir=$(mktemp -d "$tmp/pair.XXXXXX")
    left=$(must new_work "$label left")
    right=$(identity="$other_identity" session="$other_session" must new_work "$label right")
    left_delivery=$(field "$left" .delivery); right_delivery=$(field "$right" .delivery)
    mapfile -t pair < <(@ AgentRun startFor: "$session" profile: shell)
    r1=${pair[0]}; t1=${pair[1]}
    must @ "$r1" transitionTo: running >/dev/null
    mapfile -t pair < <(@ AgentRun startFor: "$other_session" profile: shell)
    r2=${pair[0]}; t2=${pair[1]}
    must @ "$r2" transitionTo: running >/dev/null
    check "$label: left claim" true "$(TRASHTALK_RUN_TOKEN="$t1" @ AgentDelivery claim: "$left_delivery" run: "$r1")"
    check "$label: right claim" true "$(TRASHTALK_RUN_TOKEN="$t2" @ AgentDelivery claim: "$right_delivery" run: "$r2")"
    _store_tx_before_commit() {
        touch "$pair_dir/ready-$BASHPID"
        local attempt
        for ((attempt=0; attempt<1500; attempt++)); do
            [[ ! -f "$pair_dir/release" ]] || return 0
            sleep 0.01
        done
        return 1
    }
    (TRASHTALK_RUN_TOKEN="$t1" complete "$left" done >"$pair_dir/left.out" 2>"$pair_dir/left.err"; echo "$?" >"$pair_dir/left.status") & p1=$!
    (TRASHTALK_RUN_TOKEN="$t2" complete "$right" done >"$pair_dir/right.out" 2>"$pair_dir/right.err"; echo "$?" >"$pair_dir/right.status") & p2=$!
    for ((attempt=0; attempt<1500; attempt++)); do
        ready=("$pair_dir"/ready-*)
        ((${#ready[@]} == 2)) && break
        sleep 0.01
    done
    # All this traffic occurs AFTER both snapshots. Background actor operations
    # use ordinary public APIs and must survive both commits without lost data.
    @ "$traffic_msg" markRead >/dev/null
    (unset -f _store_tx_before_commit; @ "$traffic_work" progress: "$label background evidence" >/dev/null)
    new_traffic=$(must @ Inbox send: "$label arriving message" to: "session:$traffic_session" from: assignment-owner)
    if [[ "$label" == affected ]]; then
        @ Store patch: "$left_delivery" with: '{"note":"Concurrent intervention"}' >/dev/null
    fi
    touch "$pair_dir/release"
    must wait "$p1"; must wait "$p2"
    unset -f _store_tx_before_commit
    check "$label: both transactions staged before interference" 2 "${#ready[@]}"
    if [[ "$label" == affected ]]; then
        check 'changed delivery rejects only affected completion' 75 "$(cat "$pair_dir/left.status")"
        check 'rejected completion emits no result' '' "$(cat "$pair_dir/left.out")"
        check 'affected work remains open' open "$(field "$left" .state)"
        check 'competing delivery edit survives' 'Concurrent intervention' "$(field "$left_delivery" .note)"
        check 'affected work publishes no result' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE id='message_${left}_outcome';")"
    else
        if [[ $(cat "$pair_dir/left.status") != 0 ]]; then cat "$pair_dir/left.err" >&2; fi
        check 'left independent completion succeeds' 0 "$(cat "$pair_dir/left.status")"
        check 'left completion emits correct result' "$left" "$(cat "$pair_dir/left.out")"
    fi
    if [[ $(cat "$pair_dir/right.status") != 0 ]]; then cat "$pair_dir/right.err" >&2; fi
    check "$label: right independent completion succeeds" 0 "$(cat "$pair_dir/right.status")"
    check "$label: right completion emits correct result" "$right" "$(cat "$pair_dir/right.out")"
    check "$label: read message stays read" read "$(field "$traffic_msg" .status)"
    check "$label: background progress survives" "$label background evidence" "$(field "$traffic_work" '.events[-1].body')"
    check "$label: arriving message survives" "$label arriving message" "$(field "$new_traffic" .body)"
    check "$label: unrelated outbox entry survives" 1 "$(_db_sql "SELECT count(*) FROM agent_outbox WHERE message_id='$new_traffic';")"
    check "$label: unrelated work stays open" open "$(field "$traffic_work" .state)"
    must @ "$r1" finishWith: succeeded outcome: '{}' error: '' >/dev/null
    must @ "$r2" finishWith: succeeded outcome: '{}' error: '' >/dev/null
}

independent_pair independent
independent_pair affected

# Inserts AND updates that change predicate membership must be detected. These
# bypass Assignment mutation intentionally, so only the query guard can see them.
g=$(must new_work 'Question inserted after an empty query')
gdelivery=$(field "$g" .delivery)
_store_tx_before_commit() {
    _db_sql "INSERT INTO instances(id,data) VALUES('message_new_question',json_object('class','Message','assignment','$g','body','New question'));
      INSERT INTO agent_questions(message_id,session,run,delivery_ids) VALUES('message_new_question','$session','','[]');"
}
reject 'new unanswered question invalidates empty query' complete "$g" done
unset -f _store_tx_before_commit
unpublished "$g" 'new question'
_db_sql "UPDATE agent_questions SET answer_id='answered' WHERE message_id='message_new_question';"

_db_sql "INSERT INTO instances(id,data) VALUES('agentrun_becomes_active',json_object('class','AgentRun','session','$session','state','succeeded'));"
_store_tx_before_commit() { @ Store patch: agentrun_becomes_active with: '{"state":"running"}' >/dev/null; }
reject 'previously inactive run becoming active invalidates query' complete "$g" done
unset -f _store_tx_before_commit
unpublished "$g" 'new query member'
db_delete agentrun_becomes_active

# A deterministic result key/outbox key must never overwrite a competing writer,
# even though those records were not part of the transaction's original reads.
_store_tx_before_commit() {
    _db_sql "INSERT INTO instances(id,data) VALUES('message_${g}_outcome',json_object('class','Message','body','Competing message'));"
}
reject 'new result key collision rolls back state changes' complete "$g" done
unset -f _store_tx_before_commit
check 'collision preserves competing message' 'Competing message' "$(field "message_${g}_outcome" .body)"
check 'message collision leaves Assignment open' open "$(field "$g" .state)"
check 'message collision leaves delivery pending' pending "$(field "$gdelivery" .state)"
db_delete "message_${g}_outcome"
_store_tx_before_commit() {
    _db_sql "INSERT INTO agent_outbox(message_id,inbox) VALUES('message_${g}_outcome','competing-inbox');"
}
reject 'new outbox key collision rolls back publication' complete "$g" done
unset -f _store_tx_before_commit
check 'collision preserves competing outbox entry' competing-inbox "$(_db_sql "SELECT inbox FROM agent_outbox WHERE message_id='message_${g}_outcome';")"
check 'outbox collision leaves no Message' 0 "$(_db_sql "SELECT count(*) FROM instances WHERE id='message_${g}_outcome';")"
check 'outbox collision leaves Assignment open' open "$(field "$g" .state)"
_db_sql "DELETE FROM agent_outbox WHERE message_id='message_${g}_outcome';"

_store_tx_before_commit() {
    _db_sql "INSERT INTO instances(id,data) VALUES('agentrun_unrelated_active',json_object('class','AgentRun','session','$traffic_session','state','running'));
      INSERT INTO instances(id,data) VALUES('message_unrelated_question',json_object('class','Message','assignment','$traffic_work'));
      INSERT INTO agent_questions(message_id,session,run,delivery_ids) VALUES('message_unrelated_question','$traffic_session','','[]');"
}
must complete "$g" done >/dev/null
unset -f _store_tx_before_commit
check 'unrelated active run and unanswered question do not conflict' completed "$(field "$g" .state)"

# Three runs at each size. Index creation and synthetic history seeding are
# excluded from timings. Count logical dependencies, not just successful writes.
for history_size in 100 1000 10000; do
    _db_sql "WITH RECURSIVE n(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM n WHERE x<$history_size)
      INSERT OR IGNORE INTO instances(id,data) SELECT 'history_run_'||x,json_object('class','AgentRun','session','$traffic_session','state','running') FROM n;
      WITH RECURSIVE n(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM n WHERE x<$history_size)
      INSERT OR IGNORE INTO instances(id,data) SELECT 'history_message_'||x,json_object('class','Message','assignment','$traffic_work','body',printf('%01024d',x)) FROM n;
      WITH RECURSIVE n(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM n WHERE x<$history_size)
      INSERT OR IGNORE INTO agent_questions(message_id,session,run,delivery_ids) SELECT 'history_message_'||x,'$traffic_session','','[]' FROM n;
      WITH RECURSIVE n(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM n WHERE x<$history_size)
      INSERT OR IGNORE INTO agent_outbox(message_id,inbox) SELECT 'history_message_'||x,'agent:history' FROM n;" || exit 1
    # Capture the actual stored query definitions and explain those same queries.
    _store_tx_before_commit() {
        SQLITE_JSON_DB="$_STORE_BARRIER_TX/work.db" _db_sql "SELECT json_object(
          'copied_records',(SELECT count(*) FROM store_reads WHERE data IS NOT NULL),
          'record_guards',(SELECT count(*) FROM store_reads),
          'query_guards',(SELECT count(*) FROM store_queries),
          'query_result_rows',(SELECT coalesce(sum(json_array_length(result)),0) FROM store_queries),
          'written_records',(SELECT count(*) FROM instances s JOIN store_reads b USING(id) WHERE b.data IS NULL OR s.data!=b.data),
          'outbox_rows',(SELECT count(*) FROM store_inserts));" > "$tmp/metrics.json"
        SQLITE_JSON_DB="$_STORE_BARRIER_TX/work.db" _db_sql -json 'SELECT sql FROM store_queries;' > "$tmp/queries.json"
    }
    for trial in 1 2 3; do
        h=$(unset -f _store_tx_before_commit; must new_work "History size $history_size trial $trial")
        started=$EPOCHREALTIME
        must complete "$h" done >/dev/null
        elapsed=$(awk -v start="$started" -v end="$EPOCHREALTIME" 'BEGIN {printf "%.3f", end-start}')
        metrics=$(cat "$tmp/metrics.json")
        check 'only three relevant objects copied for human completion' 3 "$(jq -r .copied_records <<< "$metrics")"
        check 'three objects and one absent result key guarded' 4 "$(jq -r .record_guards <<< "$metrics")"
        check 'both negative queries guarded' 2 "$(jq -r .query_guards <<< "$metrics")"
        check 'unrelated query results are not copied' 0 "$(jq -r .query_result_rows <<< "$metrics")"
        check 'completion writes exactly three objects' 3 "$(jq -r .written_records <<< "$metrics")"
        check 'completion publishes one outbox row' 1 "$(jq -r .outbox_rows <<< "$metrics")"
        jq -cn --argjson metrics "$metrics" --argjson size "$history_size" --argjson trial "$trial" --argjson seconds "$elapsed" \
          '$metrics + {unrelated_objects:($size*2),unrelated_questions:$size,unrelated_outbox_rows:$size,trial:$trial,seconds:$seconds}' | sed 's/^/MEASURE /'
    done
    unset -f _store_tx_before_commit
    mapfile -t plans < <(jq -r '.[] | .sql | @base64' "$tmp/queries.json")
    : > "$tmp/plans"
    for query in "${plans[@]}"; do
        query=$(printf '%s' "$query" | base64 -d)
        _db_sql "EXPLAIN QUERY PLAN $query" >> "$tmp/plans"
    done
    check 'run query uses the session/state index' true "$(rg -q 'USING INDEX agent_runs_session_state' "$tmp/plans" && echo true || echo false)"
    check 'question query uses the Assignment index' true "$(rg -q 'USING INDEX agent_messages_assignment' "$tmp/plans" && echo true || echo false)"
done
