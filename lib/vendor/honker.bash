#!/usr/bin/env bash
# honker.bash - Bash wrapper for the Honker SQLite extension
# Provides pub/sub, work queues, durable streams, and scheduling
# Requires: honker SQLite extension (libhonker_ext.dylib/.so)
# Depends on: sqlite-json.bash (must be sourced first for _db_sql)

[[ "${BASH_SOURCE[0]}" != "${0}" ]] || {
    echo "ERROR: source this file, don't run it directly" >&2
    exit 1
}

########################
# AVAILABILITY
########################

honker_available() {
    [[ "$_HONKER_AVAILABLE" == "1" ]]
}

# Bootstrap honker tables (idempotent)
honker_bootstrap() {
    honker_available || return 1
    _db_sql "SELECT honker_bootstrap();" >/dev/null 2>&1
}

########################
# EPHEMERAL PUB/SUB
########################

# Fire-and-forget notification on a channel
# Usage: honker_notify <channel> <payload_json>
honker_notify() {
    local channel="$1" payload="$2"
    local escaped_payload
    escaped_payload=$(_db_escape "$payload")
    _db_sql "SELECT notify('$channel', '$escaped_payload');" >/dev/null
}

# Start a background listener on a channel
# Calls callback_fn with each message payload
# Returns the PID of the background listener process
# Usage: honker_listen <channel> <callback_fn> [poll_ms=10]
honker_listen() {
    local channel="$1" callback="$2" poll_ms="${3:-10}"
    local poll_sleep
    if [[ "$poll_ms" -lt 100 ]]; then
        poll_sleep="0.0${poll_ms}"
    else
        poll_sleep="0.${poll_ms}"
    fi

    # Detach the subshell from the caller's stdin/stdout so that
    # `pid=$(honker_listen ...)` doesn't block waiting for the inherited
    # cmdsub pipe to close. Callbacks fire with the parent's original stdout.
    # Each _db_sql invocation opens a fresh sqlite3 connection, so
    # `PRAGMA data_version` always reads 1 from this side and can't be
    # used as a change-detection shortcut. Poll the table directly.
    # Track a per-listener cursor (last seen id) instead of deleting,
    # so multiple subscribers on the same channel each see every event.
    (
        exec </dev/null >/dev/null
        local last_id=0
        # Skip messages already in the table at start-up
        last_id=$(_db_sql "SELECT COALESCE(MAX(id), 0) FROM _honker_notifications WHERE channel = '$channel';")
        last_id="${last_id:-0}"
        while true; do
            local rows
            # '|' is a safe separator: id is numeric, so the first '|' is
            # always ours even when the payload contains one. A control
            # char (char(31)) is not safe: sqlite3 >= 3.5x escapes control
            # characters in output by default, breaking the split.
            rows=$(_db_sql "SELECT id || '|' || payload FROM _honker_notifications WHERE channel = '$channel' AND id > $last_id ORDER BY id;")
            if [[ -n "$rows" ]]; then
                while IFS= read -r row; do
                    local id="${row%%|*}"
                    local payload="${row#*|}"
                    "$callback" "$payload"
                    last_id="$id"
                done <<<"$rows"
            fi
            sleep "$poll_sleep"
        done
    ) &
    echo $!
}

# Stop a background listener
# Usage: honker_unlisten <pid>
honker_unlisten() {
    local pid="$1"
    # SIGTERM can be deferred while the subshell is inside a command
    # substitution, so escalate to SIGKILL after a short grace period.
    kill "$pid" 2>/dev/null
    for _ in 1 2 3 4 5; do
        kill -0 "$pid" 2>/dev/null || break
        sleep 0.05
    done
    kill -9 "$pid" 2>/dev/null
    wait "$pid" 2>/dev/null
}

########################
# WORK QUEUE
########################

# Enqueue a job onto a named queue
# Usage: honker_enqueue <queue> <payload_json> [priority=0] [max_attempts=3] [delay_seconds=0]
honker_enqueue() {
    local queue="$1" payload="$2"
    local priority="${3:-0}" max_attempts="${4:-3}" delay="${5:-0}"
    local escaped_payload run_at
    escaped_payload=$(_db_escape "$payload")

    if [[ "$delay" -gt 0 ]]; then
        run_at="datetime('now', '+${delay} seconds')"
    else
        run_at="NULL"
    fi

    _db_sql "SELECT honker_enqueue('$queue', '$escaped_payload', NULL, NULL, $priority, $max_attempts, $run_at);" 2>/dev/null
}

# Claim jobs from a queue for processing
# Returns JSON array of claimed jobs
# Usage: honker_claim <queue> [worker_id] [batch_size=1] [visibility_timeout=30]
honker_claim() {
    local queue="$1"
    local worker_id="${2:-worker_$$}"
    local batch="${3:-1}" timeout="${4:-30}"
    # honker_claim_batch returns a JSON string; -json mode wraps it in a
    # row keyed by the function-call text, so unwrap it here.
    _db_sql_json "SELECT honker_claim_batch('$queue', '$worker_id', $batch, $timeout) AS jobs;" \
        | jq -c '.[0].jobs | fromjson // []' 2>/dev/null
}

# Acknowledge successful processing of jobs
# Usage: honker_ack <job_ids_json> [worker_id]
honker_ack() {
    local job_ids="$1" worker_id="${2:-worker_$$}"
    _db_sql "SELECT honker_ack_batch('$job_ids', '$worker_id');" >/dev/null
}

# Sweep expired/timed-out jobs back to pending
# Usage: honker_sweep <queue>
honker_sweep() {
    local queue="$1"
    _db_sql "SELECT honker_sweep_expired('$queue');" >/dev/null
}

########################
# DISTRIBUTED LOCKS
########################

# Acquire a named lock
# Usage: honker_lock <name> [owner_id] [ttl_seconds=30]
honker_lock() {
    local name="$1" owner="${2:-$$}" ttl="${3:-30}"
    _db_sql "SELECT honker_lock_acquire('$name', '$owner', $ttl);"
}

# Release a named lock
# Usage: honker_unlock <name> [owner_id]
honker_unlock() {
    local name="$1" owner="${2:-$$}"
    _db_sql "SELECT honker_lock_release('$name', '$owner');" >/dev/null
}

########################
# RATE LIMITING
########################

# Try to perform a rate-limited action
# Returns 1 if allowed, 0 if rate limited
# Usage: honker_rate_limit <key> <limit> <window_seconds>
honker_rate_limit() {
    local key="$1" limit="$2" window="$3"
    _db_sql "SELECT honker_rate_limit_try('$key', $limit, $window);"
}

########################
# DURABLE STREAMS
########################

# Publish a message to a durable stream
# Usage: honker_stream_publish <stream> <key> <payload_json>
honker_stream_publish() {
    local stream="$1" key="${2:-default}" payload="$3"
    local escaped_payload
    escaped_payload=$(_db_escape "$payload")
    _db_sql "SELECT honker_stream_publish('$stream', '$key', '$escaped_payload');" >/dev/null
}

# Read messages from a stream since an offset
# Returns JSON array of messages
# Usage: honker_stream_read <stream> [offset=0] [limit=10]
honker_stream_read() {
    local stream="$1" offset="${2:-0}" limit="${3:-10}"
    _db_sql_json "SELECT honker_stream_read_since('$stream', $offset, $limit) AS msgs;" \
        | jq -c '.[0].msgs | fromjson // []' 2>/dev/null
}

# Save consumer offset for a stream
# Usage: honker_stream_save_offset <consumer> <stream> <offset>
honker_stream_save_offset() {
    local consumer="$1" stream="$2" offset="$3"
    _db_sql "SELECT honker_stream_save_offset('$consumer', '$stream', $offset);" >/dev/null
}

# Get consumer offset for a stream
# Usage: honker_stream_get_offset <consumer> <stream>
honker_stream_get_offset() {
    local consumer="$1" stream="$2"
    _db_sql "SELECT honker_stream_get_offset('$consumer', '$stream');"
}

########################
# SCHEDULER
########################

# Register a scheduled task
# Usage: honker_schedule <schedule_name> <task_name> <cron_expr> <payload_json> [max_attempts=3]
honker_schedule() {
    local schedule="$1" task="$2" cron="$3" payload="$4" max_attempts="${5:-3}"
    local escaped_payload
    escaped_payload=$(_db_escape "$payload")
    _db_sql "SELECT honker_scheduler_register('$schedule', '$task', '$cron', '$escaped_payload', $max_attempts, NULL);" >/dev/null
}

# Unregister a scheduled task
# Usage: honker_unschedule <schedule_name>
honker_unschedule() {
    local schedule="$1"
    _db_sql "SELECT honker_scheduler_unregister('$schedule');" >/dev/null
}

# Tick the scheduler — claims and returns due tasks
# Usage: honker_scheduler_tick
honker_scheduler_tick() {
    _db_sql_json "SELECT honker_scheduler_tick(unixepoch()) AS due;" \
        | jq -c '.[0].due | fromjson // []' 2>/dev/null
}

# Get seconds until the next scheduled task fires
# Usage: honker_scheduler_soonest
honker_scheduler_soonest() {
    _db_sql "SELECT honker_scheduler_soonest();"
}

########################
# RESULT STORAGE
########################

# Save a result for a job
# Usage: honker_result_save <job_id> <result_json> [ttl_seconds=3600]
honker_result_save() {
    local job_id="$1" result="$2" ttl="${3:-3600}"
    local escaped_result
    escaped_result=$(_db_escape "$result")
    _db_sql "SELECT honker_result_save($job_id, '$escaped_result', $ttl);" >/dev/null
}

# Get the result for a job
# Usage: honker_result_get <job_id>
honker_result_get() {
    local job_id="$1"
    _db_sql "SELECT honker_result_get($job_id);"
}

# Sweep expired results
honker_result_sweep() {
    _db_sql "SELECT honker_result_sweep();" >/dev/null
}

########################
# TRANSACTIONAL HELPERS
########################

# Atomically persist an instance AND notify a channel
# Both happen in a single SQLite transaction
# Usage: honker_persist_and_notify <instance_id> <data_json> <channel> <payload_json>
honker_persist_and_notify() {
    local id="$1" data="$2" channel="$3" payload="$4"
    local escaped_data escaped_payload
    escaped_data=$(_db_escape "$data")
    escaped_payload=$(_db_escape "$payload")
    _db_sql "
        BEGIN IMMEDIATE;
        INSERT OR REPLACE INTO instances (id, data) VALUES ('$id', json('$escaped_data'));
        SELECT notify('$channel', '$escaped_payload');
        COMMIT;
    " >/dev/null
}

# Atomically persist an instance AND enqueue a job
# Both happen in a single SQLite transaction
# Usage: honker_persist_and_enqueue <instance_id> <data_json> <queue> <payload_json>
honker_persist_and_enqueue() {
    local id="$1" data="$2" queue="$3" payload="$4"
    local escaped_data escaped_payload
    escaped_data=$(_db_escape "$data")
    escaped_payload=$(_db_escape "$payload")
    _db_sql "
        BEGIN IMMEDIATE;
        INSERT OR REPLACE INTO instances (id, data) VALUES ('$id', json('$escaped_data'));
        SELECT honker_enqueue('$queue', '$escaped_payload', NULL, NULL, 0, 3, NULL);
        COMMIT;
    " >/dev/null
}

########################
# EXPORTS
########################

export -f honker_available honker_bootstrap
export -f honker_notify honker_listen honker_unlisten
export -f honker_enqueue honker_claim honker_ack honker_sweep
export -f honker_lock honker_unlock
export -f honker_rate_limit
export -f honker_stream_publish honker_stream_read honker_stream_save_offset honker_stream_get_offset
export -f honker_schedule honker_unschedule honker_scheduler_tick honker_scheduler_soonest
export -f honker_result_save honker_result_get honker_result_sweep
export -f honker_persist_and_notify honker_persist_and_enqueue
