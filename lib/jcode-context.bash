#!/usr/bin/env bash
# Read Jcode's persisted compaction metadata without rewriting provider history.
# Newer metadata may be journaled; a partial final append is not a receipt.
_jcode_context_value() {
    local snapshot=$1 journal=${2:-${1%.json}.journal.jsonl}
    local field=${3:-compaction}
    [[ -f "$journal" ]] || journal=/dev/null
    jq -Sc --arg field "$field" --rawfile journal "$journal" '
      . as $snapshot |
      reduce ($journal | split("\n")[] | fromjson? | .meta |
        select(type == "object" and has("compaction"))) as $meta
        ({updated_at: ($snapshot.updated_at // ""), compaction: ($snapshot.compaction // null), working_dir: ($snapshot.working_dir // null)};
         if ($meta.updated_at // "") >= .updated_at then $meta else . end) |
      .[$field] // null' "$snapshot"
}

_jcode_compaction_state() { _jcode_context_value "$@"; }
