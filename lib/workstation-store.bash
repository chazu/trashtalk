# Internal SQL serialization boundary. Guard live membership and remember staged
# claims separately because Store auxiliary inserts are applied only at commit.
_workstation_coordinate_key() {
    @ Store requireTransaction || return
    printf '%s' "$1" | jq -er '
      select(type=="object" and (keys==["offset","partition","streamName","subscription"])) |
      select((.subscription|test("^eventsubscription_[A-Za-z0-9-]{1,64}$")) and
        (.streamName|type=="string" and length>0 and length<=256 and (test("[\\x00-\\x1f\\x7f]")|not)) and
        (.partition|test("^[A-Za-z0-9_.-]{1,64}$")) and
        (.offset|type=="number" and floor==. and .>=0 and .<=9007199254740991)) |
      def q: "\u0027" + (tostring|gsub("\u0027";"\u0027\u0027")) + "\u0027";
      "subscription_id="+(.subscription|q)+" AND stream_name="+(.streamName|q)+
      " AND partition="+(.partition|q)+" AND offset="+(.offset|tostring)'
}
_workstation_claimed() {
    local key local_link
    key=$(_workstation_coordinate_key "$1") || return 1
    _store_tx_sql 'CREATE TABLE IF NOT EXISTS workstation_pending(subscription_id TEXT,stream_name TEXT,partition TEXT,offset INTEGER,attention_id TEXT);' || return
    local_link=$(_store_tx_sql "SELECT attention_id FROM workstation_pending WHERE $key;") || return
    if [[ -n $local_link ]]; then printf '%s\n' "$local_link"; else
      _store_guarded_query "SELECT coalesce((SELECT attention_id FROM workstation_coordinates WHERE $key),'')"
    fi
}
_workstation_stage_coordinate() {
    local row sql
    @ Store requireTransaction || return
    [[ ${_STORE_READONLY:-0} == 0 ]] || { _store_tx_fail 'Coordinate write in read-only replay'; return 1; }
    _workstation_coordinate_key "$1" >/dev/null || return
    row=$(jq -cn --argjson c "$1" --arg a "$2" '{subscription_id:$c.subscription,stream_name:$c.streamName,partition:$c.partition,offset:$c.offset,attention_id:$a}') || return
    @ Store insert: "$row" into: workstation_coordinates || return
    sql=$(jq -nr --argjson r "$row" '$r|[.subscription_id,.stream_name,.partition,.offset,.attention_id]|map("\u0027"+(tostring|gsub("\u0027";"\u0027\u0027"))+"\u0027")|join(",")') || return
    _store_tx_sql "INSERT INTO workstation_pending VALUES($sql);"
}
_workstation_coordinate_range() {
    local key start end limit
    key=$(_workstation_coordinate_key "$1") || return
    start=$(jq -r .offset <<<"$1")
    end=$2 limit=$3
    [[ $end =~ ^[0-9]+$ && $limit =~ ^[0-9]+$ && ${#end} -le 16 && ${#limit} -le 4 ]] || return 1
    ((end >= start && end <= 9007199254740991 && limit >= 1 && limit <= 1000)) || return 1
    key=${key% AND offset=*}
    _store_guarded_query "SELECT coalesce(json_group_array(json_object('offset',offset,'attention',attention_id)),'[]') FROM (SELECT offset,attention_id FROM workstation_coordinates WHERE $key AND offset BETWEEN $start AND $end ORDER BY offset LIMIT $limit)"
}
