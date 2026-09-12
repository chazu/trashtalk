# Append only if all newly projected entries sort after the prior frontier.
# Otherwise request a full reread, preserving interleaving across runs and mail.
def key: [.order,.id];
def coalesce_entries($initial):
  reduce .[] as $entry ($initial;
    if ($entry.kind|endswith("_delta")) and length>0 and .[-1].kind==$entry.kind and .[-1].run==$entry.run
    then .[-1].text += $entry.text else . + [$entry] end);
$previous[0] as $old | $manifest[0] as $m | $records[0] as $r
| ($additions + (if $m.full then $rows else [] end) | sort_by(.order,.id)) as $new
| if ($m.full|not) and ($old.lastKey != null) and any($new[]; key < $old.lastKey)
  then {retry:true}
  else ($new | coalesce_entries(if $m.full then [] else $old.entries // [] end)) as $entries
    | {schema_version:1,key:$cache_key,rows:$rows,files:$m.files,
       lastKey:($new[-1] | if .==null then (if $m.full then null else $old.lastKey end) else key end),entries:$entries,
       snapshot:{schema_version:1,type:"snapshot",session:$r.session,
         has_earlier:($r.has_earlier==1 or ($entries|length)>$limit),window:$limit,entries:$entries[-$limit:]}}
  end
