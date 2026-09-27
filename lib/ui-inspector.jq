# Projection of an explicit, inert JSON snapshot; no runtime reflection here.
def envelope($type): {schema_version:1,view:"inspector",type:$type};
def pane_key($path): "pane-"+($path|tojson);
def slots($c;$path): ($c.record|getpath($path)) as $value |
    (if ($value|type)=="object" or ($value|type)=="array" then $value|to_entries else [{key:"value",value:$value}] end) |
    map(. as $slot | {key:($slot.key|tostring),fields:{text:(($slot.key|tostring)+": "+($slot.value|tojson)),detail:($slot.value|tojson),path:(if ($value|type)=="object" or ($value|type)=="array" then $path+[$slot.key] else $path end|tojson),drillable:((($slot.value|type)=="object" or ($slot.value|type)=="array")|tostring)}});
def snapshot($c): envelope("init")+{revision:$c.revision,root:{kind:"panel",key:"inspector",props:{direction:"column",back_action:"back",forward_action:"forward"},children:[
  {kind:"panel",key:"panes",props:{direction:"row",gap:1},children:[$c.paths|to_entries[]|.key as $i|.value as $path|pane_key($path) as $key|
    {kind:"list",key:$key,props:{source:$key,field:"text",focused:($i==$c.active),title:(if $i==0 then $c.title else ($path[-1]|tostring) end),border:true,hidden:($i!=$c.active and $i!=($c.active-1)),min_parent_width:(if $i==($c.active-1) then 80 else 0 end),action:"drill"}}]},
  {kind:"panel",key:"navigation",props:{direction:"row",size:1},children:([
    {kind:"button",key:"back",props:{text:"← Back",action:"back",disabled:($c.active==0)}},
    {kind:"button",key:"forward",props:{text:"Forward →",action:"forward",disabled:($c.active==(($c.paths|length)-1))}}
  ]+[$c.paths|to_entries[]|{kind:"button",key:("dot-"+(.key|tostring)),props:{text:(if .key==$c.active then "●" else "○" end),action:("goto:"+(.key|tostring))}}])}
]},collections:[$c.paths[]|. as $path|slots($c;$path) as $rows|{id:pane_key($path),revision:$c.revision,total:($rows|length),rows:$rows}]};
if $mode=="init" then snapshot($context)
elif $mode=="handle" then
  (if $frame.intent=="resync" then $context
   elif $frame.intent!="action" then error("unknown inspector intent")
   elif $frame.action=="back" then $context+{active:([$context.active-1,0]|max)}
   elif $frame.action=="forward" then $context+{active:([$context.active+1,($context.paths|length)-1]|min)}
   elif ($frame.action|startswith("goto:")) then ($frame.action|ltrimstr("goto:")|tonumber) as $index|
     if $index>=0 and $index<($context.paths|length) then $context+{active:$index} else error("invalid stack index") end
   elif $frame.action=="drill" then
     # Validate against slots we actually presented, including the pane identity.
     [$context.paths|to_entries[]|select(pane_key(.value)==$frame.widget)]|.[0] as $entry|
     if $entry==null then error("unknown pane") else
       (slots($context;$entry.value)|map(select(.key==$frame.value.key))|.[0]) as $slot|
       if $slot==null or $slot.fields.drillable!="true" then error("select an object or array to drill")
       elif $entry.key>=15 then error("inspection stack limit (16 entries)")
       else $context+{paths:($context.paths[:$entry.key+1]+[($slot.fields.path|fromjson)]),active:($entry.key+1)} end end
   else error("unknown inspector action") end) as $next|
  ($next+{revision:($context.revision+1)}) as $next|
  {context:$next,frames:([snapshot($next)]+if $frame.intent=="action" then [envelope("ack")+{request_id:$frame.request_id,ok:true,message:""}] else [] end)}
else error("unknown inspector operation") end
| if $mode=="handle" then .frames |= map(.+{caused_by:$frame.request_id}) else . end
