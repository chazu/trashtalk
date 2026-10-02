# Projection of an explicit, inert JSON snapshot; no runtime reflection here.
# An editable context inspects a live object record. Its scalar leaves under
# `data` can be staged as ObjectEditProposal results; the commit stays in the DSL.
def envelope($type): {schema_version:1,view:"inspector",type:$type};
def ack($ok;$message): envelope("ack")+{request_id:$frame.request_id,ok:$ok,message:$message};
def pane_key($path): "pane-"+($path|tojson);
def edit_key($path): "edit-"+($path|tojson);
def scalar: type!="object" and type!="array";
def editable($c;$path): $c.editable==true and ($path|length)>1 and $path[0]=="data" and ($c.record|getpath($path)|scalar);
def slots($c;$path): ($c.record|getpath($path)) as $value |
    (if $value|scalar then [{key:"value",value:$value}] else $value|to_entries end) |
    map(. as $slot | {key:($slot.key|tostring),fields:{text:(($slot.key|tostring)+": "+($slot.value|tojson)),detail:($slot.value|tojson),path:(if $value|scalar then $path else $path+[$slot.key] end|tojson),drillable:($slot.value|scalar|not|tostring)}});
def edit_panel($c): $c.edit.path as $path | edit_key($path) as $key |
  {kind:"panel",key:"edit",props:{direction:"row",size:3,gap:1},children:[
    {kind:"input",key:$key,props:{title:("Edit "+($path[1:]|map(tostring)|join("."))+" as JSON"),border:true,value:($c.record|getpath($path)|tojson),action:"apply",focused:true}},
    {kind:"button",key:"apply",props:{text:"Apply",input:$key,action:"apply",size:9}},
    {kind:"button",key:"cancel",props:{text:"Cancel",action:"cancel_edit",size:10}}]};
def snapshot($c): envelope("init")+{revision:$c.revision,root:{kind:"panel",key:"inspector",props:{direction:"column",back_action:"back",forward_action:"forward"},children:([
  {kind:"panel",key:"panes",props:{direction:"row",gap:1},children:[$c.paths|to_entries[]|.key as $i|.value as $path|pane_key($path) as $key|
    {kind:"list",key:$key,props:{source:$key,field:"text",focused:($i==$c.active and $c.edit==null),title:(if $i==0 then $c.title else ($path[-1]|tostring) end),border:true,hidden:($i!=$c.active and $i!=($c.active-1)),min_parent_width:(if $i==($c.active-1) then 80 else 0 end),action:"drill"}}]}]
  +(if $c.edit==null then [] else [edit_panel($c)] end)+[
  {kind:"panel",key:"navigation",props:{direction:"row",size:1},children:([
    {kind:"button",key:"back",props:{text:"← Back",action:"back",disabled:($c.active==0)}},
    {kind:"button",key:"forward",props:{text:"Forward →",action:"forward",disabled:($c.active==(($c.paths|length)-1))}}
  ]+[$c.paths|to_entries[]|{kind:"button",key:("dot-"+(.key|tostring)),props:{text:(if .key==$c.active then "●" else "○" end),action:("goto:"+(.key|tostring))}}])}
])},collections:[$c.paths[]|. as $path|slots($c;$path) as $rows|{id:pane_key($path),revision:$c.revision,total:($rows|length),rows:$rows}]};
def respond($next;$ok;$message):
  ($next+{revision:($context.revision+1)}) as $next |
  {context:$next,frames:([snapshot($next)]+if $frame.intent=="action" then [ack($ok;$message)] else [] end)};
if $mode=="init" then snapshot($context)
elif $mode=="handle" then
  (if $frame.intent=="resync" then $context
   elif $frame.intent!="action" then error("unknown inspector intent")
   elif $frame.action=="back" then $context+{active:([$context.active-1,0]|max),edit:null}
   elif $frame.action=="forward" then $context+{active:([$context.active+1,($context.paths|length)-1]|min),edit:null}
   elif $frame.action=="cancel_edit" then $context+{edit:null}
   elif ($frame.action|startswith("goto:")) then ($frame.action|ltrimstr("goto:")|tonumber) as $index|
     if $index>=0 and $index<($context.paths|length) then $context+{active:$index,edit:null} else error("invalid stack index") end
   elif $frame.action=="drill" then
     # Validate against slots we actually presented, including the pane identity.
     [$context.paths|to_entries[]|select(pane_key(.value)==$frame.widget)]|.[0] as $entry|
     if $entry==null then error("unknown pane") else
       (slots($context;$entry.value)|map(select(.key==$frame.value.key))|.[0]) as $slot|
       if $slot==null then error("unknown slot")
       elif $slot.fields.drillable!="true" then
         # Enter on a scalar leaf of a live object opens its editor.
         ($slot.fields.path|fromjson) as $path|
         if editable($context;$path) then $context+{edit:{path:$path}}
         else error("select an object or array to drill") end
       elif $entry.key>=15 then error("inspection stack limit (16 entries)")
       else $context+{paths:($context.paths[:$entry.key+1]+[($slot.fields.path|fromjson)]),active:($entry.key+1),edit:null} end end
   else error("unknown inspector action") end) as $next|
  respond($next;true;"")
elif $mode=="propose" then
  # The draft must be JSON. The proposal carries the presented snapshot, so a
  # concurrent change is rejected as stale rather than overwritten.
  ($context.edit.path // error("no leaf is being edited")) as $path|
  if ($frame.widget!=edit_key($path) and $frame.widget!="apply") or ($frame.value|type)!="string" then error("unknown edit widget")
  elif editable($context;$path)|not then error("leaf is not editable")
  else (try {value:($frame.value|fromjson)} catch null) as $parsed|
    if $parsed==null then respond($context;false;"Invalid JSON value; quote strings")
    else {proposal:{schema_version:1,outcome:"proposed",object_id:$context.record.object_id,class_name:$context.record.class_name,
      base_data:$context.record.data,proposal:{path:$path[1:],old_value:($context.record|getpath($path)),new_value:$parsed.value}}} end end
elif $mode=="settle" then
  # $extra carries the ObjectEditProposal result and a fresh record (or null).
  ($extra.result.outcome=="applied") as $ok|
  ($context+(if $extra.record==null then {} else {record:$extra.record} end)
           +(if $extra.result.outcome=="invalid" then {} else {edit:null} end)) as $next|
  respond($next;$ok;if $ok then "Applied" else $extra.result.message end)
else error("unknown inspector operation") end
| if has("frames") then .frames |= map(.+{caused_by:$frame.request_id}) else . end
