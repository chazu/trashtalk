# Synthetic event workload and executable protocol example. One jq evaluation
# handles a whole batch; no per-event getters, serializers, or database reads.
def row($i): {key: ("event-"+($i|tostring)),fields:{x:($i|tostring),y:(($i%100)|tostring),text:("Event "+($i|tostring)+"  ·  worker heartbeat"),detail:("Event "+($i|tostring)+"\nSource: demonstration\nBatched detail.")}};
def envelope($type): {schema_version:1,view:"events",type:$type};
def rows($n): [range([$n-10000,0]|max;$n)|row(.)];
def snapshot($c): envelope("init")+{revision:0,root:{kind:"panel",key:"root",props:{direction:"row",gap:1},children:[
    {kind:"list",key:"events",props:{title:"Live events",source:"events",field:"text",border:true,follow_end:true}},
    {kind:"panel",key:"right",props:{direction:"column"},children:[
        {kind:"text",key:"detail",props:{title:"Detail",detail_of:"events",field:"detail",border:true}},
        {kind:"plot",key:"rate",props:{title:"Sample values (+/- zoom, arrows pan)",source:"events",x_field:"x",y_field:"y",domain_y:[0,100],border:true,size:7}},
        {kind:"input",key:"filter",props:{title:"Filter (Enter to apply)",placeholder:"Event number or text",action:"filter",debounce_ms:200,border:true,size:3}},
        {kind:"button",key:"refresh",props:{text:"Add a batch",action:"append",size:1}}
    ]}
]},collections:[{id:"events",revision:$c.revision,total:(rows($c.count)|map(select(.fields.text|contains($c.filter // "")))|length),rows:(rows($c.count)|map(select(.fields.text|contains($c.filter // ""))))}]};
def advance($c): ($c+{count:($c.count+16),revision:($c.revision+1)}) as $next |
    {context:$next,frames:[envelope("change")+{source:"events",base_revision:$c.revision,revision:$next.revision,changes:[{op:"append",rows:[range($c.count;$next.count)|row(.)]}]}]};
if $mode=="init" then snapshot($context)
elif $mode=="poll" then if ($context.filter // "")=="" then advance($context) else {frames:[]} end
elif $mode=="handle" then
    if $frame.intent=="resync" then {frames:[snapshot($context)]}
    elif $frame.intent=="action" and $frame.action=="append" then
        advance($context) | if ($context.filter // "")!="" then .frames=[snapshot(.context)] else . end | .frames += [envelope("ack")+{request_id:$frame.request_id,ok:true,message:"Added 16 events"}]
    elif $frame.intent=="query" and $frame.action=="filter" and ($frame.value|type)=="string" then
        ($context+{revision:($context.revision+1),filter:$frame.value}) as $next|
        (rows($context.count)|map(select(.fields.text|contains($frame.value)))) as $rows|
        {context:$next,frames:[envelope("query_result")+{request_id:$frame.request_id,generation:$frame.generation,collection:{id:"events",revision:$next.revision,total:($rows|length),rows:$rows},message:"Filter applied"}]}
    elif $frame.intent=="action" and $frame.action=="filter" and ($frame.value|type)=="string" then
        ($context+{revision:($context.revision+1),filter:$frame.value}) as $next |
        (rows($context.count)|map(select(.fields.text|contains($frame.value)))) as $rows |
        {context:$next,frames:[envelope("collection")+{collection:{id:"events",revision:$next.revision,total:($rows|length),rows:$rows}},envelope("ack")+{request_id:$frame.request_id,ok:true,message:"Filter applied"}]}
    else {frames:[envelope("ack")+{request_id:$frame.request_id,ok:false,message:"Unknown event action"}]} end
else error("unknown event operation") end
| if $mode=="handle" then .frames |= map(.+{caused_by:$frame.request_id}) else . end
