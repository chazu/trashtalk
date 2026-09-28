# Plan one content-validated dependency graph. No cache content is executable.
include "protocols";
def dependencies($m): [$m.resolved_parent, $m.traits[]?, $m.implementedProtocols[]?] | map(select(. != "")) | unique;
. as $nodes |
(reduce ($nodes | sort_by(.priority))[] as $node ({};
  if has($node.key) then . else .[$node.key]=$node.index end)) as $lookup |
def visit($i; $stack):
  ($i|tostring) as $id |
  if .seen[$id] then .
  elif ($stack | index($i)) then error("Cyclic build dependency: " + $nodes[$i].source)
  elif $nodes[$i].metadata == null then .missing += [$i] | .seen[$id]=true
  else
    reduce dependencies($nodes[$i].metadata)[] as $dep (. ;
      if $lookup[$dep] != null then
        if ([$nodes[] | select(.key==$dep)] | length)>1 then error("Ambiguous build identity: " + $dep)
        else visit($lookup[$dep]; $stack+[$i]) end
      elif $dep == "Object" then .
      else error("Missing build dependency '" + $dep + "' required by " + $nodes[$i].source +
        (if ($nodes[$i].metadata.implementedProtocols|index($dep)) then
          "; nonlocal protocols require a fully qualified identity" +
          ([$nodes[] | select(.key|endswith("::"+($dep|split("::")|last))) | .key] | if length>0 then ": " + join(", ") else "" end)
        else "" end)) end)
    | .seen[$id]=true | .order += [$i]
  end;
(reduce $nodes[] as $node ({seen:{},order:[],missing:[]};
  if $node.requested then visit($node.index; []) else . end)) as $graph |
if $mode == "frontier" then $graph.missing
elif ($graph.missing|length)>0 then error("unresolved build metadata")
else
  reduce $graph.order[] as $i ({};
    $nodes[$i] as $node |
    [dependencies($node.metadata)[] | $lookup[.] | select(. != null) | tostring] as $deps |
    (reduce $deps[] as $d ({}; .[$nodes[($d|tonumber)].source]={
      source_hash:$nodes[($d|tonumber)].hash,output:$nodes[($d|tonumber)].output,
      output_hash:$nodes[($d|tonumber)].output_hash})) as $inputs |
    ($node.old.version == 2 and $node.old.source == $node.source and $node.old.compiler == $compiler
      and $node.old.source_hash == $node.hash and $node.output_hash != null
      and $node.old.output_hash == $node.output_hash and $node.old.dependencies == $inputs
      and $node.old.value_send == $value_send and $node.old.strict == $strict and $node.old.lenient == $lenient) as $valid |
    . as $done |
    ($node.metadata | protocol_semantics) as $m |
    if $m.identity != $node.key and ($m.kind == "protocol" or ($m.implementedProtocols|length)>0) then error("Build identity mismatch: " + $node.source + " declares " + $m.identity + " but is registered as " + $node.key) else . end |
    if $done[($lookup[$m.resolved_parent]|tostring)].metadata.kind == "protocol" then protocol_error($m; "protocol inheritance is not supported") else . end |
    (reduce ($done|to_entries[]) as $entry ({}; .[$entry.value.key]=$entry.value.surface)) as $resolved |
    ($m | protocol_surface($resolved)) as $surface |
    ([ $m.implementedProtocols[] as $p |
      $done[($lookup[$p]|tostring)] as $protocol |
      if $protocol.metadata.kind != "protocol" or $protocol.metadata.identity != $p then protocol_error($m; $p + " is not a protocol") else . end |
      protocol_check($surface; $protocol.metadata.requirements) as $check |
      if ($check.missing|length)+($check.conflicts|length)>0 then
        "implements " + $p + " but lacks: " + ($check.missing|join(", ")) +
        (if ($check.conflicts|length)>0 then "; conflicting direct traits: " + ($check.conflicts|join(", ")) else "" end)
      else empty end] | join("\n")) as $failures |
    if $failures != "" then protocol_error($m; $failures) else . end |
    .[($i|tostring)] = ($node + {dependencies:$inputs, surface:$surface,
      dirty:(($valid|not) or any($deps[]; $done[.].dirty)),
      level:([0, ($deps[] | $done[.].level+1)] | max)}))
  | [.[]] | sort_by(.level,.index)
end
