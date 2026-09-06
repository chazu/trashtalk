# Plan one content-validated dependency graph. No cache content is executable.
def parent($m):
  $m.parent as $p |
  if $p == null or $p == "" or $p == "nil" then ""
  elif ($p | contains("::")) then $p
  elif $m.parentPackage then $m.parentPackage + "::" + $p
  elif (["Object","Tool","TestCase"] | index($p)) then $p
  elif $m.package then $m.package + "::" + $p else $p end;
def dependencies($m): [parent($m), $m.traits[]?] | map(select(. != "")) | unique;
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
      if $lookup[$dep] != null then visit($lookup[$dep]; $stack+[$i])
      elif $dep == "Object" then .
      else error("Missing build dependency '" + $dep + "' required by " + $nodes[$i].source) end)
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
    .[($i|tostring)] = ($node + {dependencies:$inputs,
      dirty:(($valid|not) or any($deps[]; $done[.].dirty)),
      level:([0, ($deps[] | $done[.].level+1)] | max)}))
  | [.[]] | sort_by(.level,.index)
end
