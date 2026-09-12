# Native JSONL is evidence, not authority. Only presentation fields leave here.
def entry($id; $kind; $title; $text; $order):
  {id:$id,kind:$kind,title:$title,text:($text // "" | tostring),order:$order};
def participant($address; $session):
  if $address == ("session:" + $session.id) then $session.title
  elif ($address|startswith("agent:")) then $address[6:] else $address end;
def native:
  . as $row | .value as $x | ($run + "/" + (.line|tostring)) as $id |
  [$seq,.line] as $order |
  if $stream == "stderr" then entry($id + "/stderr"; "error"; "Diagnostics"; $x; $order)
  elif ($x|type) != "object" then entry($id; "output"; "Output"; $x; $order)
  elif $x.ev == "text_delta" then entry($id; "assistant_delta"; "Assistant"; $x.text; $order)
  elif $x.ev == "reasoning_delta" then entry($id; "reasoning_delta"; "Reasoning"; $x.text; $order)
  elif $x.ev == "tool_start" or $x.ev == "tool_exec" then entry($id; "tool"; ($x.name // "Tool"); ($x.ev + " " + ($x.call_id // "")); $order)
  elif $x.ev == "tool_input_delta" then entry($id; "tool_input_delta"; "Tool input"; $x.delta; $order)
  elif $x.ev == "tool_done" then entry($id; "tool"; ($x.name // "Tool result"); (($x.output // "") + (if $x.error then "\n" + $x.error else "" end)); $order)
  elif $x.ev == "error" or $x.type == "error" then entry($id; "error"; "Harness error"; ($x.message // $x.error.message // "Unknown error"); $order)
  elif $x.ev == "turn_done" or $x.type == "turn.completed" then entry($id; "status"; "Turn finished"; ""; $order)
  elif $x.type == "turn.failed" then entry($id; "error"; "Turn failed"; $x.error.message; $order)
  elif ($x.type == "item.completed" or $x.type == "item.started" or $x.type == "item.updated") then
    $x.item as $item |
    if $item.type == "agent_message" then entry($id; "assistant"; "Assistant"; $item.text; $order)
    elif $item.type == "reasoning" then entry($id; "reasoning"; "Reasoning"; $item.text; $order)
    elif $item.type == "command_execution" then entry($id; "tool"; ($item.command // "Command"); ($item.aggregated_output // $item.status); $order)
    elif $item.type == "file_change" then entry($id; "tool"; "File changes"; ($item.changes|tojson); $order)
    else empty end
  elif $x.type == "assistant" then
    ($x.message.content // [] | to_entries[]) as $part |
    if $part.value.type == "text" then entry($id + "/" + ($part.key|tostring); "assistant"; "Assistant"; $part.value.text; $order)
    elif $part.value.type == "tool_use" then entry($id + "/" + ($part.key|tostring); "tool"; $part.value.name; ($part.value.input|tojson); $order)
    else empty end
  elif $x.type == "stream_event" and $x.event.delta.type == "text_delta" then entry($id; "assistant_delta"; "Assistant"; $x.event.delta.text; $order)
  elif $x.type == "result" then entry($id; "status"; "Run result"; ($x.result // $x.subtype); $order)
  elif $profile == "shell" then entry($id; "output"; "Output"; ($x|tojson); $order)
  else empty end;
if $mode == "native" then
  . as $line | (index("\t")) as $tab |
  {line:($line[:$tab]|tonumber),value:($line[$tab+1:] | if $stream == "stderr" then . elif $profile == "shell" then (. as $text | try fromjson catch $text) else fromjson? end)} | native
elif $mode == "rows" then
  .session as $session | .rows[] | . as $r | .data as $d |
  if $d.class == "Message" then entry($r.id; (if $d.kind == "question" then "question" else "message" end);
    (participant(($d.from // "");$session) + " → " + participant(($d.to // "");$session) + " · " + ($d.created // ""));
    ((if ($d.subject // "") == "" then "" else $d.subject + "\n" end) + ($d.body // "")); [$r.seq,0])
  else empty end
else . end
