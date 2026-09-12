# Normalization only; marker, label, date choice and record policy live in DSL.
def single_line:
  gsub("[\u0000-\u001f\u007f-\u009f]";" ") | gsub("\\s+";" ") | sub("^ +";"") | sub(" +$";"");
def participant:
  if .==$human then "You" else . as $address
    | ($names[$address] // $address) | if .=="" then $address else . end | single_line end;
# Match the scalar text getter and Bash capture, including trailing newlines.
def text: (if type=="string" then . else tojson end) | sub("\n+$";"");
map(.id as $id | (.data | . + (with_entries(select(.key | IN("from","to","kind","status","subject","body","created","sentAt")) | .value |= text))) as $d
  | ($d.sentAt | if .=="" then 0 else tonumber end | ./1000 | floor | tostring) as $stamp
  | ($calendar[$stamp] // {}) as $time
  | ($d.from | participant) as $sender
  | {id:$id, sender:$sender, rowSender:($sender | if length>20 then .[:19]+"…" else . end),
     recipient:($d.to|participant), from:$d.from,to:$d.to,kind:$d.kind,status:$d.status,
     subject:($d.subject|single_line),body:$d.body,
     firstLine:($d.body | split("\n") | map(select(test("\\S"))) | .[0] // "" | single_line),
     day:($time.day // ""),today:$today,shortDate:($time.short // $d.created),
     fullDate:($time.full // $d.created),clock:($time.clock // "")})
