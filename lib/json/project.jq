# Input is JSONL. A template is data, never a jq expression.
def project($row):
  if type == "object" then
    if keys == ["_at"] then
      .["_at"] | (if type == "string" then split(".") else . end) as $path
      | $row | getpath($path)
    elif keys == ["_literal"] then .["_literal"]
    elif keys == ["_concat"] then .["_concat"] | map(project($row) | tostring) | join("")
    else with_entries(.value |= project($row)) end
  elif type == "array" then map(project($row))
  else . end;
# Buffer the result before emitting, so errors cannot publish a partial batch.
map(. as $row | $template | project($row))[]
