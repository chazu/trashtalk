# A single JSON document in; no jq source is interpolated from caller paths.
def path_parts:
  if type == "array" then .
  elif type == "string" then
    if startswith("[") then fromjson
    elif . == "" then [] else split(".") end
  else error("JSON path must be a string or array") end
  | if type == "array" and all(.[]; type == "string" or (type == "number" and . >= 0 and floor == .))
    then . else error("invalid JSON path") end;
def lookup($path):
  reduce ($path | path_parts)[] as $key ({present:true,value:.};
    if .present and ((.value | type) == "object" and ($key | type) == "string") then
      {present:(.value | has($key)),value:.value[$key]}
    elif .present and ((.value | type) == "array" and ($key | type) == "number") then
      {present:(.value | has($key)),value:.value[$key]}
    else {present:false,value:null} end);
def text_value:
  (if type == "string" then . else tojson end)
  | if contains("\u0000") then error("Bash text cannot contain NUL; use jsonAt: for encoded JSON") else . end;
# Match jq -r's legacy rendering of nested containers, including indentation.
def pretty_json($indent):
  if type == "array" and length > 0 then
    "[\n" + (map((" " * ($indent + 2)) + pretty_json($indent + 2)) | join(",\n")) + "\n" + (" " * $indent) + "]"
  elif type == "object" and length > 0 then
    "{\n" + ([to_entries[] | (" " * ($indent + 2)) + (.key | tojson) + ": " + (.value | pretty_json($indent + 2))] | join(",\n")) + "\n" + (" " * $indent) + "}"
  else tojson end;
def legacy_text:
  if . == null or . == false then ""
  elif type == "string" then text_value else pretty_json(0) end;
if length != 1 then error("expected exactly one JSON document") else .[0] end
| if $operation == "get" or $operation == "text" or $operation == "has" or $operation == "default" then
    lookup($path) as $found
    | if $operation == "has" then $found.present | tostring
      elif $found.present then $found.value | if $operation == "text" then text_value else tojson end
      elif $operation == "default" then $fallback | fromjson | tojson
      else error("missing JSON path: " + $path) end
  elif $operation == "unpack" then
    . as $data | ($path | fromjson) | if type != "array" then error("field paths must be an array") else . end
    | map(. as $field | $data | lookup($field)
      | if .present then .value | text_value else error("missing JSON field: " + ($field | tojson)) end)
    | @sh
  elif $operation == "array" then
    if type != "array" then error("expected an array") else map(legacy_text) | @sh end
  elif $operation == "object" then
    if type != "object" then error("expected an object")
    else [to_entries | sort_by(.key)[] | (.key | text_value), (.value | legacy_text)] | @sh end
  elif $operation == "keys" then
    if type != "object" then error("expected an object") else keys | map(text_value) | @sh end
  elif $operation == "values" then
    if type != "object" then error("expected an object") else [.[] | legacy_text] | @sh end
  elif $operation == "state" then
    if type != "object" then error("expected instance state")
    else [(.class | legacy_text), (to_entries[]
      | select(.key | contains("\u0000") | not)
      # Leave containers to the ordinary field reader. Eager pretty-printing
      # copies potentially large collections even when a send never reads them.
      | select(.value | type != "array" and type != "object")
      | select(.value | if type == "string" then contains("\u0000") | not else true end)
      | .key, (.value | if . == null or . == false then "" else legacy_text + "\n" end))] | @sh end
  elif $operation == "block" then
    [(.code | legacy_text), (.captured._RECEIVER | legacy_text),
     (.params[0] | legacy_text), (.params[1] | legacy_text)] | @sh
  else error("unknown JSON operation") end
