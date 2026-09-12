def instance_summary:
def shown:
        if type == "string" then .
        elif type == "null" then "null"
        else tojson
        end;
      if type != "object" then shown
      elif (._vars | type) == "array" then
        . as $object
        | [._vars[] as $key
           | select($object | has($key))
           | "\($key)=\($object[$key] | shown)"]
        | join(" | ")
      else
        [to_entries[]
         | select(.key != "class" and .key != "created_at" and .key != "_vars")
         | "\(.key)=\(.value | shown)"]
        | join(" | ")
      end;

def instance_columns:
def shown:
        if type == "string" then .
        elif type == "null" then "null"
        else tojson
        end;
      if type != "object" then []
      else . as $object
        | [._vars[]? as $key
           | select($object | has($key))
           | {name:$key, value:($object[$key] | shown)}]
      end;

def instance_view:
if type != "object" then {}
      else . as $object
        | reduce ($object._vars[]?) as $key
            ({}; if ($object | has($key)) then .[$key] = $object[$key] else . end)
      end;
