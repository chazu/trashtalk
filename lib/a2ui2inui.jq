# A2UI (a2ui.org, v1.0 basic catalog) -> inui surface frame (lib/ui-duplex.bash).
# Input: one A2UI message stream, either NDJSON messages or a whole-file
# {"messages":[...]} object (slurped with jq -s). Output: one inui "init"
# frame {schema_version,view,type,revision,root,collections}. Data bindings
# are baked into props at map time; re-map an updated surface as a new revision.
def resolve($dm):
  if type == "object" then
    if has("@path") then
      .["@path"] | ltrimstr("/") | split("/") | reduce .[] as $p ($dm; .[$p])
    else . end
  elif type == "array" then map(resolve($dm))
  else . end;
def unmark: tostring | gsub("^###+ "; "") | gsub("  +"; " ");
def node($byid; $dm):
  . as $c
  | ($c.component // "Card") as $comp
  | ($c.children // (if $c.child == null then [] else [$c.child] end)) as $kids
  | {kind: (
       if $comp == "Card" or $comp == "Column" or $comp == "Row" then "panel"
       elif $comp == "Text" or $comp == "Image" then "text"
       elif $comp == "TextField" or $comp == "CheckBox" or $comp == "Slider" or $comp == "DateTimeInput" or $comp == "MultipleChoice" or $comp == "SingleChoice" then "input"
       elif $comp == "Button" then "button"
       elif $comp == "List" then "list"
       elif $comp == "Table" then "table"
       else "panel" end),
     key: ($c.id // "node"),
     props: (({title: ($c.label // $c.text // null)}
       + ($c | with_entries(select(.key as $k | $k != "id" and $k != "component" and $k != "children" and $k != "child") | .value |= resolve($dm))))
       | with_entries(select(.value != null))
       | if $comp == "Card" or $comp == "Column" or $comp == "Row"
         then {direction: (if $comp == "Row" then "row" else "column" end)} + (if $comp == "Card" then {border: true} else {} end) + .
         elif $comp == "Text" then . + {text: (.text | unmark)} + (if .variant == "caption" then {style: "primary"} else {} end)
         elif $comp == "TextField" then . + {text: (.value // "")}
         else . end)}
  | ([$kids[] | $byid[.]] | map(select(. != null)) | map(node($byid; $dm))) as $children
  | if ($children | length) == 0 then . else . + {children: $children} end;
def messages:
  if type == "array" then (if length == 1 and (.[0] | type == "object") and (.[0] | has("messages")) then .[0].messages else . end)
  elif has("messages") then .messages
  else [] end
  | map(select(type == "object" and (has("createSurface") or has("updateComponents") or has("updateDataModel"))));
messages
| (reduce .[] as $m ({surface: "", dm: {}};
    if $m.createSurface != null then .surface = $m.createSurface.surfaceId
    elif $m.updateDataModel != null then .dm = ($m.updateDataModel.value // {})
    else . end)) as $st
| (map(.updateComponents // {components: []} | .components[])) as $comps
| (reduce $comps[] as $c ([]; . + [$c]) | INDEX(.[]; .id // "node")) as $byid
| ($comps | map(select(.id == "root")) | first // $comps[0]) as $root
| if $root == null or $st.surface == "" then {error: "a2ui2inui: need createSurface and at least one component"}
  else {schema_version: 1, view: $view, type: "init", revision: 0,
        root: ($root | node($byid; $st.dm)),
        collections: []}
  end
