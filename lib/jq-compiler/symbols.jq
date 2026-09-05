# Convert one parser CompilationUnit into versioned browser records.

def qualified_name:
  if (.package // "") == "" then .name else "\(.package)::\(.name)" end;

def display_selector:
  if ((.keywords // []) | length) > 0
  then (.keywords | map(. + ":") | join(""))
  else .selector
  end;

def method_kind:
  if .kind == "class" then "class_method"
  elif .kind == "test" then "test_method"
  else "instance_method"
  end;

def method_owner_label($class; $selector):
  if .kind == "class" then "\($class) class>>\($selector)"
  elif .kind == "test" then "\($class) test>>\($selector)"
  else "\($class)>>\($selector)"
  end;

def resolved_parent:
  .parent as $p |
  if $p == null or $p == "" then ""
  elif ($p | contains("::")) then $p
  elif .parentPackage then .parentPackage + "::" + $p
  elif (["Object", "Tool", "TestCase"] | index($p)) then $p
  elif .package then .package + "::" + $p else $p end;

(.path // $path) as $source_path |
(.ast // .class // .) as $class |
($class | qualified_name) as $qualified |
(
  {
    schema_version: 1,
    id: $qualified,
    path: $source_path,
    line: ($class.location.line // 1),
    column: (($class.location.col // 0) + 1),
    label: $qualified,
    kind: (if $class.isTrait then "trait" else "class" end),
    detail: ($class.parent // ""),
    class_name: $qualified,
    superclass: ($class | resolved_parent),
    traits: ($class.traits // [])
  },
  ($class.instanceVars[]? |
    {
      schema_version: 1,
      id: "\($qualified).\(.name)",
      path: $source_path,
      line: (.location.line // $class.location.line // 1),
      column: ((.location.col // 0) + 1),
      label: "\($qualified).\(.name)",
      kind: "instance_variable",
      detail: ((.default.value // "") | tostring),
      class_name: $qualified,
      variable: .name
    }
  ),
  ($class.classInstanceVars[]? |
    {
      schema_version: 1,
      id: "\($qualified) class.\(.name)",
      path: $source_path,
      line: (.location.line // $class.location.line // 1),
      column: ((.location.col // 0) + 1),
      label: "\($qualified) class.\(.name)",
      kind: "class_variable",
      detail: ((.default.value // "") | tostring),
      class_name: $qualified,
      variable: .name
    }
  ),
  ($class.methods[]? |
    (. | display_selector) as $selector |
    (. | method_owner_label($qualified; $selector)) as $label |
    {
      schema_version: 1,
      id: $label,
      path: $source_path,
      line: (.location.line // $class.location.line // 1),
      column: ((.location.col // 0) + 1),
      label: $label,
      kind: method_kind,
      detail: (if .raw then "raw" else "DSL" end),
      class_name: $qualified,
      selector: $selector,
      method_kind: .kind,
      raw: (.raw // false)
    }
  )
)
