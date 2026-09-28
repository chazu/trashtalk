# Shared public instance surface. The runtime consumes these exact summaries;
# it never reconstructs a second dispatch algorithm with Bash reflection.
def protocol_identity:
  if (.package // "") == "" then .name else .package + "::" + .name end;
def protocol_parent:
  .parent as $p |
  if $p == null or $p == "" or $p == "nil" then ""
  elif $p | contains("::") then $p
  elif .parentPackage then .parentPackage + "::" + $p
  elif ["Object","Tool","TestCase","Protocol"] | index($p) then $p
  elif .package then .package + "::" + $p else $p end;
def protocol_ref($m):
  if contains("::") or ($m.package // "") == "" then . else $m.package + "::" + . end;
def public: map(select(startswith("_") | not)) | unique;
def canonical_selector:
  if (.keywords // [] | length) > 0 then [.keywords[] | . + ":"] | join("") else .selector end;
def protocol_metadata:
  . as $m |
  {name,package,imports,parent,parentPackage,traits,requirements:(.methodRequirements // [] | unique),
   requirementDeclarations:(.requirementDeclarations // []), declarations:(.implementedProtocols // []),
   identity:protocol_identity, resolved_parent:protocol_parent,
   kind:(if .isTrait then "trait" elif protocol_parent == "Protocol" then "protocol" else "class" end),
   implementedProtocols:[.implementedProtocols[]?.protocol | protocol_ref($m)],
   own:([.methods[]? | select(.kind == "instance") | canonical_selector] +
     [.instanceVars[]? | .name as $n | ($n, $n+":", "get"+($n[0:1]|ascii_upcase)+$n[1:], "set"+($n[0:1]|ascii_upcase)+$n[1:]+":")] | public),
   aliases:(.aliases // [])};
def protocol_error($m; $message):
  ($m.declarations[0].location // $m.requirementDeclarations[0].location // null) as $loc |
  error("ProtocolError: " + $m.identity + (if $loc then " (line " + ($loc.line|tostring) + ")" else "" end) + ": " + $message);
def protocol_semantics:
  . as $m |
  if (.requirements|length)>0 and .kind != "protocol" then protocol_error($m; "selector requires: is only allowed on a direct Protocol subclass")
  elif any(.requirements[]; startswith("_")) then protocol_error($m; "private required selector")
  elif (.implementedProtocols|length)>0 and .kind != "class" then protocol_error($m; "implements: is only allowed on classes")
  elif (.implementedProtocols|length) != (.implementedProtocols|unique|length) then protocol_error($m; "duplicate implements: declaration")
  else . end;
# Aliases wrap a function on their own class, not a dispatched inherited send.
# Include a public alias only if its instance-side target actually exists.
def protocol_own:
  . as $m | reduce range(0; (.aliases|length)+1) as $pass (.own;
    . as $known | ($known + [$m.aliases[] | .originalMethod as $target | select($known|index($target)) | .aliasName]) | public);
def protocol_surface($resolved):
  . as $m |
  ($m | protocol_own) as $own |
  ($resolved[$m.resolved_parent].lineage // []) as $inherited |
  [$m.traits[]? as $t | $resolved[$t].own[]?] as $traits |
  ($traits | group_by(.) | map(select(length>1) | .[0]) | . - $own) as $conflicts |
  {own:$own, lineage:(($own+$inherited)|unique),
   selectors:(($own+$traits+$inherited)|unique), conflicts:$conflicts};
def protocol_check($surface; $requirements):
  {missing:($requirements - $surface.selectors), conflicts:($requirements - ($requirements - $surface.conflicts))};
