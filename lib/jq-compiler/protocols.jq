# Shared public instance surface. The runtime consumes these exact summaries;
# it never reconstructs a second dispatch algorithm with Bash reflection.
def protocol_identity:
  if (.package // "") == "" then .name else .package + "::" + .name end;
def protocol_parent:
  .parent as $p |
  if $p == null or $p == "" or $p == "nil" then ""
  elif $p | contains("::") then $p
  elif .parentPackage then .parentPackage + "::" + $p
  elif ["Object","Tool","TestCase","Protocol","Settings","Preferences"] | index($p) then $p
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
   aliases:(.aliases // []),
   settingsPrefix:(.settingsPrefix.name // null), settingsLocation:(.settingsPrefix.location // null),
   settingsDeclarations:(.settings // []), preferences:(.preferences // []),
   preferenceHost:(.preferenceHost.name // null),
   shape:{methods:([.methods[]? | select(.category != "settings")] | length),
     instanceVars:(.instanceVars // [] | length), classInstanceVars:(.classInstanceVars // [] | length)}};
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

# ------------------------------------------------------------------------------
# Settings groups and preferences (docs/settings-design.md)
# ------------------------------------------------------------------------------
def settings_error($m; $loc; $message):
  error("SettingsError: " + $m.identity + (if $loc then " (line " + ($loc.line|tostring) + ")" else "" end) + ": " + $message);
# Bash type names as lib/config.bash validates them.
def settings_type: if .name == "enum" then "enum:" + (.choices | join(" ")) else .name end;
def settings_env($prefix):
  "TRASHTALK_" + ($prefix | ascii_upcase) + "_" + (.name | gsub("(?<u>[A-Z])"; "_\(.u)") | ascii_upcase);
# Does a parsed literal satisfy a parsed type?
def settings_accepts($type; $value):
  if $type.name == "string" then $value.kind == "string"
  elif $type.name == "integer" then $value.kind == "integer" and ($value.value | test("^[0-9]+$"))
  elif $type.name == "boolean" then $value.kind == "boolean"
  elif $type.name == "enum" then $value.kind == "string" and ($type.choices | index($value.value)) != null
  else false end;
def settings_describe_type: if .name == "enum" then "one of " + (.choices | join(", ")) elif .name == "integer" then "an integer" else "a " + .name end;
# Selectors every group answers; a setting may not shadow them.
def settings_reserved: ["new","describe","groups","reset","class","id","inspect","printString"];
# The compiled description of a group, used by the manifest and the runtime table.
def settings_spec:
  .settingsPrefix as $prefix |
  {group:.identity, prefix:$prefix, settings:[.settingsDeclarations[] |
    {key:($prefix + "." + .name), name, type:(.type | settings_type), default:.default.value, doc,
     env:(.env // settings_env($prefix))}]};
# Checks that need only this class. Preference checks that need the groups run
# in the build plan.
def settings_semantics:
  . as $m |
  if (.settingsPrefix != null or (.settingsDeclarations|length) > 0) and .resolved_parent != "Settings" then
    settings_error($m; .settingsLocation // .settingsDeclarations[0].location; "prefix: and setting: are only allowed on a direct Settings subclass")
  elif .resolved_parent == "Settings" then
    if .settingsPrefix == null then settings_error($m; null; "a settings group needs prefix: name")
    elif (.settingsPrefix | test("^[a-z][A-Za-z0-9]*$") | not) then settings_error($m; .settingsLocation; "prefix must be a lowercase identifier")
    elif .shape.instanceVars + .shape.classInstanceVars > 0 or (.traits|length) > 0 then
      settings_error($m; .settingsLocation; "a settings group cannot declare instance variables or include traits")
    elif (.preferences|length) > 0 or .preferenceHost != null then
      settings_error($m; .preferences[0].location; "preference lines belong in a Preferences class")
    else
      reduce .settingsDeclarations[] as $s ({seen:{}, envs:{}};
        if (settings_reserved | index($s.name)) then settings_error($m; $s.location; "setting " + $s.name + " would shadow a class method")
        elif ($s.name | test("^[a-z][A-Za-z0-9]*$") | not) then settings_error($m; $s.location; "setting names start with a lowercase letter")
        elif .seen[$s.name] then settings_error($m; $s.location; "setting " + $s.name + " is declared twice")
        elif (["string","integer","boolean","enum"] | index($s.type.name)) == null then
          settings_error($m; $s.location; "type must be string, integer, boolean, or #(choice ...)")
        elif $s.type.name == "enum" and (($s.type.choices|length) == 0 or ($s.type.choices|unique|length) != ($s.type.choices|length)) then
          settings_error($m; $s.location; "an enum type needs distinct choices")
        elif (settings_accepts($s.type; $s.default) | not) then
          settings_error($m; $s.location; "default for " + $s.name + " must be " + ($s.type | settings_describe_type))
        elif $s.env != null and ($s.env | test("^[A-Z_][A-Z0-9_]*$") | not) then
          settings_error($m; $s.location; "env: must name an environment variable")
        else
          ($s.env // ($s | settings_env($m.settingsPrefix))) as $env |
          if .envs[$env] then settings_error($m; $s.location; $env + " is used by two settings") else . end |
          .seen[$s.name] = true | .envs[$env] = true
        end) |
      $m
    end
  else . end;
# The settings index: every group and preferences class in a published manifest.
# Earlier entries the manifest does not mention are kept while their source is live.
def settings_index($previous; $live):
  (.entries | keys) as $known |
  def carried($kind; $id): [$previous[$kind][]? | select((.[$id] as $i | $known | index($i)) == null and (.source as $s | $live | index($s)))];
  ([.entries[] | select(.settings != null) | .settings + {source}] + carried("groups"; "group") | sort_by(.prefix)) as $groups |
  ([.entries[] | select(.preferences != null) | .preferences + {source, source_hash:.receipt_data.source_hash}]
    + carried("preferences"; "identity") | sort_by(.identity)) as $preferences |
  ($groups | group_by(.prefix) | map(select(length > 1)) | first) as $prefix |
  if $prefix then error("SettingsError: prefix " + $prefix[0].prefix + " is declared by " + ([$prefix[].group] | join(" and ")))
  else . end |
  ([$preferences[] | select(.host != null)] | group_by(.host | ascii_downcase) | map(select(length > 1)) | first) as $host |
  if $host then error("SettingsError: host " + $host[0].host + " is claimed by " + ([$host[].identity] | join(" and ")))
  else . end |
  {schema:1, groups:$groups, preferences:$preferences};
# The runtime table lib/config.bash sources: Bash builtins only, no jq at read time.
def settings_table:
  (.groups[] | . as $g |
    "_trash_settings_group \(.group | @sh) \(.prefix | @sh) \(.source | @sh)",
    (.settings[] | "_trash_config_declare \(.key | @sh) \(.env | @sh) \(.type | @sh) \(.default | @sh) \(.doc | @sh) \($g.group | @sh)")),
  (.preferences[] | . as $p |
    "_trash_prefs_class \(.identity | @sh) \(.parent | @sh) \((.host // "") | @sh) \(.source | @sh) \((.source_hash // "") | @sh)",
    (.values[] | "_trash_prefs_value \($p.identity | @sh) \(.key | @sh) \(.value | @sh)"));
