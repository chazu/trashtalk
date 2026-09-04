# Find complete message selectors in method token streams. Keyword parts are
# grouped within the parsed send boundary instead of compared token-by-token.

def qualified_name:
  if (.package // "") == "" then .name else "\(.package)::\(.name)" end;

def display_selector:
  if ((.keywords // []) | length) > 0
  then (.keywords | map(. + ":") | join(""))
  else .selector
  end;

def receiver_end($tokens; $at):
  ($at + 1) as $receiver |
  if $tokens[$receiver].type == "IDENTIFIER" and
     $tokens[$receiver + 1].type == "NAMESPACE_SEP" and
     $tokens[$receiver + 2].type == "IDENTIFIER"
  then $receiver + 3
  else $receiver + 1
  end;

def opens_scope:
  . == "LPAREN" or . == "LBRACKET" or . == "HASH_LPAREN" or . == "HASH_LBRACE";

def closes_scope:
  . == "RPAREN" or . == "RBRACKET" or . == "RBRACE";

def depth_before($tokens; $end):
  reduce range(0; $end) as $index (0;
    ($tokens[$index].type // "") as $type |
    if ($type | opens_scope) then . + 1
    elif ($type | closes_scope) then [.-1, 0] | max
    else .
    end
  );

def keyword_is_start($tokens; $index):
  depth_before($tokens; $index) as $depth |
  reduce range(0; $index) as $previous ({depth: 0, seen: false};
    ($tokens[$previous].type // "") as $type |
    (if .depth == $depth and ($type == "NEWLINE" or $type == "DOT" or $type == "SEMI") then
      .seen = false
    elif .depth == $depth and $type == "KEYWORD" then
      .seen = true
    else .
    end) |
    if ($type | opens_scope) then .depth += 1
    elif ($type | closes_scope) then .depth = ([.depth - 1, 0] | max)
    else .
    end
  ) |
  (.seen | not);

def keyword_send($tokens; $start):
  {
    index: $start,
    depth: 0,
    parts: [],
    line: ($tokens[$start].line // 1),
    column: (($tokens[$start].col // 0) + 1)
  } |
  until(
    .index >= ($tokens | length) or
    (.index > $start and .depth == 0 and
      (($tokens[.index].type // "") as $type |
        $type == "NEWLINE" or $type == "DOT" or $type == "SEMI" or ($type | closes_scope)));
    ($tokens[.index].type // "") as $type |
    if ($type | opens_scope) then .depth += 1
    elif ($type | closes_scope) then .depth = ([.depth - 1, 0] | max)
    elif .depth == 0 and $type == "KEYWORD" then .parts += [$tokens[.index].value]
    else .
    end |
    .index += 1
  ) |
  if (.parts | length) > 0
  then {selector: (.parts | join("")), line, column}
  else empty
  end;

def send_at($tokens; $at):
  receiver_end($tokens; $at) as $message |
  if $tokens[$message].type == "IDENTIFIER" then
    {
      selector: $tokens[$message].value,
      line: ($tokens[$message].line // 1),
      column: (($tokens[$message].col // 0) + 1)
    }
  elif $tokens[$message].type == "KEYWORD" then
    keyword_send($tokens; $message)
  else empty
  end;

def method_kind:
  if .kind == "class" then "class_method"
  elif .kind == "test" then "test_method"
  else "instance_method"
  end;

def owner_label($class; $selector):
  if .kind == "class" then "\($class) class>>\($selector)"
  elif .kind == "test" then "\($class) test>>\($selector)"
  else "\($class)>>\($selector)"
  end;

(.path // $path) as $source_path |
(.ast // .class // .) as $class |
($class | qualified_name) as $qualified |
$class.methods[]? |
. as $method |
($method | display_selector) as $method_selector |
($method | owner_label($qualified; $method_selector)) as $label |
(.body.tokens // []) as $tokens |
([
  (range(0; $tokens | length) as $index |
    select($tokens[$index].type == "AT") |
    send_at($tokens; $index)),
  (range(0; $tokens | length) as $index |
    select($tokens[$index].type == "KEYWORD") |
    select($wanted | startswith($tokens[$index].value)) |
    select(keyword_is_start($tokens; $index)) |
    keyword_send($tokens; $index))
] | unique_by([.selector, .line, .column])[]) |
select(.selector == $wanted) |
{
  schema_version: 1,
  id: "\($label)@\(.line):\(.column)",
  path: $source_path,
  line: .line,
  column: .column,
  label: $label,
  kind: "sender",
  detail: "sends \($wanted)",
  class_name: $qualified,
  selector: $method_selector,
  method_kind: ($method | method_kind),
  sent_selector: $wanted
}
