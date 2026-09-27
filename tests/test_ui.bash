#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export TRASHTALK_UI_STATE=$work
@ UI::Signal set: count to: 7
value=$(@ UI::Signal value: count)
[[ $value == 7 ]]
# Ordinary blocks capture reads even across the runtime's command substitutions.
cat > "$TRASHTALK_DIR/trash/UITest.trash" <<'TRASH'
UITest subclass: Object
  classMethod: bind [
    ^ @ UI::Binding named: 'label' block: [ @ UI::Signal value: 'count' ]
  ]
  classMethod: bindOther [
    ^ @ UI::Binding named: 'other' block: [ @ UI::Signal value: 'unrelated' ]
  ]
  classMethod: badBinding [
    ^ @ UI::Binding named: 'bad' block: [ @ UI::Signal set: 'count' to: '999' ]
  ]
TRASH
make single CLASS=UITest > "$work/compile.log" 2>&1
@ UI::Signal set: unrelated to: x
[[ $(@ UITest bind) == 7 ]]
[[ $(@ UITest bindOther) == x ]]
@ UI::Binding clearInvalidations
@ UI::Signal set: count to: 8
[[ $(@ UI::Binding invalidated) == label ]]
[[ $(@ UI::Binding evaluate: label) == 8 ]]
if @ UITest badBinding > /dev/null 2>&1;then echo 'FAIL read-only binding';exit 1;fi
[[ $(@ UI::Signal value: count) == 8 ]]
@ UI::Binding clearInvalidations
@ UI::Signal set: count to: 8
[[ -z $(@ UI::Binding invalidated) ]]
@ UI::Signal invalidate: count
[[ $(@ UI::Binding invalidated) == label ]]
node=$(@ UI::Node text: 'literal $() " text' key: title)
[[ $(jq -r '.props.text' <<< "$node") == 'literal $() " text' ]]
form=$(@ UI::Form fields: '[{"key":"name","label":"Name","value":"Ada"}]' key: settings submit: save)
jq -e '.children[1].kind=="input" and .children[1].props.value=="Ada" and .children[2].props.inputs==["name"]' <<< "$form" >/dev/null
context='{"record":{"a":{"b":{"c":1}},"other":{"d":2}},"title":"Object","paths":[[]],"active":0,"revision":0}'
initial=$(@ UI::Inspector frameFor: "$context")
jq -e '.root.kind=="panel" and .collections[0].rows[0].key=="a"' <<< "$initial" >/dev/null
frame='{"schema_version":1,"view":"inspector","request_id":1,"intent":"action","widget":"pane-[]","action":"drill","value":{"key":"a"}}'
result=$(@ UI::Inspector handleFrame: "$frame" context: "$context")
context=$(jq -c .context <<< "$result")
jq -e '.context.active==1 and .context.paths==[[],["a"]] and .frames[0].root.children[0].children[0].props.min_parent_width==80' <<< "$result" >/dev/null
frame='{"schema_version":1,"view":"inspector","request_id":2,"intent":"action","widget":"pane-[\"a\"]","action":"drill","value":{"key":"b"}}'
result=$(@ UI::Inspector handleFrame: "$frame" context: "$context")
context=$(jq -c '.context|.active=0' <<< "$result")
frame='{"schema_version":1,"view":"inspector","request_id":3,"intent":"action","widget":"pane-[]","action":"drill","value":{"key":"other"}}'
result=$(@ UI::Inspector handleFrame: "$frame" context: "$context")
jq -e '.context.paths==[[],["other"]]' <<< "$result" >/dev/null
# A fake surface exercises the real session bridge and duplicate receipt policy.
cat > "$work/surface.py" <<'PY'
import json,sys,os
first=json.loads(sys.stdin.readline())
rows=first['collections'][0]['rows'];assert len(rows)==10000 and rows[0]['key']=='event-0' and rows[-1]['key']=='event-9999'
request=dict(schema_version=1,view='events',request_id=1,intent='action',widget='refresh',action='append',value=None)
def send(): print(json.dumps(request),flush=True)
send()
first_change=json.loads(sys.stdin.readline());ack=json.loads(sys.stdin.readline())
assert first_change['revision']==1 and len(first_change['changes'][0]['rows'])==16 and ack['ok']
send()
assert json.loads(sys.stdin.readline())==first_change
assert json.loads(sys.stdin.readline())==ack
open(os.environ['UI_SUCCESS'],'w').write('ok')
PY
export UI_SUCCESS="$work/success" TRASHTALK_UI_PROFILE="$work/profile.json"
argv=$(jq -cn --arg script "$work/surface.py" '["python3",$script]')
@ UI::Surface openArgv: "$argv" handler: UI::Events context: '{"count":10000,"revision":0}'
[[ $(<"$work/success") == ok ]]
jq -e '.schema_version==1 and .handlers>=2 and .bytes_out>1000000 and .handler_timing.count==.handlers' "$work/profile.json" >/dev/null
printf '%s\n' 'PASS: UI signals, ordinary bindings, resolved descriptions, 10k bulk feed, duplicate receipts, bridge profiling'
