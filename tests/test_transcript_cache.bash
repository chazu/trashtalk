#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
root=$TRASHTALK_DIR
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/logs/r1" "$tmp/logs/r2" "$tmp/cache"
records='{"session":{"id":"s","title":"Gusgus"},"conversation_runs":["r1","r2"],"rows":[{"id":"m","seq":2.5,"data":{"class":"Message","from":"human","to":"session:s","body":"mail"}}],"has_earlier":0}'
limit=400
# Independent reference: full JSONL parse, global sort, then coalesce, as in the
# pre-cache adapter. Tests with partial records assert their boundary separately.
reference() {
    printf '%s' "$records" | jq -c --arg mode rows --arg run '' --argjson seq 0 --arg profile '' --arg stream '' -f "$root/lib/agent-transcript.jq" > "$tmp/reference-rows"
    for run in r1 r2; do
        [[ -f "$tmp/logs/$run/conversation.jsonl" ]] || continue
        jq -Rc --arg run "$run" 'fromjson? | select(.kind=="user" or .kind=="assistant_delta") |
          {id:($run+"/chat/"+(input_line_number|tostring)),kind:.kind,title:(if .kind=="user" then "You" else "Assistant" end),text:.text,run:$run,order:[.time,input_line_number]}' "$tmp/logs/$run/conversation.jsonl" >> "$tmp/reference-rows"
    done
    jq -sc --argjson records "$records" --argjson limit "$limit" 'sort_by(.order,.id) |
      reduce .[] as $entry ([]; if ($entry.kind|endswith("_delta")) and length>0 and .[-1].kind==$entry.kind and .[-1].run==$entry.run then .[-1].text += $entry.text else .+[$entry] end) |
      {schema_version:1,type:"snapshot",session:$records.session,has_earlier:($records.has_earlier==1 or length>$limit),window:$limit,entries:.[-$limit:]}' "$tmp/reference-rows"
}
cached() { printf '%s' "$records" | bash "$root/lib/agent-transcript.bash" "$limit" "$tmp/logs" "$tmp/cache"; }
check() {
    actual=$(cached)
    expected=$(reference)
    [[ $(jq -Sc . <<< "$actual") == "$(jq -Sc . <<< "$expected")" ]] || { echo "FAIL: $1"; diff <(jq . <<< "$expected") <(jq . <<< "$actual"); exit 1; }
    printf 'PASS: %s\n' "$1"
}
printf '%s\n' '{"kind":"assistant_delta","text":"first","time":1}' > "$tmp/logs/r1/conversation.jsonl"
printf '%s\n' '{"kind":"user","text":"input","time":2}' > "$tmp/logs/r2/conversation.jsonl"
check 'cold projection preserves interleaved runs and mail'
check 'unchanged refresh is identical'
printf '%s\n' '{"kind":"assistant_delta","text":"later","time":3}' >> "$tmp/logs/r1/conversation.jsonl"
check 'append does not merge across another speaker or mail'
printf '%s\n' '{"kind":"assistant_delta","text":"early","time":1.5}' >> "$tmp/logs/r1/conversation.jsonl"
check 'late arrival with earlier time rebuilds global ordering'
perl -e 'open my $f, "+<:raw", $ARGV[0] or die $!; local $/; my $v=<$f>; $v =~ s/first/other/; seek($f,0,0); print $f $v' "$tmp/logs/r1/conversation.jsonl"
check 'same-size edit rebuilds'
perl -e 'open my $f, "+<:raw", $ARGV[0] or die $!; local $/; my $v=<$f>; $v =~ s/other/third/; seek($f,0,0); print $f $v' "$tmp/logs/r1/conversation.jsonl"
printf '%s\n' '{"kind":"user","text":"new","time":4}' >> "$tmp/logs/r1/conversation.jsonl"
check 'prefix rewrite followed by growth is not mistaken for append'
cp "$tmp/logs/r1/conversation.jsonl" "$tmp/replacement"
mv "$tmp/replacement" "$tmp/logs/r1/conversation.jsonl"
check 'file replacement rebuilds'
printf '%s\n' '{"kind":"user","text":"reset","time":5}' > "$tmp/logs/r1/conversation.jsonl"
check 'truncation rebuilds'
printf '%s' '{"kind":"assistant_delta","text":"雪' >> "$tmp/logs/r1/conversation.jsonl"
before=$(cached)
check 'partial JSON is withheld'
printf '%s' '","time":6}' >> "$tmp/logs/r1/conversation.jsonl"
[[ $(cached) == "$before" ]]
printf '\n' >> "$tmp/logs/r1/conversation.jsonl"
check 'completed record appears exactly once'
check 'completed record is not repeated on another poll'
rm "$tmp/logs/r2/conversation.jsonl"
check 'deleted run file disappears'
limit=1; check 'smaller window preserves has-earlier'
limit=10; check 'larger window restores earlier entries'
records=$(jq -c '.session.activity="running" | .rows[0].data.body="edited mail"' <<< "$records")
check 'message and session changes refresh cached presentation'
printf 'corrupt' > "$tmp/cache/state.json"
check 'corrupt cache is disposable'
# Representative streaming history: only one appended record reaches jq again.
jq -cn 'range(1;10001) | {kind:"assistant_delta",text:"x",time:(100+.)}' > "$tmp/logs/r1/conversation.jsonl"
check 'ten thousand deltas preserve coalesced text'
printf '%s\n' '{"kind":"assistant_delta","text":"end","time":20000}' >> "$tmp/logs/r1/conversation.jsonl"
printf '%s' "$records" > "$tmp/records"
perl "$root/lib/transcript-files.pl" "$tmp/records" "$tmp/cache/state.json" "$tmp/logs" 0 "$tmp/new-lines" "$tmp/manifest"
[[ $(wc -l < "$tmp/new-lines" | tr -d ' ') == 1 ]]
[[ $(jq -r .full "$tmp/manifest") == false ]]
check 'large history parses only the appended record'
printf 'PASS: transcript cache differential and append-boundary checks\n'
