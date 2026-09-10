#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_NO_AUTOTICK=1
export PICKS="$tmp/picks" PAGES="$tmp/pages"
mkdir "$tmp/bin"
cat > "$tmp/bin/inpick" <<'PICK'
#!/usr/bin/env bash
records=$(cat)
choice=$(head -n 1 "$PICKS")
tail -n +2 "$PICKS" > "$PICKS.next"; mv "$PICKS.next" "$PICKS"
[[ -n "$choice" ]] || { echo '{"outcome":"cancelled"}'; exit 130; }
action=''
if [[ "$choice" == terminate:* ]]; then action=terminate; choice=${choice#terminate:}; fi
printf '%s\n' "$records" | jq -sc --arg id "$choice" --arg action "$action" '{outcome:"selected",action:$action,selection:(map(select(.id==$id))[0])}'
PICK
cat > "$tmp/bin/inpage" <<'PAGE'
#!/usr/bin/env bash
cat >> "$PAGES"
echo '{"outcome":"unchanged"}'
PAGE
chmod +x "$tmp/bin/"*
export PATH="$tmp/bin:$PATH"
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; [[ ! -f "$PAGES" ]] || cat "$PAGES"; exit 1; fi; }

# Old AgentSession records on the user's store predate lifecycleState. Exercise
# the actual browser -> confirmation -> worker -> persisted transition chain.
legacy=agentsession_legacy
_db_sql "INSERT INTO instances(id,data) VALUES('$legacy',
  '{\"class\":\"AgentSession\",\"_vars\":[\"sessionName\",\"sessionType\",\"context\",\"messageCount\"],\"sessionName\":null,\"sessionType\":null,\"context\":\"preserve legacy context\",\"messageCount\":0}');"
printf '%s\n' "terminate:$legacy" terminate '' > "$PICKS"
check 'browser dismisses after legacy termination' dismissed "$(@ AgentSession browse)"
check 'legacy session terminates durably' terminated "$(db_get "$legacy" | jq -r '.lifecycleState // empty')"
check 'legacy context is retained' 'preserve legacy context' "$(db_get "$legacy" | jq -r .context)"
check 'successful termination shows no error page' false "$([[ -s "$PAGES" ]] && echo true || echo false)"
check 'legacy termination remains idempotent' terminated "$(@ "$legacy" terminate)"
check 'fresh runtime retains terminated lifecycle' terminated "$(
    unset TRASH_SESSION_ID
    bash -c 'source "$1/lib/trash.bash"; @ AgentSession findAll >/dev/null; @ "$2" lifecycleState' fixture "$root" "$legacy"
)"

# Missing/null/empty lifecycle values permit retirement, never resurrection.
for value in null '""'; do
    id=$(@ AgentSession new)
    _db_sql "UPDATE instances SET data=json_set(data,'$.lifecycleState',json('$value')) WHERE id='$id';"
    @ "$id" resume >/dev/null 2>&1; status=$?
    check 'uninitialized lifecycle cannot resume' 1 "$status"
    check 'uninitialized lifecycle can terminate' terminated "$(@ "$id" terminate)"
done
id=$(@ AgentSession new)
@ Store patch: "$id" with: '{"lifecycleState":"invalid-state"}'
@ "$id" terminate >/dev/null 2>&1; status=$?
check 'unknown nonempty lifecycle still rejected' 1 "$status"
echo "=== $passed legacy termination checks passed ==="
