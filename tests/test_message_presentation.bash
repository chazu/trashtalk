#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
export SQLITE_JSON_DB="$tmp/state.db" TRASHTALK_USER=reader TRASHTALK_NO_AUTOTICK=1 TZ=America/New_York
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
contains() { [[ "$3" == *"$2"* ]] || { printf 'FAIL: %s missing %s in %s\n' "$1" "$2" "$3"; exit 1; }; passed=$((passed+1)); }
identity=$(@ AgentIdentity named: gusgus)
@ "$identity" displayName: Gusgus
@ "$identity" save
archetype=$(@ Gusgus archetype)
role=$(@ Gusgus role)
session=$(@ AgentSession openFor: "$identity" archetype: "$archetype" role: "$role" workspace: "$root" profile: shell)
msg=$(@ Inbox send: $'\n  Tests\tpass.\n\nSecond paragraph.' to: reader from: "session:$session" subject: result kind: result)
data=$(@ MessagePresentation dataFor: "$msg")
check 'generic subject falls back to first nonblank body line' 'Tests pass.' "$(jq -r .label <<< "$data")"
contains 'session resolves to display name' '● Gusgus' "$(jq -r .prefix <<< "$data")"
sent_millis=$(@ "$msg" sentAt)
contains 'today has short local timestamp' "$(@ Time format: "$((sent_millis / 1000))" as: '%H:%M')" "$(jq -r .prefix <<< "$data")"
contains 'millisecond timestamp resolves to today' 'Today ' "$(jq -r .header <<< "$data")"
text=$(@ MessagePresentation textFrom: "$data")
contains 'header has human participants' 'Gusgus → You' "$text"
contains 'body preserves paragraph breaks' $'Tests\tpass.\n\nSecond paragraph.' "$text"
check 'preview omits raw routing metadata' false "$([[ "$text" == *"$session"* || "$text" == *'Id:'* ]] && echo true || echo false)"
check 'presentation leaves unread state intact' unread "$(@ "$msg" status)"
record=$(@ MessagePresentation recordFor: "$msg" data: "$data")
check 'picker retains exact message id' "$msg" "$(jq -r .id <<< "$record")"
contains 'full source address stays searchable' "session:$session" "$(jq -r .display.search_text <<< "$record")"
contains 'body stays searchable beyond the headline' 'Second paragraph.' "$(jq -r .display.search_text <<< "$record")"
@ "$msg" kind: question
@ "$msg" subject: $'  Config\n\tchoice  '
data=$(@ MessagePresentation dataFor: "$msg")
check 'multiline subject collapses to one row' 'Config choice' "$(jq -r .label <<< "$data")"
contains 'question and unread have separate markers' '●?' "$(jq -r .prefix <<< "$data")"
@ "$msg" markRead >/dev/null
data=$(@ MessagePresentation dataFor: "$msg")
check 'read question keeps kind but loses unread dot' true "$(jq -r '.prefix | contains("?") and (contains("●") | not)' <<< "$data")"
@ "$msg" sentAt: 1704202500000
data=$(@ MessagePresentation dataFor: "$msg")
contains 'old message uses local date and time including year' "$(@ Time format: 1704202500 as: '%Y-%m-%d %H:%M')" "$(jq -r .prefix <<< "$data")"
check 'identity inbox resolves to same display name' Gusgus "$(@ MessagePresentation participant: agent:gusgus)"
check 'unknown sender is retained and SQL escaped' "someone's-bot" "$(@ MessagePresentation participant: "someone's-bot")"
check 'control characters do not enter picker row' 'one two three' "$(@ MessagePresentation singleLine: $'one\ttwo\033three')"
@ "$msg" subject: ''
@ "$msg" body: ''
check 'empty messages remain selectable' '(empty message)' "$(@ MessagePresentation dataFor: "$msg" | jq -r .label)"
contains 'full details still contain exact source' "session:$session" "$(@ "$msg" show)"
check 'shortened names preserve UTF-8' 'あいうえおかきくけこさしすせそたちつて…' "$(@ MessagePresentation rowName: 'あいうえおかきくけこさしすせそたちつてとなに')"
echo "=== $passed presentation checks passed ==="
