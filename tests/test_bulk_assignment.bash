#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
trap '_env_cleanup || true' EXIT
export TRASHTALK_NO_AUTOTICK=1
left=$(@ Message new)
right=$(@ Message new)
fields='{"to":"reader","from":"sender","subject":"00042","body":"quote '\'' 雪\nline\n","kind":"true"}'
@ "$left" to: reader
@ "$left" from: sender
@ "$left" subject: 00042
@ "$left" body: $'quote \' 雪\nline\n'
@ "$left" kind: true
calls="$TMPDIR/assignment-calls"
jq() { printf 'jq\n' >> "$calls"; command jq "$@"; }
: > "$calls"
[[ $(@ Runtime assign: "$fields" to: "$right") == "$right" ]]
[[ $(wc -l < "$calls" | tr -d ' ') -le 3 ]]
[[ $(@ "$left" asJson | command jq -Sc 'del(.created_at)') == "$(@ "$right" asJson | command jq -Sc 'del(.created_at)')" ]]
# Assignments remain cache changes, preserving initial constructor persistence.
command jq -e '.to==null and .from==null and .subject==null' <<< "$(db_get "$right")" >/dev/null
@ "$right" save >/dev/null
[[ $(db_get "$right" | command jq -Sc .) == "$(@ "$right" asJson | command jq -Sc .)" ]]
# Numeric and container-looking text follow the existing setter's coercion.
for value in '[]' '{"nested":[false,null,"雪"]}' '{broken}' '-001'; do
    @ "$left" body: "$value"
    fields=$(command jq -cn --arg value "$value" '{body:$value}')
    @ Runtime assign: "$fields" to: "$right" >/dev/null
    [[ $(@ "$left" asJson | command jq -c .body) == "$(@ "$right" asJson | command jq -c .body)" ]]
done
# Existing or subsequently replaced setter bodies prevent batching.
@ "$right" asJson >/dev/null
__Message__body_() { _ivar_set body "custom:$1"; }
@ Runtime assign: '{"subject":"first","body":"second","kind":"third"}' to: "$right" >/dev/null
[[ $(@ "$right" body) == custom:second ]]
# A failed custom setter stops later sends and retains earlier public effects.
__Message__body_() { return 7; }
if @ Runtime assign: '{"subject":"before failure","body":"fail","kind":"must not run"}' to: "$right" >/dev/null; then exit 1; fi
[[ $(@ "$right" subject) == 'before failure' && $(@ "$right" kind) == third ]]
# Restore the class, then prove advice sees the original property send order.
source "$TRASHDIR/.compiled/Message"
record_advice() { printf '%s\n' "$_SELECTOR" >> "$TMPDIR/advice-order"; }
_add_before_advice Message '*' record_advice
@ Runtime assign: '{"subject":"one","body":"two","kind":"three"}' to: "$right" >/dev/null
[[ $(cat "$TMPDIR/advice-order") == $'subject_\nbody_\nkind_' ]]
_remove_advice Message '*'
# Invalid field maps fail before cache modification.
before=$(@ "$right" asJson)
if @ Runtime assign: '{"body":"changed","kind":null}' to: "$right" >/dev/null 2>&1; then exit 1; fi
[[ $(@ "$right" asJson) == "$before" ]]
message=$(@ Message to: reader from: sender subject: '' body: 'delivered body' kind: note)
command jq -e '.to==null and .body==null' <<< "$(db_get "$message")" >/dev/null
box=$(@ Inbox named: reader)
[[ $(@ "$box" deliver: "$message") == "$message" ]]
command jq -e --arg id "$message" '.to=="reader" and .body=="delivered body" and .thread==$id and .sentAt>0' <<< "$(db_get "$message")" >/dev/null
[[ $(db_get "$message" | command jq -Sc .) == "$(@ "$message" asJson | command jq -Sc .)" ]]
printf 'PASS: batch setter parity, bounded serializers, overrides, advice, failures and immediate persistence\n'
