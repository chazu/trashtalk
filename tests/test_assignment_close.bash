#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash"
export TRASHTALK_USER=close-owner TRASHTALK_NO_AUTOTICK=1 TRASHTALK_GUSGUS_PROFILE=shell
unset TRASHTALK_RUN_TOKEN TRASHTALK_ASSIGNMENT_ID
db_init
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else printf 'FAIL: %s expected=%s got=%s\n' "$1" "$2" "$3"; exit 1; fi; }
must() { "$@" || { printf 'FAIL: command failed: %s\n' "$*" >&2; exit 1; }; }
reject() { local name="$1"; shift; if "$@" >"$TMPDIR/rejected" 2>&1; then echo "FAIL: accepted $name"; exit 1; else echo "PASS: $name"; passed=$((passed+1)); fi; }
field() { db_get "$1" | jq -r "$2"; }
coordinate() {
    mapfile -t pair < <(@ Agent::Run startFor: "$1" profile: shell)
    must @ "${pair[0]}" transitionTo: running >/dev/null
    parent=${pair[0]}; token=${pair[1]}
}
# A specialist run that claimed the work and is still live.
work() {
    unset TRASHTALK_RUN_TOKEN
    mapfile -t wpair < <(@ Agent::Run startFor: "$1" profile: shell)
    worker=${wpair[0]}
    must @ "$worker" transitionTo: running >/dev/null
    check "worker claims $2" true "$(@ Agent::Delivery claim: "$2" run: "$worker")"
}

coordinator=$(must @ Gusgus sessionFor: "$TRASHTALK_DIR")
coordinate "$coordinator"
export TRASHTALK_RUN_TOKEN=$token
a=$(must @ Agent::Run delegate: 'Outlive the requesting conversation' criteria: evidence key: closed)
b=$(must @ Agent::Run delegate: 'Cancelled by a later conversation' criteria: evidence key: later)
check 'a busy specialist queues more work' "$(field "$a" .currentSession)" "$(field "$b" .currentSession)"
unset TRASHTALK_RUN_TOKEN
specialist=$(field "$a" .currentSession)
must @ "$parent" finishWith: succeeded outcome: '{}' error: '' >/dev/null
must @ "$coordinator" close >/dev/null

# The outcome cannot go to a closed conversation; the owner can still cancel.
must @ "$a" cancel: 'Cancelled after requester closed' >/dev/null
check 'owner cancels after requester closed' cancelled "$(field "$a" .state)"
check 'outcome falls back to the owner inbox' close-owner "$(field "message_${a}_outcome" .to)"

# A later conversation of the same coordinator identity manages the work,
# including stopping the specialist run that holds it.
next=$(must @ Gusgus sessionFor: "$TRASHTALK_DIR")
check 'coordinator continues in a new conversation' true "$([[ "$next" != "$coordinator" ]] && echo true || echo false)"
bd=$(field "$b" .delivery)
work "$specialist" "$bd"
coordinate "$next"
export TRASHTALK_RUN_TOKEN=$token
must @ "$b" cancel: 'Withdrawn by coordinator' >/dev/null
unset TRASHTALK_RUN_TOKEN
check 'coordinator cancels its delegated work' cancelled "$(field "$b" .state)"
check 'cancel stops the run working the Assignment' interrupted "$(field "$worker" .state)"
check 'cancel skips the stopped delivery' skipped "$(field "$bd" .state)"
check 'specialist session reopens for queued work' open "$(field "$specialist" .lifecycleState)"
check 'outcome for a closed requester goes to the owner' close-owner "$(field "message_${b}_outcome" .to)"

# Uncertain work no longer blocks cancel.
export TRASHTALK_RUN_TOKEN=$token
c=$(must @ Agent::Run delegate: 'Leave uncertain effects' criteria: evidence key: uncertain)
d=$(must @ Agent::Run delegate: 'Unrelated queued work' criteria: evidence key: unrelated)
unset TRASHTALK_RUN_TOKEN
cd_=$(field "$c" .delivery)
work "$specialist" "$cd_"
must @ "$cd_" transitionTo: uncertain >/dev/null
must @ "$worker" finishWith: unsettled outcome: '{}' error: '' >/dev/null
must @ "$c" cancel: 'Effects inspected; abandon' >/dev/null
check 'owner cancels uncertain work' cancelled "$(field "$c" .state)"
check 'uncertain delivery is skipped' skipped "$(field "$cd_" .state)"

# A run working a different Assignment in the same session fences nothing here.
dd=$(field "$d" .delivery)
mapfile -t wpair < <(@ Agent::Run startFor: "$specialist" profile: shell)
must @ "${wpair[0]}" transitionTo: running >/dev/null
must @ "$d" cancel: 'Not needed' >/dev/null
check 'queued work cancels while another run is live' cancelled "$(field "$d" .state)"
check 'unrelated live run is left alone' running "$(field "${wpair[0]}" .state)"

# Authority is still the owner or the requesting coordinator identity.
must @ "${wpair[0]}" finishWith: succeeded outcome: '{}' error: '' >/dev/null
export TRASHTALK_RUN_TOKEN=$token
e=$(must @ Agent::Run delegate: 'Guarded work' criteria: evidence key: guarded)
unset TRASHTALK_RUN_TOKEN
export TRASHTALK_USER=foreign-owner
reject 'foreign human cannot cancel' @ "$e" cancel: nope
export TRASHTALK_USER=close-owner
other=$(must @ Agent::Identity named: other-coordinator)
@ "$other" owner: close-owner
@ "$other" save
other_session=$(must @ Agent::Session openFor: "$other" archetype: "$(@ "$next" archetype)" role: "$(@ "$next" role)" workspace: "$TRASHTALK_DIR" profile: shell)
coordinate "$other_session"
export TRASHTALK_RUN_TOKEN=$token
reject 'another coordinator identity cannot cancel' @ "$e" cancel: nope
unset TRASHTALK_RUN_TOKEN
check 'guarded work stays open' open "$(field "$e" .state)"
echo "=== $passed assignment close checks passed ==="
