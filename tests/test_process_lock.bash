#!/usr/bin/env bash
if [[ ${TRASHTALK_TEST_ISOLATED:-} != 1 ]]; then
 exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source lib/trash.bash 2>/dev/null
trap - EXIT
lock="$TMPDIR/store.lock"
hold() { ( exec 9>"$lock"; perl -e 'flock(STDIN, 2) or die; sleep $ARGV[0]' "$1" <&9 ); }
# Nonblocking: a held lock is a silent no-op, exactly as before.
hold 2 & holder=$!
sleep 0.3
out=$(@ Process withLock: "$lock" receiver: Trash selector: info argument: '')
[[ -z "$out" ]]
# Waiting: a bounded wait times out with the same silent contract...
start=$SECONDS
out=$(@ Process withLock: "$lock" receiver: Trash selector: info argument: '' waiting: 1)
[[ -z "$out" && $((SECONDS - start)) -ge 1 ]]
# ...and otherwise queues behind the holder instead of losing the race.
out=$(@ Process withLock: "$lock" receiver: Trash selector: info argument: '' waiting: 10)
[[ -n "$out" ]]
wait "$holder"
# Whole seconds only; the lock is released when the callee returns.
if @ Process withLock: "$lock" receiver: Trash selector: info argument: '' waiting: 1.5 >/dev/null 2>&1; then exit 1; fi
out=$(@ Process withLock: "$lock" receiver: Trash selector: info argument: '')
[[ -n "$out" ]]
# Control operations queue for a bounded, configurable number of seconds.
[[ $(@ Agent::Worker controlWait) == 30 ]]
[[ $(TRASHTALK_CONTROL_WAIT=5 @ Agent::Worker controlWait) == 5 ]]
[[ $(TRASHTALK_CONTROL_WAIT=abc @ Agent::Worker controlWait) == 30 ]]
echo 'PASS: nonblocking and bounded waiting worker locks'
