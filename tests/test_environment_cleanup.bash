#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
# Both an unused environment and repeated explicit cleanup are successful.
env -u TRASH_SESSION_ID bash -ce 'source "$TRASHTALK_DIR/lib/trash.bash"; [[ ! -d "$_ENV_DIR" ]]'
env -u TRASH_SESSION_ID bash -ce 'source "$TRASHTALK_DIR/lib/trash.bash"; _env_init; _env_cleanup; _env_cleanup'
# Cleanup also preserves an intentional failing exit status.
status=0
env -u TRASH_SESSION_ID bash -ce 'source "$TRASHTALK_DIR/lib/trash.bash"; exit 7' || status=$?
[[ $status == 7 ]]
echo 'PASS: absent/repeated environment cleanup preserves shell exit status'
# An EXIT trap set before sourcing still runs after the runtime's own cleanup,
# sees the exiting status, and runs only once when the runtime is re-sourced.
prior='rc=$?; [[ -d $_ENV_DIR ]] && echo kept:$rc || echo removed:$rc'
out=$(env -u TRASH_SESSION_ID prior="$prior" bash -c 'trap "$prior" EXIT
    source "$TRASHTALK_DIR/lib/trash.bash"; _env_init; exit 3') && status=0 || status=$?
[[ "$out" == 'removed:3' && $status == 3 ]] || { echo "FAIL: prior EXIT trap: out=[$out] status=$status" >&2; exit 1; }
out=$(env -u TRASH_SESSION_ID bash -c 'trap "echo prior" EXIT
    source "$TRASHTALK_DIR/lib/trash.bash"; source "$TRASHTALK_DIR/lib/trash.bash"')
[[ "$out" == 'prior' ]] || { echo "FAIL: re-sourced prior EXIT trap: out=[$out]" >&2; exit 1; }
# A subshell lists its parent's EXIT trap without inheriting it; sourcing the
# runtime there must not adopt and run the parent's cleanup early.
out=$(env -u TRASH_SESSION_ID bash -c 'trap "echo parent" EXIT
    (source "$TRASHTALK_DIR/lib/trash.bash"); echo after-subshell')
[[ "$out" == $'after-subshell\nparent' ]] || { echo "FAIL: subshell ran parent trap: out=[$out]" >&2; exit 1; }
out=$(env -u TRASH_SESSION_ID bash -c 'trap "echo parent" EXIT; source "$TRASHTALK_DIR/lib/trash.bash"
    (source "$TRASHTALK_DIR/lib/trash.bash"); echo after-subshell')
[[ "$out" == $'after-subshell\nparent' ]] || { echo "FAIL: re-sourcing subshell ran parent trap: out=[$out]" >&2; exit 1; }
echo 'PASS: an EXIT trap set before sourcing the runtime still runs once'
