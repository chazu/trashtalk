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
