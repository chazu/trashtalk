#!/usr/bin/env bash
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
set -euo pipefail
source "$TRASHTALK_DIR/lib/trash.bash" 2>/dev/null
# Offline transport seam, preserving exactly the real result/status contract.
trash_decision_http() { printf '%s\n' "$fixture"; return 1; }
message='{"id":"fixture","from":"sender","subject":"test","body":"text","bodySource":"text/plain","truncated":false}'
target='{"name":"jev","provider":"jev","model":"typesafe/jev-1.13"}'
for fixture in '{"outcome":"http_error","status":429,"body":"rate limited"}' '{"outcome":"transport_error","exit_code":28,"stderr":"timeout"}' '{"outcome":"response_shape_error","body":"broken"}'; do
    for receiver in Examples::DecisionTicket Gmail::Review Gmail::Junk; do
        case "$receiver" in
            Examples::DecisionTicket) args=(decide: fixture using: "$target");;
            Gmail::Review) args=(assess: "$message" using: "$target");;
            Gmail::Junk) args=(assess: "$message" examples: '[]' using: "$target");;
        esac
        if output=$(@ "$receiver" "${args[@]}" 2>/dev/null); then echo 'FAIL: failure returned success'; exit 1; fi
        [[ "$output" == "$fixture" ]] || { printf 'FAIL %s lost receipt: <%s>\n' "$receiver" "$output"; exit 1; }
    done
done
# A successful HTTP exchange containing a malformed decision is also a receipt.
trash_decision_http() { printf '{"answers":{}}\n'; }
if output=$(@ Examples::DecisionTicket decide: fixture using: "$target" 2>/dev/null); then exit 1; fi
jq -e '.outcome=="response_shape_error" and .body=="{\"answers\":{}}"' <<< "$output" >/dev/null
echo 'PASS: typed failures survive stages and Gmail workflows intact'

# A streaming preview must emit each error receipt once, not once before the
# handler and again when the handler rethrows it.
decision_test_message=$message
decision_test_target=$target
_ensure_class_sourced Gmail::Client
_ensure_class_sourced Decision::Target
__Gmail__Client__class__search_limit_() { printf '%s\n' '{"messages":[{"id":"fixture"}]}'; }
__Gmail__Client__class__message_() { printf '%s\n' "$decision_test_message"; }
__Decision__Target__class__selected() { printf '%s\n' "$decision_test_target"; }
fixture='{"outcome":"http_error","status":429,"body":"rate limited"}'
trash_decision_http() { printf '%s\n' "$fixture"; return 1; }
if output=$(@ Gmail::Review preview: inbox limit: 1 2>/dev/null); then exit 1; fi
[[ "$output" == "$fixture" ]] || { printf 'FAIL: preview receipt <%s>\n' "$output"; exit 1; }
echo 'PASS: streaming preview emits one failure receipt'
