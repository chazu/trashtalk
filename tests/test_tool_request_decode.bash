#!/usr/bin/env bash
# Standalone invocations use the same isolated checkout as the suite runner.
if [[ "${TRASHTALK_TEST_ISOLATED:-}" != 1 ]]; then
    exec bash "$(dirname "${BASH_SOURCE[0]}")/../lib/test-isolated.bash" "${BASH_SOURCE[0]}" "$@"
fi
# Tool runProcessRequestJson: decodes a request in one jq pass. These checks
# pin the process count and the output, status, stdin, directory, and error
# contracts that every exact-argv wrapper relies on.
set -uo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
export TRASHTALK_PROGRESS=0
passed=0
check() { if [[ "$2" == "$3" ]]; then echo "PASS: $1"; passed=$((passed+1)); else echo "FAIL: $1 expected=$2 got=$3"; exit 1; fi; }
calls="$TMPDIR/jq.calls"
jq() { printf 'jq\n' >> "$calls"; command jq "$@"; }
jq_count() { wc -l < "$calls" | tr -d ' '; }
field() { command jq -r --arg key "$2" '.[$key]' <<<"$1"; }

: > "$calls"
result=$(@ Tool runProcessRequestJson: '{"argv":["/usr/bin/true"],"capture":true}')
check 'decoding a capture request uses two jq processes' 2 "$(jq_count)"
check 'capture reports the exit status' 0 "$(field "$result" exit_code)"
check 'capture envelope is versioned' 1 "$(field "$result" schema_version)"
: > "$calls"
result=$(@ Tool captureArgvJson: '["/usr/bin/true"]')
check 'the public capture adds one jq for building the typed request' 3 "$(jq_count)"
check 'public capture reports the exit status' 0 "$(field "$result" exit_code)"

: > "$calls"
result=$(@ Tool captureArgvJson: '["/bin/sh","-c","printf out; printf err >&2; exit 7"]')
check 'capture keeps a non-zero status' 7 "$(field "$result" exit_code)"
check 'capture keeps stdout' out "$(field "$result" stdout)"
check 'capture keeps stderr' err "$(field "$result" stderr)"

: > "$calls"
result=$(@ Tool runProcessRequestJson: '{"argv":["/bin/cat"],"capture":true,"stdin":"line one\nline two\n"}')
check 'inline stdin uses two jq processes' 2 "$(jq_count)"
check 'inline stdin reaches the child byte-for-byte' true "$(command jq -r '.stdout == "line one\nline two\n"' <<<"$result")"
result=$(@ Tool captureArgvJson: '["/bin/cat"]' input: $'tab\there')
check 'public capture with input keeps the text' $'tab\there' "$(command jq -j '.stdout' <<<"$result")"

: > "$calls"
request=$(command jq -cn '{argv:["/bin/cat"],capture:true,stdin:("a"+([0]|implode)+"b")}')
result=$(@ Tool runProcessRequestJson: "$request")
check 'stdin containing NUL is decoded separately' 3 "$(jq_count)"
check 'stdin containing NUL reaches the child intact' '[97,0,98]' "$(command jq -c '.stdout | explode' <<<"$result")"

odd_dir="$TMPDIR/odd"$'\n'
mkdir -p "$odd_dir"
result=$(@ Tool captureArgvJson: '["/bin/pwd"]' inDirectory: "$odd_dir")
check 'working directory with a trailing newline is honoured' true "$(command jq -r --arg dir "$odd_dir" '.stdout == $dir + "\n"' <<<"$result")"
result=$(@ Tool captureArgvJson: '["/bin/pwd"]' inDirectory: "$TMPDIR/missing")
check 'a missing working directory reports the cd failure status' 72 "$(field "$result" exit_code)"

output=$(@ Tool runArgvJson: '["/bin/cat"]' input: 'hello')
check 'run with inline stdin streams the child output' hello "$output"
@ Tool runArgvJson: '["/bin/sh","-c","exit 5"]' >/dev/null 2>&1
check 'run preserves the child status' 5 "$?"
@ Tool runArgvJson: '["/bin/sh","-c","exit 6"]' input: '' >/dev/null 2>&1
check 'run with stdin preserves the child status' 6 "$?"

error=$(@ Tool runProcessRequestJson: '{"argv":"true"}' 2>&1 >/dev/null)
check 'a malformed request is rejected' 1 "$?"
check 'malformed request names the error' true "$([[ "$error" == *'Invalid process request JSON'* ]] && echo true || echo "false: $error")"
error=$(@ Tool runProcessRequestJson: '{"argv":[]}' 2>&1 >/dev/null)
check 'an empty argv is rejected' 1 "$?"
check 'empty argv names the error' true "$([[ "$error" == *'Invalid or empty argv JSON'* ]] && echo true || echo "false: $error")"
request=$(command jq -cn '{argv:["/usr/bin/true"],progress:("x"+([0]|implode))}')
error=$(@ Tool runProcessRequestJson: "$request" 2>&1 >/dev/null)
check 'a progress label containing NUL is rejected' 1 "$?"
echo "=== $passed request decoding checks passed ==="
