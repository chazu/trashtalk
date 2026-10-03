#!/usr/bin/env bash
# Read-only Gmail -> two decision stages -> JSONL proposals. Requires gws OAuth.
# Decider (the default target) stays on the tailnet; TRASHTALK_DECISION_TARGET=jev
# needs OPENROUTER_API_KEY and sends bounded email content to OpenRouter/TypeSafe.
set -euo pipefail
if [[ $# -lt 1 || $# -gt 3 ]]; then
    printf 'Usage: bash examples/gmail-jev.bash EXPECTED_EMAIL [QUERY] [LIMIT=5]\n' >&2
    exit 2
fi
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
expected_email=$1
query=${2:-in:inbox}
limit=${3:-5}
@ Gmail::Client requireLimit: "$limit" >/dev/null
actual_email=$(@ Gmail::Client requireAccount: "$expected_email")
printf 'Assessing up to %s emails for %s; suggestions only, two decision requests per email.\n' "$limit" "$actual_email" >&2
@ Gmail::Review preview: "$query" limit: "$limit"
