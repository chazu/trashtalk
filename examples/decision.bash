#!/usr/bin/env bash
# Explicit target: decider (default) or jev (billable OpenRouter request).
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
target=$(@ Decision::Target named: "${1:-decider}")
@ Examples::DecisionTicket decide: "${2:-I was charged twice. Please refund the duplicate before Friday.}" using: "$target"
