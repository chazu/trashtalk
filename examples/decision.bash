#!/usr/bin/env bash
# Explicit target: jev, clm-local, clm-bc250, clm-prefer-bc250.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
target=$(@ Decision::Target named: "${1:-clm-local}")
@ Examples::DecisionTicket decide: "${2:-I was charged twice. Please refund the duplicate before Friday.}" using: "$target"
