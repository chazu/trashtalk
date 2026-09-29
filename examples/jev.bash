#!/usr/bin/env bash
# Requires OPENROUTER_API_KEY. One real, billable Jev decision request.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source "$root/lib/trash.bash"
@ Examples::JevTicket assess: "${1:-I was charged twice. Please refund the duplicate before Friday.}"
