#!/usr/bin/env bash
# Child entry point for Process/Future. Share the parent's exported object cache;
# a new Bash owns evaluation and exit, while Tool owns detachment and receipts.
source "${TRASHTALK_DIR:-$HOME/.trashtalk}/lib/trash.bash" || exit 1
[[ "$2" != true ]] || exec 2>&1
eval "$1"
