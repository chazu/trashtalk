#!/usr/bin/env bash
# Compatibility entry point; every test runs in its own isolated process.
set -euo pipefail
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
case "${1:-all}" in
    all) [[ $# == 0 ]] || shift; exec bash "$SCRIPT_DIR/../../run-tests.sh" "$SCRIPT_DIR" "$@" ;;
    tokenizer|parser|codegen|integration)
        section=$1; shift
        exec bash "$SCRIPT_DIR/../../test-isolated.bash" "$SCRIPT_DIR/test_$section.bash" "$@" ;;
    *) echo 'Usage: run_tests.bash [all|tokenizer|parser|codegen|integration] [runner options]' >&2; exit 2 ;;
esac
