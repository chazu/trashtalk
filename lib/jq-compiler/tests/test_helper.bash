#!/usr/bin/env bash
# ==============================================================================
# Shared Test Helper
# ==============================================================================
# Source this file at the start of any test that needs COMPILER_DIR and run_test
# ==============================================================================

# Only set up if not already set (allows harness to override)
if [[ -z "${COMPILER_DIR:-}" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[1]:-${BASH_SOURCE[0]}}")" && pwd)"
    COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Counters (only initialize if not already set by harness)
: "${TESTS_RUN:=0}"
: "${TESTS_PASSED:=0}"
: "${TESTS_FAILED:=0}"

# Run a test and check result
# Usage: run_test "test name" expected actual
run_test() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    ((TESTS_RUN++)) || true

    if [[ "$expected" == "$actual" ]]; then
        echo -e "  ${GREEN}✓${NC} $name"
        ((TESTS_PASSED++)) || true
        return 0
    else
        echo -e "  ${RED}✗${NC} $name"
        echo -e "    Expected: $expected"
        echo -e "    Got:      $actual"
        ((TESTS_FAILED++)) || true
        return 1
    fi
}

# Print summary if running standalone
print_test_summary() {
    echo ""
    echo "================================"
    echo "Results: $TESTS_PASSED passed, $TESTS_FAILED failed"
    if [[ $TESTS_FAILED -gt 0 ]]; then
        exit 1
    fi
}

# Trap to print summary on exit when running standalone
if [[ "${BASH_SOURCE[0]}" != "${BASH_SOURCE[1]:-}" ]]; then
    # Being sourced by a test file, set up exit trap
    trap print_test_summary EXIT
fi
