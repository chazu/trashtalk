#!/usr/bin/env bash
# ==============================================================================
# ✓/✗ tally for compiler feature tests that also load the runtime
# ==============================================================================
# Sets SCRIPT_DIR, COMPILER_DIR, PROJECT_DIR and TRASHDIR for the test that
# sources it. compile_helpers.bash is the PASS/FAIL counting variant.
# ==============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_DIR="$(dirname "$(dirname "$COMPILER_DIR")")"
TRASHDIR="$PROJECT_DIR/trash"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Test counter
TESTS_RUN=0
TESTS_PASSED=0

pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    echo "  Expected: $2"
    echo "  Got: $3"
    ((TESTS_RUN++))
}

# Print the tally and exit 1 unless every test passed.
print_results() {
    echo ""
    echo "=== Results ==="
    echo "Passed: $TESTS_PASSED / $TESTS_RUN"

    if [[ $TESTS_PASSED -eq $TESTS_RUN ]]; then
        exit 0
    else
        exit 1
    fi
}
