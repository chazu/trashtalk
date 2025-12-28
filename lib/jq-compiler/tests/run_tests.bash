#!/usr/bin/env bash
# ==============================================================================
# Trashtalk jq-compiler Test Runner
# ==============================================================================
# Runs all compiler tests and reports results.
#
# Usage:
#   ./run_tests.bash           # Run all tests
#   ./run_tests.bash tokenizer # Run only tokenizer tests
#   ./run_tests.bash parser    # Run only parser tests
#   ./run_tests.bash codegen   # Run only codegen tests
#   ./run_tests.bash integration # Run only integration tests
# ==============================================================================

set -uo pipefail
# Note: We don't use -e because we want tests to continue after failures

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER_DIR="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# ------------------------------------------------------------------------------
# Test Utilities
# ------------------------------------------------------------------------------

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
    else
        echo -e "  ${RED}✗${NC} $name"
        echo -e "    ${YELLOW}Expected:${NC} $expected"
        echo -e "    ${YELLOW}Actual:${NC}   $actual"
        ((TESTS_FAILED++)) || true
    fi
    return 0  # Always return success to continue test suite
}

# Run a test checking that actual contains expected
# Usage: run_test_contains "test name" expected actual
run_test_contains() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    ((TESTS_RUN++)) || true

    if [[ "$actual" == *"$expected"* ]]; then
        echo -e "  ${GREEN}✓${NC} $name"
        ((TESTS_PASSED++)) || true
    else
        echo -e "  ${RED}✗${NC} $name"
        echo -e "    ${YELLOW}Expected to contain:${NC} $expected"
        echo -e "    ${YELLOW}Actual:${NC} $actual"
        ((TESTS_FAILED++)) || true
    fi
    return 0  # Always return success to continue test suite
}

# Run a test checking JSON equality
# Usage: run_test_json "test name" expected_json actual_json
run_test_json() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    ((TESTS_RUN++)) || true

    # Normalize JSON for comparison
    local exp_norm=$(echo "$expected" | jq -Sc '.' 2>/dev/null || echo "$expected")
    local act_norm=$(echo "$actual" | jq -Sc '.' 2>/dev/null || echo "$actual")

    if [[ "$exp_norm" == "$act_norm" ]]; then
        echo -e "  ${GREEN}✓${NC} $name"
        ((TESTS_PASSED++)) || true
    else
        echo -e "  ${RED}✗${NC} $name"
        echo -e "    ${YELLOW}Expected:${NC} $exp_norm"
        echo -e "    ${YELLOW}Actual:${NC}   $act_norm"
        ((TESTS_FAILED++)) || true
    fi
    return 0  # Always return success to continue test suite
}

# Print section header
section() {
    echo -e "\n${BLUE}═══ $1 ═══${NC}"
}

# ------------------------------------------------------------------------------
# Test Suites
# ------------------------------------------------------------------------------

run_tokenizer_tests() {
    section "Tokenizer Tests"
    source "$SCRIPT_DIR/test_tokenizer.bash"
}

run_parser_tests() {
    section "Parser Tests"
    source "$SCRIPT_DIR/test_parser.bash"
}

run_codegen_tests() {
    section "Codegen Tests"
    source "$SCRIPT_DIR/test_codegen.bash"
}

run_integration_tests() {
    section "Integration Tests"
    source "$SCRIPT_DIR/test_integration.bash"
}

run_namespace_tests() {
    section "Namespace Tests"
    source "$SCRIPT_DIR/test_namespaces.bash"
}

run_triplestring_tests() {
    section "Triple-Quoted String Tests"
    source "$SCRIPT_DIR/test_triplestrings.bash"
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    echo -e "${BLUE}╔════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║   Trashtalk jq-compiler Test Suite         ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════╝${NC}"

    local filter="${1:-all}"

    case "$filter" in
        tokenizer)
            run_tokenizer_tests
            ;;
        parser)
            run_parser_tests
            ;;
        codegen)
            run_codegen_tests
            ;;
        integration)
            run_integration_tests
            ;;
        namespaces)
            run_namespace_tests
            ;;
        triplestrings)
            run_triplestring_tests
            ;;
        all|*)
            run_tokenizer_tests
            run_parser_tests
            run_namespace_tests
            run_triplestring_tests
            run_codegen_tests
            run_integration_tests
            ;;
    esac

    # Summary
    echo -e "\n${BLUE}═══ Summary ═══${NC}"
    echo -e "Tests run:    $TESTS_RUN"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"

    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "\n${GREEN}All tests passed!${NC}"
        exit 0
    else
        echo -e "\n${RED}Some tests failed.${NC}"
        exit 1
    fi
}

main "$@"
