#!/usr/bin/env bash
# ==============================================================================
# Parallel test runner for Trashtalk
# ==============================================================================
# Runs test files in parallel with per-test timeouts.
# Each test gets its own disposable checkout, database, cache, and session.
#
# Usage:
#   ./lib/run-tests.sh [tests_dir] [--serial] [--timeout SECS]
# ==============================================================================

set -euo pipefail
export LC_ALL=C
RUNNER_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

TESTS_DIR="${1:-tests}"
PARALLEL=true
# Default per-test timeout; override with TRASH_TEST_TIMEOUT env or --timeout flag
# (flag wins) so slow CI machines don't need to edit this script.
TIMEOUT="${TRASH_TEST_TIMEOUT:-120}"

# Parse flags
shift || true
while [[ $# -gt 0 ]]; do
    case "$1" in
        --serial) PARALLEL=false ;;
        --timeout) TIMEOUT="$2"; shift ;;
        *) echo "Unknown flag: $1" >&2; exit 1 ;;
    esac
    shift
done

# Platform detection for parallel jobs
if [[ "$(uname)" == "Darwin" ]]; then
    NPROCS=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)
else
    NPROCS=$(nproc 2>/dev/null || echo 4)
fi
NPROCS=${TRASH_TEST_JOBS:-$NPROCS}
[[ "$NPROCS" =~ ^[1-9][0-9]*$ ]] || { echo 'TRASH_TEST_JOBS must be positive' >&2; exit 2; }

# Collect test files
test_files=()
for test in "$TESTS_DIR"/test_*.bash; do
    [[ -f "$test" ]] && test_files+=("$test")
done

if [[ ${#test_files[@]} -eq 0 ]]; then
    echo "No test files found in $TESTS_DIR"
    exit 1
fi

echo "Running ${#test_files[@]} test files..."

# Results directory
RESULTS_DIR=$(mktemp -d)
trap 'rm -rf "$RESULTS_DIR"' EXIT

# Run a single test file with timeout and isolated temp dir
run_one_test() {
    local test_file="$1"
    local test_name
    test_name=$(basename "$test_file")
    local result_file="$RESULTS_DIR/$test_name"

    # Run with timeout, capture output
    local output exit_code=0
    output=$(timeout "$TIMEOUT" bash "$RUNNER_DIR/test-isolated.bash" "$test_file" 2>&1) || exit_code=$?

    if [[ $exit_code -eq 124 ]]; then
        echo "TIMEOUT" > "$result_file"
        echo "=== $test_name === TIMEOUT (${TIMEOUT}s)"
        # Show what the test printed before it was killed, to help locate the hang.
        echo "$output" | tail -20
    elif [[ $exit_code -eq 0 ]]; then
        echo "PASS" > "$result_file"
        echo "=== $test_name === PASS"
    else
        echo "FAIL" > "$result_file"
        echo "=== $test_name === FAIL"
        # Show output for failed tests
        echo "$output" | tail -20
    fi
}

export -f run_one_test
export RESULTS_DIR TIMEOUT RUNNER_DIR

if $PARALLEL; then
    echo "(parallel: $NPROCS jobs, timeout: ${TIMEOUT}s per test)"
    echo ""
    printf '%s\n' "${test_files[@]}" | xargs -P"$NPROCS" -I{} bash -c 'run_one_test "$@"' _ {} || true
else
    echo "(serial, timeout: ${TIMEOUT}s per test)"
    echo ""
    for test in "${test_files[@]}"; do
        run_one_test "$test"
    done
fi

# Tally results
echo ""
echo "================================"
passed=0; failed=0; timedout=0
failed_names=()
for test_file in "${test_files[@]}"; do
    result="$RESULTS_DIR/$(basename "$test_file")"
    if [[ -f "$result" ]]; then status=$(cat "$result"); else status=MISSING; fi
    case "$status" in
        PASS) ((passed++)) || true ;;
        FAIL) ((failed++)) || true; failed_names+=("$(basename "$result")") ;;
        TIMEOUT) ((timedout++)) || true; failed_names+=("$(basename "$result") (timeout)") ;;
        *) ((failed++)) || true; failed_names+=("$(basename "$result") (missing or invalid result)") ;;
    esac
done

echo "Passed: $passed, Failed: $failed, Timed out: $timedout"

if [[ ${#failed_names[@]} -gt 0 ]]; then
    echo ""
    echo "Failed tests:"
    for name in "${failed_names[@]}"; do
        echo "  - $name"
    done
    exit 1
fi
