#!/usr/bin/env bash
# ==============================================================================
# Compare jq-compiler and Procyon Compiler Outputs
# ==============================================================================
#
# This script compares the outputs of the jq-compiler and Procyon compiler
# at each stage of the compilation pipeline.
#
# Usage:
#   ./compare_compilers.bash <file.trash>           # Compare all stages
#   ./compare_compilers.bash tokenize <file.trash>  # Compare tokenization only
#   ./compare_compilers.bash parse <file.trash>     # Compare parsing only
#   ./compare_compilers.bash bash <file.trash>      # Compare bash output only
#   ./compare_compilers.bash --all <directory>      # Compare all .trash files in directory
#
# ==============================================================================

set -euo pipefail

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_ROOT="${SCRIPT_DIR}/.."

# Tool paths
JQ_COMPILER="${TRASHTALK_ROOT}/lib/jq-compiler/driver.bash"
PROCYON_COMPARE="${HOME}/dev/go/procyon/trash-compare"

# Colors for output
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    CYAN='\033[0;36m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    CYAN=''
    NC=''
fi

# Track statistics
PASS_COUNT=0
FAIL_COUNT=0
SKIP_COUNT=0

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

error() {
    echo -e "${RED}Error:${NC} $1" >&2
    exit 1
}

info() {
    echo -e "${BLUE}Info:${NC} $1" >&2
}

success() {
    echo -e "${GREEN}MATCH${NC}"
}

failure() {
    echo -e "${RED}DIFFER${NC}"
}

skip() {
    echo -e "${YELLOW}SKIP${NC}: $1"
}

# Select diff tool
diff_tool() {
    if command -v colordiff &>/dev/null; then
        colordiff -u "$@"
    else
        diff -u "$@"
    fi
}

usage() {
    cat << 'EOF'
Compare jq-compiler and Procyon Compiler Outputs

Usage:
  compare_compilers.bash <file.trash>           Compare all stages
  compare_compilers.bash tokenize <file.trash>  Compare tokenization only
  compare_compilers.bash parse <file.trash>     Compare parsing only
  compare_compilers.bash bash <file.trash>      Compare bash output only
  compare_compilers.bash --all <directory>      Compare all .trash files in directory

Options:
  -v, --verbose    Show full diff output even on match
  -q, --quiet      Only show summary (pass/fail counts)
  -h, --help       Show this help message

Examples:
  ./compare_compilers.bash tokenize Counter.trash
  ./compare_compilers.bash parse Process.trash
  ./compare_compilers.bash --all ./trash/
EOF
}

# Check prerequisites
check_prerequisites() {
    if [[ ! -x "$JQ_COMPILER" ]]; then
        error "jq-compiler not found at: $JQ_COMPILER"
    fi

    if [[ ! -x "$PROCYON_COMPARE" ]]; then
        error "Procyon trash-compare not found at: $PROCYON_COMPARE\nRun: cd ~/dev/go/procyon && go build -o trash-compare ./cmd/trash-compare"
    fi
}

# ------------------------------------------------------------------------------
# Comparison Functions
# ------------------------------------------------------------------------------

# Compare tokenization output
compare_tokenize() {
    local file="$1"
    local verbose="${2:-false}"

    echo -ne "${CYAN}Tokenize${NC} $(basename "$file"): "

    local jq_output procyon_output
    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    # Get jq-compiler output
    if ! jq_output=$("$JQ_COMPILER" tokenize "$file" 2>/dev/null); then
        skip "jq-compiler tokenize failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Get Procyon output
    if ! procyon_output=$("$PROCYON_COMPARE" tokenize "$file" 2>/dev/null); then
        skip "Procyon tokenize failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Normalize JSON for comparison (sort keys, consistent formatting)
    local jq_normalized procyon_normalized
    jq_normalized=$(echo "$jq_output" | jq -S '.' 2>/dev/null || echo "$jq_output")
    procyon_normalized=$(echo "$procyon_output" | jq -S '.' 2>/dev/null || echo "$procyon_output")

    if [[ "$jq_normalized" == "$procyon_normalized" ]]; then
        success
        ((PASS_COUNT++))
        return 0
    else
        failure
        ((FAIL_COUNT++))

        if [[ "$verbose" == "true" ]] || [[ "${SHOW_DIFF:-true}" == "true" ]]; then
            echo "--- jq-compiler tokens"
            echo "+++ Procyon tokens"
            echo "$jq_normalized" > "$tmpdir/jq.json"
            echo "$procyon_normalized" > "$tmpdir/procyon.json"
            diff_tool "$tmpdir/jq.json" "$tmpdir/procyon.json" || true
        fi
        return 1
    fi
}

# Compare parsing output
compare_parse() {
    local file="$1"
    local verbose="${2:-false}"

    echo -ne "${CYAN}Parse${NC} $(basename "$file"): "

    local jq_output procyon_output
    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    # Get jq-compiler output
    if ! jq_output=$("$JQ_COMPILER" parse "$file" 2>/dev/null); then
        skip "jq-compiler parse failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Get Procyon output
    if ! procyon_output=$("$PROCYON_COMPARE" parse "$file" 2>/dev/null); then
        skip "Procyon parse failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Normalize JSON for comparison
    # Note: Some fields may differ in format, so we normalize more aggressively
    local jq_normalized procyon_normalized
    jq_normalized=$(echo "$jq_output" | jq -S 'del(.warnings) | del(.sourceHash) | del(.sourceCode)' 2>/dev/null || echo "$jq_output")
    procyon_normalized=$(echo "$procyon_output" | jq -S 'del(.Warnings) | del(.warnings)' 2>/dev/null || echo "$procyon_output")

    if [[ "$jq_normalized" == "$procyon_normalized" ]]; then
        success
        ((PASS_COUNT++))
        return 0
    else
        failure
        ((FAIL_COUNT++))

        if [[ "$verbose" == "true" ]] || [[ "${SHOW_DIFF:-true}" == "true" ]]; then
            echo "--- jq-compiler AST"
            echo "+++ Procyon AST"
            echo "$jq_normalized" > "$tmpdir/jq.json"
            echo "$procyon_normalized" > "$tmpdir/procyon.json"
            diff_tool "$tmpdir/jq.json" "$tmpdir/procyon.json" || true
        fi
        return 1
    fi
}

# Compare bash output
compare_bash() {
    local file="$1"
    local verbose="${2:-false}"

    echo -ne "${CYAN}Bash${NC} $(basename "$file"): "

    local jq_output procyon_output
    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" EXIT

    # Get jq-compiler output
    if ! jq_output=$("$JQ_COMPILER" compile "$file" 2>/dev/null); then
        skip "jq-compiler compile failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Get Procyon output
    if ! procyon_output=$("$PROCYON_COMPARE" bash "$file" 2>/dev/null); then
        skip "Procyon bash generation failed"
        ((SKIP_COUNT++))
        return 1
    fi

    # Normalize bash output (remove timestamps, comments that may differ)
    local jq_normalized procyon_normalized
    jq_normalized=$(echo "$jq_output" | grep -v '^# Generated:' | grep -v '^# Source:')
    procyon_normalized=$(echo "$procyon_output" | grep -v '^# Generated:' | grep -v '^# Source:')

    if [[ "$jq_normalized" == "$procyon_normalized" ]]; then
        success
        ((PASS_COUNT++))
        return 0
    else
        failure
        ((FAIL_COUNT++))

        if [[ "$verbose" == "true" ]] || [[ "${SHOW_DIFF:-true}" == "true" ]]; then
            echo "--- jq-compiler bash"
            echo "+++ Procyon bash"
            echo "$jq_normalized" > "$tmpdir/jq.bash"
            echo "$procyon_normalized" > "$tmpdir/procyon.bash"
            diff_tool "$tmpdir/jq.bash" "$tmpdir/procyon.bash" || true
        fi
        return 1
    fi
}

# Compare all stages for a single file
compare_all() {
    local file="$1"
    local verbose="${2:-false}"

    echo -e "\n${BLUE}=== Comparing: $(basename "$file") ===${NC}"

    compare_tokenize "$file" "$verbose" || true
    compare_parse "$file" "$verbose" || true
    compare_bash "$file" "$verbose" || true
}

# Compare all files in a directory
compare_directory() {
    local dir="$1"
    local verbose="${2:-false}"

    if [[ ! -d "$dir" ]]; then
        error "Directory not found: $dir"
    fi

    local files
    files=$(find "$dir" -name "*.trash" -type f | sort)

    if [[ -z "$files" ]]; then
        error "No .trash files found in: $dir"
    fi

    echo -e "${BLUE}Comparing all .trash files in: $dir${NC}\n"

    while IFS= read -r file; do
        compare_all "$file" "$verbose"
    done <<< "$files"

    # Print summary
    echo -e "\n${BLUE}=== Summary ===${NC}"
    echo -e "  ${GREEN}Passed:${NC} $PASS_COUNT"
    echo -e "  ${RED}Failed:${NC} $FAIL_COUNT"
    echo -e "  ${YELLOW}Skipped:${NC} $SKIP_COUNT"

    if [[ $FAIL_COUNT -gt 0 ]]; then
        return 1
    fi
    return 0
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    local verbose=false
    local quiet=false

    # Parse global options
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -v|--verbose)
                verbose=true
                shift
                ;;
            -q|--quiet)
                quiet=true
                SHOW_DIFF=false
                shift
                ;;
            -h|--help|help)
                usage
                exit 0
                ;;
            *)
                break
                ;;
        esac
    done

    if [[ $# -lt 1 ]]; then
        usage
        exit 1
    fi

    check_prerequisites

    local command="$1"
    shift

    case "$command" in
        tokenize)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            compare_tokenize "$1" "$verbose"
            ;;

        parse)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            compare_parse "$1" "$verbose"
            ;;

        bash)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            compare_bash "$1" "$verbose"
            ;;

        --all)
            if [[ $# -lt 1 ]]; then
                error "Missing directory"
            fi
            compare_directory "$1" "$verbose"
            ;;

        *.trash)
            # If first arg is a .trash file, compare all stages
            compare_all "$command" "$verbose"

            # Print mini summary
            echo -e "\n  ${GREEN}Passed:${NC} $PASS_COUNT | ${RED}Failed:${NC} $FAIL_COUNT | ${YELLOW}Skipped:${NC} $SKIP_COUNT"
            ;;

        *)
            # Try to treat as a file path
            if [[ -f "$command" ]]; then
                compare_all "$command" "$verbose"
                echo -e "\n  ${GREEN}Passed:${NC} $PASS_COUNT | ${RED}Failed:${NC} $FAIL_COUNT | ${YELLOW}Skipped:${NC} $SKIP_COUNT"
            else
                error "Unknown command or file not found: $command"
            fi
            ;;
    esac
}

main "$@"
