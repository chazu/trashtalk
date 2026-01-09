#!/usr/bin/env bash
# ==============================================================================
# Trashtalk Fallback Tracking Dashboard
# ==============================================================================
#
# Analyzes all .trash classes through Procyon to show:
# - Native compilation rates by class
# - Top fallback reasons
# - Optimization targets
#
# Usage:
#   ./fallback_dashboard.bash           # Full analysis
#   ./fallback_dashboard.bash --summary # Summary only
#   ./fallback_dashboard.bash --json    # JSON output
#
# ==============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TRASHTALK_DIR="${TRASHTALK_DIR:-$HOME/.trashtalk}"
PROCYON="${PROCYON:-$HOME/dev/go/procyon/procyon}"
JQ_COMPILER="$TRASHTALK_DIR/lib/jq-compiler/driver.bash"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Output mode
OUTPUT_MODE="full"  # full, summary, json

# Temp files
ANALYSIS_LOG=$(mktemp)
RESULTS_JSON=$(mktemp)
trap 'rm -f "$ANALYSIS_LOG" "$RESULTS_JSON"' EXIT

# ------------------------------------------------------------------------------
# Parse arguments
# ------------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --summary) OUTPUT_MODE="summary"; shift ;;
        --json) OUTPUT_MODE="json"; shift ;;
        --help|-h)
            echo "Usage: $0 [--summary|--json|--help]"
            echo ""
            echo "Options:"
            echo "  --summary  Show only summary statistics"
            echo "  --json     Output results as JSON"
            echo "  --help     Show this help message"
            exit 0
            ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# ------------------------------------------------------------------------------
# Check dependencies
# ------------------------------------------------------------------------------
if [[ ! -x "$PROCYON" ]]; then
    echo -e "${RED}Error:${NC} Procyon not found at $PROCYON" >&2
    echo "Set PROCYON environment variable to the procyon binary path" >&2
    exit 1
fi

if [[ ! -f "$JQ_COMPILER" ]]; then
    echo -e "${RED}Error:${NC} jq-compiler not found at $JQ_COMPILER" >&2
    exit 1
fi

# ------------------------------------------------------------------------------
# Analyze a single class
# ------------------------------------------------------------------------------
analyze_class() {
    local trash_file="$1"
    local class_name
    class_name=$(basename "$trash_file" .trash)

    # Handle namespaced classes (in subdirectories)
    local parent_dir
    parent_dir=$(dirname "$trash_file")
    parent_dir=$(basename "$parent_dir")
    if [[ "$parent_dir" != "trash" && "$parent_dir" != "traits" && "$parent_dir" != "user" ]]; then
        class_name="${parent_dir}::${class_name}"
    fi

    # Parse to AST
    local ast
    ast=$("$JQ_COMPILER" parse "$trash_file" 2>/dev/null) || return 1

    # Run through Procyon and capture stderr
    local procyon_output
    procyon_output=$(echo "$ast" | "$PROCYON" 2>&1 >/dev/null) || true

    # Parse the output
    local compiled=0
    local skipped=0
    local reasons=()

    while IFS= read -r line; do
        if [[ "$line" =~ ^[[:space:]]*(✓|✔)[[:space:]]+([^[:space:]]+) ]]; then
            ((compiled++))
        elif [[ "$line" =~ ^[[:space:]]*(⚠|!)[[:space:]]+([^[:space:]]+)[[:space:]]+-[[:space:]]+skipped:[[:space:]]*(.+)$ ]]; then
            ((skipped++))
            reasons+=("${BASH_REMATCH[3]}")
        fi
    done <<< "$procyon_output"

    local total=$((compiled + skipped))
    local rate=0
    if [[ $total -gt 0 ]]; then
        rate=$((compiled * 100 / total))
    fi

    # Output as JSON line
    local reasons_json
    reasons_json=$(printf '%s\n' "${reasons[@]}" 2>/dev/null | jq -R . | jq -s . || echo "[]")

    jq -n \
        --arg class "$class_name" \
        --argjson compiled "$compiled" \
        --argjson skipped "$skipped" \
        --argjson total "$total" \
        --argjson rate "$rate" \
        --argjson reasons "$reasons_json" \
        '{class: $class, compiled: $compiled, skipped: $skipped, total: $total, rate: $rate, reasons: $reasons}'
}

# ------------------------------------------------------------------------------
# Collect all results
# ------------------------------------------------------------------------------
echo -e "${BLUE}Analyzing Trashtalk classes...${NC}" >&2

results=()

# Find all .trash files
while IFS= read -r -d '' trash_file; do
    result=$(analyze_class "$trash_file" 2>/dev/null) || continue
    if [[ -n "$result" ]]; then
        results+=("$result")
    fi
done < <(find "$TRASHTALK_DIR/trash" -name "*.trash" -type f -print0 2>/dev/null)

# Combine into JSON array
printf '%s\n' "${results[@]}" | jq -s '.' > "$RESULTS_JSON"

# ------------------------------------------------------------------------------
# Calculate aggregates
# ------------------------------------------------------------------------------
total_classes=$(jq 'length' "$RESULTS_JSON")
total_methods=$(jq '[.[].total] | add // 0' "$RESULTS_JSON")
total_compiled=$(jq '[.[].compiled] | add // 0' "$RESULTS_JSON")
total_skipped=$(jq '[.[].skipped] | add // 0' "$RESULTS_JSON")

if [[ $total_methods -gt 0 ]]; then
    overall_rate=$((total_compiled * 100 / total_methods))
else
    overall_rate=0
fi

# Get top fallback reasons
top_reasons=$(jq -r '
    [.[].reasons[]] |
    group_by(.) |
    map({reason: .[0], count: length}) |
    sort_by(-.count) |
    .[0:10]
' "$RESULTS_JSON")

# Get classes with lowest compilation rate (optimization targets)
optimization_targets=$(jq '
    [.[] | select(.total > 0)] |
    sort_by(.rate) |
    .[0:10] |
    map({class: .class, rate: .rate, skipped: .skipped, total: .total})
' "$RESULTS_JSON")

# Get classes with highest compilation rate (success stories)
success_stories=$(jq '
    [.[] | select(.total > 0)] |
    sort_by(-.rate) |
    .[0:5] |
    map({class: .class, rate: .rate, compiled: .compiled, total: .total})
' "$RESULTS_JSON")

# ------------------------------------------------------------------------------
# Output
# ------------------------------------------------------------------------------
if [[ "$OUTPUT_MODE" == "json" ]]; then
    jq -n \
        --argjson classes "$(cat "$RESULTS_JSON")" \
        --argjson total_classes "$total_classes" \
        --argjson total_methods "$total_methods" \
        --argjson total_compiled "$total_compiled" \
        --argjson total_skipped "$total_skipped" \
        --argjson overall_rate "$overall_rate" \
        --argjson top_reasons "$top_reasons" \
        --argjson optimization_targets "$optimization_targets" \
        --argjson success_stories "$success_stories" \
        '{
            summary: {
                total_classes: $total_classes,
                total_methods: $total_methods,
                compiled: $total_compiled,
                skipped: $total_skipped,
                rate_percent: $overall_rate
            },
            top_fallback_reasons: $top_reasons,
            optimization_targets: $optimization_targets,
            success_stories: $success_stories,
            classes: $classes
        }'
    exit 0
fi

# Text output
echo ""
echo -e "${BOLD}╔══════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}║           TRASHTALK NATIVE COMPILATION DASHBOARD                 ║${NC}"
echo -e "${BOLD}╚══════════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Summary section
echo -e "${CYAN}── Summary ──────────────────────────────────────────────────────────${NC}"
printf "  Classes analyzed:     %d\n" "$total_classes"
printf "  Total methods:        %d\n" "$total_methods"
printf "  Natively compiled:    ${GREEN}%d${NC} (%.1f%%)\n" "$total_compiled" "$overall_rate"
printf "  Bash fallback:        ${YELLOW}%d${NC} (%.1f%%)\n" "$total_skipped" "$((100 - overall_rate))"
echo ""

# Progress bar
bar_width=50
filled=$((overall_rate * bar_width / 100))
empty=$((bar_width - filled))
echo -n "  ["
printf "${GREEN}%0.s█${NC}" $(seq 1 $filled 2>/dev/null) || true
printf "${RED}%0.s░${NC}" $(seq 1 $empty 2>/dev/null) || true
echo "] ${overall_rate}%"
echo ""

if [[ "$OUTPUT_MODE" == "summary" ]]; then
    exit 0
fi

# Top fallback reasons
echo -e "${CYAN}── Top Fallback Reasons ─────────────────────────────────────────────${NC}"
echo "$top_reasons" | jq -r '
    .[] |
    "  \(.count | tostring | if length < 3 then " " * (3 - length) + . else . end)  \(.reason)"
' | head -10
echo ""

# Optimization targets (lowest compilation rates)
echo -e "${CYAN}── Optimization Targets (Lowest Compilation Rates) ──────────────────${NC}"
printf "  %-35s %8s %8s %8s\n" "Class" "Rate" "Skipped" "Total"
printf "  %-35s %8s %8s %8s\n" "───────────────────────────────────" "────────" "────────" "────────"
echo "$optimization_targets" | jq -r '
    .[] |
    "  \(.class[:35] | . + " " * (35 - length))  \(.rate)%\(" " * (6 - (.rate | tostring | length)))  \(.skipped)\(" " * (8 - (.skipped | tostring | length)))  \(.total)"
' | head -10
echo ""

# Success stories (highest compilation rates with at least some methods)
echo -e "${CYAN}── Success Stories (Highest Compilation Rates) ──────────────────────${NC}"
printf "  %-35s %8s %10s %8s\n" "Class" "Rate" "Compiled" "Total"
printf "  %-35s %8s %10s %8s\n" "───────────────────────────────────" "────────" "──────────" "────────"
echo "$success_stories" | jq -r '
    .[] |
    "  \(.class[:35] | . + " " * (35 - length))  \(.rate)%\(" " * (6 - (.rate | tostring | length)))  \(.compiled)\(" " * (10 - (.compiled | tostring | length)))  \(.total)"
' | head -5
echo ""

# Per-class breakdown (if not too many)
if [[ $total_classes -le 30 ]]; then
    echo -e "${CYAN}── Per-Class Breakdown ──────────────────────────────────────────────${NC}"
    printf "  %-35s %8s %10s %8s %8s\n" "Class" "Rate" "Compiled" "Skipped" "Total"
    printf "  %-35s %8s %10s %8s %8s\n" "───────────────────────────────────" "────────" "──────────" "────────" "────────"
    jq -r '
        sort_by(-.rate) |
        .[] |
        select(.total > 0) |
        "\(.class[:35])\t\(.rate)%\t\(.compiled)\t\(.skipped)\t\(.total)"
    ' "$RESULTS_JSON" | while IFS=$'\t' read -r class rate compiled skipped total; do
        # Color based on rate
        rate_num=${rate%\%}
        if [[ $rate_num -ge 80 ]]; then
            color=$GREEN
        elif [[ $rate_num -ge 50 ]]; then
            color=$YELLOW
        else
            color=$RED
        fi
        printf "  %-35s ${color}%8s${NC} %10s %8s %8s\n" "$class" "$rate" "$compiled" "$skipped" "$total"
    done
    echo ""
fi

echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
echo ""
