#!/usr/bin/env bash
# ==============================================================================
# Trashtalk Fallback Analysis Script
# ==============================================================================
#
# Analyzes why methods require Bash fallback and provides actionable suggestions.
# Complements fallback_dashboard.bash with detailed per-method analysis.
#
# Usage:
#   ./analyze_fallbacks.bash                    # Analyze all classes
#   ./analyze_fallbacks.bash Counter.trash      # Analyze specific class
#   ./analyze_fallbacks.bash --by-reason        # Group output by fallback reason
#   ./analyze_fallbacks.bash --fixable          # Show only fixable methods
#   ./analyze_fallbacks.bash --json             # JSON output
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
DIM='\033[2m'
NC='\033[0m'

# Options
OUTPUT_MODE="detailed"  # detailed, by-reason, fixable, json
TARGET_FILE=""

# Temp files
RESULTS_JSON=$(mktemp)
trap 'rm -f "$RESULTS_JSON"' EXIT

# ------------------------------------------------------------------------------
# Fallback reason categories and fix suggestions
# ------------------------------------------------------------------------------
declare -A REASON_CATEGORY=(
    ["raw method"]="intentional"
    ["raw method requires Bash"]="intentional"
    ["bashOnly pragma"]="intentional"
    ["uses bash runtime function"]="fixable"
    ["subshell expressions not supported"]="fixable"
    ["bash variable references"]="fixable"
    ["dynamic block invocation requires Bash"]="complex"
    ["procyonOnly pragma"]="native-only"
    ["procyonNative pragma"]="native-only"
)

declare -A REASON_FIX=(
    ["raw method"]="Convert to method: if possible. See docs/trashtalk-patterns.md"
    ["raw method requires Bash"]="Convert to method: if possible. See docs/trashtalk-patterns.md"
    ["bashOnly pragma"]="Intentionally Bash-only. No action needed."
    ["uses bash runtime function: _ivar"]="Use instance variable name directly in method body"
    ["uses bash runtime function: _ivar_set"]="Use := assignment in method body"
    ["uses bash runtime function: _throw"]="Consider restructuring error handling"
    ["uses bash runtime function: _on_error"]="Consider restructuring error handling"
    ["uses bash runtime function: _ensure"]="Consider restructuring cleanup logic"
    ["subshell expressions not supported"]="Replace \$(...) with method calls or temp variables"
    ["bash variable references (\$var) not supported"]="Use Trashtalk variable syntax instead"
    ["dynamic block invocation requires Bash"]="Use literal block [:x | ...] instead of block variable"
    ["procyonOnly pragma"]="Native-only method. Bash fallback expected."
    ["procyonNative pragma"]="Native-only method. Bash fallback expected."
)

get_category() {
    local reason="$1"
    for pattern in "${!REASON_CATEGORY[@]}"; do
        if [[ "$reason" == *"$pattern"* ]]; then
            echo "${REASON_CATEGORY[$pattern]}"
            return
        fi
    done
    echo "unknown"
}

get_fix_suggestion() {
    local reason="$1"
    for pattern in "${!REASON_FIX[@]}"; do
        if [[ "$reason" == *"$pattern"* ]]; then
            echo "${REASON_FIX[$pattern]}"
            return
        fi
    done
    echo "Review method for Bash-specific constructs"
}

# ------------------------------------------------------------------------------
# Parse arguments
# ------------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --by-reason) OUTPUT_MODE="by-reason"; shift ;;
        --fixable) OUTPUT_MODE="fixable"; shift ;;
        --json) OUTPUT_MODE="json"; shift ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS] [file.trash]"
            echo ""
            echo "Options:"
            echo "  --by-reason  Group output by fallback reason"
            echo "  --fixable    Show only methods that can likely be fixed"
            echo "  --json       Output results as JSON"
            echo "  --help       Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0                      # Analyze all classes"
            echo "  $0 Counter.trash        # Analyze specific class"
            echo "  $0 --by-reason          # Group by reason"
            echo "  $0 --fixable            # Show fixable methods only"
            exit 0
            ;;
        *.trash)
            if [[ -f "$1" ]]; then
                TARGET_FILE="$1"
            elif [[ -f "$TRASHTALK_DIR/trash/$1" ]]; then
                TARGET_FILE="$TRASHTALK_DIR/trash/$1"
            else
                echo "Error: File not found: $1" >&2
                exit 1
            fi
            shift
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
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

# ------------------------------------------------------------------------------
# Analyze a single class
# ------------------------------------------------------------------------------
analyze_class() {
    local trash_file="$1"
    local class_name
    class_name=$(basename "$trash_file" .trash)

    # Handle namespaced classes
    local parent_dir
    parent_dir=$(dirname "$trash_file")
    parent_dir=$(basename "$parent_dir")
    if [[ "$parent_dir" != "trash" && "$parent_dir" != "traits" && "$parent_dir" != "user" ]]; then
        class_name="${parent_dir}::${class_name}"
    fi

    # Parse to AST
    local ast
    ast=$("$JQ_COMPILER" parse "$trash_file" 2>/dev/null) || return 1

    # Run through Procyon
    local procyon_output
    procyon_output=$(echo "$ast" | "$PROCYON" 2>&1 >/dev/null) || true

    # Parse output and build JSON
    local methods=()
    while IFS= read -r line; do
        if [[ "$line" =~ ^[[:space:]]*(✓|✔)[[:space:]]+([^[:space:]]+) ]]; then
            local selector="${BASH_REMATCH[2]}"
            methods+=("$(jq -n --arg s "$selector" --arg status "compiled" '{selector: $s, status: $status}')")
        elif [[ "$line" =~ ^[[:space:]]*(⚠|!)[[:space:]]+([^[:space:]]+)[[:space:]]+-[[:space:]]+skipped:[[:space:]]*(.+)$ ]]; then
            local selector="${BASH_REMATCH[2]}"
            local reason="${BASH_REMATCH[3]}"
            local category
            category=$(get_category "$reason")
            local fix
            fix=$(get_fix_suggestion "$reason")
            methods+=("$(jq -n \
                --arg s "$selector" \
                --arg status "skipped" \
                --arg reason "$reason" \
                --arg category "$category" \
                --arg fix "$fix" \
                '{selector: $s, status: $status, reason: $reason, category: $category, fix: $fix}')")
        fi
    done <<< "$procyon_output"

    # Output class result
    if [[ ${#methods[@]} -gt 0 ]]; then
        printf '%s\n' "${methods[@]}" | jq -s --arg class "$class_name" '{class: $class, methods: .}'
    fi
}

# ------------------------------------------------------------------------------
# Collect results
# ------------------------------------------------------------------------------
echo -e "${BLUE}Analyzing fallback reasons...${NC}" >&2

results=()

if [[ -n "$TARGET_FILE" ]]; then
    # Single file
    result=$(analyze_class "$TARGET_FILE" 2>/dev/null) || true
    if [[ -n "$result" ]]; then
        results+=("$result")
    fi
else
    # All files
    while IFS= read -r -d '' trash_file; do
        result=$(analyze_class "$trash_file" 2>/dev/null) || continue
        if [[ -n "$result" ]]; then
            results+=("$result")
        fi
    done < <(find "$TRASHTALK_DIR/trash" -name "*.trash" -type f -print0 2>/dev/null)
fi

# Combine results
printf '%s\n' "${results[@]}" | jq -s '.' > "$RESULTS_JSON"

# ------------------------------------------------------------------------------
# Output: JSON mode
# ------------------------------------------------------------------------------
if [[ "$OUTPUT_MODE" == "json" ]]; then
    # Enrich with aggregations
    jq '
        . as $classes |
        {
            classes: $classes,
            by_reason: (
                [.[] | .methods[] | select(.status == "skipped")] |
                group_by(.reason) |
                map({
                    reason: .[0].reason,
                    category: .[0].category,
                    fix: .[0].fix,
                    count: length,
                    methods: [.[] | {class: (input_line_number | tostring), selector: .selector}]
                }) |
                sort_by(-.count)
            ),
            summary: {
                total_methods: ([.[] | .methods | length] | add // 0),
                compiled: ([.[] | .methods[] | select(.status == "compiled")] | length),
                skipped: ([.[] | .methods[] | select(.status == "skipped")] | length),
                fixable: ([.[] | .methods[] | select(.category == "fixable")] | length),
                intentional: ([.[] | .methods[] | select(.category == "intentional")] | length)
            }
        }
    ' "$RESULTS_JSON"
    exit 0
fi

# ------------------------------------------------------------------------------
# Output: By-reason mode
# ------------------------------------------------------------------------------
if [[ "$OUTPUT_MODE" == "by-reason" ]]; then
    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}         FALLBACK ANALYSIS BY REASON                               ${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""

    # Group by reason
    jq -r '
        [.[] | .class as $class | .methods[] | select(.status == "skipped") | . + {class: $class}] |
        group_by(.reason) |
        sort_by(-length) |
        .[] |
        "\(.length)\t\(.[0].reason)\t\(.[0].category)\t\(.[0].fix)\t\([.[] | "\(.class).\(.selector)"] | join(", "))"
    ' "$RESULTS_JSON" | while IFS=$'\t' read -r count reason category fix methods; do
        # Color based on category
        case "$category" in
            fixable) color=$YELLOW ;;
            intentional) color=$DIM ;;
            native-only) color=$CYAN ;;
            *) color=$RED ;;
        esac

        echo -e "${color}${BOLD}[$count] $reason${NC}"
        echo -e "    ${DIM}Category:${NC} $category"
        echo -e "    ${DIM}Fix:${NC} $fix"
        echo -e "    ${DIM}Methods:${NC}"
        echo "$methods" | tr ',' '\n' | while read -r method; do
            echo "      - ${method## }"
        done
        echo ""
    done
    exit 0
fi

# ------------------------------------------------------------------------------
# Output: Fixable mode
# ------------------------------------------------------------------------------
if [[ "$OUTPUT_MODE" == "fixable" ]]; then
    echo ""
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}         FIXABLE METHODS (can likely be converted to pure DSL)     ${NC}"
    echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
    echo ""

    fixable_count=$(jq '[.[] | .methods[] | select(.category == "fixable")] | length' "$RESULTS_JSON")
    echo -e "Found ${GREEN}$fixable_count${NC} methods that can likely be fixed."
    echo ""

    jq -r '
        .[] |
        .class as $class |
        .methods[] |
        select(.category == "fixable") |
        "\($class)\t\(.selector)\t\(.reason)\t\(.fix)"
    ' "$RESULTS_JSON" | while IFS=$'\t' read -r class selector reason fix; do
        echo -e "${YELLOW}$class.$selector${NC}"
        echo -e "    Reason: $reason"
        echo -e "    Fix: $fix"
        echo ""
    done

    echo -e "${DIM}See docs/trashtalk-patterns.md for detailed rewrite patterns.${NC}"
    exit 0
fi

# ------------------------------------------------------------------------------
# Output: Detailed mode (default)
# ------------------------------------------------------------------------------
echo ""
echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}         FALLBACK ANALYSIS - DETAILED REPORT                       ${NC}"
echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
echo ""

# Summary
total=$(jq '[.[] | .methods | length] | add // 0' "$RESULTS_JSON")
compiled=$(jq '[.[] | .methods[] | select(.status == "compiled")] | length' "$RESULTS_JSON")
skipped=$(jq '[.[] | .methods[] | select(.status == "skipped")] | length' "$RESULTS_JSON")
fixable=$(jq '[.[] | .methods[] | select(.category == "fixable")] | length' "$RESULTS_JSON")
intentional=$(jq '[.[] | .methods[] | select(.category == "intentional")] | length' "$RESULTS_JSON")

echo -e "${CYAN}── Summary ──────────────────────────────────────────────────────────${NC}"
printf "  Total methods:      %d\n" "$total"
printf "  Compiled:           ${GREEN}%d${NC}\n" "$compiled"
printf "  Skipped:            ${YELLOW}%d${NC}\n" "$skipped"
printf "    - Fixable:        ${YELLOW}%d${NC} (can likely be converted)\n" "$fixable"
printf "    - Intentional:    ${DIM}%d${NC} (raw/bashOnly by design)\n" "$intentional"
echo ""

# Per-class breakdown
echo -e "${CYAN}── Per-Class Analysis ───────────────────────────────────────────────${NC}"
echo ""

jq -r '
    .[] |
    select(.methods | any(.status == "skipped")) |
    .class as $class |
    .methods |
    (map(select(.status == "compiled")) | length) as $compiled |
    (map(select(.status == "skipped")) | length) as $skipped |
    "\($class)\t\($compiled)\t\($skipped)\t\(map(select(.status == "skipped")) | map("\(.selector):\(.reason)") | join("|"))"
' "$RESULTS_JSON" | sort | while IFS=$'\t' read -r class compiled skipped methods; do
    total=$((compiled + skipped))
    rate=$((compiled * 100 / total))

    # Color based on rate
    if [[ $rate -ge 80 ]]; then
        color=$GREEN
    elif [[ $rate -ge 50 ]]; then
        color=$YELLOW
    else
        color=$RED
    fi

    echo -e "${BOLD}$class${NC} ${color}($rate% native)${NC}"

    # Show skipped methods
    echo "$methods" | tr '|' '\n' | while read -r method_info; do
        selector="${method_info%%:*}"
        reason="${method_info#*:}"
        category=$(get_category "$reason")

        case "$category" in
            fixable) icon="⚠" ; mcolor=$YELLOW ;;
            intentional) icon="○" ; mcolor=$DIM ;;
            native-only) icon="◆" ; mcolor=$CYAN ;;
            *) icon="✗" ; mcolor=$RED ;;
        esac

        echo -e "  ${mcolor}${icon} $selector${NC}: $reason"
    done
    echo ""
done

echo -e "${BOLD}═══════════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Legend: ${YELLOW}⚠ fixable${NC}  ${DIM}○ intentional${NC}  ${CYAN}◆ native-only${NC}  ${RED}✗ unknown${NC}"
echo ""
echo -e "Run with ${BOLD}--fixable${NC} to see only methods that can be converted."
echo -e "Run with ${BOLD}--by-reason${NC} to group by fallback reason."
echo -e "See ${BOLD}docs/trashtalk-patterns.md${NC} for rewrite patterns."
echo ""
