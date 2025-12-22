#!/usr/bin/env bash
# ==============================================================================
# Trashtalk jq Compiler - Driver Script
# ==============================================================================
#
# Main entry point for the jq-based Trashtalk compiler.
# Provides commands for tokenizing, parsing, and compiling .trash files.
#
# Usage:
#   ./driver.bash tokenize <file.trash>     # Output JSON tokens
#   ./driver.bash parse <file.trash>        # Output JSON AST
#   ./driver.bash compile <file.trash>      # Output compiled bash
#   ./driver.bash ast <file.trash>          # Pretty-print AST
#
# Pipeline:
#   .trash source -> tokenizer.bash -> JSON tokens -> parser.jq -> JSON AST -> codegen.jq -> bash
#
# ==============================================================================

set -euo pipefail

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Tool paths
TOKENIZER="$SCRIPT_DIR/tokenizer.bash"
PARSER="$SCRIPT_DIR/parser.jq"
CODEGEN="$SCRIPT_DIR/codegen.jq"

# Colors for output (if terminal supports it)
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

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
    echo -e "${GREEN}Success:${NC} $1" >&2
}

# Show source context around an error
# Args: $1=source_file, $2=line_number, $3=column, $4=message, $5=type (optional)
show_error_context() {
    local file="$1"
    local line_num="$2"
    local col="$3"
    local message="$4"
    local err_type="${5:-}"

    # Print the error location and message
    if [[ -n "$err_type" ]]; then
        echo -e "  ${line_num}:${col}: ${message} ${YELLOW}[${err_type}]${NC}" >&2
    else
        echo -e "  ${line_num}:${col}: ${message}" >&2
    fi

    # Get the source line (if file exists and line is valid)
    if [[ -f "$file" && "$line_num" -gt 0 ]]; then
        local source_line
        source_line=$(sed -n "${line_num}p" "$file" 2>/dev/null || echo "")

        if [[ -n "$source_line" ]]; then
            # Print line number gutter and source line
            # Gutter format: "    %4s | " = 4 spaces + 4-char field + " | " = 11 chars
            printf "    %4s | %s\n" "$line_num" "$source_line" >&2

            # Print caret pointing to the column
            # Gutter width is 11 chars, then add column offset
            local gutter_width=11
            local padding=$((gutter_width + col))
            printf "%${padding}s${RED}^${NC}\n" "" >&2
        fi
    fi
}

# Show multiple errors/warnings with context
# Args: $1=source_file, $2=json_array of errors/warnings, $3=color (RED/YELLOW)
show_errors_with_context() {
    local file="$1"
    local errors_json="$2"
    local color="$3"

    # Parse each error and show context
    echo "$errors_json" | jq -r '.[] | "\(.token.line)\t\(.token.col)\t\(.message)\t\(.type)"' 2>/dev/null | \
    while IFS=$'\t' read -r line col message err_type; do
        show_error_context "$file" "$line" "$col" "$message" "$err_type"
    done
}

usage() {
    cat << 'EOF'
Trashtalk jq Compiler

Usage:
  driver.bash <command> <file.trash> [options]

Commands:
  tokenize <file>     Output JSON token array from source file
  parse <file>        Output JSON AST from source file
  ast <file>          Pretty-print the AST with syntax highlighting
  compile <file>      Compile to bash and output to stdout
  compile <file> -o <output>  Compile to bash and write to file

Options:
  -o, --output <file>   Write output to file instead of stdout
  -c, --check           Validate compiled output with bash -n
  -v, --verbose         Show intermediate steps
  -h, --help            Show this help message

Examples:
  ./driver.bash tokenize Counter.trash
  ./driver.bash parse Counter.trash | jq .
  ./driver.bash compile Counter.trash -o Counter.bash
  ./driver.bash compile Counter.trash --check
  ./driver.bash ast Process.trash

EOF
}

# ------------------------------------------------------------------------------
# Commands
# ------------------------------------------------------------------------------

# Tokenize a .trash file to JSON
cmd_tokenize() {
    local source_file="$1"

    if [[ ! -f "$source_file" ]]; then
        error "Source file not found: $source_file"
    fi

    "$TOKENIZER" "$source_file"
}

# Parse a .trash file to JSON AST
cmd_parse() {
    local source_file="$1"

    if [[ ! -f "$source_file" ]]; then
        error "Source file not found: $source_file"
    fi

    local tokens
    tokens=$("$TOKENIZER" "$source_file")

    if [[ $? -ne 0 ]]; then
        error "Tokenization failed"
    fi

    local ast
    ast=$(echo "$tokens" | jq -f "$PARSER")

    if [[ $? -ne 0 ]]; then
        error "Parsing failed"
    fi

    # Check for parse errors in the result
    if echo "$ast" | jq -e '.error == true' >/dev/null 2>&1; then
        echo -e "${RED}Parse errors in ${source_file}:${NC}" >&2
        local errors_json
        errors_json=$(echo "$ast" | jq '.errors // []')
        show_errors_with_context "$source_file" "$errors_json" "$RED"
        echo "$ast" | jq '.partial // {}'
        exit 1
    fi

    # Check for warnings (non-fatal errors)
    if echo "$ast" | jq -e '.warnings | length > 0' >/dev/null 2>&1; then
        echo -e "${YELLOW}Parse warnings in ${source_file}:${NC}" >&2
        local warnings_json
        warnings_json=$(echo "$ast" | jq '.warnings')
        show_errors_with_context "$source_file" "$warnings_json" "$YELLOW"
    fi

    echo "$ast"
}

# Pretty-print the AST
cmd_ast() {
    local source_file="$1"
    local ast
    ast=$(cmd_parse "$source_file")

    if [[ $? -ne 0 ]]; then
        exit 1
    fi

    # Pretty print with jq, highlighting key fields
    echo "$ast" | jq '.'
}

# Compile a .trash file to bash
cmd_compile() {
    local source_file="$1"
    local output_file="${2:-}"
    local check_syntax="${3:-false}"

    if [[ ! -f "$source_file" ]]; then
        error "Source file not found: $source_file"
    fi

    # Check if codegen exists
    if [[ ! -f "$CODEGEN" ]]; then
        error "Code generator not found: $CODEGEN (not yet implemented)"
    fi

    # Parse to AST
    local ast
    ast=$(cmd_parse "$source_file")

    if [[ $? -ne 0 ]]; then
        exit 1
    fi

    # Generate code (strip warnings field before codegen)
    local output
    output=$(echo "$ast" | jq 'del(.warnings)' | jq -r -f "$CODEGEN")

    if [[ $? -ne 0 ]]; then
        error "Code generation failed"
    fi

    # Optionally validate bash syntax
    if [[ "$check_syntax" == "true" ]]; then
        local syntax_errors
        syntax_errors=$(bash -n <<<"$output" 2>&1)
        if [[ $? -ne 0 ]]; then
            echo -e "${RED}Syntax errors in compiled output:${NC}" >&2
            echo "$syntax_errors" >&2
            exit 1
        fi
        info "Syntax check passed"
    fi

    # Output result
    if [[ -n "$output_file" ]]; then
        echo "$output" > "$output_file"
        success "Compiled: $source_file -> $output_file"
    else
        echo "$output"
    fi
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    if [[ $# -lt 1 ]]; then
        usage
        exit 1
    fi

    local command="$1"
    shift

    case "$command" in
        tokenize)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            cmd_tokenize "$1"
            ;;

        parse)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            cmd_parse "$1"
            ;;

        ast)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            cmd_ast "$1"
            ;;

        compile)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            local source_file="$1"
            local output_file=""
            local check_syntax="false"
            shift

            # Parse options
            while [[ $# -gt 0 ]]; do
                case "$1" in
                    -o|--output)
                        if [[ $# -lt 2 ]]; then
                            error "Missing output file after $1"
                        fi
                        output_file="$2"
                        shift 2
                        ;;
                    -c|--check)
                        check_syntax="true"
                        shift
                        ;;
                    *)
                        error "Unknown option: $1"
                        ;;
                esac
            done

            cmd_compile "$source_file" "$output_file" "$check_syntax"
            ;;

        -h|--help|help)
            usage
            ;;

        *)
            error "Unknown command: $command"
            ;;
    esac
}

main "$@"
