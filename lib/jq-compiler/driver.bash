#!/bin/bash
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
  -v, --verbose         Show intermediate steps
  -h, --help            Show this help message

Examples:
  ./driver.bash tokenize Counter.trash
  ./driver.bash parse Counter.trash | jq .
  ./driver.bash compile Counter.trash -o Counter.bash
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
        echo -e "${RED}Parse errors:${NC}" >&2
        echo "$ast" | jq -r '.errors[] | "  \(.line):\(.col): \(.message)"' >&2
        echo "$ast" | jq '.partial // {}'
        exit 1
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

    # Generate code
    local output
    output=$(echo "$ast" | jq -r -f "$CODEGEN")

    if [[ $? -ne 0 ]]; then
        error "Code generation failed"
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
                    *)
                        error "Unknown option: $1"
                        ;;
                esac
            done

            cmd_compile "$source_file" "$output_file"
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
