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

# Preflight: the compiler shells out to jq and shasum throughout. Check up front
# so a missing tool produces one clear message instead of a cascade of failures.
for _tool in jq shasum; do
    if ! command -v "$_tool" >/dev/null 2>&1; then
        echo "Error: trashtalk compiler requires '$_tool' but it was not found in PATH." >&2
        exit 1
    fi
done
unset _tool

# AST cache directory - avoids re-tokenizing/parsing unchanged files
TRASHTALK_DIR="${TRASHTALK_DIR:-$HOME/.trashtalk}"
AST_CACHE_DIR="$TRASHTALK_DIR/trash/.compiled/.astcache"
mkdir -p "$AST_CACHE_DIR" 2>/dev/null || true

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

# Fingerprint of the compiler itself (tokenizer + parser + codegen + grammar +
# this driver). Mixed into the AST cache key so that editing the compiler
# invalidates every cached AST -- otherwise stale entries silently produce
# output from the old grammar/codegen. Computed once per process.
_compiler_version() {
    if [[ -z "${_COMPILER_VERSION:-}" ]]; then
        _COMPILER_VERSION=$(cat "$SCRIPT_DIR"/*.jq "$SCRIPT_DIR"/grammar/*.jq \
            "$TOKENIZER" "${BASH_SOURCE[0]}" 2>/dev/null \
            | shasum -a 256 | cut -d' ' -f1 | cut -c1-16)
    fi
    echo "$_COMPILER_VERSION"
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
# Helper: Collect inherited instance variables from parent classes
# ------------------------------------------------------------------------------

# Get the compiled file path for a qualified class name
# Args: $1=qualified_class_name (e.g., "Yutani::Widget" or "Object")
get_compiled_path() {
    local class_name="$1"
    local trashtalk_dir="${TRASHTALK_DIR:-$HOME/.trashtalk}"

    # Convert Yutani::Widget to Yutani__Widget
    local file_name="${class_name//::/__}"

    echo "$trashtalk_dir/trash/.compiled/$file_name"
}

# Extract instance variable names from a compiled class file
# Args: $1=compiled_file_path
# Returns: space-separated list of ivar names
extract_ivars_from_compiled() {
    local compiled_file="$1"

    if [[ ! -f "$compiled_file" ]]; then
        return
    fi

    # Look for __ClassName__instanceVars="var1: var2: var3:" line
    local ivars_line
    ivars_line=$(grep '__instanceVars=' "$compiled_file" | head -1)

    if [[ -z "$ivars_line" ]]; then
        return
    fi

    # Extract the quoted value and parse var names
    # Format: __Foo__instanceVars="var1: var2: var3:"
    local ivars_value
    ivars_value=$(echo "$ivars_line" | sed 's/.*__instanceVars="\([^"]*\)".*/\1/')

    # Convert "var1:default1 var2:default2" to "var1 var2"
    # Split on spaces, extract only the name part (before colon), rejoin
    echo "$ivars_value" | tr ' ' '\n' | sed 's/:.*$//' | tr '\n' ' ' | xargs
}

# Get parent class from a compiled file
# Args: $1=compiled_file_path
extract_parent_from_compiled() {
    local compiled_file="$1"

    if [[ ! -f "$compiled_file" ]]; then
        return
    fi

    # Look for __ClassName__superclass="ParentName" line
    local parent_line
    parent_line=$(grep '__superclass=' "$compiled_file" | head -1)

    if [[ -z "$parent_line" ]]; then
        return
    fi

    # Extract the parent class name
    echo "$parent_line" | sed 's/.*__superclass="\([^"]*\)".*/\1/'
}

# Recursively collect all inherited ivars from parent chain
# Args: $1=parent_class_name (qualified, e.g., "Yutani::Widget")
# Returns: JSON array of ivar names
collect_inherited_ivars() {
    local class_name="$1"
    local all_ivars=()

    # Walk up the inheritance chain
    while [[ -n "$class_name" && "$class_name" != "Object" ]]; do
        local compiled_path
        compiled_path=$(get_compiled_path "$class_name")

        if [[ ! -f "$compiled_path" ]]; then
            # Parent not compiled yet, stop here
            break
        fi

        # Get this class's ivars
        local ivars
        ivars=$(extract_ivars_from_compiled "$compiled_path")

        if [[ -n "$ivars" ]]; then
            for ivar in $ivars; do
                all_ivars+=("$ivar")
            done
        fi

        # Get parent and continue up the chain
        class_name=$(extract_parent_from_compiled "$compiled_path")
    done

    # Output as JSON array
    if [[ ${#all_ivars[@]} -eq 0 ]]; then
        echo "[]"
    else
        printf '%s\n' "${all_ivars[@]}" | jq -R . | jq -s .
    fi
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

# Internal: Parse a single .trash file to JSON AST (no trait merging)
# Uses content-hash caching to avoid re-tokenizing/parsing unchanged files.
_parse_single_file() {
    local source_file="$1"

    if [[ ! -f "$source_file" ]]; then
        error "Source file not found: $source_file"
    fi

    # Check AST cache by content hash. The key includes the compiler fingerprint
    # so a changed tokenizer/parser/codegen invalidates stale entries.
    local content_hash cache_file
    content_hash=$(shasum -a 256 "$source_file" | cut -d' ' -f1)
    cache_file="$AST_CACHE_DIR/$content_hash-$(_compiler_version).json"

    if [[ -f "$cache_file" ]]; then
        # Validate the cached entry; a truncated/corrupt cache (e.g. an
        # interrupted write) must not be fed into codegen. If invalid, fall
        # through and re-parse, overwriting the bad entry below.
        if jq -e . "$cache_file" >/dev/null 2>&1; then
            cat "$cache_file"
            return 0
        fi
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

    # Check for warnings (non-fatal by default). Under TRASHTALK_STRICT they are
    # promoted to errors so CI can reject malformed-but-recoverable input instead
    # of silently emitting code from a partial parse.
    if echo "$ast" | jq -e '.warnings | length > 0' >/dev/null 2>&1; then
        local warnings_json _warn_label="$YELLOW" _warn_word="warnings"
        if [[ -n "${TRASHTALK_STRICT:-}" ]]; then
            _warn_label="$RED"; _warn_word="warnings (strict mode)"
        fi
        echo -e "${_warn_label}Parse ${_warn_word} in ${source_file}:${NC}" >&2
        warnings_json=$(echo "$ast" | jq '.warnings')
        show_errors_with_context "$source_file" "$warnings_json" "$_warn_label"
        if [[ -n "${TRASHTALK_STRICT:-}" ]]; then
            exit 1
        fi
    fi

    # Cache the result for future compilations. Write to a temp file and rename
    # so a concurrent reader (or an interrupted write) never sees a partial entry.
    local cache_tmp="$cache_file.$$.tmp"
    if echo "$ast" > "$cache_tmp" 2>/dev/null; then
        mv -f "$cache_tmp" "$cache_file" 2>/dev/null || rm -f "$cache_tmp" 2>/dev/null
    else
        rm -f "$cache_tmp" 2>/dev/null || true
    fi

    echo "$ast"
}

# Parse a .trash file to CompilationUnit JSON (includes traits automatically)
# Outputs: { "class": {...}, "traits": {"TraitName": {...}, ...} }
cmd_parse() {
    local source_file="$1"
    local trashtalk_dir="${TRASHTALK_DIR:-$HOME/.trashtalk}"

    if [[ ! -f "$source_file" ]]; then
        error "Source file not found: $source_file"
    fi

    # Resolve where trait sources live. Search, in order: TRASHTALK_DIR, then
    # locations relative to the file being compiled (so building from a repo
    # checkout finds trash/traits/ without TRASHTALK_DIR pointing at it).
    local src_dir traits_dir="" cand
    src_dir=$(cd "$(dirname "$source_file")" && pwd)
    for cand in "$trashtalk_dir/trash/traits" "$src_dir/traits" "$src_dir/../traits"; do
        if [[ -d "$cand" ]]; then traits_dir="$cand"; break; fi
    done
    # Fall back to the TRASHTALK_DIR location for error messages if none existed.
    [[ -z "$traits_dir" ]] && traits_dir="$trashtalk_dir/trash/traits"

    # Parse the main class
    local class_ast
    class_ast=$(_parse_single_file "$source_file")
    if [[ $? -ne 0 ]]; then
        exit 1
    fi

    # Extract trait names
    local trait_names
    trait_names=$(echo "$class_ast" | jq -r '.traits[]? // empty')

    # Start building the CompilationUnit
    local traits_json="{}"

    # Parse each trait
    for trait_name in $trait_names; do
        local trait_file="$traits_dir/$trait_name.trash"
        if [[ -f "$trait_file" ]]; then
            local trait_ast
            trait_ast=$(_parse_single_file "$trait_file" 2>/dev/null)
            if [[ $? -eq 0 ]]; then
                # Add trait to the traits object
                # Pass trait AST via process substitution (--slurpfile) instead of
                # --argjson so large traits don't overflow ARG_MAX.
                traits_json=$(echo "$traits_json" | jq --arg name "$trait_name" --slurpfile ast <(printf '%s' "$trait_ast") '. + {($name): $ast[0]}')
            else
                echo "Warning: Failed to parse trait $trait_name from $trait_file" >&2
                [[ -n "${TRASHTALK_STRICT:-}" ]] && error "Trait '$trait_name' failed to parse (strict mode)"
            fi
        else
            echo "Warning: Trait '$trait_name' not found (searched: $traits_dir)" >&2
            [[ -n "${TRASHTALK_STRICT:-}" ]] && error "Trait '$trait_name' not found (strict mode)"
        fi
    done

    # Output the CompilationUnit. Use --slurpfile with process substitution rather
    # than --argjson: a large class/trait AST passed as an argv string overflows
    # ARG_MAX ("jq: Argument list too long") and silently breaks the build.
    jq -n --slurpfile class <(printf '%s' "$class_ast") --slurpfile traits <(printf '%s' "$traits_json") \
        '{ "class": $class[0], "traits": $traits[0] }'
}

# Alias for backwards compatibility
cmd_parse_with_traits() {
    cmd_parse "$@"
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

    # Compute source hash (SHA-256) for cache invalidation
    local source_hash
    source_hash=$(shasum -a 256 "$source_file" | cut -d' ' -f1)

    # Collect inherited instance variables from parent classes
    local parent_class inherited_ivars
    parent_class=$(echo "$ast" | jq -r '.class.parent // empty')
    if [[ -n "$parent_class" ]]; then
        inherited_ivars=$(collect_inherited_ivars "$parent_class")
    else
        inherited_ivars="[]"
    fi

    # Add source metadata and inherited ivars to AST
    local ast_with_source
    ast_with_source=$(echo "$ast" | jq --arg hash "$source_hash" --slurpfile inherited <(printf '%s' "$inherited_ivars") \
        'del(.warnings) | . + {sourceHash: $hash, inheritedInstanceVars: $inherited[0]}')

    # Generate code
    local output
    output=$(echo "$ast_with_source" | jq -r -f "$CODEGEN")

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

        parse-with-traits)
            if [[ $# -lt 1 ]]; then
                error "Missing source file"
            fi
            cmd_parse_with_traits "$1"
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
